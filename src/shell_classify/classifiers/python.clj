(ns shell-classify.classifiers.python
  "Per-language stdlib effect classifier for the Python interpreter
  dialect — Plan §7 Move 5 / P5.1.

  Consumes shell-shape's `:script :dialect :python` tree (produced by
  shell-shape.dialect.python) and emits effect-records with coordinate
  axes (`:module`, `:function`, `:program`) populated for the policy
  surface to predicate on.

  ---

  ## What this layer is responsible for

  Mapping individual `:py-call` / `:py-import` / `:py-from-import`
  nodes to effect-records. Two distinct cases:

  1. **Direct stdlib calls** — `os.remove(path)` → `:fs-delete`,
     `urllib.request.urlopen(url)` → `:net-out`, etc. The mapping
     table at [[call-effects]] enumerates the recognized callable
     paths.
  2. **Recursive shell descent** — `subprocess.run([...])` /
     `os.system(\"...\")` produce shell-shape `:command` records that
     recursively traverse through the existing
     `shell-classify.classify` pipeline. The shell-shape side has
     already synthesized `:spawned-commands` from list-literal argv;
     for string-form (`shell=True`, `os.system`), this ns parses the
     string via the bash dialect.

  ## What this layer is NOT responsible for

  - Long-tail third-party libraries (e.g. `boto3`, `flask`,
    `pandas`). They surface as `:py-call` records with no entry in
    [[call-effects]]; we emit an `:opaque :reason
    :unknown-effect-for-program` effect carrying the dotted callable
    path so the policy author can author a coordinate-axis grant.
  - Control-flow analysis. A call inside `if False: ...` emits the
    same effect as a top-level call (conservative — false positives
    over false negatives).
  - Name resolution beyond the local-binding table built from
    `:py-import` and `:py-from-import` statements. Indirect bindings
    (`f = os.remove; f(x)`) fall to `:opaque
    :reason :unknown-effect-for-program`.

  ## Data shapes

    Input:  `{:kind :script :dialect :python :nodes [...]}`
    Output: vector of effect-records, same shape classify.clj emits:

    ```
    {:class      <effect-class-kw>     ; :fs-read | :fs-delete | :net-out | :opaque | ...
     :scope      <scope-string>
     :provenance {:rule    :py-call
                  :module  <str>       ; \"os\"
                  :function <str>      ; \"os.remove\"
                  :program  \"python\"  ; canonical interpreter name
                  :reason   <kw>}      ; for :opaque only
     :module     <str>                 ; mirror of provenance for enrich
     :function   <str>                 ; mirror
    }
    ```

  The `:module` / `:function` fields at top-level mirror the
  provenance so `normalize/enrich-effect` lifts them into coordinate
  axes."
  (:require [clojure.string :as str]
            [shell-shape.core :as ss]))

;; ctx :classify-tree is threaded from shell-classify.classify —
;; using it (rather than a direct require) breaks the circular
;; dependency between classify ↔ classifiers/python.

;; ---- Helpers ----------------------------------------------------------

(defn- literal-string?
  "Argument record represents a single concrete string literal."
  [arg]
  (and (= :py-literal (:kind arg)) (string? (:value arg))))

(defn- arg-string
  "Extract the literal string value at index `i` in `args`, or nil."
  [args i]
  (let [a (nth args i nil)]
    (when (literal-string? a) (:value a))))

(defn- module-of
  "First segment of the dotted callable path — the module name. Returns
   the literal string (`\"os\"`) or nil if the callable is bare."
  [callable]
  (when (>= (count callable) 2) (first callable)))

(defn- function-of
  "Dotted form of the callable path — `[\"os\" \"remove\"]` →
   `\"os.remove\"`."
  [callable]
  (when (seq callable) (str/join "." callable)))

(defn- mk-effect
  "Construct an effect-record with coordinate axes set. `module` may be
   nil for builtin calls (`open`, `eval`). `extra-prov` merges into the
   provenance map."
  [class scope callable & {:keys [extra-prov reason]}]
  (let [module   (module-of callable)
        function (function-of callable)
        prov     (cond-> {:rule     :py-call
                          :program  "python"
                          :function function}
                   module (assoc :module module)
                   reason (assoc :reason reason))]
    (cond-> {:class      class
             :scope      (str scope)
             :provenance (merge prov extra-prov)}
      module   (assoc :module module)
      function (assoc :function function))))

(defn- mk-opaque
  [scope callable reason]
  (mk-effect :opaque scope callable :reason reason))

;; ---- Recursive shell descent -------------------------------------------

(defn- effects-from-spawned-commands
  "When the shell-shape python parser synthesized :spawned-commands
   from a list-literal argv, classify each as a normal shell-shape
   :command via ctx's :classify-tree (threaded from classify.clj to
   avoid a circular require). Propagates the :module/:function
   coordinates onto each emitted effect so the policy surface sees
   the python-side coordinates even on the shell-spawned effects."
  [ctx callable spawned]
  (let [classify-tree (or (:classify-tree ctx)
                          (throw (ex-info "ctx :classify-tree missing"
                                          {:status :wiring-error})))
        module        (module-of callable)
        function      (function-of callable)]
    (vec
     (for [cmd spawned
           eff (classify-tree ctx cmd)]
       (-> eff
           (assoc :module   module
                  :function function)
           (update :provenance
                   (fn [p]
                     (merge {:program "python"} p
                            (cond-> {:module module :function function
                                     :via    :python-spawn}
                              module (assoc :module module))))))))))

(defn- effects-from-shell-string
  "For os.system / subprocess.run(shell=True) etc., the argv is a
   single literal string we re-parse via shell-shape's bash dialect.
   Returns the effects of the embedded shell, propagating coordinates."
  [ctx callable shell-str]
  (let [classify-tree (or (:classify-tree ctx)
                          (throw (ex-info "ctx :classify-tree missing"
                                          {:status :wiring-error})))
        ss-parse (or (:ss-parse ctx) ss/parse)
        tree     (try (ss-parse shell-str) (catch Throwable _ nil))
        module   (module-of callable)
        function (function-of callable)]
    (if (nil? tree)
      [(mk-effect :opaque (str "shell-parse-error:" shell-str) callable
                  :reason :parse-error)]
      (vec
       (for [eff (classify-tree ctx tree)]
         (-> eff
             (assoc :module module :function function)
             (update :provenance
                     (fn [p]
                       (merge {:program "python"} p
                              (cond-> {:module   module
                                       :function function
                                       :via      :python-spawn-shell}
                                module (assoc :module module)))))))))))

;; ---- Effect mapping table ---------------------------------------------

(defn- path-effect
  "Builder factory: emit `class` whose `:scope` is the i-th positional
   string argument. If that arg isn't a literal, fall to
   `:opaque :variable-args`."
  [class arg-index]
  (fn [_ctx callable {:keys [args]}]
    (if-let [p (arg-string args arg-index)]
      [(mk-effect class p callable)]
      [(mk-opaque (str function-of callable ":variable-arg-" arg-index)
                  callable
                  :variable-args)])))

(defn- two-path-effect
  "Builder for fns like shutil.copy(src, dst): emit fs-read on src AND
   fs-write on dst."
  []
  (fn [_ctx callable {:keys [args]}]
    (let [src (arg-string args 0)
          dst (arg-string args 1)]
      (cond
        (and src dst)
        [(mk-effect :fs-read  src callable)
         (mk-effect :fs-write dst callable)]
        src
        [(mk-effect :fs-read  src callable)
         (mk-opaque (str (function-of callable) ":variable-dst")
                    callable :variable-args)]
        dst
        [(mk-opaque (str (function-of callable) ":variable-src")
                    callable :variable-args)
         (mk-effect :fs-write dst callable)]
        :else
        [(mk-opaque (str (function-of callable) ":both-variable")
                    callable :variable-args)]))))

(defn- spawn-list-or-shell
  "Builder for subprocess.run/.call/.Popen etc.: if `:spawned-commands`
   was populated by the shell-shape parser, descend; if first arg is a
   string literal AND shell=True, parse-as-shell; otherwise opaque
   :variable-args."
  []
  (fn [ctx callable {:keys [args kwargs spawned-commands]}]
    (cond
      (seq spawned-commands)
      (effects-from-spawned-commands ctx callable spawned-commands)

      ;; shell=True with string literal: parse via bash dialect.
      (and (= true (get-in kwargs ["shell" :value]))
           (literal-string? (first args)))
      (effects-from-shell-string ctx callable (:value (first args)))

      ;; First arg is a literal string but no shell=True flag: still a
      ;; common pattern in practice (subprocess auto-shell on string).
      (literal-string? (first args))
      (effects-from-shell-string ctx callable (:value (first args)))

      :else
      [(mk-opaque (str (function-of callable) ":variable-argv")
                  callable :variable-args)])))

(defn- os-system-like
  "Builder for os.system / os.popen / subprocess.getoutput — single
   string arg passed to the shell."
  []
  (fn [ctx callable {:keys [args]}]
    (if (literal-string? (first args))
      (effects-from-shell-string ctx callable (:value (first args)))
      [(mk-opaque (str (function-of callable) ":variable-shell-arg")
                  callable :variable-args)])))

(defn- net-out-effect
  "Builder for fns where i-th arg is the URL/host: emit :net-out scoped
   to that literal, or opaque if non-literal."
  [arg-index]
  (fn [_ctx callable {:keys [args]}]
    (if-let [url (arg-string args arg-index)]
      [(mk-effect :net-out url callable)]
      [(mk-opaque (str (function-of callable) ":variable-target")
                  callable :variable-args)])))

(defn- urlretrieve-builder
  "urllib.request.urlretrieve(url, filename) — :net-out + :fs-write
   when both arg literals are present."
  []
  (fn [_ctx callable {:keys [args]}]
    (let [url      (arg-string args 0)
          dst-path (arg-string args 1)]
      (cond-> []
        url      (conj (mk-effect :net-out  url callable))
        dst-path (conj (mk-effect :fs-write dst-path callable))
        (and (nil? url) (nil? dst-path))
        (conj (mk-opaque (str (function-of callable) ":both-variable")
                         callable :variable-args))))))

(defn- open-builder
  "open(path, [mode]) — fs-read by default, fs-write when mode contains
   'w'/'a'/'x'/'+'."
  []
  (fn [_ctx callable {:keys [args]}]
    (let [path (arg-string args 0)
          mode (or (arg-string args 1) "r")
          write? (some #(str/includes? mode (str %)) ["w" "a" "x" "+"])
          read?  (and (not write?) (not= mode ""))]
      (cond
        (nil? path)
        [(mk-opaque "open:variable-path" callable :variable-args)]
        write?
        [(mk-effect :fs-write path callable)]
        read?
        [(mk-effect :fs-read  path callable)]
        :else
        [(mk-effect :fs-read  path callable)]))))

(defn- eval-builder
  "eval/exec/compile — opaque :dynamic-eval. The arg can be a string
   literal but we don't dispatch into Python again (eval'd Python could
   itself eval — recursion bounded by classify pipeline's wrap-depth
   cap, but in practice we just deny here)."
  []
  (fn [_ctx callable {:keys [args]}]
    (let [s (arg-string args 0)]
      [(mk-opaque (or s (str (function-of callable) ":dynamic"))
                  callable :dynamic-eval)])))

(def call-effects
  "Map from dotted callable path → effect-builder fn.
   Builder: (fn [ctx callable py-call-record] → [effect-record ...])"
  {;; os module
   ["os" "remove"]   (path-effect :fs-delete 0)
   ["os" "unlink"]   (path-effect :fs-delete 0)
   ["os" "rmdir"]    (path-effect :fs-delete 0)
   ["os" "removedirs"] (path-effect :fs-delete 0)
   ["os" "mkdir"]    (path-effect :fs-write 0)
   ["os" "makedirs"] (path-effect :fs-write 0)
   ["os" "chmod"]    (path-effect :fs-write 0)
   ["os" "chown"]    (path-effect :fs-write 0)
   ["os" "lchown"]   (path-effect :fs-write 0)
   ["os" "chdir"]    (path-effect :fs-read 0)
   ["os" "listdir"]  (path-effect :fs-read 0)
   ["os" "stat"]     (path-effect :fs-read 0)
   ["os" "lstat"]    (path-effect :fs-read 0)
   ["os" "access"]   (path-effect :fs-read 0)
   ["os" "readlink"] (path-effect :fs-read 0)
   ["os" "scandir"]  (path-effect :fs-read 0)
   ["os" "walk"]     (path-effect :fs-read 0)
   ["os" "rename"]   (two-path-effect)
   ["os" "replace"]  (two-path-effect)
   ["os" "link"]     (two-path-effect)
   ["os" "symlink"]  (two-path-effect)
   ["os" "getenv"]
   (fn [_ctx callable {:keys [args]}]
     (let [k (or (arg-string args 0) "?")]
       [(mk-effect :env-read k callable)]))
   ["os" "putenv"]
   (fn [_ctx callable {:keys [args]}]
     (let [k (or (arg-string args 0) "?")]
       [(mk-effect :env-mutate k callable)]))
   ["os" "unsetenv"]
   (fn [_ctx callable {:keys [args]}]
     (let [k (or (arg-string args 0) "?")]
       [(mk-effect :env-mutate k callable)]))
   ["os" "system"]   (os-system-like)
   ["os" "popen"]    (os-system-like)
   ;; os.exec* and os.spawn* paths take argv as a list — handled by
   ;; shell-shape's :spawned-commands. Funnel through spawn-list-or-shell.
   ["os" "execv"]    (spawn-list-or-shell)
   ["os" "execve"]   (spawn-list-or-shell)
   ["os" "execvp"]   (spawn-list-or-shell)
   ["os" "execvpe"]  (spawn-list-or-shell)
   ["os" "execl"]    (spawn-list-or-shell)
   ["os" "execle"]   (spawn-list-or-shell)
   ["os" "execlp"]   (spawn-list-or-shell)
   ["os" "execlpe"]  (spawn-list-or-shell)
   ["os" "spawnv"]   (spawn-list-or-shell)
   ["os" "spawnve"]  (spawn-list-or-shell)
   ["os" "spawnvp"]  (spawn-list-or-shell)
   ["os" "spawnvpe"] (spawn-list-or-shell)
   ["os" "spawnl"]   (spawn-list-or-shell)
   ["os" "spawnle"]  (spawn-list-or-shell)
   ["os" "spawnlp"]  (spawn-list-or-shell)
   ["os" "spawnlpe"] (spawn-list-or-shell)

   ;; subprocess module
   ["subprocess" "run"]          (spawn-list-or-shell)
   ["subprocess" "call"]         (spawn-list-or-shell)
   ["subprocess" "check_call"]   (spawn-list-or-shell)
   ["subprocess" "check_output"] (spawn-list-or-shell)
   ["subprocess" "Popen"]        (spawn-list-or-shell)
   ["subprocess" "getoutput"]       (os-system-like)
   ["subprocess" "getstatusoutput"] (os-system-like)

   ;; shutil module
   ["shutil" "rmtree"]  (path-effect :fs-delete 0)
   ["shutil" "copy"]    (two-path-effect)
   ["shutil" "copy2"]   (two-path-effect)
   ["shutil" "copyfile"] (two-path-effect)
   ["shutil" "move"]    (two-path-effect)
   ["shutil" "make_archive"]
   (fn [_ctx callable {:keys [args]}]
     (let [base (arg-string args 0)]
       [(mk-effect :fs-write (str (or base "?") ".*") callable)]))

   ;; urllib.request
   ["urllib" "request" "urlopen"]     (net-out-effect 0)
   ["urllib" "request" "urlretrieve"] (urlretrieve-builder)

   ;; requests (3rd-party but ubiquitous)
   ["requests" "get"]    (net-out-effect 0)
   ["requests" "post"]   (net-out-effect 0)
   ["requests" "put"]    (net-out-effect 0)
   ["requests" "delete"] (net-out-effect 0)
   ["requests" "patch"]  (net-out-effect 0)
   ["requests" "head"]   (net-out-effect 0)
   ["requests" "request"] (net-out-effect 1)

   ;; http.client
   ["http" "client" "HTTPConnection"]  (net-out-effect 0)
   ["http" "client" "HTTPSConnection"] (net-out-effect 0)

   ;; socket — conservative: any socket() call emits :net-out at
   ;; create time. Sub-method connect/bind on the returned object
   ;; isn't tracked.
   ["socket" "socket"]
   (fn [_ctx callable _]
     [(mk-effect :net-out "socket-create" callable)])
   ["socket" "create_connection"] (net-out-effect 0)

   ;; builtins (no module qualifier)
   ["open"]    (open-builder)
   ["eval"]    (eval-builder)
   ["exec"]    (eval-builder)
   ["compile"] (eval-builder)
   ["__import__"]
   (fn [_ctx callable {:keys [args]}]
     (let [m (or (arg-string args 0) "?")]
       [(mk-effect :opaque (str "dynamic-import:" m) callable
                   :reason :dynamic-eval)]))})

;; ---- Binding table (import resolution) -------------------------------

(defn- build-binding-table
  "Pre-walk the script :nodes building a map of local name → resolved
   callable path. Two binding sources:

   - `import os as o`   → {\"o\"  [\"os\"]}
   - `from os import remove as rm` → {\"rm\" [\"os\" \"remove\"]}
   - `from os import remove`        → {\"remove\" [\"os\" \"remove\"]}
   - `import os`                    → no binding entry (callable stays
                                       dotted: `os.remove(...)`)

   Indirect bindings (`f = os.remove`) — handled separately via
   :py-assign records: a target → attr-path mapping joins this table."
  [nodes]
  (reduce
   (fn [acc node]
     (case (:kind node)
       :py-import
       (if-let [alias (:alias node)]
         (assoc acc alias [(:module node)])
         acc)
       :py-from-import
       (reduce
        (fn [a {:keys [name alias]}]
          (assoc a (or alias name) [(:module node) name]))
        acc
        (:names node))
       :py-assign
       (let [v (:value node)]
         (case (:kind v)
           :py-attr-access
           (assoc acc (:target node) (:path v))
           ;; Could extend to :py-var (alias chain) — kept minimal for now.
           acc))
       acc))
   {}
   nodes))

(defn- resolve-callable
  "Apply the binding table to a callable path. If the first segment is
   a binding key, substitute the bound prefix and concat the remaining
   segments."
  [bindings callable]
  (if-let [bound (get bindings (first callable))]
    (vec (concat bound (rest callable)))
    callable))

;; ---- Per-call effect derivation --------------------------------------

(defn- effects-of-call
  "Map a single :py-call record to effect-records via [[call-effects]].
   Unknown callables fall to :opaque :reason :unknown-effect-for-program
   carrying the dotted callable path in :scope."
  [ctx bindings call]
  (let [resolved (resolve-callable bindings (:callable call))]
    (if-let [builder (get call-effects resolved)]
      (builder ctx resolved call)
      [(mk-opaque (str "py-call:" (function-of resolved))
                  resolved :unknown-effect-for-program)])))

(defn classify-python-script
  "Walk a `:script :dialect :python` tree, emit effect-records.

   Two-pass: pass 1 builds the binding table from imports + assignments;
   pass 2 walks :py-call records resolving callables and dispatching
   through [[call-effects]]."
  ([script] (classify-python-script {} script))
  ([ctx script]
   (let [nodes    (:nodes script)
         bindings (build-binding-table nodes)
         calls    (filter #(= :py-call (:kind %)) nodes)]
     (vec (mapcat #(effects-of-call ctx bindings %) calls)))))
