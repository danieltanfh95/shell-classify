(ns shell-classify.classifiers.ruby
  "Per-language stdlib effect classifier for the Ruby interpreter
  dialect — Plan §7 Move 5 / P5.3.

  Consumes shell-shape's `:script :dialect :ruby` tree (produced by
  shell-shape.dialect.ruby) and emits effect-records with coordinate
  axes populated.

  Mirrors the Python / Node classifier structure: a `call-effects`
  registry maps dotted callable paths (receiver path + method name)
  to effect-builder fns. Ruby has no binding-table aliasing of
  modules at the call surface (unlike Python's `import os as o` or
  Node's destructured require), so resolution is direct.

  ## Recursive shell descent

  Ruby's shell-spawning surface:
  - `system('rm -rf /tmp/x')` / `exec(...)` / `spawn(...)` —
    single-string form parses via the bash dialect.
  - `system('rm', '-rf', '/tmp/x')` / `Process.spawn(prog, *args)` —
    list form; shell-shape pre-synthesizes :spawned-commands.
  - `IO.popen('cmd')` / `Open3.capture3('cmd')` — string form.

  ## Module-loading surface

  Ruby's `require` / `require_relative` produce :rb-require records
  that don't get classified into effects directly here (they're
  scope-of-the-script metadata, not OS effects). A dynamic
  `require(some_var)` would surface as a :rb-call with callable
  `[\"require\"]` and is mapped to :dynamic-eval.

  ## Coordinates

  Every emitted effect carries :module / :function / :program \"ruby\"
  on both the top-level fields and the :provenance map."
  (:require [clojure.string :as str]
            [shell-shape.core :as ss]))

;; ---- Helpers ----------------------------------------------------------

(defn- literal-string?
  [arg]
  (and (= :rb-literal (:kind arg)) (string? (:value arg))))

(defn- arg-string
  [args i]
  (let [a (nth args i nil)]
    (when (literal-string? a) (:value a))))

(defn- module-of [callable]
  (when (>= (count callable) 2) (first callable)))

(defn- function-of [callable]
  (when (seq callable) (str/join "." callable)))

(defn- mk-effect
  [class scope callable & {:keys [extra-prov reason]}]
  (let [module   (module-of callable)
        function (function-of callable)
        prov     (cond-> {:rule     :rb-call
                          :program  "ruby"
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

;; ---- Recursive shell descent -----------------------------------------

(defn- effects-from-spawned
  [ctx callable spawned]
  (let [classify-tree (or (:classify-tree ctx)
                          (throw (ex-info "ctx :classify-tree missing"
                                          {:status :wiring-error})))
        module   (module-of callable)
        function (function-of callable)]
    (vec
     (for [cmd spawned
           eff (classify-tree ctx cmd)]
       (-> eff
           (assoc :module module :function function)
           (update :provenance
                   #(merge {:program "ruby"} %
                           {:module module :function function
                            :via :ruby-spawn})))))))

(defn- effects-from-shell-string
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
                     #(merge {:program "ruby"} %
                             {:module module :function function
                              :via :ruby-spawn-shell}))))))))

;; ---- Effect builders -------------------------------------------------

(defn- path-effect
  [class arg-index]
  (fn [_ctx callable {:keys [args]}]
    (if-let [p (arg-string args arg-index)]
      [(mk-effect class p callable)]
      [(mk-opaque (str (function-of callable) ":variable-arg-" arg-index)
                  callable :variable-args)])))

(defn- two-path-effect
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

(defn- synth-cmd-from-argv
  [argv-strs]
  (let [[prog & rest-strs] argv-strs
        mk-tok (fn [s] {:kind :token :literal s
                        :parts [{:kind :literal :value s}]
                        :raw s :offset 0})]
    {:kind :command :program prog
     :args (mapv mk-tok rest-strs)
     :invokes [] :redirects []
     :raw (str/join " " argv-strs)}))

(defn- all-literal-strings?
  [args]
  (and (seq args)
       (every? literal-string? args)))

(defn- spawn-shell-or-list
  "Builder for `system / exec / spawn / IO.popen / Open3.popen3` family
   where a single string-arg form is treated as a shell string and a
   multi-arg-of-literals form is the list form (shell-shape
   pre-synthesizes :spawned-commands for the list form when receiver
   path matches its hard-coded set)."
  []
  (fn [ctx callable {:keys [args spawned-commands]}]
    (cond
      (seq spawned-commands)
      (effects-from-spawned ctx callable spawned-commands)

      ;; Multi-arg list form whose elements are all literal strings —
      ;; synthesize a :command and descend (covers the
      ;; binding-receiver-mismatch case Node had).
      (and (>= (count args) 2) (all-literal-strings? args))
      (let [argv (mapv :value args)]
        (effects-from-spawned ctx callable [(synth-cmd-from-argv argv)]))

      ;; Single-string form: parse as bash.
      (literal-string? (first args))
      (effects-from-shell-string ctx callable (:value (first args)))

      :else
      [(mk-opaque (str (function-of callable) ":variable-argv")
                  callable :variable-args)])))

(defn- net-out-effect
  [arg-index]
  (fn [_ctx callable {:keys [args]}]
    (if-let [target (arg-string args arg-index)]
      [(mk-effect :net-out target callable)]
      [(mk-opaque (str (function-of callable) ":variable-target")
                  callable :variable-args)])))

(defn- open-file-by-mode
  "File.open(path, mode='r') — :fs-read by default, :fs-write when
   mode contains w/a/+."
  []
  (fn [_ctx callable {:keys [args]}]
    (let [path (arg-string args 0)
          mode (or (arg-string args 1) "r")
          write? (some #(str/includes? mode (str %)) ["w" "a" "+"])]
      (cond
        (nil? path)
        [(mk-opaque "File.open:variable-path" callable :variable-args)]
        write?
        [(mk-effect :fs-write path callable)]
        :else
        [(mk-effect :fs-read path callable)]))))

(defn- eval-builder
  []
  (fn [_ctx callable {:keys [args]}]
    (let [s (arg-string args 0)]
      [(mk-opaque (or s (str (function-of callable) ":dynamic"))
                  callable :dynamic-eval)])))

(defn- load-builder
  "Kernel.load(file) — :fs-read on the file path (loads ruby source);
   :dynamic-eval as a secondary effect since arbitrary ruby executes."
  []
  (fn [_ctx callable {:keys [args]}]
    (let [path (arg-string args 0)]
      (if path
        [(mk-effect :fs-read path callable)
         (mk-opaque (str "load:" path) callable :dynamic-eval)]
        [(mk-opaque "load:variable-path" callable :variable-args)]))))

;; ---- Effect mapping table --------------------------------------------

(def call-effects
  "Map from callable path → effect-builder fn.
   Builder: (fn [ctx callable rb-call-record] → [effect-record ...])"
  {;; File class
   ["File" "delete"]   (path-effect :fs-delete 0)
   ["File" "unlink"]   (path-effect :fs-delete 0)
   ["File" "read"]     (path-effect :fs-read   0)
   ["File" "binread"]  (path-effect :fs-read   0)
   ["File" "readlines"](path-effect :fs-read   0)
   ["File" "size"]     (path-effect :fs-read   0)
   ["File" "stat"]     (path-effect :fs-read   0)
   ["File" "lstat"]    (path-effect :fs-read   0)
   ["File" "exist?"]   (path-effect :fs-read   0)
   ["File" "exists?"]  (path-effect :fs-read   0)
   ["File" "readlink"] (path-effect :fs-read   0)
   ["File" "write"]    (path-effect :fs-write  0)
   ["File" "binwrite"] (path-effect :fs-write  0)
   ["File" "chmod"]
   (fn [_ctx callable {:keys [args]}]
     (let [path (arg-string args 1)]
       (if path
         [(mk-effect :fs-write path callable)]
         [(mk-opaque "File.chmod:variable-path" callable :variable-args)])))
   ["File" "chown"]
   (fn [_ctx callable {:keys [args]}]
     (let [path (arg-string args 2)]
       (if path
         [(mk-effect :fs-write path callable)]
         [(mk-opaque "File.chown:variable-path" callable :variable-args)])))
   ["File" "rename"]   (two-path-effect)
   ["File" "link"]     (two-path-effect)
   ["File" "symlink"]  (two-path-effect)
   ["File" "open"]     (open-file-by-mode)
   ["File" "new"]      (open-file-by-mode)
   ["File" "truncate"] (path-effect :fs-write 0)

   ;; Dir class
   ["Dir" "mkdir"]    (path-effect :fs-write  0)
   ["Dir" "rmdir"]    (path-effect :fs-delete 0)
   ["Dir" "delete"]   (path-effect :fs-delete 0)
   ["Dir" "unlink"]   (path-effect :fs-delete 0)
   ["Dir" "entries"]  (path-effect :fs-read   0)
   ["Dir" "glob"]     (path-effect :fs-read   0)
   ["Dir" "[]"]       (path-effect :fs-read   0)
   ["Dir" "children"] (path-effect :fs-read   0)
   ["Dir" "open"]     (path-effect :fs-read   0)
   ["Dir" "chdir"]    (path-effect :fs-read   0)
   ["Dir" "foreach"]  (path-effect :fs-read   0)

   ;; FileUtils module
   ["FileUtils" "rm"]        (path-effect :fs-delete 0)
   ["FileUtils" "rm_f"]      (path-effect :fs-delete 0)
   ["FileUtils" "rm_r"]      (path-effect :fs-delete 0)
   ["FileUtils" "rm_rf"]     (path-effect :fs-delete 0)
   ["FileUtils" "remove"]    (path-effect :fs-delete 0)
   ["FileUtils" "remove_dir"](path-effect :fs-delete 0)
   ["FileUtils" "remove_entry"](path-effect :fs-delete 0)
   ["FileUtils" "remove_file"](path-effect :fs-delete 0)
   ["FileUtils" "mkdir"]     (path-effect :fs-write 0)
   ["FileUtils" "mkdir_p"]   (path-effect :fs-write 0)
   ["FileUtils" "makedirs"]  (path-effect :fs-write 0)
   ["FileUtils" "touch"]     (path-effect :fs-write 0)
   ["FileUtils" "chmod"]
   (fn [_ctx callable {:keys [args]}]
     (let [path (arg-string args 1)]
       (if path
         [(mk-effect :fs-write path callable)]
         [(mk-opaque "FileUtils.chmod:variable-path" callable :variable-args)])))
   ["FileUtils" "chown"]
   (fn [_ctx callable {:keys [args]}]
     (let [path (arg-string args 2)]
       (if path
         [(mk-effect :fs-write path callable)]
         [(mk-opaque "FileUtils.chown:variable-path" callable :variable-args)])))
   ["FileUtils" "cp"]    (two-path-effect)
   ["FileUtils" "cp_r"]  (two-path-effect)
   ["FileUtils" "copy"]  (two-path-effect)
   ["FileUtils" "mv"]    (two-path-effect)
   ["FileUtils" "move"]  (two-path-effect)
   ["FileUtils" "ln"]    (two-path-effect)
   ["FileUtils" "ln_s"]  (two-path-effect)
   ["FileUtils" "ln_sf"] (two-path-effect)

   ;; IO class — both file I/O AND shell I/O via popen
   ["IO" "read"]     (path-effect :fs-read  0)
   ["IO" "binread"]  (path-effect :fs-read  0)
   ["IO" "readlines"](path-effect :fs-read  0)
   ["IO" "write"]    (path-effect :fs-write 0)
   ["IO" "binwrite"] (path-effect :fs-write 0)
   ["IO" "popen"]    (spawn-shell-or-list)
   ["IO" "foreach"]  (path-effect :fs-read 0)

   ;; Process module
   ["Process" "spawn"] (spawn-shell-or-list)
   ["Process" "exec"]  (spawn-shell-or-list)
   ["Process" "fork"]
   (fn [_ctx callable _] [(mk-effect :proc-spawn "fork" callable)])
   ["Process" "kill"]
   (fn [_ctx callable {:keys [args]}]
     (let [tgt (or (arg-string args 1) "?")]
       [(mk-effect :proc-signal (str "kill:" tgt) callable)]))

   ;; Kernel + bare global methods
   ["Kernel" "system"]  (spawn-shell-or-list)
   ["Kernel" "exec"]    (spawn-shell-or-list)
   ["Kernel" "spawn"]   (spawn-shell-or-list)
   ["system"]           (spawn-shell-or-list)
   ["exec"]             (spawn-shell-or-list)
   ["spawn"]            (spawn-shell-or-list)
   ["`"]                (spawn-shell-or-list)   ; backtick literal
   ["Kernel" "eval"]    (eval-builder)
   ["eval"]             (eval-builder)
   ["Kernel" "load"]    (load-builder)
   ["load"]             (load-builder)
   ["instance_eval"]    (eval-builder)
   ["class_eval"]       (eval-builder)
   ["module_eval"]      (eval-builder)

   ;; Net::HTTP
   ["Net" "HTTP" "get"]        (net-out-effect 0)
   ["Net" "HTTP" "get_response"] (net-out-effect 0)
   ["Net" "HTTP" "post"]       (net-out-effect 0)
   ["Net" "HTTP" "post_form"]  (net-out-effect 0)
   ["Net" "HTTP" "put"]        (net-out-effect 0)
   ["Net" "HTTP" "delete"]     (net-out-effect 0)
   ["Net" "HTTP" "head"]       (net-out-effect 0)
   ["Net" "HTTP" "patch"]      (net-out-effect 0)
   ["Net" "HTTP" "start"]      (net-out-effect 0)
   ["Net" "HTTP" "new"]        (net-out-effect 0)

   ;; OpenURI / URI
   ["URI" "open"]   (net-out-effect 0)
   ["URI" "parse"]
   (fn [_ctx callable {:keys [args]}]
     (let [u (or (arg-string args 0) "?")]
       [(mk-effect :net-out u callable
                   :extra-prov {:via :uri-parse})]))
   ["open"]  ; Ruby's Kernel#open — file OR URL (open-uri)
   (fn [ctx callable {:keys [args] :as call}]
     (let [target (arg-string args 0)]
       (cond
         (nil? target)
         [(mk-opaque "open:variable-target" callable :variable-args)]
         (or (str/starts-with? target "http://")
             (str/starts-with? target "https://")
             (str/starts-with? target "ftp://"))
         [(mk-effect :net-out target callable)]
         :else
         ((open-file-by-mode) ctx callable call))))

   ;; Open3
   ["Open3" "capture3"]  (spawn-shell-or-list)
   ["Open3" "capture2"]  (spawn-shell-or-list)
   ["Open3" "capture2e"] (spawn-shell-or-list)
   ["Open3" "popen3"]    (spawn-shell-or-list)
   ["Open3" "popen2"]    (spawn-shell-or-list)
   ["Open3" "popen2e"]   (spawn-shell-or-list)
   ["Open3" "pipeline"]  (spawn-shell-or-list)})

;; ---- Public entry ----------------------------------------------------

(defn- effects-of-call
  [ctx call]
  (let [callable (:callable call)]
    (if-let [builder (get call-effects callable)]
      (builder ctx callable call)
      [(mk-opaque (str "rb-call:" (function-of callable))
                  callable :unknown-effect-for-program)])))

(defn classify-ruby-script
  ([script] (classify-ruby-script {} script))
  ([ctx script]
   (let [nodes (:nodes script)
         calls (filter #(= :rb-call (:kind %)) nodes)]
     (vec (mapcat #(effects-of-call ctx %) calls)))))
