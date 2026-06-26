(ns shell-classify.classifiers.perl
  "Per-language stdlib effect classifier for the Perl interpreter
  dialect — Plan §7 Move 5 / P5.4.

  Consumes shell-shape's `:script :dialect :perl` tree (produced by
  shell-shape.dialect.perl) and emits effect-records with coordinate
  axes populated.

  ## Module surface

  Perl callable resolution is by fully-qualified package path
  (`File::Path::rmtree`). `use Foo::Bar qw(name1 name2);` makes
  `name1` callable bare, but in practice Perl scripts often invoke
  with the fully-qualified path. Both are supported via the
  use-imports binding table.

  ## Cross-dialect descent

  - `system 'cmd'` / `system('cmd')` — single-string descends via
    the bash dialect.
  - `system 'rm', '-rf', '/x'` — multi-arg literal list form;
    shell-shape pre-synthesizes :spawned-commands when callable
    matches `[\"system\"]` / `[\"exec\"]`.
  - `` `cmd` `` and `qx{cmd}` — surface as :pl-call with callable
    `[\":backtick\"]` whose first arg is the literal body string,
    classified through the shell-string descent path.
  - `open(\\$fh, '|-', 'cmd')` — pipe-open form. Not yet classified
    — falls to :opaque :unknown-effect-for-program."
  (:require [clojure.string :as str]
            [shell-shape.core :as ss]))

;; ---- Helpers ----------------------------------------------------------

(defn- literal-string?
  [arg]
  (and (= :pl-literal (:kind arg)) (string? (:value arg))))

(defn- arg-string
  [args i]
  (let [a (nth args i nil)]
    (when (literal-string? a) (:value a))))

(defn- module-of [callable]
  (when (>= (count callable) 2) (str/join "::" (butlast callable))))

(defn- function-of [callable]
  (when (seq callable) (str/join "::" callable)))

(defn- mk-effect
  [class scope callable & {:keys [extra-prov reason]}]
  (let [module   (module-of callable)
        function (function-of callable)
        prov     (cond-> {:rule     :pl-call
                          :program  "perl"
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
                   #(merge {:program "perl"} %
                           {:module module :function function
                            :via :perl-spawn})))))))

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
                     #(merge {:program "perl"} %
                             {:module module :function function
                              :via :perl-spawn-shell}))))))))

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
        :else
        [(mk-opaque (str (function-of callable) ":variable-args")
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
  (and (seq args) (every? literal-string? args)))

(defn- spawn-shell-or-list
  []
  (fn [ctx callable {:keys [args spawned-commands]}]
    (cond
      (seq spawned-commands)
      (effects-from-spawned ctx callable spawned-commands)

      (and (>= (count args) 2) (all-literal-strings? args))
      (effects-from-spawned ctx callable
                            [(synth-cmd-from-argv (mapv :value args))])

      (literal-string? (first args))
      (effects-from-shell-string ctx callable (:value (first args)))

      ;; Backtick / qx surfaces as :pl-call callable [":backtick"]
      ;; with the body string as the first arg — handled above.
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

(defn- open-effect
  "Perl's `open` is polymorphic. Signatures:
    open(FH, '<', '/path')       → fs-read
    open(FH, '>', '/path')       → fs-write
    open(FH, '>>', '/path')      → fs-write (append)
    open(FH, 'cmd |')            → pipe-from-cmd (opaque, deferred)
    open(FH, '| cmd')            → pipe-to-cmd (opaque, deferred)
    open(FH, '/path')            → fs-read (2-arg form, default read)
  Handles 3-arg-with-mode-literal and 2-arg path forms.
  Pipe-open forms are :opaque :unknown-effect-for-program."
  []
  (fn [_ctx callable {:keys [args]}]
    (let [mode (or (arg-string args 1) "<")
          path-2arg (when (= 2 (count args)) (arg-string args 1))
          path-3arg (when (>= (count args) 3) (arg-string args 2))]
      (cond
        ;; 3-arg form with a path
        path-3arg
        (cond
          (or (str/includes? mode ">") (str/includes? mode "+"))
          [(mk-effect :fs-write path-3arg callable)]
          (re-find #"\|" mode)
          [(mk-opaque (str "open:pipe-open:" mode) callable
                      :unknown-effect-for-program)]
          :else
          [(mk-effect :fs-read path-3arg callable)])
        ;; 2-arg form
        path-2arg
        [(mk-effect :fs-read path-2arg callable)]
        :else
        [(mk-opaque "open:variable-args" callable :variable-args)]))))

(defn- eval-builder
  []
  (fn [_ctx callable {:keys [args]}]
    (let [s (arg-string args 0)]
      [(mk-opaque (or s (str (function-of callable) ":dynamic"))
                  callable :dynamic-eval)])))

;; ---- Effect mapping table --------------------------------------------

(def call-effects
  "Map from callable path → effect-builder fn."
  {;; Core file builtins
   ["unlink"]   (path-effect :fs-delete 0)
   ["rmdir"]    (path-effect :fs-delete 0)
   ["mkdir"]    (path-effect :fs-write  0)
   ["chmod"]
   (fn [_ctx callable {:keys [args]}]
     ;; chmod MODE, FILE -- the path is arg 1
     (let [p (arg-string args 1)]
       (if p
         [(mk-effect :fs-write p callable)]
         [(mk-opaque "chmod:variable-path" callable :variable-args)])))
   ["chown"]
   (fn [_ctx callable {:keys [args]}]
     ;; chown UID, GID, FILE -- the path is arg 2
     (let [p (arg-string args 2)]
       (if p
         [(mk-effect :fs-write p callable)]
         [(mk-opaque "chown:variable-path" callable :variable-args)])))
   ["truncate"] (path-effect :fs-write 0)
   ["rename"]   (two-path-effect)
   ["link"]     (two-path-effect)
   ["symlink"]  (two-path-effect)
   ["open"]     (open-effect)
   ["sysopen"]  (open-effect)
   ["close"]
   (fn [_ctx callable _]
     [(mk-effect :fs-read "fh-close" callable)])

   ;; Process / shell
   ["system"]   (spawn-shell-or-list)
   ["exec"]     (spawn-shell-or-list)
   ["fork"]
   (fn [_ctx callable _] [(mk-effect :proc-spawn "fork" callable)])
   ["kill"]
   (fn [_ctx callable {:keys [args]}]
     (let [tgt (or (arg-string args 1) "?")]
       [(mk-effect :proc-signal (str "kill:" tgt) callable)]))
   ["wait"]
   (fn [_ctx callable _] [(mk-effect :proc-spawn "wait" callable)])
   [":backtick"] (spawn-shell-or-list)

   ;; eval / dynamic
   ["eval"]    (eval-builder)

   ;; File::Path
   ["File" "Path" "rmtree"]       (path-effect :fs-delete 0)
   ["File" "Path" "remove_tree"]  (path-effect :fs-delete 0)
   ["File" "Path" "mkpath"]       (path-effect :fs-write  0)
   ["File" "Path" "make_path"]    (path-effect :fs-write  0)
   ;; Bare bound via `use File::Path qw(rmtree mkpath)`:
   ["rmtree"]      (path-effect :fs-delete 0)
   ["remove_tree"] (path-effect :fs-delete 0)
   ["mkpath"]      (path-effect :fs-write  0)
   ["make_path"]   (path-effect :fs-write  0)

   ;; File::Copy
   ["File" "Copy" "copy"]  (two-path-effect)
   ["File" "Copy" "move"]  (two-path-effect)
   ["File" "Copy" "cp"]    (two-path-effect)
   ["File" "Copy" "mv"]    (two-path-effect)
   ["copy"]                (two-path-effect)
   ["move"]                (two-path-effect)

   ;; File::Slurp
   ["File" "Slurp" "read_file"]   (path-effect :fs-read  0)
   ["File" "Slurp" "write_file"]  (path-effect :fs-write 0)
   ["File" "Slurp" "append_file"] (path-effect :fs-write 0)
   ["read_file"]   (path-effect :fs-read  0)
   ["write_file"]  (path-effect :fs-write 0)
   ["append_file"] (path-effect :fs-write 0)

   ;; LWP / HTTP
   ["LWP" "Simple" "get"]     (net-out-effect 0)
   ["LWP" "Simple" "getstore"]
   (fn [_ctx callable {:keys [args]}]
     (let [url (arg-string args 0)
           dst (arg-string args 1)]
       (cond-> []
         url (conj (mk-effect :net-out url callable))
         dst (conj (mk-effect :fs-write dst callable))
         (and (nil? url) (nil? dst))
         (conj (mk-opaque (str (function-of callable) ":variable-args")
                          callable :variable-args)))))
   ["LWP" "Simple" "head"]    (net-out-effect 0)
   ["LWP" "Simple" "mirror"]  (net-out-effect 0)
   ;; Bare bound via `use LWP::Simple qw(get)`:
   ["get"]      (net-out-effect 0)
   ["getstore"] (net-out-effect 0)
   ["mirror"]   (net-out-effect 0)

   ;; IPC::Open3
   ["IPC" "Open3" "open3"] (spawn-shell-or-list)
   ["open3"]               (spawn-shell-or-list)

   ;; Net::HTTP / HTTP::Tiny
   ["HTTP" "Tiny" "new"]
   (fn [_ctx callable _] [(mk-opaque "http-tiny-new" callable nil)])
   ["Net" "HTTP" "new"]
   (fn [_ctx callable {:keys [args]}]
     (let [host (or (arg-string args 0) "?")]
       [(mk-effect :net-out host callable)]))})

;; ---- Use-imports binding table (qw imports) --------------------------

(defn- build-import-table
  "`use Foo::Bar qw(name1 name2);` makes `name1` callable bare. The
   table maps bare names → fully-qualified path so resolve-callable
   can match into the registry."
  [nodes]
  (reduce
   (fn [acc node]
     (if (and (= :pl-use (:kind node)) (seq (:imports node)))
       (let [module-parts (str/split (or (:module node) "") #"::")]
         (reduce (fn [m name]
                   (assoc m name (vec (concat module-parts [name]))))
                 acc
                 (:imports node)))
       acc))
   {}
   nodes))

(defn- resolve-callable
  [bindings callable]
  ;; Only resolve when callable is single-segment (bare); a multi-
  ;; segment callable is already fully qualified.
  (if (and (= 1 (count callable))
           (contains? bindings (first callable)))
    (get bindings (first callable))
    callable))

;; ---- Public entry ----------------------------------------------------

(defn- effects-of-call
  [ctx bindings call]
  (let [resolved (resolve-callable bindings (:callable call))]
    (if-let [builder (get call-effects resolved)]
      (builder ctx resolved call)
      ;; Fall back to literal callable lookup before opaque.
      (if-let [builder (get call-effects (:callable call))]
        (builder ctx (:callable call) call)
        [(mk-opaque (str "pl-call:" (function-of resolved))
                    resolved :unknown-effect-for-program)]))))

(defn classify-perl-script
  ([script] (classify-perl-script {} script))
  ([ctx script]
   (let [nodes    (:nodes script)
         bindings (build-import-table nodes)
         calls    (filter #(= :pl-call (:kind %)) nodes)]
     (vec (mapcat #(effects-of-call ctx bindings %) calls)))))
