(ns shell-classify.classifiers.node
  "Per-language stdlib effect classifier for the Node.js interpreter
  dialect — Plan §7 Move 5 / P5.2.

  Consumes shell-shape's `:script :dialect :node` tree (produced by
  shell-shape.dialect.node v0.5.0+) and emits effect-records with
  coordinate axes populated.

  ---

  ## What this layer handles

  Node's CommonJS + ESM module systems both surface through the same
  :js-require / :js-import records on the shell-shape side. The
  classifier:

  1. Builds a binding table from imports + requires (handles
     destructuring: `const { unlinkSync } = require('fs')` binds
     `unlinkSync → ['fs', 'unlinkSync']`).
  2. Walks :js-call records, resolves their callable through the
     binding table.
  3. Maps recognized stdlib paths through the [[call-effects]]
     registry to effect-records.
  4. For unrecognized paths, emits `:opaque
     :reason :unknown-effect-for-program` carrying the dotted callable.

  ## Recursive shell descent

  - `child_process.execFileSync('git', ['push', '--force'])` —
    shell-shape's parser populated :spawned-commands; we descend
    via ctx :classify-tree.
  - `child_process.execSync('rm -rf /tmp/x')` — string arg, parsed
    via shell-shape's bash dialect.

  ## Effect coordinates

  Every emitted effect carries:
    :module     (e.g. \"fs\", \"child_process\", \"https\")
    :function   (e.g. \"fs.unlinkSync\", \"child_process.execFileSync\")
    :provenance :program \"node\""
  (:require [clojure.string :as str]
            [shell-shape.core :as ss]))

;; ---- Helpers ----------------------------------------------------------

(defn- literal-string?
  [arg]
  (and (= :js-literal (:kind arg)) (string? (:value arg))))

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
        prov     (cond-> {:rule     :js-call
                          :program  "node"
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
                   #(merge {:program "node"} %
                           {:module module :function function
                            :via :node-spawn})))))))

(defn- effects-from-shell-string
  [ctx callable shell-str]
  (let [classify-tree (or (:classify-tree ctx)
                          (throw (ex-info "ctx :classify-tree missing"
                                          {:status :wiring-error})))
        tree     (try (ss/parse shell-str) (catch Throwable _ nil))
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
                     #(merge {:program "node"} %
                             {:module module :function function
                              :via :node-spawn-shell}))))))))

;; ---- Effect builders -------------------------------------------------

(defn- path-effect
  [class arg-index]
  (fn [_ctx callable {:keys [args]}]
    (if-let [p (arg-string args arg-index)]
      [(mk-effect class p callable)]
      [(mk-opaque (str (function-of callable) ":variable-arg") callable
                  :variable-args)])))

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

(defn- array-of-literals?
  [arg]
  (and (= :js-array (:kind arg))
       (every? #(and (= :js-literal (:kind %)) (string? (:value %)))
               (:items arg))))

(defn- synth-cmd-from-argv
  "Build a minimal shell-shape :command from `argv-strs` so the
   witness's bash classifier can walk it."
  [argv-strs]
  (let [[prog & rest-strs] argv-strs
        mk-tok (fn [s] {:kind :token :literal s
                        :parts [{:kind :literal :value s}]
                        :raw s :offset 0})]
    {:kind :command :program prog
     :args (mapv mk-tok rest-strs)
     :invokes [] :redirects []
     :raw (str/join " " argv-strs)}))

(defn- exec-list-form
  "child_process.execFileSync / spawnSync / spawn(file, args, ...).
   Use :spawned-commands when shell-shape populated it (matches when
   the original callable was the full `child_process.X` path); fall
   back to synthesizing here when the binding-table resolution at the
   witness side is what produced the match (e.g. `cp.execFileSync`
   under a `const cp = require('child_process')` binding)."
  []
  (fn [ctx callable {:keys [args spawned-commands]}]
    (cond
      (seq spawned-commands)
      (effects-from-spawned ctx callable spawned-commands)

      ;; Synth here when shell-shape couldn't: first arg is literal
      ;; program, second is array-of-literals args.
      (and (literal-string? (first args)) (array-of-literals? (second args)))
      (let [argv (cons (:value (first args))
                       (mapv :value (:items (second args))))]
        (effects-from-spawned ctx callable [(synth-cmd-from-argv argv)]))

      (literal-string? (first args))
      [(mk-effect :proc-spawn (:value (first args)) callable)]

      :else
      [(mk-opaque (str (function-of callable) ":variable-argv")
                  callable :variable-args)])))

(defn- exec-shell-form
  "child_process.execSync / exec(cmd-string)."
  []
  (fn [ctx callable {:keys [args]}]
    (if (literal-string? (first args))
      (effects-from-shell-string ctx callable (:value (first args)))
      [(mk-opaque (str (function-of callable) ":variable-shell-arg")
                  callable :variable-args)])))

(defn- net-out-effect
  [arg-index]
  (fn [_ctx callable {:keys [args]}]
    (if-let [url (arg-string args arg-index)]
      [(mk-effect :net-out url callable)]
      [(mk-opaque (str (function-of callable) ":variable-target")
                  callable :variable-args)])))

(defn- open-file-by-flags
  "fs.openSync / fs.open(path, flags) — fs-write when flags includes
   w/a/+, else fs-read."
  []
  (fn [_ctx callable {:keys [args]}]
    (let [path  (arg-string args 0)
          flags (or (arg-string args 1) "r")
          write? (some #(str/includes? flags (str %)) ["w" "a" "+"])]
      (cond
        (nil? path)
        [(mk-opaque "fs.open:variable-path" callable :variable-args)]
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

;; ---- Effect mapping table --------------------------------------------

(def call-effects
  {;; fs module — sync forms
   ["fs" "unlinkSync"]     (path-effect :fs-delete 0)
   ["fs" "rmSync"]         (path-effect :fs-delete 0)
   ["fs" "rmdirSync"]      (path-effect :fs-delete 0)
   ["fs" "readFileSync"]   (path-effect :fs-read 0)
   ["fs" "readdirSync"]    (path-effect :fs-read 0)
   ["fs" "statSync"]       (path-effect :fs-read 0)
   ["fs" "lstatSync"]      (path-effect :fs-read 0)
   ["fs" "accessSync"]     (path-effect :fs-read 0)
   ["fs" "existsSync"]     (path-effect :fs-read 0)
   ["fs" "readlinkSync"]   (path-effect :fs-read 0)
   ["fs" "writeFileSync"]  (path-effect :fs-write 0)
   ["fs" "appendFileSync"] (path-effect :fs-write 0)
   ["fs" "mkdirSync"]      (path-effect :fs-write 0)
   ["fs" "chmodSync"]      (path-effect :fs-write 0)
   ["fs" "chownSync"]      (path-effect :fs-write 0)
   ["fs" "truncateSync"]   (path-effect :fs-write 0)
   ["fs" "renameSync"]     (two-path-effect)
   ["fs" "copyFileSync"]   (two-path-effect)
   ["fs" "linkSync"]       (two-path-effect)
   ["fs" "symlinkSync"]    (two-path-effect)
   ["fs" "createReadStream"]  (path-effect :fs-read 0)
   ["fs" "createWriteStream"] (path-effect :fs-write 0)
   ["fs" "openSync"]       (open-file-by-flags)

   ;; fs async — same effect shape (we don't track callbacks)
   ["fs" "unlink"]   (path-effect :fs-delete 0)
   ["fs" "rm"]       (path-effect :fs-delete 0)
   ["fs" "rmdir"]    (path-effect :fs-delete 0)
   ["fs" "readFile"] (path-effect :fs-read 0)
   ["fs" "writeFile"] (path-effect :fs-write 0)
   ["fs" "mkdir"]    (path-effect :fs-write 0)
   ["fs" "rename"]   (two-path-effect)
   ["fs" "copyFile"] (two-path-effect)

   ;; fs.promises — same effects
   ["fs" "promises" "unlink"]    (path-effect :fs-delete 0)
   ["fs" "promises" "rm"]        (path-effect :fs-delete 0)
   ["fs" "promises" "rmdir"]     (path-effect :fs-delete 0)
   ["fs" "promises" "readFile"]  (path-effect :fs-read 0)
   ["fs" "promises" "writeFile"] (path-effect :fs-write 0)
   ["fs" "promises" "mkdir"]     (path-effect :fs-write 0)
   ["fs" "promises" "rename"]    (two-path-effect)
   ["fs" "promises" "copyFile"]  (two-path-effect)

   ;; child_process module
   ["child_process" "execFileSync"] (exec-list-form)
   ["child_process" "execFile"]     (exec-list-form)
   ["child_process" "spawnSync"]    (exec-list-form)
   ["child_process" "spawn"]        (exec-list-form)
   ["child_process" "execSync"]     (exec-shell-form)
   ["child_process" "exec"]         (exec-shell-form)
   ["child_process" "fork"]         (path-effect :proc-spawn 0)

   ;; http / https — Node network
   ["http" "get"]       (net-out-effect 0)
   ["http" "request"]   (net-out-effect 0)
   ["https" "get"]      (net-out-effect 0)
   ["https" "request"]  (net-out-effect 0)

   ;; net — TCP/Unix sockets
   ["net" "connect"]      (net-out-effect 0)
   ["net" "createConnection"] (net-out-effect 0)

   ;; eval-like
   ["eval"] (eval-builder)
   ["Function"] (eval-builder)   ; `new Function(s)` — captured at call form
   ["vm" "runInThisContext"]  (eval-builder)
   ["vm" "runInNewContext"]   (eval-builder)
   ["vm" "runInContext"]      (eval-builder)})

;; ---- Binding table ---------------------------------------------------

(defn- build-binding-table
  "Pre-walk :nodes building local-name → resolved callable path.

   - `const fs = require('fs')` → {\"fs\" [\"fs\"]}
   - `const { unlinkSync } = require('fs')` → {\"unlinkSync\" [\"fs\" \"unlinkSync\"]}
   - `const { unlinkSync: rm } = require('fs')` → {\"rm\" [\"fs\" \"unlinkSync\"]}
   - `import fs from 'fs'` → {\"fs\" [\"fs\"]}
   - `import { unlinkSync } from 'fs'` → {\"unlinkSync\" [\"fs\" \"unlinkSync\"]}
   - `import * as fs from 'fs'` → {\"fs\" [\"fs\"]}
   - `import fs, { unlinkSync } from 'fs'` → both default + named"
  [nodes]
  (reduce
   (fn [acc node]
     (case (:kind node)
       :js-require
       (let [{:keys [module binding destructure]} node]
         (cond-> acc
           binding (assoc binding [module])
           (seq destructure)
           (as-> a
                 (reduce (fn [m {:keys [name alias]}]
                           (assoc m (or alias name) [module name]))
                         a
                         destructure))))
       :js-import
       (let [{:keys [module default namespace names]} node]
         (cond-> acc
           default   (assoc default [module])
           namespace (assoc namespace [module])
           (seq names)
           (as-> a (reduce (fn [m {:keys [name alias]}]
                             (assoc m (or alias name) [module name]))
                           a names))))
       :js-assign
       (let [v (:value node)]
         (case (:kind v)
           :js-member (assoc acc (:target node) (:path v))
           acc))
       acc))
   {}
   nodes))

(defn- resolve-callable
  [bindings callable]
  (if-let [bound (get bindings (first callable))]
    (vec (concat bound (rest callable)))
    callable))

;; ---- Public entry -----------------------------------------------------

(defn- effects-of-call
  [ctx bindings call]
  (let [resolved (resolve-callable bindings (:callable call))]
    (if-let [builder (get call-effects resolved)]
      (builder ctx resolved call)
      [(mk-opaque (str "js-call:" (function-of resolved))
                  resolved :unknown-effect-for-program)])))

(defn classify-node-script
  ([script] (classify-node-script {} script))
  ([ctx script]
   (let [nodes    (:nodes script)
         bindings (build-binding-table nodes)
         calls    (filter #(= :js-call (:kind %)) nodes)]
     (vec (mapcat #(effects-of-call ctx bindings %) calls)))))
