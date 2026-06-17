(ns shell-shape-classify.effects
  "Effect-class taxonomy + default program-classifier registry —
   shell-shape-classify's core namespace.

   Given a `shell-shape.core/parse` tree, this layer walks each
   :command node and emits a bounded effect-set per program. The
   substrate is consumer-agnostic: it knows nothing about biscuit,
   Claude Code tool inputs, or policy formats — those are
   each consumer's concern. continuity-witness is the first
   consumer; muschel and others can layer on top.

   The namespace defines:

    1. The bounded effect-class set — what kinds of effects a tool
       call can have on the system.
    2. The effect-instance shape — {class, scope, provenance} triple.
    3. The default program-classifier registry — per-program rules
       that map a `:command` node to a set of effect-instances.
    4. Helpers for arg parsing (literal-paths, host-from-url, …).

  Effect classes (the semantic domain):

    :fs-read              read filesystem at scope (path-glob)
    :fs-write             modify filesystem at scope (path-glob)
    :fs-delete            unlink filesystem at scope (path-glob)
    :fs-exec              execute file at scope (path-glob)
    :proc-spawn           spawn a process whose program matches scope
    :proc-signal          send signal to a process
    :net-out              outbound network to scope (host-glob)
    :net-in               inbound listener on scope (port)
    :env-mutate           modify environment variable at scope (var name)
    :env-read              read environment / process metadata
    :stdout-emit          emit data to stdout (content varies; scope `\"?\"`)
    :stdin-consume        consume data from stdin (content varies)
    :shell-interpret      execute shell source code under scope (dialect kw str)
    :interp-interpret     execute interpreter source code under scope
    :privilege-elevate    run as elevated user
    :opaque               static analysis can't determine effects;
                          fail-closed gate fires false

  Effect-instance:

    {:class       <effect-class-kw>
     :scope       <scope-string>
     :provenance  {:source-node <node-id>
                   :rule        <kw>
                   :inputs      [<token>]}}

  Program-classifier:

    {:doc       <docstring>
     :classify  (fn [cmd-node ctx] [<effect-instance> ...])
     :stdin     :none | :data | :shell-source | :interp-source
     :stdout    :none | :data | :file-contents | :passthrough
     :reads-stdin? <bool>}

  Programs not in the registry produce {:class :opaque} so the
  witness's `effect_all_authorized` positive-gate denies."
  (:require [clojure.string :as str]
            [shell-shape-classify.argv-shape :as as]
            [shell-shape-classify.bindings :as bind]
            [shell-shape-classify.getopt-specs :as gs]))

;; ---- Bounded class set --------------------------------------------------

(def effect-classes
  "The complete set of shell-program effect classes this library
   understands. Validated at consumer policy-issue time so authors
   can't name a class the classifier never emits.

   Consumers that need additional effect classes (e.g.
   continuity-witness's agent-graph axis for Claude Code tools)
   `(into effect-classes #{:custom-class-1 :custom-class-2 ...})`
   in their own taxonomy ns and validate against the merged set."
  #{:fs-read :fs-write :fs-delete :fs-exec
    :proc-spawn :proc-signal
    :net-out :net-in
    :env-mutate :env-read
    :stdout-emit :stdin-consume
    :shell-interpret :interp-interpret
    :privilege-elevate
    :opaque})

(defn known-class?
  "True if `c` (string or keyword) names a known effect class."
  [c]
  (let [kw (if (keyword? c) c (keyword c))]
    (contains? effect-classes kw)))

;; ---- Token helpers ------------------------------------------------------

(defn- literal-token?
  [tok]
  (and (= :token (:kind tok))
       (every? #(= :literal (:kind %)) (:parts tok))))

(defn- token-literal-value
  [tok]
  (when (literal-token? tok)
    (apply str (mapv :value (:parts tok)))))

(def ^:const process-sub-fd-scope
  "Synthetic scope for process-substitution arg-elements. The outer
   command reads from (or writes to) `/dev/fd/N` at runtime; we model
   that with a single glob scope so policy authors can grant
   `/dev/fd/**` once and process-sub becomes seamless. The inner
   command's effects flow through classify-arg-token's recursive
   descent into the :process-sub :body."
  "/dev/fd/*")

(defn- arg-literal-scope
  "Scope of a single arg-element:
     - :token literal           → its literal string value
     - :process-sub             → /dev/fd/* (v0.7.0)
     - :token with vars/substs  → nil sentinel for non-literal"
  [arg]
  (case (:kind arg)
    :token       (token-literal-value arg)
    :process-sub process-sub-fd-scope
    nil))

(defn arg-literals
  "Sequence of arg-element scopes from a command's args. Non-literal
   :token args (containing :var / :subst parts) yield nil so callers
   can detect them; :process-sub args yield the /dev/fd/* glob.

   Public for shims that extend the registry (e.g. continuity-witness's
   self-classifier) and need to inspect literal positionals/flags
   without re-deriving the literal-token logic."
  [cmd]
  (mapv arg-literal-scope (:args cmd)))

(defn option?
  "Crude predicate: arg looks like a CLI option (starts with `-`).
   Special cases: `-` alone is stdin (positional, not an option);
   `--` is the POSIX option terminator (handled by the caller, not
   here).

   Public for shims extending the registry — see `arg-literals`."
  [s]
  (and (string? s)
       (str/starts-with? s "-")
       (not= s "-")
       (not= s "--")))

(defn- non-option-positional-literals
  "Literal-only positional args after stripping options. Returns
     {:paths           [<path-str> ...]   ; literal positional file args
      :stdin-consumed? <bool>}            ; true iff a literal `-` was
                                          ; present as a positional
   or nil when any arg is non-literal (caller emits :opaque).

   `opts` (optional):
     :value-flags  set of flags that consume their NEXT arg as a value
                   (e.g. head `-n N`, sort `-k FIELD`). The value is
                   dropped from positionals. Honored only PRE-`--`.
     :path-value-flags  set of flags whose VALUE is a path. The value
                        is emitted into the :paths vector (so the
                        caller's classifier picks it up as fs-read).
                        Honored only PRE-`--`.

   Two POSIX conventions honored beyond the v0.5.0 substrate:
     - `--` terminates option parsing — every arg after `--` is
       positional even if it starts with `-`.
     - bare `-` as a positional signals stdin consumption rather than
       a file with that name. Callers split it into a separate
       :stdin-consume effect."
  ([cmd] (non-option-positional-literals cmd {}))
  ([cmd {:keys [value-flags path-value-flags]
         :or {value-flags #{} path-value-flags #{}}}]
   (let [lits (arg-literals cmd)]
     (when (every? some? lits)
       (let [[pre post]      (split-with #(not= "--" %) lits)
             ;; Walk pre-`--` args, consuming flag values per the spec.
             walk            (fn walk [xs positionals path-vals]
                               (cond
                                 (empty? xs)
                                 [positionals path-vals]

                                 (contains? path-value-flags (first xs))
                                 (let [val (second xs)]
                                   (recur (drop 2 xs) positionals
                                          (if val (conj path-vals val) path-vals)))

                                 (contains? value-flags (first xs))
                                 (recur (drop 2 xs) positionals path-vals)

                                 (option? (first xs))
                                 (recur (rest xs) positionals path-vals)

                                 :else
                                 (recur (rest xs)
                                        (conj positionals (first xs))
                                        path-vals)))
             [pre-positional path-vals] (walk pre [] [])
             post-positional (rest post) ; drop the "--" itself
             positional      (vec (concat pre-positional post-positional))
             stdin-consumed? (boolean (some #{"-"} positional))
             paths           (vec (concat path-vals
                                          (filterv (complement #{"-"}) positional)))]
         {:paths           paths
          :stdin-consumed? stdin-consumed?})))))

;; ---- Path / URL parsing ------------------------------------------------

(defn- path-args
  "Backwards-compatible accessor — returns just the :paths vector
   from `non-option-positional-literals` (nil if non-literal arg).
   Callers that need the :stdin-consumed? signal must call
   `non-option-positional-literals` directly."
  [cmd]
  (:paths (non-option-positional-literals cmd)))

(defn- host-from-url
  "Extract host from a URL-shaped arg. Returns nil if not a URL."
  [s]
  (when (string? s)
    (when-let [m (re-find #"^[a-zA-Z]+://([^/]+)" s)]
      (nth m 1))))

(defn- net-targets-from-args
  "For network commands, find host targets in args. Conservative:
   parses URL arg first; falls back to first non-option positional."
  [cmd]
  (let [lits (arg-literals cmd)]
    (when (every? some? lits)
      (let [from-urls (keep host-from-url lits)
            others    (->> lits (drop-while option?) (remove option?))]
        (if (seq from-urls)
          (vec from-urls)
          (vec (take 1 others)))))))

;; ---- Effect-instance constructors --------------------------------------

(defn mk
  "Construct one effect-instance map. Public for shims extending the
   registry — see `arg-literals`."
  [class scope rule cmd]
  {:class      class
   :scope      (str scope)
   :provenance {:rule    rule
                :program (:program cmd)}})

(defn- opaque [reason cmd]
  (mk :opaque (str (name reason)) reason cmd))

(defn- stdin-consume-effect
  "Auxiliary :stdin-consume effect emitted by fs-* classifiers when a
   literal `-` was present in args."
  [rule cmd]
  (mk :stdin-consume "stdin" rule cmd))

;; ---- Common classifier patterns ----------------------------------------

(defn- with-stdin
  "Append a `:stdin-consume` effect to `effs` when `:stdin-consumed?`
   was true on the destructured `non-option-positional-literals` result."
  [effs stdin-consumed? rule cmd]
  (if stdin-consumed?
    (conj (vec effs) (stdin-consume-effect rule cmd))
    (vec effs)))

(defn classify-fs-read
  "Public classifier factory — emits :fs-read on positional path args,
   :fs-read:. on the bare form, :stdin-consume on `-` positional, and
   :fs-read:? when xargs descent provides positionals from stdin.

   2-arity `spec` honors `:value-flags` (consume next arg) and
   `:path-value-flags` (consume next arg AND treat its value as a
   path). Both default to `#{}`.

   Used by `default-registry` for ls/stat/file/tree/diff/… and exposed
   for the operator-facing overlay (shell-shape-classify.overlay)."
  ([rule] (classify-fs-read rule {}))
  ([rule spec]
   (fn [cmd ctx]
     (let [{:keys [paths stdin-consumed?]}
           (non-option-positional-literals cmd spec)]
       (cond
         (nil? paths)
         [(opaque :variable-arg cmd)]

         (and (empty? paths) (not stdin-consumed?)
              (:stdin-fed-positionals? ctx))
         ;; xargs/parallel descent: paths come from stdin → unknown scope.
         [(mk :fs-read "?" rule cmd)]

         (empty? paths)
         (with-stdin (if stdin-consumed?
                       []
                       [(mk :fs-read "." rule cmd)])
                     stdin-consumed? rule cmd)

         :else
         (with-stdin (mapv #(mk :fs-read % rule cmd) paths)
                     stdin-consumed? rule cmd))))))

(defn classify-stdin-or-fs-read
  "Like classify-fs-read but bare-invocation (no positional paths)
   resolves to `:stdin-consume :stdin` rather than `:fs-read \".\"`.
   Use for stream-processors whose unargumented form reads stdin
   (tail/head/cat/wc/sort/uniq/awk/sed/jq…) so a pipe stage like
   `cmd | tail -30` does not spuriously emit `fs-read:.` on every
   call. Closes the v0.24.0 false-defer that fired under auto-mode-
   aggressive whenever a pipe ended in one of these tools.

   With explicit positional paths, behavior matches `classify-fs-read`.

   2-arity accepts a `spec` map honored by
   `non-option-positional-literals`:
     :value-flags       set of flags that consume next arg (e.g. head
                        `-n`, sort `-k`). Without it, `head -n 20 file`
                        leaks `20` as a positional fs-read."
  ([rule] (classify-stdin-or-fs-read rule {}))
  ([rule spec]
   (fn [cmd ctx]
     (let [{:keys [paths stdin-consumed?]}
           (non-option-positional-literals cmd spec)]
       (cond
         (nil? paths)
         [(opaque :variable-arg cmd)]

         (and (empty? paths) (not stdin-consumed?)
              (:stdin-fed-positionals? ctx))
         ;; xargs descent: paths come from stdin → fs-read with unknown
         ;; scope (NOT stdin-consume, since xargs's stdin feeds the
         ;; positionals, not the wrapped command's own stdin).
         [(mk :fs-read "?" rule cmd)]

         (empty? paths)
         ;; Bare form: implicit stdin. `with-stdin` already appends a
         ;; stdin-consume effect when `-` was a positional; here we emit
         ;; one unconditionally (no positional `-` to dedup).
         [(stdin-consume-effect rule cmd)]

         :else
         (with-stdin (mapv #(mk :fs-read % rule cmd) paths)
                     stdin-consumed? rule cmd))))))

(defn classify-script-then-files
  "Classifier for programs whose FIRST positional is NOT a path but a
   script / pattern / mode / owner-spec — awk, sed, jq, grep, chmod,
   chown. The bare form (no files) reads stdin (unless overridden via
   `:bare-effect`); with files, each becomes a `:fs-read` (or whatever
   `:file-class` is set to — chmod/chown use `:fs-write`).

   `opts`:
     :rule              keyword tag (e.g. :awk)
     :script-via-flags  set of flags whose VALUE is the script inline
                        (e.g. awk's `-e`, sed's `-e`, grep's `-e`).
                        When any such flag appears, NO positional is
                        consumed as the leading non-path token.
     :script-file-flags set of flags whose value is a script/pattern
                        FILE (awk/sed `-f`, jq `--from-file`, grep
                        `-f`). Emitted as `:fs-read` AND marks the
                        leading token resolved.
     :value-flags       other flags taking a 1-arg value (e.g. `-F`,
                        `-v`, head `-n`, grep `-A/-B/-C`). Consume the
                        next arg without emitting an effect.
     :two-value-flags   flags taking 2-arg values (jq `--arg NAME VAL`).
     :inplace-flags     flags that turn file args into `:fs-write`
                        (sed's `-i`). Optional.
     :file-class        effect class for resolved file positionals.
                        Default `:fs-read`. Set to `:fs-write` for
                        chmod/chown.
     :emit-stdout?      bool — emit `:stdout-emit \"?\"`.

   Walks args once. Any non-literal arg → `:opaque :variable-arg`."
  [{:keys [rule script-via-flags script-file-flags value-flags
           two-value-flags inplace-flags file-class emit-stdout?]
    :or {script-via-flags  #{}
         script-file-flags #{}
         value-flags       #{}
         two-value-flags   #{}
         inplace-flags     #{}
         file-class        :fs-read
         emit-stdout?      false}}]
  (fn [cmd _ctx]
    (let [lits (arg-literals cmd)]
      (if-not (every? some? lits)
        [(opaque :variable-arg cmd)]
        (loop [xs              lits
               script-resolved? false
               in-place?       false
               script-files    []
               positionals     []
               after-dashdash? false]
          (cond
            (empty? xs)
            (let [files (if script-resolved?
                          positionals
                          ;; First positional IS the leading non-path
                          ;; token (script / pattern / mode / owner) —
                          ;; drop it.
                          (vec (rest positionals)))
                  resolved-class (if in-place? :fs-write file-class)
                  script-effs (mapv #(mk :fs-read % rule cmd) script-files)
                  file-effs   (mapv #(mk resolved-class % rule cmd) files)
                  stdout      (when emit-stdout?
                                [(mk :stdout-emit "?" rule cmd)])
                  body        (if (empty? file-effs)
                                [(stdin-consume-effect rule cmd)]
                                file-effs)]
              (vec (concat script-effs body stdout)))

            after-dashdash?
            (recur (rest xs) script-resolved? in-place? script-files
                   (conj positionals (first xs)) true)

            (= (first xs) "--")
            (recur (rest xs) script-resolved? in-place? script-files
                   positionals true)

            (contains? script-via-flags (first xs))
            (recur (drop 2 xs) true in-place? script-files positionals false)

            (contains? script-file-flags (first xs))
            (recur (drop 2 xs) true in-place?
                   (conj script-files (second xs)) positionals false)

            (contains? inplace-flags (first xs))
            ;; -i may or may not take a backup suffix. Treat the next
            ;; arg as a positional if it doesn't look like an option;
            ;; conservative: just consume nothing extra here, and let
            ;; the suffix flow as a positional → harmless extra
            ;; fs-write on a likely-bogus literal.
            (recur (rest xs) script-resolved? true script-files positionals false)

            (contains? two-value-flags (first xs))
            ;; `--arg NAME VALUE` and friends — consume the flag + 2
            ;; subsequent arg tokens. Without this, the second value
            ;; would leak as a positional and confuse the script /
            ;; file split.
            (recur (drop 3 xs) script-resolved? in-place? script-files
                   positionals false)

            (contains? value-flags (first xs))
            (recur (drop 2 xs) script-resolved? in-place? script-files
                   positionals false)

            (option? (first xs))
            ;; Single-letter / boolean flag with no value.
            (recur (rest xs) script-resolved? in-place? script-files
                   positionals false)

            :else
            (recur (rest xs) script-resolved? in-place? script-files
                   (conj positionals (first xs)) false)))))))

(defn classify-fs-delete [rule]
  (fn [cmd ctx]
    (let [{:keys [paths stdin-consumed?]} (non-option-positional-literals cmd)]
      (cond
        (nil? paths)   [(opaque :variable-arg cmd)]
        (and (empty? paths) (not stdin-consumed?)
             (:stdin-fed-positionals? ctx))
        [(mk :fs-delete "?" rule cmd)]
        (and (empty? paths) (not stdin-consumed?)) [(opaque :no-target cmd)]
        (empty? paths) [(stdin-consume-effect rule cmd)]
        :else          (with-stdin (mapv #(mk :fs-delete % rule cmd) paths)
                                   stdin-consumed? rule cmd)))))

(defn classify-fs-write [rule]
  (fn [cmd ctx]
    (let [{:keys [paths stdin-consumed?]} (non-option-positional-literals cmd)]
      (cond
        (nil? paths)   [(opaque :variable-arg cmd)]
        (and (empty? paths) (not stdin-consumed?)
             (:stdin-fed-positionals? ctx))
        [(mk :fs-write "?" rule cmd)]
        (and (empty? paths) (not stdin-consumed?)) [(opaque :no-target cmd)]
        (empty? paths) [(stdin-consume-effect rule cmd)]
        :else          (with-stdin (mapv #(mk :fs-write % rule cmd) paths)
                                   stdin-consumed? rule cmd)))))

(defn classify-fs-read-write [rule]
  ;; mv/cp: read src, write dst
  (fn [cmd _ctx]
    (let [{:keys [paths stdin-consumed?]} (non-option-positional-literals cmd)]
      (cond
        (nil? paths)         [(opaque :variable-arg cmd)]
        (< (count paths) 2)  [(opaque :no-target cmd)]
        :else
        (let [srcs (butlast paths)
              dst  (last paths)]
          (with-stdin
            (vec (concat (mapv #(mk :fs-read % rule cmd) srcs)
                         [(mk :fs-write dst rule cmd)]))
            stdin-consumed? rule cmd))))))

(defn classify-net-out [rule]
  (fn [cmd _ctx]
    (let [targets (net-targets-from-args cmd)]
      (cond
        (nil? targets)   [(opaque :variable-arg cmd)]
        (empty? targets) [(opaque :no-target cmd)]
        :else            (mapv #(mk :net-out % rule cmd) targets)))))

(defn env-read-only [rule]
  (fn [cmd _ctx] [(mk :env-read "?" rule cmd)]))

(defn stdout-emit-only [rule]
  (fn [cmd _ctx] [(mk :stdout-emit "?" rule cmd)]))

(defn pure [rule]
  (fn [cmd _ctx] [(mk :stdout-emit "?" rule cmd)]))

(defn proc-signal-classifier
  "Public factory — emits a single :proc-signal effect with the given
   scope. Programs in the default-registry that signal processes
   (kill, pkill) wire to this; the operator overlay also uses it for
   custom signal-sending tools."
  [rule scope]
  (fn [cmd _ctx] [(mk :proc-signal (or scope "?") rule cmd)]))

(defn proc-spawn-classifier
  "Public factory — emits a single :proc-spawn effect scoped on the
   program name. Use when the program's only observable from outside
   is that it ran (no fs / net / env effects we can statically reason
   about). The overlay reaches for this for tools like `sleep` whose
   only side effect is the timing of the call itself."
  [rule]
  (fn [cmd _ctx] [(mk :proc-spawn (or (:program cmd) "?") rule cmd)]))

;; ---- Wrapper classifiers (delegate to wrapped command's :invokes) -----

(defn- effects-of-invokes
  "Recursively collect effects from :invokes entries on this command.
   Each invoke is either {:kind :script ...}, {:kind :command ...},
   or {:kind :unresolved ...}. Caller (classify-tree) handles the
   script/command descent; here we collect the :unresolved entries
   directly as opaque effects.

   P7a: when ctx carries a :bindings table and an :unresolved entry
   is structurally resolvable via that table, suppress the :opaque
   here — classify-invocation's `try-resolve-unresolved` produces the
   resolved-form effects from the same :unresolved entry."
  [cmd ctx]
  (let [bindings (or (:bindings ctx) {})
        unresolved-invokes (filter #(= :unresolved (:kind %)) (:invokes cmd))
        opaque-emitting
        (if (seq bindings)
          (remove (fn [u] (bind/invocation-resolvable? bindings cmd u))
                  unresolved-invokes)
          unresolved-invokes)]
    (mapv (fn [u] (mk :opaque (str (name (:reason u))) :wrapper-unresolved cmd))
          opaque-emitting)))

(defn- wrapper-classifier
  "Wrapper effects = wrapper's own (sometimes none) + delegate to
   :invokes. classify-tree walks :invokes recursively, so this fn
   only needs to emit the wrapper's own effects."
  [own-effects]
  (fn [cmd ctx]
    (vec (concat (when own-effects (own-effects cmd ctx))
                 (effects-of-invokes cmd ctx)))))

(defn- sudo-own-effects [cmd _ctx]
  [(mk :privilege-elevate "root" :sudo cmd)])

;; ---- env-mutate detection (env VAR=val cmd) ----------------------------
;;
;; Shell-shape's :positional-skip-assignments strategy populates env's
;; :invokes with the wrapped command and keeps the VAR=val literals in
;; :args. env's classifier walks the leading args and emits one
;; :env-mutate per literal KEY=value, the wrapped command's effects
;; come from classify-command's invoke descent. T14 — closes env-mutate
;; smuggling (env LD_PRELOAD=/x.so curl evil.com).

(defn- env-assignment-var
  "Extract the variable name from a literal `VAR=value` arg, or nil
   when the arg isn't an assignment."
  [s]
  (when (string? s)
    (when-let [m (re-find #"^([A-Za-z_][A-Za-z0-9_]*)=" s)]
      (nth m 1))))

(defn- env-wrapper-own-effects
  "env's own effects: one :env-mutate per leading literal VAR=val,
   :opaque for non-literal tokens in leading position (we can't tell
   what gets mutated), :env-read if there are no assignments at all."
  [cmd _ctx]
  (let [arg-toks (:args cmd)
        ;; Leading slice: stop at the first arg that's clearly the
        ;; wrapped program (a literal that's NOT an assignment).
        leading  (take-while (fn [t]
                               (or (not (literal-token? t))
                                   (env-assignment-var (token-literal-value t))))
                             arg-toks)
        any-non-literal? (some (complement literal-token?) leading)
        assigned-vars    (keep (comp env-assignment-var token-literal-value)
                               leading)]
    (cond
      any-non-literal?
      [(opaque :env-assign-non-literal cmd)]

      (seq assigned-vars)
      (mapv (fn [v] (mk :env-mutate v :env-assign cmd)) assigned-vars)

      :else
      [(mk :env-read "?" :env-listing cmd)])))

;; ---- Shell-builtin classifiers (T14) ----------------------------------

(defn- cd-classifier
  "cd PATH — emits :env-mutate on PWD. cd is a builtin: PATH is just
   a directory, no side effects beyond the shell's PWD/OLDPWD update.
   Conservative scope: PWD covers both."
  [cmd _ctx]
  [(mk :env-mutate "PWD" :cd cmd)])

(defn- export-or-declare-classifier
  "export / declare / typeset — emits :env-mutate per VAR (with or
   without =val). Non-literal target args → :opaque (we can't tell
   which variable is being mutated)."
  [rule]
  (fn [cmd _ctx]
    (let [arg-toks       (:args cmd)
          ;; Drop leading -flags (export -p, declare -x, declare -r ...)
          positional     (drop-while (fn [t]
                                       (when-let [lit (token-literal-value t)]
                                         (str/starts-with? lit "-")))
                                     arg-toks)
          any-non-lit?   (some (complement literal-token?) positional)
          assigned-vars  (keep (fn [t]
                                 (when-let [lit (token-literal-value t)]
                                   ;; `VAR=val` — take VAR. Plain `VAR` —
                                   ;; whole literal is the name.
                                   (or (env-assignment-var lit)
                                       (when (re-matches #"[A-Za-z_][A-Za-z0-9_]*" lit)
                                         lit))))
                               positional)]
      (cond
        any-non-lit?
        [(opaque :variable-arg cmd)]

        (seq assigned-vars)
        (mapv (fn [v] (mk :env-mutate v rule cmd)) assigned-vars)

        :else
        ;; export with no args is "list exported vars" (env-read).
        [(mk :env-read "?" rule cmd)]))))

;; ---- v0.7.0 opacity reduction: ssh / trap / mail -----------------------
;;
;; The tool_input arriving at witness-exercise time is the LLM's literal
;; output string. The bytes inside `eval "rm /etc/passwd"`,
;; `ssh host "rm /etc/passwd"`, `trap "rm /tmp/x" INT`, and
;; `mail user@example.com` are all STATIC at classify time, even
;; though they look bash-runtime-dynamic from a semantic perspective.
;; v0.7.0 reduces opacity on these classes by parsing the literal body
;; and recursively classifying it under :bash. Scope prefixing
;; (`ssh:<host>:` and `trap:`) lets policy authors discriminate remote
;; / deferred execution from local / immediate execution.

(def ^:private ssh-opts-with-value
  "ssh option flags that consume the next argument as their value.
   Used by `ssh-host-from-args` to skip past option groups before
   identifying the host positional."
  #{"-p" "-i" "-o" "-l" "-L" "-R" "-D" "-J" "-S" "-W" "-c" "-m"
    "-Q" "-F" "-e" "-b" "-B" "-I" "-O"})

(defn- ssh-skip-opts
  "Returns the literal-arg sequence with leading ssh option flags
   stripped. `-pPORT`-style (no space) is treated as one token; `-p
   PORT`-style consumes the next token as the value."
  [lits]
  (loop [as lits]
    (cond
      (empty? as) as
      (contains? ssh-opts-with-value (first as)) (drop 2 as)
      (option? (first as)) (recur (rest as))
      :else as)))

(defn- ssh-host-from-args
  "First non-option positional arg, stripped of `user@` prefix. nil if
   any arg in the path is non-literal."
  [cmd]
  (let [lits (arg-literals cmd)]
    (when (every? some? lits)
      (let [positional (vec (ssh-skip-opts lits))
            host (first positional)]
        (when (string? host)
          (or (some-> (re-find #"^[^@]+@(.+)$" host) (nth 1))
              host))))))

(defn- ssh-remote-cmd-string
  "Concatenation of all args AFTER the host, if every such arg is a
   literal token. nil if no remote-cmd args; :dynamic if any are
   non-literal."
  [cmd]
  (let [lits (arg-literals cmd)
        toks (:args cmd)]
    (when (every? some? lits)
      (let [;; Walk the args, dropping leading options + the host
            ;; position, returning the remaining literal-tokens.
            after-host
            (loop [as lits, ts toks]
              (cond
                (empty? as) []
                (contains? ssh-opts-with-value (first as))
                (recur (drop 2 as) (drop 2 ts))
                (option? (first as))
                (recur (rest as) (rest ts))
                :else
                ;; First positional = host; drop and return the rest.
                (rest ts)))]
        (cond
          (empty? after-host) nil

          ;; Every remaining token must be a pure literal :token —
          ;; process-sub args would inject /dev/fd/* into the body,
          ;; var/subst tokens introduce dynamic content. Either case
          ;; means we can't recurse-classify the body literally.
          (every? literal-token? after-host)
          (str/join " " (mapv token-literal-value after-host))

          :else :dynamic)))))

(def ^:private bash-signal-names
  "POSIX + Linux/BSD signal names recognized by bash trap. If every
   positional arg in `trap … SIG […]` is one of these (plus optional
   numeric forms / SIGFOO), the form is a reset/list — no body present."
  #{"EXIT" "ERR" "DEBUG" "RETURN"
    "HUP" "INT" "QUIT" "ILL" "TRAP" "ABRT" "BUS" "FPE" "KILL"
    "USR1" "SEGV" "USR2" "PIPE" "ALRM" "TERM" "STKFLT" "CHLD"
    "CONT" "STOP" "TSTP" "TTIN" "TTOU" "URG" "XCPU" "XFSZ"
    "VTALRM" "PROF" "WINCH" "IO" "PWR" "SYS"
    "1" "2" "3" "6" "9" "13" "14" "15"
    "-" ""})

(defn- signal-arg?
  "True if `s` looks like a signal name (bash trap RESET form)."
  [s]
  (or (contains? bash-signal-names s)
      (and (string? s)
           (or (str/starts-with? s "SIG")
               (re-matches #"\d+" s)))))

(defn- trap-body-literal
  "First positional arg of `trap` interpreted as the CMD body. Bash
   disambiguation: if every positional arg looks like a signal name,
   `trap` is in its reset form (no body) — return nil. Otherwise the
   first positional is the body (literal-string) or :dynamic if
   non-literal."
  [cmd]
  (let [lits (arg-literals cmd)
        first-arg (first (:args cmd))]
    (cond
      (nil? first-arg) nil

      ;; Every arg is a literal signal name → reset form, no body.
      (and (every? some? lits)
           (every? signal-arg? lits))
      nil

      (literal-token? first-arg) (token-literal-value first-arg)
      :else :dynamic)))

(def ^:private mail-recipient-regex
  ;; Minimal email shape — host class allows alnum/dot/dash; we are
  ;; matching for scope, not transport validity. Subject strings
  ;; containing `@` won't match because they contain whitespace or
  ;; quotes, both of which fail the no-whitespace constraint.
  #"^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$")

(defn- mail-recipients-from-args
  "Collect literal arg values that match the email regex. Returns
   vector of recipient strings (possibly empty); nil if any literal
   `--` was followed by non-literal positional args we can't inspect."
  [cmd]
  (let [lits (arg-literals cmd)]
    (when (every? some? lits)
      (filterv #(re-matches mail-recipient-regex %) lits))))

(defn- prefix-effect-scope
  "Prepend `prefix` to an effect's scope string. The (class, prefixed-
   scope, provenance) shape is preserved exactly."
  [prefix e]
  (update e :scope #(str prefix %)))

(defn- recursive-classify
  "Parse `body` and classify the resulting tree, all using callables
   threaded through `ctx`. The cross-namespace cycle between
   effects.clj and classify.clj is broken by `ctx :classify-tree` +
   `ctx :ss-parse` — classify.clj registers these on every top-level
   call. Returns [] if either callable is missing (the classifier
   degrades to no-effects rather than misbehaving)."
  [body ctx]
  (let [parse-fn   (:ss-parse ctx)
        classify-fn (:classify-tree ctx)]
    (if (and parse-fn classify-fn (string? body) (seq body))
      (let [tree (parse-fn body {:dialect :bash})]
        (classify-fn ctx tree))
      [])))

(defn- ssh-classifier
  "ssh — emits :net-out on the host, plus the effects of the remote
   command body (recursively classified) with every scope prefixed
   `ssh:<host>:`. Discriminates literal-host / literal-body / dynamic
   variants and falls back to :opaque only for the dynamic body case
   (where the bytes aren't present at classify time)."
  [cmd ctx]
  (let [host       (ssh-host-from-args cmd)
        remote-cmd (ssh-remote-cmd-string cmd)
        host-scope (or host "?")
        base       [(mk :net-out host-scope :ssh cmd)]]
    (cond
      ;; No remote command — interactive ssh; just the net-out.
      (nil? remote-cmd)
      base

      ;; Dynamic — we can see args but can't read the literal body.
      (= :dynamic remote-cmd)
      (conj base
            (mk :opaque (str "ssh:" host-scope ":remote-dynamic")
                :ssh-remote-dynamic cmd))

      ;; Static body — recursively classify and prefix scopes.
      :else
      (let [inner-effs (recursive-classify remote-cmd ctx)]
        (into base
              (mapv #(prefix-effect-scope (str "ssh:" host-scope ":") %)
                    inner-effs))))))

(defn- trap-classifier
  "trap CMD SIG [SIG …] — registers CMD to fire on signal. The body
   is a literal at classify time (the LLM emitted it as the tool_input),
   so we recursively classify and prefix scopes with `trap:` to
   discriminate registration from immediate execution. Bare `trap`,
   `trap -p`, and `trap SIG …` (reset/list forms) emit a single
   :env-read effect — they don't run user code."
  [cmd ctx]
  (let [body (trap-body-literal cmd)]
    (cond
      ;; Reset / list forms — no body, just hand back env-read so the
      ;; registry-closure property has at least one effect to inspect.
      (nil? body)
      [(mk :env-read "trap-handlers" :trap-list cmd)]

      ;; Dynamic — first arg is non-literal.
      (= :dynamic body)
      [(mk :opaque "trap:dynamic" :trap-dynamic cmd)]

      :else
      (let [inner-effs (recursive-classify body ctx)]
        (mapv #(prefix-effect-scope "trap:" %) inner-effs)))))

(defn- mail-classifier
  "mail RECIPIENT [-s SUBJECT] — sends email to recipient(s). The
   recipient(s) are literal args at classify time, so we emit one
   :net-out per recipient (scope = email address). Bodies (delivered
   via stdin) are not surfaced here; composition rules already handle
   pipelines that produce mail bodies."
  [cmd _ctx]
  (let [recipients (mail-recipients-from-args cmd)]
    (cond
      (nil? recipients) [(opaque :variable-arg cmd)]
      (empty? recipients) [(mk :opaque "mail:no-recipient"
                               :mail-no-recipient cmd)]
      :else (mapv #(mk :net-out % :mail cmd) recipients))))

(defn- source-classifier
  "source FILE / . FILE — reads FILE then interprets its bytes as
   shell source in the current process. Effects emitted:
     - :fs-read on the FILE path (or :opaque if non-literal)
     - :shell-interpret (the read bytes are interpreted)
     - :opaque scope=\"source-body\" — the script body is statically
       opaque to the classifier (no FS access at parse time).
   This is the equivalent of `bash -c \"$(cat FILE)\"` — composition
   layer recognizes the bytes-of-file-as-shell-source flow."
  [rule]
  (fn [cmd _ctx]
    (let [{:keys [paths]} (non-option-positional-literals cmd)]
      (cond
        (nil? paths)
        [(opaque :variable-arg cmd)
         (mk :shell-interpret "posix" rule cmd)
         (mk :opaque "source-body" rule cmd)]

        (empty? paths)
        [(opaque :no-target cmd)]

        :else
        (vec (concat (mapv #(mk :fs-read % rule cmd) paths)
                     [(mk :shell-interpret "posix" rule cmd)
                      (mk :opaque "source-body" rule cmd)]))))))

;; ---- find classifier (v0.8.0 P0) --------------------------------------
;;
;; find's argv shape is NOT GNU-getopt — it's `find [path...] [expression]`
;; where the expression mixes tests, actions, and operators. The generic
;; classify-fs-read pattern (read all non-option literal args as paths)
;; misclassifies two ways:
;;   1. `find /tmp -type f`  → reads "f" as a path (spurious `[:fs-read "f"]`)
;;   2. `find /tmp -delete`  → silently emits only :fs-read on /tmp; the
;;                             destructive action is invisible to policy,
;;                             so a generic `right(:fs-read, "/tmp/**", _)`
;;                             grant authorizes a delete (loaded-gun bug).
;;
;; v0.8.0 ships a find-specific classifier:
;;   - positional paths are taken from the args BEFORE the first
;;     expression token (`-XYZ`, `(`, `)`, `!`, `,`); any literal value
;;     after a predicate flag is its argument, not a path.
;;   - the expression is scanned for the `-delete` nullary action; if
;;     present, :fs-delete is emitted on each starting path (in addition
;;     to :fs-read for the walk).
;;
;; `-exec rm` / `-execdir rm` / `-ok rm` are handled by shell-shape's
;; :find-exec wrapper strategy + the classify-tree :invokes descent —
;; the wrapped rm command emits its own :fs-delete through that path,
;; so this classifier does not need to detect them.

(def ^:private find-expression-starts?
  "Predicate on a literal arg: is this the start of a find expression
   token (and therefore NOT a positional path)?"
  (fn [s]
    (and (string? s)
         (or (str/starts-with? s "-")
             (#{"(" ")" "!" ","} s)))))

(defn- find-positional-paths
  "Extract the starting paths from a `find` command — every literal arg
   before the first expression token. Returns:
     - nil      if any arg is non-literal (caller emits :opaque)
     - []       if no positional paths (find defaults to cwd)
     - [<path>] otherwise"
  [cmd]
  (let [lits (arg-literals cmd)]
    (when (every? some? lits)
      (vec (take-while (complement find-expression-starts?) lits)))))

(defn- find-has-delete-action?
  "True when `find`'s expression includes the `-delete` nullary action.
   Conservative: relies on a literal `-delete` token anywhere in the
   args. Non-literal expression tokens don't widen the detection — the
   path-scanning step above would have already flagged the command as
   :opaque if any arg is non-literal."
  [cmd]
  (let [lits (arg-literals cmd)]
    (boolean
     (when (every? some? lits)
       (some #{"-delete"} lits)))))

(defn- find-classifier
  "find /PATH /PATH... [expression]

   Emits :fs-read on each starting path (find walks the tree), and
   additionally :fs-delete on each starting path when the expression
   contains the `-delete` action. -exec/-execdir/-ok are handled by
   the wrapper machinery (shell-shape :find-exec strategy + classify-
   command :invokes descent)."
  [cmd _ctx]
  (let [paths   (find-positional-paths cmd)
        roots   (if (and paths (empty? paths)) ["."] paths)
        delete? (find-has-delete-action? cmd)]
    (cond
      (nil? paths)
      [(opaque :variable-arg cmd)]

      :else
      (let [reads (mapv #(mk :fs-read % :find cmd) roots)]
        (if delete?
          (into reads (mapv #(mk :fs-delete % :find-delete cmd) roots))
          reads)))))

;; ---- Argv-shape-driven classifier (v0.10.0 P2) -------------------------
;;
;; Builds a classifier from a per-program shape registry. Each shape is a
;; map of:
;;
;;   {:name           <str>   ; the argv-shape coordinate-axis value
;;    :match          <shape> ; predicate-set passed to as/match?
;;    :extra-effects  [{:class :scope :rule} ...]
;;                            ; effects added on top of the baseline
;;                            ; :proc-spawn; rules-only (mk wraps cmd)
;;    :function       <str>   ; optional :function coordinate
;;                            ; (e.g. "push" for git-push-destructive)}
;;
;; The classifier scans shapes top-to-bottom, runs `as/match?` against
;; the normalized argv, and on first hit emits the baseline :proc-spawn
;; plus the shape's :extra-effects. Every emitted record carries the
;; matched shape's :name in its :provenance.argv-shape (which P1's
;; enrich-effects then lifts to the top-level :argv-shape field) and
;; the shape's :function in :provenance.function. If no shape matches,
;; the classifier falls back to baseline :proc-spawn with no shape
;; pinning — same shape as `proc-spawn-classifier`.
;;
;; Shapes are TRIED IN ORDER — list the most specific first.

(defn- arg-token-literals
  "Token literals on a :command's args, in order. Variable parts and
   non-literal tokens are skipped — the shape match must operate on
   concrete tokens. Returns a vector of strings."
  [cmd]
  (->> (:args cmd)
       (keep token-literal-value)
       vec))

(defn- argv-shape-classifier
  "Build a classifier fn for a program from a list of shapes (see
   docstring at top of section). `getopt-spec` is the keyword or map
   passed to `gs/normalize-argv` — typically `:gnu-with-subcommand`
   for git/docker/kubectl and `:tar-bundled` for tar."
  [program rule getopt-spec shapes]
  (fn [cmd _ctx]
    (let [argv (arg-token-literals cmd)
          norm (gs/normalize-argv getopt-spec argv)
          hit  (some (fn [{:keys [match] :as shape}]
                       (when (as/match? match norm) shape))
                     shapes)
          shape-name (:name hit)
          fn-name    (:function hit)
          base       (mk :proc-spawn program rule cmd)
          base*      (cond-> base
                       shape-name (assoc-in [:provenance :argv-shape] shape-name)
                       fn-name    (assoc-in [:provenance :function]   fn-name))
          extras     (mapv (fn [{:keys [class scope rule]}]
                             (let [e (mk class scope (or rule (:rule hit) :argv-shape) cmd)]
                               (cond-> e
                                 shape-name (assoc-in [:provenance :argv-shape] shape-name)
                                 fn-name    (assoc-in [:provenance :function]   fn-name))))
                           (:extra-effects hit))]
      (into [base*] extras))))

;; ---- Interpreter classifiers -------------------------------------------

(defn shell-interpret-classifier
  "Public factory — emits a :shell-interpret effect under the given
   dialect (e.g. :posix, :bash, :clojure when reused by the bb overlay).
   Operator overlays use this to register interpreter-style tools that
   read their first non-option argument as shell source."
  [dialect rule]
  (fn [cmd _ctx]
    [(mk :shell-interpret (name dialect) rule cmd)]))

(defn interp-interpret-classifier
  "Public factory — emits :interp-interpret on the dialect plus an
   :opaque effect tagged `interp-source` because the body bytes are
   not analyzed at classify time. Operator overlays use this to
   register interpreters (perl, node, ruby, python variants) without
   touching source."
  [dialect rule]
  (fn [cmd _ctx]
    [(mk :interp-interpret (name dialect) rule cmd)
     ;; Body source is parse-stub-dialect → opaque
     (opaque :interp-source cmd)]))

;; ---- Default registry --------------------------------------------------

(def default-registry
  "Default program-classifier registry. ~60 programs across:
   core POSIX read, destructive, wrappers, interpreters, network,
   process. Programs not present → :opaque (fail-closed)."
  (merge
   ;; --- Core POSIX read ---
   ;; Two groups:
   ;;   (a) Listing/inspection commands — bare form reads cwd or errors.
   ;;       Keep `classify-fs-read` (bare form emits `fs-read:.`).
   ;;   (b) Stream processors — bare form reads stdin (common pipe-tail
   ;;       shape: `cmd | tail -30`, `cmd | jq .`, `cmd | wc -l`).
   ;;       Use `classify-stdin-or-fs-read` so the bare form emits
   ;;       `stdin-consume:stdin` instead of `fs-read:.`. v0.24.0 fix —
   ;;       closes the false-defer that fired under auto-mode-aggressive
   ;;       on every pipeline ending in one of these tools.
   ;; `find` is in this group because its leaf classifier is fs-read
   ;; over its path arg; the -exec body is composed in via :invokes
   ;; (shell-shape :find-exec strategy + classify-command's invoke
   ;; descent), so the :wraps? flag is set here even though the leaf
   ;; effect is fs-read rather than a wrapper's own effect.
   ;; Group (a) — listing / inspection (bare → fs-read:.). grep moved
   ;; to script-then-files (pattern at pos 1, not a path). basename/
   ;; dirname/which moved to pure / env-read (no fs read).
   (into {}
         (for [[prog rule]
               [["ls"       :ls]
                ["stat"     :stat]
                ["file"     :file-cmd]
                ["tree"     :tree]
                ["diff"     :diff]
                ["join"     :join]
                ["readlink" :readlink]
                ["realpath" :realpath]]]
           [prog {:doc      (str prog " — reads filesystem paths in args")
                  :classify (classify-fs-read rule)
                  :stdin    :data
                  :stdout   :data}]))

   ;; Group (b) — stream processors (bare → stdin-consume).
   ;; All positionals are paths; no script positional. Per-program
   ;; value-flags so e.g. `head -n 20 file` doesn't leak `20` as a
   ;; positional fs-read.
   (let [stream-progs
         [["cat"     :cat     #{}]
          ["head"    :head    #{"-n" "--lines" "-c" "--bytes" "-q" "--quiet"
                                "--silent" "--verbose"}]
          ["tail"    :tail    #{"-n" "--lines" "-c" "--bytes"}]
          ["wc"      :wc      #{}]
          ["sort"    :sort    #{"-k" "--key" "-t" "--field-separator"
                                "-T" "--temporary-directory"
                                "-S" "--buffer-size" "-o" "--output"
                                "--compress-program" "--files0-from"
                                "--random-source" "--batch-size"
                                "--parallel"}]
          ["uniq"    :uniq    #{"-f" "--skip-fields" "-s" "--skip-chars"
                                "-w" "--check-chars" "-d" "--group"}]
          ["cut"     :cut     #{"-d" "--delimiter" "-f" "--fields"
                                "-c" "--characters" "-b" "--bytes"
                                "--output-delimiter"}]
          ["tr"      :tr      #{}]
          ["fold"    :fold    #{"-w" "--width"}]
          ["paste"   :paste   #{"-d" "--delimiters"}]
          ["comm"    :comm    #{"--output-delimiter"}]
          ["od"      :od      #{"-A" "-j" "-N" "-S" "-t" "-w"}]
          ["xxd"     :xxd     #{"-c" "-g" "-l" "-s"}]
          ["hexdump" :hexdump #{"-e" "-f" "-n" "-s"}]
          ["less"    :less    #{}]
          ["more"    :more    #{}]]]
     (into {}
           (for [[prog rule vfs] stream-progs]
             [prog {:doc      (str prog " — stream processor: positional paths "
                                   "→ fs-read, bare form → stdin-consume")
                    :classify (classify-stdin-or-fs-read
                               rule
                               (when (seq vfs) {:value-flags vfs}))
                    :stdin    :data
                    :stdout   :data}])))

   ;; --- Script-then-files (awk / sed / jq) ---
   ;;
   ;; Programs whose FIRST positional is a script (filter expression /
   ;; program body) rather than a file path. v0.24.0 fix — the older
   ;; classify-fs-read / classify-stdin-or-fs-read would have treated
   ;; `awk '{}'` and `jq .` as `fs-read:"{}"` / `fs-read:"."`. The new
   ;; classify-script-then-files walks args once, recognizes
   ;; script-supplying flags (`-e`, `-f`), and emits the right
   ;; fs-read/stdin-consume shape.
   {"awk"
    {:doc      "awk — script positional + files; -f script-file, -F sep, -v var=val"
     :classify (classify-script-then-files
                {:rule              :awk
                 :script-via-flags  #{"-e" "--source"}
                 :script-file-flags #{"-f" "--file" "--exec"}
                 :value-flags       #{"-F" "-v" "--field-separator" "--assign"}
                 :emit-stdout?      true})
     :stdin    :data
     :stdout   :data}

    "sed"
    {:doc      "sed — script positional + files; -e expr, -f script-file, -i in-place"
     :classify (classify-script-then-files
                {:rule              :sed
                 :script-via-flags  #{"-e" "--expression"}
                 :script-file-flags #{"-f" "--file"}
                 :inplace-flags     #{"-i" "--in-place"}
                 :emit-stdout?      true})
     :stdin    :data
     :stdout   :data}

    "jq"
    {:doc      "jq — filter positional + files; -f filter-file, --arg/--argjson take 2"
     :classify (classify-script-then-files
                {:rule              :jq
                 :script-via-flags  #{}
                 :script-file-flags #{"-f" "--from-file"}
                 :value-flags       #{"--indent" "-L"}
                 :two-value-flags   #{"--arg" "--argjson"
                                      "--slurpfile" "--rawfile"}
                 :emit-stdout?      true})
     :stdin    :data
     :stdout   :data}

    ;; grep — PATTERN at first positional (not a path); files follow.
    ;; `-e PATTERN` / `-f PATTERNFILE` make the leading positional
    ;; implicit (no positional pattern needed). Without this, the
    ;; classifier wrongly emitted `:fs-read PATTERN`, deferring every
    ;; grep call under auto-mode-aggressive. v0.24.3 fix.
    "grep"
    {:doc      "grep — pattern at pos 1, paths follow; bare → stdin-consume"
     :classify (classify-script-then-files
                {:rule              :grep
                 :script-via-flags  #{"-e" "--regexp"}
                 :script-file-flags #{"-f" "--file"}
                 :value-flags       #{"-A" "--after-context"
                                      "-B" "--before-context"
                                      "-C" "--context"
                                      "-D" "--devices"
                                      "-d" "--directories"
                                      "-m" "--max-count"
                                      "--color" "--colour"
                                      "--include" "--exclude"
                                      "--exclude-from" "--exclude-dir"
                                      "--label" "--group-separator"}
                 :emit-stdout?      true})
     :stdin    :data
     :stdout   :data}}

   ;; --- find (v0.8.0 P0 — predicate/action discrimination) ---
   {"find" {:doc      "find — :fs-read on each starting path; :fs-delete on each
                       starting path when expression contains `-delete`. -exec/
                       -execdir/-ok handled by shell-shape :find-exec strategy
                       + classify-command :invokes descent."
            :classify find-classifier
            :wraps?   true
            :stdin    :data
            :stdout   :data}}

   ;; --- Pure / env-read-only ---
   ;; basename/dirname are pure string ops — they don't touch the
   ;; filesystem; their argv is a path-shaped *literal*, not a path
   ;; read. v0.24.3 fix: previously in Group (a), wrongly emitting
   ;; fs-read on every call.
   ;;
   ;; which scans $PATH (env-read) for the named command; it does not
   ;; fs-read the command-name literal. v0.24.3 fix.
   (into {}
         (for [[prog rule]
               [["echo"     :echo]
                ["printf"   :printf]
                ["basename" :basename]
                ["dirname"  :dirname]
                ["pwd"      :pwd]
                ["whoami"   :whoami]
                ["id"       :id]
                ["date"     :date]
                ["uname"    :uname]
                ["hostname" :hostname]
                ["type"     :type]
                ["alias"    :alias]
                ["which"    :which]
                ["true"     :true-cmd]
                ["false"    :false-cmd]
                ["test"     :test-cmd]
                ["["        :bracket]]]
           [prog {:doc      (str prog " — no filesystem side effects")
                  :classify (if (#{"echo" "printf" "basename" "dirname"} prog)
                              (stdout-emit-only rule)
                              (env-read-only rule))
                  :stdin    :data
                  :stdout   :data}]))

   ;; --- env (wrapper that may emit :env-mutate per VAR=val) ---
   {"env" {:doc      "env — sets vars (:env-mutate) and wraps a command"
           :classify (wrapper-classifier env-wrapper-own-effects)
           :wraps?   true
           :stdin    :data
           :stdout   :data}}

   ;; --- Destructive ---
   {"rm"      {:doc "rm — unlinks paths"
               :classify (classify-fs-delete :rm)
               :stdin :none :stdout :none}
    "unlink"  {:doc "unlink — single-path delete"
               :classify (classify-fs-delete :unlink)
               :stdin :none :stdout :none}
    "rmdir"   {:doc "rmdir — remove empty dir"
               :classify (classify-fs-delete :rmdir)
               :stdin :none :stdout :none}
    "mv"      {:doc "mv — read src, write dst"
               :classify (classify-fs-read-write :mv)
               :stdin :none :stdout :none}
    "cp"      {:doc "cp — read src, write dst"
               :classify (classify-fs-read-write :cp)
               :stdin :none :stdout :none}
    "ln"      {:doc "ln — write link path"
               :classify (classify-fs-write :ln)
               :stdin :none :stdout :none}
    "mkdir"   {:doc "mkdir — create directory"
               :classify (classify-fs-write :mkdir)
               :stdin :none :stdout :none}
    "touch"   {:doc "touch — create/update file"
               :classify (classify-fs-write :touch)
               :stdin :none :stdout :none}
    ;; chmod / chown — first positional is MODE / OWNER:GROUP spec,
    ;; NOT a path. Remaining positionals are paths. v0.24.3 fix —
    ;; previously emitted fs-write on the spec literal (e.g.
    ;; `fs-write:644`), polluting effect sets. Reuses
    ;; classify-script-then-files with file-class :fs-write.
    "chmod"   {:doc "chmod — MODE at pos 1, fs-write on paths"
               :classify (classify-script-then-files
                          {:rule        :chmod
                           :file-class  :fs-write
                           :value-flags #{"--reference"}})
               :stdin :none :stdout :none}
    "chown"   {:doc "chown — OWNER:GROUP at pos 1, fs-write on paths"
               :classify (classify-script-then-files
                          {:rule        :chown
                           :file-class  :fs-write
                           :value-flags #{"--reference" "--from"}})
               :stdin :none :stdout :none}}

   ;; --- Wrappers (effects = wrapper's own + delegate to :invokes) ---
   {"sudo"    {:doc "sudo — privilege elevation + wrapped command"
               :classify (wrapper-classifier sudo-own-effects)
               :stdin :data :stdout :data :wraps? true}
    "xargs"   {:doc "xargs — invokes wrapped command per stdin line"
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}
    "nice"    {:doc "nice — adjusts priority, delegates to wrapped"
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}
    "nohup"   {:doc "nohup — disowns signals, delegates"
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}
    "time"    {:doc "time — measures wrapped command"
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}
    "exec"    {:doc "exec — replaces shell with wrapped command"
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}
    "command" {:doc "command — runs program bypassing shell functions"
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}
    "eval"    {:doc "eval — interprets args as shell source"
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}

    ;; --- v0.7.0 ---
    "!"       {:doc "! cmd — negates exit status of wrapped command;
                          transparent wrapper, no own effects."
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}
    "coproc"  {:doc "coproc cmd — runs cmd in a coprocess; transparent
                          wrapper, no own effects. NAME-discrimination
                          (`coproc NAME cmd`) is deferred — NAME is
                          treated as the wrapped program for now."
               :classify (wrapper-classifier nil)
               :stdin :data :stdout :data :wraps? true}

    ;; --- T16: additional process-wrapper utilities (v0.6.0) ---
    ;; All delegate to :invokes; shell-shape's :positional-after-opts
    ;; strategy + :skip-positionals (for timeout/chrt/flock) populates
    ;; :invokes with the wrapped command.
    "timeout"     {:doc "timeout — kills wrapped command after duration"
                   :classify (wrapper-classifier nil)
                   :stdin :data :stdout :data :wraps? true}
    "ionice"      {:doc "ionice — set I/O scheduling for wrapped command"
                   :classify (wrapper-classifier nil)
                   :stdin :data :stdout :data :wraps? true}
    "chrt"        {:doc "chrt — set realtime scheduling for wrapped command"
                   :classify (wrapper-classifier nil)
                   :stdin :data :stdout :data :wraps? true}
    "setsid"      {:doc "setsid — run wrapped command in new session"
                   :classify (wrapper-classifier nil)
                   :stdin :data :stdout :data :wraps? true}
    "stdbuf"      {:doc "stdbuf — adjust stdio buffering for wrapped command"
                   :classify (wrapper-classifier nil)
                   :stdin :data :stdout :data :wraps? true}
    "flock"       {:doc "flock — acquire file lock, delegate to wrapped"
                   :classify (wrapper-classifier nil)
                   :stdin :data :stdout :data :wraps? true}
    ;; doas / runuser — like sudo, both emit :privilege-elevate.
    "doas"        {:doc "doas — OpenBSD's sudo equivalent"
                   :classify (wrapper-classifier sudo-own-effects)
                   :stdin :data :stdout :data :wraps? true}
    "runuser"     {:doc "runuser — run wrapped command as another user"
                   :classify (wrapper-classifier sudo-own-effects)
                   :stdin :data :stdout :data :wraps? true}
    ;; systemd-run — runs as transient systemd service; delegates to wrapped.
    "systemd-run" {:doc "systemd-run — run wrapped command as a systemd unit"
                   :classify (wrapper-classifier nil)
                   :stdin :data :stdout :data :wraps? true}

    ;; --- T14: shell builtins (cd, export, declare, trap, source, .) ---
    "cd"      {:doc "cd PATH — emits :env-mutate on PWD"
               :classify cd-classifier
               :stdin :none :stdout :none}
    "export"  {:doc "export VAR=val — emits :env-mutate per assigned variable"
               :classify (export-or-declare-classifier :export)
               :stdin :none :stdout :none}
    "declare" {:doc "declare/typeset — emits :env-mutate per declared variable"
               :classify (export-or-declare-classifier :declare)
               :stdin :none :stdout :none}
    "typeset" {:doc "typeset — bash alias for declare"
               :classify (export-or-declare-classifier :typeset)
               :stdin :none :stdout :none}
    "trap"    {:doc "trap 'CMD' SIG — v0.7.0 recursively classifies the
                          literal CMD body and prefixes each scope with
                          `trap:` so policy authors discriminate
                          command-may-fire-at-signal from command-runs-now.
                          Dynamic body → :opaque trap:dynamic."
               :classify trap-classifier
               :stdin :none :stdout :none}
    "source"  {:doc "source FILE — reads + interprets script in current shell"
               :classify (source-classifier :source)
               :stdin :none :stdout :none}
    "."       {:doc ". FILE — POSIX equivalent of source"
               :classify (source-classifier :dot)
               :stdin :none :stdout :none}}

   ;; --- Interpreters ---
   {"bash"    {:doc "bash — interprets shell source from -c, stdin, or file arg"
               :classify (shell-interpret-classifier :bash :bash-interp)
               :stdin :shell-source :stdout :data :reads-stdin? true}
    "sh"      {:doc "sh — POSIX shell"
               :classify (shell-interpret-classifier :posix :sh-interp)
               :stdin :shell-source :stdout :data :reads-stdin? true}
    "zsh"     {:doc "zsh — Z-shell"
               :classify (shell-interpret-classifier :zsh :zsh-interp)
               :stdin :shell-source :stdout :data :reads-stdin? true}
    "fish"    {:doc "fish — friendly interactive shell"
               :classify (interp-interpret-classifier :fish :fish-interp)
               :stdin :shell-source :stdout :data :reads-stdin? true}
    "python"  {:doc "python — interpreter; body opaque to v0.5.0"
               :classify (interp-interpret-classifier :python :python-interp)
               :stdin :interp-source :stdout :data :reads-stdin? true}
    "python3" {:doc "python3 — interpreter; body opaque to v0.5.0"
               :classify (interp-interpret-classifier :python :python3-interp)
               :stdin :interp-source :stdout :data :reads-stdin? true}
    "node"    {:doc "node — JavaScript runtime; body opaque"
               :classify (interp-interpret-classifier :node :node-interp)
               :stdin :interp-source :stdout :data :reads-stdin? true}
    "ruby"    {:doc "ruby — interpreter; body opaque"
               :classify (interp-interpret-classifier :ruby :ruby-interp)
               :stdin :interp-source :stdout :data :reads-stdin? true}
    "perl"    {:doc "perl — interpreter; body opaque"
               :classify (interp-interpret-classifier :perl :perl-interp)
               :stdin :interp-source :stdout :data :reads-stdin? true}}

   ;; --- Network ---
   {"curl"    {:doc "curl — HTTP client (net-out)"
               :classify (classify-net-out :curl)
               :stdin :data :stdout :data}
    "wget"    {:doc "wget — file download (net-out + fs-write to cwd)"
               :classify (fn [cmd ctx]
                           (vec (concat ((classify-net-out :wget) cmd ctx)
                                        [{:class :fs-write :scope "."
                                          :provenance {:rule :wget
                                                       :program "wget"}}])))
               :stdin :data :stdout :data}
    "ssh"     {:doc "ssh — remote shell; v0.7.0 recursively classifies the
                        remote command body when literal, prefixing each
                        emitted scope with `ssh:<host>:` so local grants
                        don't authorize remote effects."
               :classify ssh-classifier
               :wraps?   true
               :stdin :data :stdout :data}
    "scp"     {:doc "scp — secure copy (net-out + fs-read + fs-write)"
               :classify (fn [cmd ctx]
                           (vec (concat ((classify-net-out :scp) cmd ctx)
                                        ((classify-fs-read-write :scp) cmd ctx))))
               :stdin :data :stdout :data}
    "rsync"   {:doc "rsync — remote sync"
               :classify (fn [cmd ctx]
                           (vec (concat ((classify-net-out :rsync) cmd ctx)
                                        ((classify-fs-read-write :rsync) cmd ctx))))
               :stdin :data :stdout :data}
    "nc"      {:doc "nc — netcat; net-out or net-in depending on -l"
               :classify (fn [cmd _ctx]
                           (let [lits (arg-literals cmd)]
                             (if (some #{"-l" "--listen"} lits)
                               [(mk :net-in "?" :nc cmd)]
                               (let [targets (net-targets-from-args cmd)]
                                 (cond
                                   (nil? targets)   [(opaque :variable-arg cmd)]
                                   (empty? targets) [(opaque :no-target cmd)]
                                   :else (mapv #(mk :net-out % :nc cmd) targets))))))
               :stdin :data :stdout :data}
    "netcat"  {:doc "netcat — alias for nc"
               :classify (fn [cmd _ctx]
                           (let [targets (net-targets-from-args cmd)]
                             (cond
                               (nil? targets)   [(opaque :variable-arg cmd)]
                               (empty? targets) [(opaque :no-target cmd)]
                               :else (mapv #(mk :net-out % :netcat cmd) targets))))
               :stdin :data :stdout :data}
    "mail"    {:doc "mail RECIPIENT [-s SUBJECT] — v0.7.0 classifies the
                          literal recipient(s) as :net-out scope=address."
               :classify mail-classifier
               :stdin :data :stdout :none}
    "mailx"   {:doc "mailx — alias for mail"
               :classify mail-classifier
               :stdin :data :stdout :none}
    "sendmail" {:doc "sendmail — SMTP injection; recipient args are literal."
                :classify mail-classifier
                :stdin :data :stdout :none}}

   ;; --- v0.8.0 P0: baseline :proc-spawn for common LLM-emitted programs ---
   ;; Pre-v0.8.0 these fell to :opaque "unclassified-program:<x>", forcing
   ;; the loaded-gun right(:opaque, "**", _) grant. Baseline :proc-spawn
   ;; moves them off opaque; per-subcommand discrimination (e.g. git push
   ;; --force vs git status) lands in P2's argv-shape DSL.
   {"git"    {:doc      "git — VCS; argv-shape per-subcommand discrimination
                          via getopt-normalized predicate-set DSL."
              :classify
              (argv-shape-classifier
               "git" :git
               (merge gs/gnu-with-subcommand
                      {:opts-with-arg #{"-c" "--git-dir" "--work-tree"
                                        "--namespace" "-C"}})
               [;; Push with destructive flags first — most specific shape.
                {:name     "git-push-destructive"
                 :function "push"
                 :match    {:requires #{(as/positional 0 "push")
                                        (as/any-of [(as/has-flag "--force")
                                                    (as/has-flag "-f")
                                                    (as/has-flag "--force-with-lease")
                                                    (as/has-flag "--mirror")
                                                    (as/has-flag "--delete")])}}
                 :extra-effects [{:class :net-out :scope "git-push" :rule :git-push-destructive}]}
                ;; Plain push.
                {:name     "git-push"
                 :function "push"
                 :match    {:requires #{(as/positional 0 "push")}}
                 :extra-effects [{:class :net-out :scope "git-push" :rule :git-push}]}
                ;; Network reads: clone / fetch / pull.
                {:name     "git-clone"
                 :function "clone"
                 :match    {:requires #{(as/positional 0 "clone")}}
                 :extra-effects [{:class :net-out :scope "git-clone" :rule :git-clone}]}
                {:name     "git-fetch"
                 :function "fetch"
                 :match    {:requires #{(as/positional 0 "fetch")}}
                 :extra-effects [{:class :net-out :scope "git-fetch" :rule :git-fetch}]}
                {:name     "git-pull"
                 :function "pull"
                 :match    {:requires #{(as/positional 0 "pull")}}
                 :extra-effects [{:class :net-out :scope "git-pull" :rule :git-pull}]}
                ;; Safe-read subcommands — coordinate-only pin, no extra effects.
                {:name     "git-status"  :function "status"
                 :match    {:requires #{(as/positional 0 "status")}}}
                {:name     "git-log"     :function "log"
                 :match    {:requires #{(as/positional 0 "log")}}}
                {:name     "git-diff"    :function "diff"
                 :match    {:requires #{(as/positional 0 "diff")}}}
                {:name     "git-show"    :function "show"
                 :match    {:requires #{(as/positional 0 "show")}}}
                {:name     "git-branch"  :function "branch"
                 :match    {:requires #{(as/positional 0 "branch")}}}
                {:name     "git-config"  :function "config"
                 :match    {:requires #{(as/positional 0 "config")}}}
                ;; Local writes: commit / add / rm / restore / reset.
                {:name     "git-commit"  :function "commit"
                 :match    {:requires #{(as/positional 0 "commit")}}}
                {:name     "git-add"     :function "add"
                 :match    {:requires #{(as/positional 0 "add")}}}
                {:name     "git-rm"      :function "rm"
                 :match    {:requires #{(as/positional 0 "rm")}}}])
              :stdin :data :stdout :data}
    "tar"    {:doc      "tar — archive utility; argv-shape discriminates
                          create / extract / list via bundled-flag spec."
              :classify
              (argv-shape-classifier
               "tar" :tar
               gs/tar-bundled
               [{:name     "tar-create"   :function "create"
                 :match    {:requires #{(as/has-flag "-c")}}}
                {:name     "tar-extract"  :function "extract"
                 :match    {:requires #{(as/any-of [(as/has-flag "-x")
                                                    (as/has-flag "--extract")])}}}
                {:name     "tar-list"     :function "list"
                 :match    {:requires #{(as/has-flag "-t")}}}
                {:name     "tar-update"   :function "update"
                 :match    {:requires #{(as/has-flag "-u")}}}
                {:name     "tar-append"   :function "append"
                 :match    {:requires #{(as/has-flag "-r")}}}])
              :stdin :data :stdout :data}}

   ;; --- Process ---
   {"kill"    {:doc "kill — signal process"
               :classify (proc-signal-classifier :kill "?")
               :stdin :none :stdout :none}
    "pkill"   {:doc "pkill — signal processes selected by name pattern"
               :classify (proc-signal-classifier :pkill "?")
               :stdin :none :stdout :none}
    "ps"      {:doc "ps — process listing (env-read)"
               :classify (env-read-only :ps)
               :stdin :none :stdout :data}
    "pgrep"   {:doc "pgrep — list PIDs by name pattern (env-read)"
               :classify (env-read-only :pgrep)
               :stdin :none :stdout :data}
    "top"     {:doc "top — process listing"
               :classify (env-read-only :top)
               :stdin :none :stdout :data}
    "jobs"    {:doc "jobs — shell job listing"
               :classify (env-read-only :jobs)
               :stdin :none :stdout :data}
    "wait"    {:doc "wait — wait on background job"
               :classify (pure :wait)
               :stdin :none :stdout :none}}

   ;; --- Filesystem inventory / system observability ---
   ;;
   ;; df/free/uptime read kernel-maintained state, not the filesystem
   ;; contents (so :env-read, not :fs-read). du walks the directory
   ;; tree to sum sizes — that IS fs-read on the starting paths.
   ;; whereis scans $PATH + $MANPATH for the named binaries/man pages;
   ;; same envelope as `which` (env-read, no fs-read on the name).
   {"df"      {:doc "df — disk free / mount table (env-read)"
               :classify (env-read-only :df)
               :stdin :none :stdout :data}
    "du"      {:doc "du — disk usage; fs-read on each starting path"
               :classify (classify-fs-read :du)
               :stdin :none :stdout :data}
    "free"    {:doc "free — memory stats (env-read)"
               :classify (env-read-only :free)
               :stdin :none :stdout :data}
    "uptime"  {:doc "uptime — system uptime + load avg (env-read)"
               :classify (env-read-only :uptime)
               :stdin :none :stdout :data}
    "whereis" {:doc "whereis — locate binary/source/man pages via
                     $PATH/$MANPATH lookup (env-read)"
               :classify (env-read-only :whereis)
               :stdin :none :stdout :data}}

   ;; --- Pure / idle / stdout-only ---
   {"sleep"   {:doc "sleep — pure idle"
               :classify (pure :sleep)
               :stdin :none :stdout :none}
    "yes"     {:doc "yes — emit STRING (or `y`) until killed; stdout-only"
               :classify (stdout-emit-only :yes)
               :stdin :none :stdout :data}}

   ;; --- I/O multiplexer (tee) ---
   ;;
   ;; tee reads stdin and writes BOTH to stdout AND to each positional
   ;; file. Existing classify-fs-write only emits :stdin-consume when
   ;; the literal `-` arg is present; tee's stdin consumption is
   ;; unconditional, so we inline a small classifier here that
   ;; always emits :stdin-consume alongside per-path :fs-write.
   {"tee"     {:doc "tee — stdin → stdout AND fs-write per positional path
                     (truncated by default, appended with -a)"
               :classify (fn [cmd _ctx]
                           (let [r (non-option-positional-literals cmd)]
                             (cond
                               (nil? r)            [(opaque :variable-arg cmd)]
                               (empty? (:paths r)) [(stdin-consume-effect :tee cmd)]
                               :else               (conj (mapv #(mk :fs-write % :tee cmd)
                                                               (:paths r))
                                                         (stdin-consume-effect :tee cmd)))))
               :stdin :consume :stdout :data}}

   ;; --- GitHub CLI ---
   ;;
   ;; gh's subcommand semantics aren't statically resolvable (gh issue
   ;; create vs gh repo view differ wildly), but every non-trivial
   ;; invocation hits the GitHub API. Static :net-out to api.github.com
   ;; gives policy authors a single grant scope to allow/deny; the
   ;; overlay can layer sharper rules per-subcommand later.
   {"gh"      {:doc "gh — GitHub CLI; static :net-out to api.github.com.
                     Overlay can refine per-subcommand."
               :classify (fn [cmd _ctx]
                           [(mk :net-out "api.github.com" :gh cmd)])
               :stdin :data :stdout :data}}

   ;; --- JVM / Clojure interpreters ---
   ;;
   ;; bb/clojure/clj all execute Clojure source from -e, -f, or a
   ;; positional file path. Full body classification would require
   ;; running a Clojure analyzer over the script — out of scope for the
   ;; substrate. Conservative: :proc-spawn scoped on the program name
   ;; (so policy authors can grant the launcher itself); the overlay or
   ;; a future shell-shape `:bb` interpreter can sharpen.
   {"bb"      {:doc "bb — Babashka script runner. Conservative :proc-spawn;
                     overlay or future :bb interpreter dialect refines."
               :classify (proc-spawn-classifier :bb)
               :stdin :data :stdout :data}
    "clojure" {:doc "clojure — Clojure CLI. Conservative :proc-spawn."
               :classify (proc-spawn-classifier :clojure)
               :stdin :data :stdout :data}
    "clj"     {:doc "clj — Clojure CLI w/ rebel-readline. Alias of clojure."
               :classify (proc-spawn-classifier :clj)
               :stdin :data :stdout :data}}))

(def ^:dynamic *registry-override*
  "Dynamic-var seam used by two consumers:

   - **Operator overlay (v0.28+)**: at daemon startup,
     `shell-shape-classify.overlay/install!` does
     `(alter-var-root #'*registry-override* (constantly merged))`.
     This sets the ROOT binding so every thread sees the overlay-merged
     registry automatically.

   - **Mutation harness**: tests use `(binding [eff/*registry-override*
     mutated] (run-tests))` to swap the registry per-thread. The
     thread-local shadow wins over the root binding for the duration
     of the binding.

   When nil (the original root), `active-registry` returns
   `default-registry` unchanged."
  nil)

(defn active-registry
  "Return the registry currently in effect — `*registry-override*` if
   bound, otherwise `default-registry`. Callers that previously took
   a `:registry` ctx key should now omit it; classify-tree threads
   `(active-registry)` automatically when ctx doesn't carry one."
  []
  (or *registry-override* default-registry))

(defn lookup
  "Return the classifier spec for program (string), or nil."
  ([program] (lookup (active-registry) program))
  ([registry program] (get registry program)))

(defn classify-command
  "Apply a program-classifier (from registry) to a single :command
   node. Returns a vector of effect-instances. Programs not in the
   registry get a single :opaque effect with reason :unclassified-program.

   `ctx` is reserved for future composition state."
  [registry cmd ctx]
  (let [prog (:program cmd)]
    (cond
      (nil? prog) []
      (not (string? prog))
      [{:class :opaque :scope "variable-program"
        :provenance {:rule :variable-program :program (or prog "?")}}]
      :else
      (if-let [spec (lookup registry prog)]
        ((:classify spec) cmd ctx)
        [{:class :opaque :scope (str "unclassified-program:" prog)
          :provenance {:rule :unclassified-program :program prog}}]))))
