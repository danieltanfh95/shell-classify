(ns shell-classify.bindings
  "P7a — same-string variable resolution.

  When the LLM emits `DANGER=\"rm /tmp/x\"; eval $DANGER`, the assignment
  is visible in the same command string. This namespace builds a
  binding-table from that visible scope and exposes helpers to substitute
  resolved `:var` parts in tokens, so the classifier sees a literal form
  and emits real effects instead of `:opaque :reason :{dynamic-eval,
  variable-args, variable-program}`.

  shell-shape has no binding-table for bash (assignments are bare `:word`
  tokens). Resolution is witness-side. The binding-table flows through
  `classify-tree`'s ctx; `classify-pipeline` threads it forward across
  chain-tail (sequence) and isolates it across pipe stages (subshell
  semantics).

  Scope rules:
  - Standalone assignment (`FOO=bar` as a single-stage pipeline) — binds
    `FOO` for subsequent commands in the same sequence.
  - Prefix assignment (`FOO=bar cmd`) — binds `FOO` only for `cmd`'s
    own classification.
  - Subshell `(FOO=bar; cmd)` — does NOT escape; outer scope unchanged.
  - Brace `{ FOO=bar; cmd; }` — DOES propagate to outer scope.
  - Non-literal RHS (`FOO=$BAR`, `FOO=$(cmd)`) — binding NOT recorded;
    the var stays opaque on resolution attempt.
  - Re-assignment — last-wins by occurrence order.
  - Pipe stages (`A | B`) — each stage is a subshell; assignments inside
    don't escape, but outer bindings are visible.

  The same-string scope is sound: the value was emitted in the source
  the LLM produced. No pre-existing secret leaks. Compare P7c which
  reads from process env and requires an explicit allowlist."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Assignment shape — bash NAME=VALUE token pattern.
;; ---------------------------------------------------------------------------

(def ^:private assignment-re
  "Matches a bash assignment-shape literal: NAME=VALUE where NAME is a
   valid POSIX identifier. The VALUE part is everything after the first
   `=` — quoting has already been processed by shell-shape's tokenizer."
  #"^([A-Za-z_][A-Za-z0-9_]*)=(.*)$")

(defn assignment-shape?
  "Does `s` look like an assignment literal (`NAME=VALUE`)?"
  [s]
  (boolean (and (string? s) (re-find assignment-re s))))

(defn- parse-name-value
  "Split `s` at the first `=`. Returns `{:name <NAME> :value <VALUE>}` or
   `nil` if `s` isn't assignment-shaped."
  [s]
  (when-let [[_ nm v] (re-find assignment-re s)]
    {:name nm :value v}))

;; ---------------------------------------------------------------------------
;; Token / part introspection.
;;
;; In shell-shape, an arg is a `:token` with `:parts [<part>]`. Each part
;; is `{:kind :literal :value <str>}`, `{:kind :var :name <str> :default
;; <str-or-nil>}`, `{:kind :subst :body :script}`, or `{:kind :backtick
;; :body :script}`. The token's `:literal` is the concatenation of literal
;; parts (or nil/incomplete when non-literal parts are present); `:raw` is
;; the source span.
;;
;; A `:program` field is either a string (all-literal) or a token-map (one
;; or more non-literal parts — typically `$CMD`).
;; ---------------------------------------------------------------------------

(defn- all-literal-parts? [parts]
  (every? #(= :literal (:kind %)) parts))

(defn- parts->literal-string
  "Concatenate the `:value` of every `:literal` part. Caller has already
   verified `all-literal-parts?`."
  [parts]
  (apply str (mapv :value parts)))

;; ---------------------------------------------------------------------------
;; Binding extraction from a :command node.
;; ---------------------------------------------------------------------------

(defn- token-assignment-prefix
  "If `tok` (a :token map or string) starts with `NAME=`, return
   `{:name :value-prefix-string :rest-parts}` where:
   - `:value-prefix-string` is the literal-string of the part containing
     the `NAME=` prefix, AFTER the `=` (so just the part of the value
     within the first part).
   - `:rest-parts` is the parts vector for the rest of the value
     (everything after the part containing `=`).
   Returns nil if `tok` doesn't start with an assignment prefix."
  [tok]
  (cond
    (string? tok)
    (when-let [{:keys [name value]} (parse-name-value tok)]
      {:name name
       :value-prefix value
       :rest-parts []})

    (and (map? tok) (seq (:parts tok)))
    (let [first-part (first (:parts tok))]
      (when (and (= :literal (:kind first-part))
                 (assignment-shape? (:value first-part)))
        (let [{:keys [name value]} (parse-name-value (:value first-part))]
          {:name name
           :value-prefix value
           :rest-parts (vec (rest (:parts tok)))})))))

(defn- value-string-or-nil
  "Given `value-prefix` (literal string from the assignment's first
   part, after `=`) and `rest-parts` (subsequent parts of the token), return
   the full literal value string if all parts are literal, else nil
   (binding is non-literal — unresolvable)."
  [value-prefix rest-parts]
  (when (all-literal-parts? rest-parts)
    (str value-prefix (parts->literal-string rest-parts))))

(defn command-binding
  "Inspect a :command node. Returns one of:
   - `{:kind :standalone :name :value}` — assignment-only command
     (`FOO=bar` with no further args/redirects). Binding propagates to
     subsequent commands.
   - `{:kind :prefix :bindings [{:name :value} ...] :stripped-cmd}` —
     prefix-style (`FOO=bar BAZ=qux cmd ...`). Bindings propagate only
     to `:stripped-cmd`.
   - `nil` — not an assignment-shape command.

   `:value` is the literal string OR `nil` if RHS has non-literal parts
   (var/subst/backtick). A `nil` value means \"we acknowledge this is an
   assignment but can't resolve the RHS\" — the var stays opaque on
   substitution attempts."
  [cmd]
  (let [prog (:program cmd)
        args (:args cmd)
        redirects (:redirects cmd)]
    (cond
      ;; Standalone: assignment-shape program, no args, no redirects.
      (and (or (and (string? prog) (assignment-shape? prog))
               (and (map? prog)
                    (when-let [first-part (first (:parts prog))]
                      (and (= :literal (:kind first-part))
                           (assignment-shape? (:value first-part))))))
           (empty? args)
           (empty? redirects))
      (let [{:keys [name value-prefix rest-parts]}
            (token-assignment-prefix prog)]
        {:kind :standalone
         :name name
         :value (value-string-or-nil value-prefix rest-parts)})

      ;; Prefix-style: assignment-shape :program AND non-empty :args.
      ;; Walk leading args that are also assignment-shape; the first
      ;; non-assignment arg becomes the stripped program.
      (and (or (and (string? prog) (assignment-shape? prog))
               (and (map? prog)
                    (when-let [first-part (first (:parts prog))]
                      (and (= :literal (:kind first-part))
                           (assignment-shape? (:value first-part))))))
           (seq args))
      (let [first-pref (token-assignment-prefix prog)
            ;; Walk args, collecting more prefix-assignments until we hit a non-assignment.
            [more-prefs remaining]
            (loop [ps [] xs args]
              (if (empty? xs)
                [ps xs]
                (let [a (first xs)
                      pref (token-assignment-prefix a)]
                  (if pref
                    (recur (conj ps pref) (rest xs))
                    [ps xs]))))
            all-prefs (cons first-pref more-prefs)
            new-prog (first remaining)
            new-args (vec (rest remaining))]
        (when new-prog
          {:kind :prefix
           :bindings (mapv (fn [{:keys [name value-prefix rest-parts]}]
                             {:name name
                              :value (value-string-or-nil value-prefix rest-parts)})
                           all-prefs)
           :stripped-cmd (-> cmd
                             (assoc :program (if (string? new-prog)
                                               new-prog
                                               (:literal new-prog))
                                    :args new-args)
                             ;; If the new program is a non-literal token,
                             ;; we still need to set :program to the token
                             ;; so downstream variable-program resolution
                             ;; can fire on it. Use :literal if string,
                             ;; else the token map directly.
                             (assoc :program (if (and (map? new-prog)
                                                      (not (all-literal-parts?
                                                            (:parts new-prog))))
                                               new-prog
                                               (cond
                                                 (string? new-prog) new-prog
                                                 (map? new-prog) (:literal new-prog)
                                                 :else new-prog))))}))

      :else nil)))

;; ---------------------------------------------------------------------------
;; Substitution.
;; ---------------------------------------------------------------------------

(defn- resolve-part
  "If `part` is `:var` with `:name` in `bindings`, return a synthetic
   `:literal` part carrying the resolved value. Otherwise return `part`
   unchanged."
  [bindings part]
  (if (and (= :var (:kind part))
           (contains? bindings (:name part)))
    {:kind :literal :value (get bindings (:name part))}
    part))

(defn- parts-fully-resolvable?
  "Would `(map (partial resolve-part bindings) parts)` produce an all-
   literal sequence?"
  [bindings parts]
  (every? (fn [p]
            (or (= :literal (:kind p))
                (and (= :var (:kind p))
                     (contains? bindings (:name p)))))
          parts))

(defn resolve-token
  "Given a `:token` map and the current binding-table, return a new token
   with `:var` parts substituted (where the var is in `bindings`).
   Subst / backtick parts are not resolved (those are runtime computation).

   Returns `nil` if not all non-literal parts could be resolved — caller
   then falls back to the existing opaque emission."
  [bindings tok]
  (let [parts (:parts tok)]
    (when (parts-fully-resolvable? bindings parts)
      (let [new-parts (mapv (partial resolve-part bindings) parts)
            new-literal (parts->literal-string new-parts)]
        (assoc tok
               :parts new-parts
               :literal new-literal
               ;; :raw stays the source span — it's used for diagnostics,
               ;; not re-parsing. The resolved form lives in :literal.
               )))))

(defn resolve-program-token
  "When a :command's :program is a token-map (non-literal), try to
   resolve it to a literal program-name string via bindings. Returns
   the resolved string OR nil if not all :var parts resolve.

   Subst / backtick parts in :program currently make this fail (we
   don't execute substitutions). That's fine — those cases stay opaque."
  [bindings program]
  (when (and (map? program) (seq (:parts program)))
    (when-let [resolved (resolve-token bindings program)]
      (:literal resolved))))

(defn resolve-args
  "Apply `resolve-token` to each :token in `args`. :process-sub args are
   left alone (their bodies are recursive scripts, classified
   separately). Returns a new args vector with resolved tokens
   substituted; unresolvable tokens are kept as-is.

   This is non-destructive — the original args vector is left
   alone; downstream classifiers see a vector where var-references that
   COULD be resolved have been substituted in place."
  [bindings args]
  (mapv (fn [a]
          (if (and (= :token (:kind a))
                   (some #(= :var (:kind %)) (:parts a)))
            (or (resolve-token bindings a) a)
            a))
        args))

(defn resolve-command
  "Apply resolution to :program and :args of a :command node. Returns a
   new cmd map. :redirects and other fields pass through unchanged."
  [bindings cmd]
  (if (empty? bindings)
    cmd
    (let [prog (:program cmd)
          new-prog (cond
                     (string? prog) prog
                     (map? prog) (or (resolve-program-token bindings prog) prog)
                     :else prog)
          new-args (resolve-args bindings (:args cmd))]
      (assoc cmd :program new-prog :args new-args))))

;; ---------------------------------------------------------------------------
;; Re-stringify for re-parsing (for :unresolved invokes).
;;
;; The :unresolved nodes from shell-shape's wrapper layer carry :raw
;; (the source text of the wrapped body) but not the structured args.
;; To re-classify, we need to substitute via bindings.
;;
;; For most cases we have access to the parent :command's :args
;; (passed via ctx :current-cmd). For those, we use resolve-args and
;; concatenate the resulting literal strings.
;; ---------------------------------------------------------------------------

(defn args->joined-literal
  "Concatenate the :literal of every arg-token with a space separator.
   Returns nil if any arg-token's :literal is missing (some part wasn't
   resolved). :process-sub args break the join — return nil. This is
   the substitution path for `eval`-style joined-args wrappers."
  [args]
  (when (every? (fn [a]
                  (and (= :token (:kind a))
                       (some? (:literal a))
                       (every? #(= :literal (:kind %)) (:parts a))))
                args)
    (str/join " " (mapv :literal args))))

;; ---------------------------------------------------------------------------
;; Resolvability predicates — used by classify and effects to decide
;; whether to skip an :opaque emission in favor of a resolved-form
;; classification.
;; ---------------------------------------------------------------------------

(defn args-fully-resolvable?
  "True if every arg-token's :parts is either literal or a :var present
   in `bindings`. After `resolve-args`, the joined-literal would be
   non-nil. Returns false on :process-sub or unrecognized arg kinds."
  [bindings args]
  (every? (fn [a]
            (and (= :token (:kind a))
                 (parts-fully-resolvable? bindings (:parts a))))
          args))

(defn invocation-resolvable?
  "Would P7a's `try-resolve-unresolved` succeed for `inv` given the
   parent command `cmd` and binding-table `bindings`? Mirror of the
   case-dispatch in `classify/try-resolve-unresolved`. Also returns
   true when `cmd` has already been resolved by `resolve-command`
   (the wrapper's :unresolved entry is then moot — the classifier
   has classified the concrete form)."
  [bindings cmd inv]
  (case (:reason inv)
    (:dynamic-eval :variable-args)
    (args-fully-resolvable? bindings (:args cmd))

    :variable-program
    (or (string? (:program cmd))
        (some? (resolve-program-token bindings (:program cmd))))

    false))

;; ---------------------------------------------------------------------------
;; P7c — collect :var :name strings from a :command. Used by env-resolve
;; integration to determine which vars in a cmd's parts came from env vs
;; from the same-string binding table.
;; ---------------------------------------------------------------------------

(defn- token-var-names [tok]
  (when (and (map? tok) (= :token (:kind tok)))
    (keep (fn [p] (when (= :var (:kind p)) (:name p))) (:parts tok))))

(defn collect-var-names
  "Return a set of `:var :name` strings reachable from `cmd`'s
   `:program` (when a token) and each arg-token. Subst / backtick
   parts are NOT recursed (those are runtime computation; their inner
   tokens are out of scope for the same-string binding pass)."
  [cmd]
  (let [from-prog (token-var-names (:program cmd))
        from-args (mapcat token-var-names (:args cmd))]
    (set (concat from-prog from-args))))
