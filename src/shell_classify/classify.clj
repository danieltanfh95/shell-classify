(ns shell-classify.classify
  "Workflow-tree → effect-set closure pass.

  Given a populated `shell-shape.core/parse` tree, walk every command,
  apply the program-classifier registry from `effects.clj`, and union
  in the effects contributed by composition edges (pipe, chain,
  redirect, heredoc, here-string, program-source, wrappers, subshell
  substitutions, dynamic-eval).

  The result is a flat vector of effect-instances. Downstream consumers
  (`normalize.clj`) project the set into biscuit facts; the chain
  (`policy.datalog`) then decides allow/deny at the effect level.

  ---- Data shapes ----

    Input (workflow-tree):
      :script   {:kind :script   :nodes [<pipeline>]}
      :pipeline {:kind :pipeline :stages [<command>] :chain-tail [...]}
      :command  {:kind :command  :program <str-or-token>
                                 :args [<token>] :redirects [<rd>]
                                 :invokes [<inv>] :program-sources [<ps>]}
      :redirect {:kind :redirect :op :<|:>|:>>|:<<|:<<< :target <token-or-heredoc>}
      :ps       {:kind :program-source :body-parse <tree-or-unresolved>}

    Output (effect-set, flat vector):
      [{:class <effect-class-kw>
        :scope <scope-string>
        :provenance {:rule <kw> :program <str>}}]

  ---- Composition rules ----

    Pipe        (A | B): union(effects-of A, effects-of B). Program-source
                         classification is already attached as
                         :program-sources during the shell-shape
                         composition pass — handled at the stage level.
    Chain       (A ; B, A && B, A || B): conservative union — both may run.
    Redirect    > FILE: producing command gains {:fs-write FILE}.
                < FILE: consuming command gains {:fs-read FILE}.
                <<HEREDOC / <<<HERESTRING: handled via :program-sources
                  (heredoc & here-string both produce a program-source).
    Wrapper     (sudo X, xargs X, find -exec X): wrapper's own effects +
                  recursive walk of :invokes.
    Subshell    $(X), `X`: X's effects union'd; X's stdout flows into
                  outer cmd's args (opaque arg content).
    Group       (X), { X; }: classify body recursively + group-bound
                              redirects; pipeline stage peer of :command.
    Process-sub <(X), >(X): outer command emits :fs-read or :fs-write
                              on /dev/fd/* (synthesized by the program
                              classifier via arg-literals); inner cmd
                              effects flow through classify-arg-token's
                              recursive descent into :body.
    Eval        eval LITERAL: shell-shape's wrapper-spec recursively
                              parses; witness classifies the parse as
                              normal. Dynamic body falls to :opaque via
                              shell-shape's :unresolved emission.
    ssh / trap  host CMD, trap CMD SIG: registry classifier recursively
                              parses the literal CMD via ctx :ss-parse +
                              ctx :classify-tree, prefixes emitted scopes
                              with `ssh:<host>:` / `trap:` so policy
                              authors discriminate execution context.
    Heredoc on  receiver gets the body's effects via :program-source
    interpreter classification (shell-shape did the recursive parse).
    Broken      cat <<EOF | grep | sh — :program-source body-parse is
    chain      :unresolved {:reason :unknown-stdin-as-source} → opaque."
  (:require [clojure.string :as str]
            [shell-classify.bindings :as bind]
            [shell-classify.call :as call]
            [shell-classify.classifiers.node :as node-cls]
            [shell-classify.classifiers.perl :as perl-cls]
            [shell-classify.classifiers.python :as py-cls]
            [shell-classify.classifiers.ruby :as ruby-cls]
            [shell-classify.effects :as eff]
            [shell-classify.env-resolve :as env]
            [shell-classify.passthrough-payload :as pp]
            [shell-shape.core :as ss]))

(declare classify-tree classify-tree* classify-pipeline classify-pipeline*
         classify-command classify-command-resolved
         classify-group classify-group*
         classify-redirect classify-program-source classify-invocation
         classify-arg-token classify-stage classify-stage*
         try-resolve-unresolved)

;; ---- Token helpers (read literal value when present) -------------------

(defn- literal-of
  "Best-effort literal string from a :token; nil if any part is non-literal."
  [tok]
  (when (and (= :token (:kind tok))
             (every? #(= :literal (:kind %)) (:parts tok)))
    (apply str (mapv :value (:parts tok)))))

;; ---- Redirects ---------------------------------------------------------

(def ^:private fs-write-ops #{:> :>> :&>})
(def ^:private fs-read-ops  #{:<})
(def ^:private fd-dup-ops
  "fd-duplicate redirects — `>&N`, `<&N`, `>&-`, `<&-`. The kernel
   relabels file descriptors; no new I/O sink, no new fs-read source.
   shell-shape's tokenizer recognizes these inline and emits
   `:target {:kind :fd-ref}`. Classifying as no-effect prevents
   spurious `opaque:redirect-variable-target` from firing on every
   `cmd 2>&1` form."
  #{:>& :<&})

(defn- redirect-target-path
  "Literal target path of a file redirect, or nil if heredoc/non-literal."
  [rd]
  (let [t (:target rd)]
    (cond
      (= :heredoc (:kind t)) nil
      (= :token (:kind t))   (literal-of t)
      :else                  nil)))

(defn- redirect-target-token-id [rd]
  ;; Provenance reference — best-effort offset/raw label.
  (or (some-> rd :target :offset)
      (some-> rd :target :raw)
      (some-> rd :target :tag)))

(defn- heredoc-body-effects
  "When a heredoc is lexically bound to a direct receiver (e.g.
   `bash <<EOF\\n...\\nEOF`), shell-shape's heredoc enrichment populates
   `:body-parse` on the heredoc target. The pipeline composition pass
   does NOT mint a :program-source for this case — direct-receiver
   heredocs flow through the heredoc node itself. Walk that here."
  [ctx rd]
  (let [tgt (:target rd)
        bp  (when (= :heredoc (:kind tgt)) (:body-parse tgt))]
    (cond
      (nil? bp) []

      (= :unresolved (:kind bp))
      [{:class :opaque
        :scope (str "heredoc:" (name (or (:reason bp) :unknown)))
        :provenance {:rule    :heredoc-unresolved
                     :dialect (:body-dialect tgt)
                     :reason  (:reason bp)}}]

      (#{:script :pipeline :command} (:kind bp))
      (classify-tree ctx bp)

      :else [])))

(defn- content-effects-for-write
  "P7b — when `cmd` is a passthrough literal producer and the redirect
   `rd` writes to a literal `path`, extract the payload, parse it under
   the dialect derived from `path`'s extension, classify recursively,
   and mark each emitted effect with `:provenance :source
   :fs-write-content` + the target-path. Returns [] when the payload
   can't be extracted or the parse fails — the existing :fs-write
   still lands separately."
  [ctx cmd path]
  (if-let [{:keys [body origin]} (pp/extract-payload cmd)]
    (let [dialect  (pp/dialect-for-target path)
          ss-parse (or (:ss-parse ctx) ss/parse)]
      (try
        (let [parsed     (ss-parse body {:dialect dialect})
              body-effs  (classify-tree ctx parsed)]
          (mapv (fn [e]
                  (update e :provenance
                          (fn [p]
                            (assoc (or p {})
                                   :source           :fs-write-content
                                   :target-path      path
                                   :content-dialect  dialect
                                   :payload-origin   origin))))
                body-effs))
        (catch Throwable _ [])))
    []))

(defn classify-redirect
  "Effects contributed by a single redirect on `cmd`. Process-sub
   redirect targets (`> >(grep)` / `< <(curl)`) propagate the inner
   body's effects unconditionally + emit the appropriate fs-* on the
   /dev/fd/* synth scope so the outer write/read is visible. (P7b):
   `cmd > literal-path` where `cmd` is a passthrough producer (echo /
   printf / cat <<HEREDOC) also classifies the payload under the
   dialect derived from `literal-path`'s extension, emitting the
   content's effects with `:provenance :source :fs-write-content`."
  [ctx cmd rd]
  (let [op   (:op rd)
        prog (:program cmd)
        path (redirect-target-path rd)
        tgt  (:target rd)
        prov {:rule    :redirect
              :program (if (string? prog) prog "?")
              :op      op
              :target  (redirect-target-token-id rd)}]
    (cond
      ;; fd-duplicate (`2>&1`, `<&0`, `>&-`, etc.). The kernel relabels
      ;; the fd; the workload's effects on the world are unchanged.
      (contains? fd-dup-ops op)
      []

      ;; Heredoc — descend into body-parse (direct-receiver case).
      ;; Pipeline composition cases land as :program-sources instead.
      (= op :<<)
      (heredoc-body-effects ctx rd)

      ;; Here-string — body effect lands via :program-sources on the
      ;; interpreter stage; the redirect itself contributes nothing.
      (= op :<<<)
      []

      ;; Process-sub redirect target (e.g. `cmd > >(grep)`).
      ;; Inner body always runs (propagate its effects) + outer write
      ;; or read on /dev/fd/*.
      (= :process-sub (:kind tgt))
      (let [body-effs (classify-tree ctx (:body tgt))
            fd-class  (if (contains? fs-write-ops op) :fs-write :fs-read)
            fd-effect {:class      fd-class
                       :scope      "/dev/fd/*"
                       :provenance (assoc prov :rule :process-sub-redirect)}]
        (conj body-effs fd-effect))

      (and (contains? fs-write-ops op) (nil? path))
      [{:class :opaque :scope "redirect-variable-target" :provenance prov}]

      (contains? fs-write-ops op)
      (into [{:class :fs-write :scope path :provenance prov}]
            (content-effects-for-write ctx cmd path))

      (and (contains? fs-read-ops op) (nil? path))
      [{:class :opaque :scope "redirect-variable-target" :provenance prov}]

      (contains? fs-read-ops op)
      [{:class :fs-read :scope path :provenance prov}]

      :else [])))

;; ---- Program-sources ---------------------------------------------------

(defn classify-program-source
  "A :program-source carries either a parsed body (recursive classify)
   or an :unresolved (opaque). The shell-shape composition pass already
   resolved the effective dialect; we only need to walk the parse."
  [ctx ps]
  (let [bp (:body-parse ps)]
    (cond
      (nil? bp) []

      (= :unresolved (:kind bp))
      [{:class :opaque
        :scope (str "program-source:" (name (or (:reason bp) :unknown)))
        :provenance {:rule       :program-source-unresolved
                     :origin     (:origin ps)
                     :receiver   (:effective-receiver ps)
                     :dialect    (:effective-dialect ps)
                     :reason     (:reason bp)}}]

      (#{:script :pipeline :command} (:kind bp))
      (classify-tree ctx bp)

      :else [])))

;; ---- Invokes (wrapped commands, subshell scripts) ----------------------

(def ^:const default-wrap-depth-cap
  "Hard ceiling on wrapper-chain recursion depth. A `sudo sudo … rm`
   chain longer than this collapses to a single :opaque effect at the
   cap boundary instead of recursing further. T13 defense — without
   the cap, an attacker-controlled deeply-nested wrapper chain
   exhausts the JVM stack before the chain reaches a leaf classifier.
   32 leaves comfortable headroom for legitimate nestings (deepest
   real-world case ≈ 5: `nohup env X=1 timeout 5 sudo cmd`)."
  32)

(defn- mark-resolved-from
  "Annotate every effect-record in `effs` with `:provenance :resolved-from
   :same-string`. Used by `try-resolve-unresolved` to mark classifications
   that came through P7a binding substitution."
  [effs]
  (mapv (fn [e]
          (update e :provenance
                  (fn [p] (assoc (or p {}) :resolved-from :same-string))))
        effs))

(defn try-resolve-unresolved
  "P7a: attempt to substitute :var parts from the binding-table and re-
   classify an :unresolved invocation. Returns a non-empty effect-vector
   on success; nil if any :var is unbound, the substitution produces no
   re-parseable string, or the re-parse + reclassify yields no effects
   (caller then falls back to the existing :opaque emission).

   Reasons handled:
   - `:dynamic-eval` / `:variable-args` (joined-args wrappers like eval) —
     substitute :var parts in the parent cmd's args, concat with spaces,
     re-parse under the wrapper's target dialect.
   - `:variable-program` (cmd's program is a non-literal token) —
     substitute :program, then prepend it to the original :raw of args;
     re-parse."
  [ctx inv]
  (let [bindings   (or (:bindings ctx) {})
        cmd        (:current-cmd ctx)
        ss-parse   (:ss-parse ctx)
        classify-t (:classify-tree ctx)
        dialect    (or (:dialect inv) :bash)]
    (when (and (seq bindings) cmd ss-parse classify-t)
      (let [resolved-text
            (case (:reason inv)
              (:dynamic-eval :variable-args)
              (when (every? #(= :token (:kind %)) (:args cmd))
                (let [resolved-args (bind/resolve-args bindings (:args cmd))]
                  (bind/args->joined-literal resolved-args)))

              :variable-program
              (when-let [prog-tok (:program cmd)]
                (when (and (map? prog-tok) (seq (:parts prog-tok)))
                  (when-let [resolved-prog (bind/resolve-program-token
                                            bindings prog-tok)]
                    (let [arg-strs (mapv (fn [a]
                                           (or (:raw a) ""))
                                         (:args cmd))]
                      (str/trim (str resolved-prog
                                     (when (seq arg-strs) " ")
                                     (str/join " " arg-strs)))))))

              nil)]
        (when (and (string? resolved-text)
                   (not (str/blank? resolved-text)))
          (try
            (let [parsed (ss-parse resolved-text {:dialect dialect})
                  effs   (classify-t ctx parsed)]
              (when (seq effs)
                (mark-resolved-from effs)))
            (catch Throwable _ nil)))))))

(defn classify-invocation
  "Walk a wrapper :invokes entry. Script/command nodes recurse; unresolved
   becomes opaque. Depth-capped via `:wrap-depth` (current) vs
   `:wrap-depth-cap` (default 32) on the ctx — past the cap, returns a
   single :opaque effect rather than recursing further."
  [ctx inv]
  (let [depth (long (:wrap-depth ctx 0))
        cap   (long (:wrap-depth-cap ctx default-wrap-depth-cap))]
    (if (>= depth cap)
      [{:class :opaque
        :scope "wrap-depth-cap"
        :provenance {:rule  :wrap-depth-cap
                     :depth depth
                     :cap   cap}}]
      (let [ctx' (update ctx :wrap-depth (fnil inc 0))]
        (case (:kind inv)
          :script    (classify-tree ctx' inv)
          :pipeline  (classify-pipeline ctx' inv)
          :command   (classify-command ctx' inv)
          :unresolved
          ;; P7a: try same-string binding resolution before falling back
          ;; to opaque. When the LLM emitted `DANGER=...; eval $DANGER`,
          ;; the binding is in ctx :bindings; substitute and re-classify.
          ;; A second escape hatch: if `invocation-resolvable?` is true
          ;; but `try-resolve-unresolved` returns nil, upstream resolution
          ;; (e.g. classify-command-resolved having already substituted
          ;; :program for a :variable-program case) handled the classification
          ;; through the program classifier path — the wrapper's :unresolved
          ;; entry is then stale, suppress the opaque.
          (or (try-resolve-unresolved ctx' inv)
              (when (bind/invocation-resolvable? (or (:bindings ctx') {})
                                                 (:current-cmd ctx')
                                                 inv)
                [])
              [{:class :opaque
                :scope (str "invoke:" (name (or (:reason inv) :unknown)))
                :provenance {:rule    :invoke-unresolved
                             :reason  (:reason inv)
                             :dialect (:dialect inv)}}])
          [])))))

;; ---- Argument tokens (subshell-in-arg, backtick-in-arg, dynamic var) ---

(defn- part-effects
  "Effects emitted by a single token part. :subst / :backtick recurse
   into their :script; :var becomes an opaque arg if used to form a
   program-string (handled by :command-level classify); :literal is no
   effect on its own."
  [ctx part]
  (case (:kind part)
    :subst
    (if-let [s (:script part)]
      (classify-tree ctx s)
      [{:class :opaque :scope "subst-no-script"
        :provenance {:rule :subshell-subst}}])

    :backtick
    (if-let [s (:script part)]
      (classify-tree ctx s)
      [{:class :opaque :scope "backtick-no-script"
        :provenance {:rule :subshell-backtick}}])

    ;; :literal / :var contribute no standalone effect.
    []))

(defn classify-arg-token
  "Walk a single arg-element. Arg-elements may be :token (parts
   walked for subshell substitutions) or :process-sub (body
   recursively classified — outer :fs-read/:fs-write on /dev/fd/* is
   emitted by the program classifier via arg-literals)."
  [ctx arg]
  (case (:kind arg)
    :token
    (reduce (fn [acc p] (into acc (part-effects ctx p)))
            []
            (:parts arg))

    :process-sub
    (classify-tree ctx (:body arg))

    []))

;; ---- Command level -----------------------------------------------------

(defn classify-command
  "Effect-set for a single :command node.

   Composed of:
     1. Program's own effects (from program-classifier registry).
     2. Per-redirect effects (fs-read / fs-write / heredoc-as-program-source).
     3. Per-arg-token effects (subshell substitutions).
     4. Per-program-source effects (heredoc bodies, here-strings,
        arg-strings the composition pass identified).
     5. Per-invoke effects (wrappers like sudo, xargs).

   P7a: at entry, check for assignment-shape commands. A standalone
   assignment (`FOO=bar` with no args/redirects) emits no effect — the
   binding is harvested by classify-pipeline. A prefix assignment
   (`FOO=bar cmd ...`) is replaced with the stripped command and
   augmented bindings, then recursively classified."
  [ctx cmd]
  (let [binding-info (bind/command-binding cmd)]
    (cond
      ;; Standalone assignment reached here directly (not via pipeline).
      ;; classify-pipeline normally extracts these before calling classify-
      ;; command; this case applies when classify-tree is entered at a
      ;; bare :command node. Emit nothing — we recognize the binding shape.
      (and binding-info (= :standalone (:kind binding-info)))
      []

      ;; Prefix-style assignment: augment bindings with the prefix
      ;; assignments and classify the stripped command.
      (and binding-info (= :prefix (:kind binding-info)))
      (let [prefix-bnds (into {} (keep (fn [{:keys [name value]}]
                                         (when (some? value)
                                           [name value]))
                                       (:bindings binding-info)))
            new-bnds (merge (or (:bindings ctx) {}) prefix-bnds)
            new-ctx (assoc ctx :bindings new-bnds)]
        (classify-command new-ctx (:stripped-cmd binding-info)))

      :else
      (classify-command-resolved ctx cmd))))

(defn- augment-bindings-from-env
  "P7c: when env-resolve is enabled in ctx, augment same-string
   bindings with env values for vars referenced by `cmd` that are
   not already bound. Returns `{:bindings :env-used}` where
   `:env-used` is a map `{name {:hash <12-char-b64url>}}` of vars
   that came from env. Bindings unchanged when env-resolve disabled
   or no resolvable env vars apply."
  [in-bindings env-cfg cmd]
  (if (and env-cfg (:enabled? env-cfg))
    (let [cmd-vars (bind/collect-var-names cmd)
          unbound  (remove #(contains? in-bindings %) cmd-vars)]
      (reduce (fn [acc name]
                (if-let [{:keys [value hash]} (env/lookup env-cfg name)]
                  (-> acc
                      (assoc-in [:bindings name] value)
                      (assoc-in [:env-used name] {:hash hash}))
                  acc))
              {:bindings in-bindings :env-used {}}
              unbound))
    {:bindings in-bindings :env-used {}}))

(defn- mark-env-resolved
  "Attach `:env-resolved? true` and `:resolved-vars` to every effect's
   `:provenance`. No-op when `env-used` is empty."
  [effs env-used]
  (if (seq env-used)
    (mapv (fn [e]
            (update e :provenance
                    (fn [p]
                      (-> (or p {})
                          (assoc :env-resolved? true)
                          (update :resolved-vars (fn [r] (merge r env-used)))))))
          effs)
    effs))

(defn- classify-command-resolved
  "Inner of classify-command: applies binding-table substitution to
   :program and :args, then runs the standard classifier pipeline.
   P7c: when env-resolve is enabled, augments bindings with env-allowed
   values for unbound :var names; effects emitted under that
   augmentation are marked `:env-resolved? true` + `:resolved-vars`."
  [ctx cmd]
  (let [in-bindings    (or (:bindings ctx) {})
        env-cfg        (:env-resolve ctx)
        {effective-bnds :bindings
         env-used       :env-used} (augment-bindings-from-env in-bindings
                                                              env-cfg cmd)
        cmd            (if (seq effective-bnds)
                         (bind/resolve-command effective-bnds cmd)
                         cmd)
        ;; Make the original cmd available to classify-invocation so
        ;; it can resolve :unresolved invokes from the cmd's :args.
        ctx            (assoc ctx
                              :current-cmd cmd
                              :bindings    effective-bnds)
        registry       (:registry ctx)
        ;; Translate the binding-resolved shell-shape :command into a
        ;; parser-neutral normalized-call before invoking the per-program
        ;; classifier registry. The walker stays shell-shape-coupled (it
        ;; knows :script/:pipeline/:command/:group/:program-sources,
        ;; etc.); only the per-program dispatch is parser-neutral, so
        ;; non-shell-shape parsers (muschel) can reuse the registry by
        ;; constructing their own normalized-calls and calling
        ;; `eff/classify-call` directly.
        norm-call      (call/from-shell-shape-command cmd)
        own            (eff/classify-call registry norm-call ctx)
        redirs         (reduce (fn [acc rd] (into acc (classify-redirect ctx cmd rd)))
                               []
                               (:redirects cmd))
        args           (reduce (fn [acc tok] (into acc (classify-arg-token ctx tok)))
                               []
                               (:args cmd))
        psrcs          (reduce (fn [acc ps] (into acc (classify-program-source ctx ps)))
                               []
                               (:program-sources cmd))
        ;; Under xargs, the wrapped command's positional paths come from
        ;; stdin — the static argv is empty, but the invocation IS
        ;; effectful. Set :stdin-fed-positionals? on the descent ctx so
        ;; the wrapped fs-* classifiers emit scope "?" instead of opaque
        ;; :no-target.
        inv-ctx        (cond-> ctx
                         (#{"xargs"} (:program cmd))
                         (assoc :stdin-fed-positionals? true))
        invokes        (reduce (fn [acc inv] (into acc (classify-invocation inv-ctx inv)))
                               []
                               (:invokes cmd))
        all-effs       (-> []
                           (into own)
                           (into redirs)
                           (into args)
                           (into psrcs)
                           (into invokes))]
    (mark-env-resolved all-effs env-used)))

;; ---- Group (subshell + brace-group as pipeline stages) ----------------

(defn classify-group*
  "Like classify-group but returns `[effects new-bindings]`. P7a:
   `:brace` groups run in the current shell — assignments inside
   propagate to subsequent siblings. `:subshell` groups discard
   bindings at the group boundary."
  [ctx grp]
  (let [depth (long (:wrap-depth ctx 0))
        cap   (long (:wrap-depth-cap ctx default-wrap-depth-cap))
        in-bnds (or (:bindings ctx) {})]
    (if (>= depth cap)
      [[{:class :opaque
         :scope "wrap-depth-cap"
         :provenance {:rule :wrap-depth-cap
                      :depth depth
                      :cap   cap
                      :variant (:variant grp)}}]
       in-bnds]
      (let [ctx'           (update ctx :wrap-depth (fnil inc 0))
            [body-effs body-bnds] (classify-tree* ctx' (:body grp))
            synth          {:program (str "group:" (name (:variant grp)))}
            redir-effs     (reduce (fn [acc rd]
                                     (into acc (classify-redirect ctx' synth rd)))
                                   []
                                   (:redirects grp))
            out-bnds       (if (= :brace (:variant grp)) body-bnds in-bnds)]
        [(into body-effs redir-effs) out-bnds]))))

(defn classify-group
  "Effect-set for a :group pipeline stage. Body is recursively
   classified; group-bound :redirects contribute via classify-redirect
   against a synthetic command (no own effects from the group itself).
   Group recursion is depth-capped via the same `:wrap-depth` mechanism
   as :invokes, so nested `(((…)))` won't blow the stack.

   Thin wrapper over `classify-group*` that drops the propagated
   bindings to preserve the public API."
  [ctx grp]
  (first (classify-group* ctx grp)))

;; ---- Pipeline + chain-tail ---------------------------------------------

(defn- classify-stage*
  "Like classify-stage but returns `[effects new-bindings]`. :command
   stages don't propagate bindings (standalone assignments are
   extracted by classify-pipeline* before reaching classify-stage*).
   :group stages may propagate when the variant is :brace."
  [ctx stage]
  (let [in-bnds (or (:bindings ctx) {})]
    (case (:kind stage)
      :command [(classify-command ctx stage) in-bnds]
      :group   (classify-group* ctx stage)
      [[] in-bnds])))

(defn- stage-standalone-binding
  "If `stage` is a :command shaped as a pure standalone assignment
   (`FOO=bar` with no args or redirects), return `{:name :value}` so
   classify-pipeline can advance the binding-table without classifying."
  [stage]
  (when (= :command (:kind stage))
    (when-let [b (bind/command-binding stage)]
      (when (= :standalone (:kind b))
        (select-keys b [:name :value])))))

(defn- pipeline-binding-propagates?
  "Does this pipeline run in the current shell (so assignments visible
   to subsequent commands), or as a subshell? Single-stage pipelines
   run in the current shell; multi-stage pipelines (with pipes) put
   each stage in its own subshell — assignments inside don't escape."
  [pl]
  (= 1 (count (:stages pl))))

(defn classify-pipeline*
  "Like classify-pipeline but returns `[effects new-bindings]`. P7a:
   threads same-string binding-tables forward through chain-tail
   (sequence) and across :script :nodes. Pipe stages within a multi-
   stage pipeline are scope-isolated — bindings inside don't escape."
  [ctx pl]
  (let [stages (:stages pl)
        propagate? (pipeline-binding-propagates? pl)
        in-bindings (or (:bindings ctx) {})
        [stage-effs after-stages-bindings]
        (reduce (fn [[acc bnds] stage]
                  (let [sb (stage-standalone-binding stage)]
                    (cond
                      ;; Single-stage standalone assignment: record + no effect.
                      (and propagate? sb)
                      [acc
                       (if (some? (:value sb))
                         (assoc bnds (:name sb) (:value sb))
                         ;; Non-literal RHS: drop any prior binding by this
                         ;; name (the var is now opaque-valued).
                         (dissoc bnds (:name sb)))]

                      ;; Multi-stage assignment (in a subshell stage):
                      ;; emit no effect (we recognize the shape), but
                      ;; don't propagate.
                      (and (not propagate?) sb)
                      [acc bnds]

                      :else
                      (let [[effs new-bnds]
                            (classify-stage* (assoc ctx :bindings bnds) stage)]
                        [(into acc effs)
                         (if propagate? new-bnds bnds)]))))
                [[] in-bindings]
                stages)
        ;; Chain-tail uses the post-stage bindings if propagating, else
        ;; the bindings inherited from outside the pipeline.
        tail-in-bindings (if propagate? after-stages-bindings in-bindings)
        [tail-effs tail-out-bindings]
        (reduce (fn [[acc bnds] t]
                  (let [[next-effs next-bnds]
                        (classify-pipeline* (assoc ctx :bindings bnds)
                                            (:next t))]
                    [(into acc next-effs) next-bnds]))
                [[] tail-in-bindings]
                (:chain-tail pl))]
    [(into stage-effs tail-effs) tail-out-bindings]))

(defn classify-pipeline
  "Effects from every stage of a pipeline, plus the recursive walk of
   :chain-tail (sequence of `;` / `&&` / `||` / `&` continuations).
   Conservative union — short-circuit (`&&` / `||`) is not modeled.

   Thin wrapper over `classify-pipeline*` that drops the propagated
   bindings to preserve the public API."
  [ctx pl]
  (first (classify-pipeline* ctx pl)))

;; ---- Script root -------------------------------------------------------

(defn classify-tree*
  "Like classify-tree but returns `[effects new-bindings]`. Used by
   `classify-group*` to propagate same-string bindings out of a brace
   group's body. Public consumers should use `classify-tree`."
  ([tree] (classify-tree* {} tree))
  ([ctx tree]
   (let [ctx (cond-> ctx
               (nil? (:registry ctx))      (assoc :registry (eff/active-registry))
               (nil? (:classify-tree ctx)) (assoc :classify-tree classify-tree)
               (nil? (:ss-parse ctx))      (assoc :ss-parse ss/parse))
         in-bnds (or (:bindings ctx) {})]
     (case (:kind tree)
       :script
       (case (:dialect tree)
         :python [(py-cls/classify-python-script ctx tree)   in-bnds]
         :node   [(node-cls/classify-node-script ctx tree)   in-bnds]
         :ruby   [(ruby-cls/classify-ruby-script ctx tree)   in-bnds]
         :perl   [(perl-cls/classify-perl-script ctx tree)   in-bnds]
         ;; bash/posix/zsh — thread same-string bindings across :nodes.
         (reduce (fn [[acc bnds] pl]
                   (let [[pl-effs pl-bnds]
                         (classify-pipeline* (assoc ctx :bindings bnds) pl)]
                     [(into acc pl-effs) pl-bnds]))
                 [[] in-bnds]
                 (:nodes tree)))
       :pipeline (classify-pipeline* ctx tree)
       :command  [(classify-command ctx tree) in-bnds]
       :group    (classify-group* ctx tree)
       [[] in-bnds]))))

(defn classify-tree
  "Top-level walk. Accepts a :script, :pipeline, :command, or :group
   node. Returns a flat vector of effect-instances. Order preserves
   walk order; deduplication is the consumer's responsibility
   (normalize collapses by [:class :scope] when building biscuit facts).

   When `ctx` omits `:registry`, threads `(eff/active-registry)`,
   which is `eff/default-registry` in production and the mutated
   registry when the mutation harness has rebound
   `eff/*registry-override*`.

   Also threads `:classify-tree` and `:ss-parse` callables so the
   ssh / trap classifiers can recursively classify literal command
   bodies without creating a circular require with `effects.clj`.

   Thin wrapper over `classify-tree*` that drops the propagated
   bindings to preserve the public API."
  ([tree]      (first (classify-tree* {} tree)))
  ([ctx tree]  (first (classify-tree* ctx tree))))

;; ---- Public helpers ----------------------------------------------------

(defn effect-set
  "Deduplicated set of [class scope] pairs from a classification.
   The provenance is dropped for set-membership semantics; the original
   vector is preserved by `classify-tree` for fact emission."
  [effects]
  (into #{} (map (juxt :class :scope) effects)))

(defn has-opaque?
  "True if any effect in `effects` is :opaque — the fail-closed gate."
  [effects]
  (boolean (some #(= :opaque (:class %)) effects)))
