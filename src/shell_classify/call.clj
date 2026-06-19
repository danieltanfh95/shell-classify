(ns shell-classify.call
  "Parser-neutral normalized-call shape consumed by the classifier
   registry.

   v0.2.0 decomplection — separates per-program effect classification
   from any specific parse-tree shape so non-shell-shape parsers
   (e.g. muschel) can run ssc's classifier taxonomy by translating
   their command shape into a normalized-call. shell-shape stays the
   reference parser; ssc's tree walker (classify.clj) is still coupled
   to shell-shape's :script/:pipeline/:command/:group shapes — what
   moves to a neutral shape is the per-program registry dispatch.

   ## Normalized-call shape

   A normalized-call is what each program classifier receives:

     {:program  <string-or-nil>      ; program name, post-binding-resolved
      :argv     [<arg-element>]      ; in-order args (excludes the program)
      :assigns  [<assign>]           ; FOO=bar prefix assignments
      :redirs   [<redir-element>]    ; not read by universal classifiers
      :program-sources [<ps>]        ; opaque per-adapter; consumed via :raw
      :invokes  [<invoke>]           ; wrapper-mode delegates (sudo, xargs, …)
                                     ; entries with `{:kind :unresolved
                                     ; :reason <kw>}` are reflected as
                                     ; :opaque by wrapper-classifier. Other
                                     ; entries (`:command`, `:pipeline`,
                                     ; `:script`) are walked by classify.clj.
                                     ; Adapters that don't emit dynamic
                                     ; invokes just leave this empty.
      :raw      <opaque>             ; original parse node — escape hatch
                                     ; for shell-shape-coupled spots
                                     ; (bindings.clj resolution, etc.)

      ;; walker-set context fields (some classifiers read):
      :stdin-fed-positionals? <bool>}

   `:program` is a String (or nil) AFTER binding-resolution. Adapters
   are responsible for resolving variable-program tokens before
   constructing the normalized-call.

   ## arg-element (sum type tagged by :kind)

     {:kind :token       :parts [<part>]}            ; ordinary word arg
     {:kind :process-sub :direction :in|:out
                         :body <opaque>}             ; <(cmd) / >(cmd)

   ## part (sum type tagged by :kind)

     {:kind :literal  :value <str>}                  ; bare literal segment
     {:kind :var      :name <str>
                      :default <str-or-nil>}         ; $VAR / ${VAR:-default}
     {:kind :subst    :raw <str> :script <opaque>}   ; $(...) — :script
                                                     ; only when adapter
                                                     ; has a sub-parse
     {:kind :backtick :raw <str> :script <opaque>}   ; `...`

   ## assign

     {:name <str> :value <part-vec-or-nil>}

   ## redir-element

     {:op :>|:>>|:<|:<<|:<<<|:<&|:>&|:&>|...
      :fd <int-or-nil>
      :target <part-vec>
              | {:kind :process-sub ...}
              | {:kind :heredoc :body-parse <opaque>}}

   Universal program classifiers (rm, mv, cp, curl, wget, grep, sed,
   awk, jq, git, tar, etc.) only read :program / :argv / :assigns and
   are fully parser-neutral.

   Coupled classifiers (shell-interpret-classifier, source-classifier,
   ssh-classifier, trap-classifier, find-classifier — those that
   recurse into a sub-parse tree) reach back through `:raw` for shape-
   specific access. The coupled minority is named and small.

   ## Adapter usage

   - `from-shell-shape-command` builds a normalized-call from a
     shell-shape :command node. ssc's classify.clj uses this at the
     dispatch site.
   - External parsers (muschel) write their own translator producing
     the same shape, then call `effects/classify-call` directly with
     the produced normalized-call + ctx + registry.

   ## Why this shape and not shell-shape's `:args` directly?

   Two reasons:

   1. *Name a contract.* The shape doc above is the contract muschel
      reads to write their adapter. shell-shape's `:command` ns docstring
      describes shell-shape's parse-tree semantics, not the classifier
      contract.

   2. *Top-level slot rename keeps the boundary visible.* `:argv` (not
      `:args`) at the top-level signals \"this is a normalized-call,
      not a shell-shape :command node\". Callers and readers see the
      seam in 1 character."
  (:require [clojure.string :as str]))

;; ---- Part-level helpers ------------------------------------------------

(defn literal-part?
  "True when `part` is a fully-literal token-part."
  [part]
  (= :literal (:kind part)))

(defn literal-token?
  "True when `tok` is a :token arg-element with every part literal."
  [tok]
  (and (= :token (:kind tok))
       (every? literal-part? (:parts tok))))

(defn token-literal-value
  "Concatenated literal value of a :token arg-element. nil if any
   part is non-literal (`$VAR`, `$(...)`, `` `...` ``)."
  [tok]
  (when (literal-token? tok)
    (apply str (mapv :value (:parts tok)))))

(def ^:const process-sub-fd-scope
  "Synthetic scope for process-substitution arg-elements. The outer
   command reads from (or writes to) `/dev/fd/N` at runtime; one glob
   scope lets policy authors grant `/dev/fd/**` once. Inner-command
   effects flow through the walker's recursive descent, not this scope."
  "/dev/fd/*")

(defn arg-literal-scope
  "Scope of a single arg-element:
     - :token literal           → its literal string value
     - :process-sub             → /dev/fd/*
     - :token with vars/substs  → nil (caller signals :variable-arg)"
  [arg]
  (case (:kind arg)
    :token       (token-literal-value arg)
    :process-sub process-sub-fd-scope
    nil))

;; ---- Normalized-call accessors ----------------------------------------

(defn program-string
  "Program name as String, or nil. Adapters resolve variable-program
   tokens before construction, so the slot is normally already a String;
   defensive token-shape extraction supports callers that bypass the
   adapter."
  [norm-call]
  (let [p (:program norm-call)]
    (cond
      (nil? p) nil
      (string? p) p
      (= :token (:kind p)) (token-literal-value p)
      :else nil)))

(defn arg-literals
  "Vector of literal scopes per :argv element. nil entries mark non-
   literal args; callers decide whether to emit :opaque on nil."
  [norm-call]
  (mapv arg-literal-scope (:argv norm-call)))

(defn option?
  "Crude predicate: arg looks like a CLI option (starts with `-`).
   Special cases: `-` alone is stdin (positional, not an option);
   `--` is the POSIX option terminator (handled by the caller, not here)."
  [s]
  (and (string? s)
       (str/starts-with? s "-")
       (not= s "-")
       (not= s "--")))

(defn non-option-positional-literals
  "Literal-only positional args after stripping options. Returns
     {:paths           [<path-str> ...]   ; literal positional file args
      :stdin-consumed? <bool>}            ; true iff a literal `-` was
                                          ; present as a positional
   or nil when any arg is non-literal (caller emits :opaque).

   `opts` (optional):
     :value-flags       set of flags that consume their NEXT arg as a
                        non-path VALUE (e.g. head `-n N`, sort `-k F`).
                        The value is dropped from positionals. Honored
                        only PRE-`--`.
     :path-value-flags  set of flags whose VALUE is a PATH. The value
                        is emitted into the :paths vector (so the
                        caller's classifier picks it up as fs-read).
                        Honored only PRE-`--`.

   Two POSIX conventions honored:
     - `--` terminates option parsing — every arg after `--` is
       positional even if it starts with `-`.
     - bare `-` as a positional signals stdin consumption. Callers
       split it into a separate :stdin-consume effect."
  ([norm-call] (non-option-positional-literals norm-call {}))
  ([norm-call {:keys [value-flags path-value-flags]
               :or {value-flags #{} path-value-flags #{}}}]
   (let [lits (arg-literals norm-call)]
     (when (every? some? lits)
       (let [[pre post]      (split-with #(not= "--" %) lits)
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
             post-positional (rest post)
             positional      (vec (concat pre-positional post-positional))
             stdin-consumed? (boolean (some #{"-"} positional))
             paths           (vec (concat path-vals
                                          (filterv (complement #{"-"}) positional)))]
         {:paths           paths
          :stdin-consumed? stdin-consumed?})))))

;; ---- Network helpers ---------------------------------------------------

(defn host-from-url
  "Extract host from a URL-shaped arg. Returns nil if not a URL."
  [s]
  (when (string? s)
    (when-let [m (re-find #"^[a-zA-Z]+://([^/]+)" s)]
      (nth m 1))))

(defn net-targets-from-args
  "For network commands, find host targets in args. Conservative:
   parses URL arg first; falls back to first non-option positional."
  [norm-call]
  (let [lits (arg-literals norm-call)]
    (when (every? some? lits)
      (let [from-urls (keep host-from-url lits)
            others    (->> lits (drop-while option?) (remove option?))]
        (if (seq from-urls)
          (vec from-urls)
          (vec (take 1 others)))))))

;; ---- Adapters ----------------------------------------------------------

(defn from-shell-shape-command
  "Translate a shell-shape `:command` node into a normalized-call.

   shell-shape's :command shape:
     {:kind :command
      :program <str-or-token>        ; binding-resolved upstream
      :args    [<token>]
      :assigns [{:name :value}]      ; (post-shell-shape v0.7)
      :redirects       [<rd>]
      :program-sources [<ps>]
      :invokes         [<inv>]}      ; not on normalized-call —
                                     ; walker handles invokes separately

   The arg-element shape and part shape are identical between shell-
   shape and normalized-call, so this is mostly a top-level slot rename
   (`:args` → `:argv`, `:redirects` → `:redirs`)."
  [cmd]
  {:program         (:program cmd)
   :argv            (vec (:args cmd))
   :assigns         (vec (:assigns cmd))
   :redirs          (vec (:redirects cmd))
   :program-sources (vec (:program-sources cmd))
   :invokes         (vec (:invokes cmd))
   :raw             cmd})
