(ns shell-shape-classify.argv-shape
  "v0.10.0 (P2) — predicate-set DSL over normalized argv.

  Position-invariance is the load-bearing property: a policy author
  who wants to deny `git push --force` should not care whether the
  LLM emits `git push --force origin main`, `git push origin --force
  main`, or `git push origin main --force`. shell-shape's wrappers
  registry handles this for *wrapper* recognition; the argv-shape DSL
  handles it for *per-program effect discrimination*.

  ---

  ## Data model

  ### Normalized argv

  Per-program getopt normalization (`getopt-specs.clj`) produces this
  shape from a raw arg vector. Flags are SET-LIKE (position among
  flags doesn't matter); positionals stay ORDERED.

  ```
  {:flags        {<flag-name-str> <true-or-value-str> ...}
   :positionals  [<arg-str> ...]
   :raw-argv     [<arg-str> ...]}
  ```

  - `--force` → `{:flags {\"--force\" true}}`
  - `-c receive.denyDeletes=true` (when `:opts-with-arg` lists `-c`)
    → `{:flags {\"-c\" \"receive.denyDeletes=true\"}}`
  - `--git-dir=/tmp/x` → `{:flags {\"--git-dir\" \"/tmp/x\"}}`
  - bundled `-abc` (when `:bundled?`) → `{:flags {\"-a\" true \"-b\" true \"-c\" true}}`
  - `--` separator: tokens after `--` are positionals only.

  ### Predicate (data form)

  Every predicate is a small map keyed on `:type`. Same shape as
  policy.datalog primitives — serializable, inspectable, composable.

  ```
  {:type :has-flag             :name <str>}
  {:type :no-flag              :name <str>}
  {:type :flag-equals          :name <str>  :value <str>}
  {:type :flag-starts-with     :name <str>  :prefix <str>}
  {:type :positional           :n <int>     :value <str>}
  {:type :positional-starts-with :n <int>   :prefix <str>}
  {:type :positional-matches   :n <int>     :regex <str>}
  {:type :positional-count     :op :=|:>=|:<=|:> :n <int>}
  {:type :any-of               :preds [<predicate> ...]}
  {:type :none-of              :preds [<predicate> ...]}
  {:type :all-of               :preds [<predicate> ...]}
  ```

  ### Shape

  A *shape* is a conjunction of three predicate sets:

  ```
  {:requires #{<predicate> ...}   ; every one must match
   :denies   #{<predicate> ...}   ; none must match
   :any-of   #{<predicate> ...}}  ; at least one must match
  ```

  Match semantics:

  ```
  (match? shape normalized-argv)
    ⇔ (every? :requires) ∧ (not-any? :denies) ∧ (some :any-of)
  ```

  Empty `:requires` → trivially true; empty `:denies` → trivially
  satisfies the not-any clause; empty/absent `:any-of` → trivially
  satisfies the some clause.

  ## Why predicate-set, not position-sequence

  Section 2 of the working note at
  [.plans/read-internal-seed-md-as-a-witty-allen.md](../../.plans/read-internal-seed-md-as-a-witty-allen.md)
  walks the first-principles argument: muschel's
  `[\"git\" \"push\" \"--force\" :**]` shape is position-sensitive,
  and a policy author trying to deny --force has to enumerate every
  permutation. The predicate-set form composes (intersection of
  `:requires`, union across grants for `:any-of`) and is
  position-invariant by construction.")

;; ---- Predicate constructors --------------------------------------------

(defn has-flag
  "Predicate: the flag-name is present in :flags (value irrelevant)."
  [name]
  {:type :has-flag :name name})

(defn no-flag
  "Predicate: the flag-name is absent from :flags."
  [name]
  {:type :no-flag :name name})

(defn flag-equals
  "Predicate: the flag-name is present AND its value equals `value`."
  [name value]
  {:type :flag-equals :name name :value value})

(defn flag-starts-with
  "Predicate: the flag-name is present AND its value starts with `prefix`."
  [name prefix]
  {:type :flag-starts-with :name name :prefix prefix})

(defn positional
  "Predicate: positionals[n] equals `value` (literal string)."
  [n value]
  {:type :positional :n n :value value})

(defn positional-starts-with
  "Predicate: positionals[n] starts with `prefix`."
  [n prefix]
  {:type :positional-starts-with :n n :prefix prefix})

(defn positional-matches
  "Predicate: positionals[n] matches `regex` (a string). Compiled at
   match time; treat as Java-regex compatible."
  [n regex]
  {:type :positional-matches :n n :regex regex})

(defn positional-count
  "Predicate: `(op (count positionals) n)`. `op` is one of `:=` `:>=`
   `:<=` `:>` `:<` `:not=`."
  [op n]
  {:type :positional-count :op op :n n})

(defn any-of
  "Predicate: at least one of `preds` matches."
  [preds]
  {:type :any-of :preds (vec preds)})

(defn none-of
  "Predicate: none of `preds` matches."
  [preds]
  {:type :none-of :preds (vec preds)})

(defn all-of
  "Predicate: every one of `preds` matches."
  [preds]
  {:type :all-of :preds (vec preds)})

;; ---- match? — predicate evaluator over normalized argv -----------------

(declare match-pred?)

(defn- nth-positional
  "Safe nth — returns nil for out-of-range indices instead of throwing."
  [argv n]
  (when (and (some? n) (>= n 0) (< n (count argv)))
    (nth argv n)))

(defn- count-op
  [op a b]
  (case op
    :=     (= a b)
    :not=  (not= a b)
    :>=    (>= a b)
    :<=    (<= a b)
    :>     (> a b)
    :<     (< a b)
    false))

(defn- match-has-flag? [{:keys [name]} {:keys [flags]}]
  (contains? flags name))

(defn- match-no-flag? [{:keys [name]} {:keys [flags]}]
  (not (contains? flags name)))

(defn- match-flag-equals? [{:keys [name value]} {:keys [flags]}]
  (let [v (get flags name)]
    (and (some? v) (not (true? v)) (= value v))))

(defn- match-flag-starts-with? [{:keys [name prefix]} {:keys [flags]}]
  (let [v (get flags name)]
    (and (string? v) (.startsWith ^String v ^String prefix))))

(defn- match-positional? [{:keys [n value]} {:keys [positionals]}]
  (= value (nth-positional positionals n)))

(defn- match-positional-starts-with? [{:keys [n prefix]} {:keys [positionals]}]
  (let [p (nth-positional positionals n)]
    (and (string? p) (.startsWith ^String p ^String prefix))))

(defn- match-positional-matches? [{:keys [n regex]} {:keys [positionals]}]
  (let [p (nth-positional positionals n)]
    (and (string? p)
         (try (boolean (re-matches (re-pattern regex) p))
              (catch Throwable _ false)))))

(defn- match-positional-count? [{:keys [op n]} {:keys [positionals]}]
  (count-op op (count positionals) n))

(defn- match-any-of? [{:keys [preds]} argv]
  (boolean (some #(match-pred? % argv) preds)))

(defn- match-none-of? [{:keys [preds]} argv]
  (not-any? #(match-pred? % argv) preds))

(defn- match-all-of? [{:keys [preds]} argv]
  (every? #(match-pred? % argv) preds))

(defn match-pred?
  "Dispatch a single predicate against the normalized argv. Returns
   `false` on unknown `:type` — fail-closed."
  [pred argv]
  (case (:type pred)
    :has-flag               (match-has-flag?              pred argv)
    :no-flag                (match-no-flag?               pred argv)
    :flag-equals            (match-flag-equals?           pred argv)
    :flag-starts-with       (match-flag-starts-with?      pred argv)
    :positional             (match-positional?            pred argv)
    :positional-starts-with (match-positional-starts-with? pred argv)
    :positional-matches     (match-positional-matches?    pred argv)
    :positional-count       (match-positional-count?      pred argv)
    :any-of                 (match-any-of?                pred argv)
    :none-of                (match-none-of?               pred argv)
    :all-of                 (match-all-of?                pred argv)
    false))

(defn match?
  "Match `shape` against a normalized argv. Empty/absent predicate
   sets are trivially satisfied; the shape only fails when an actual
   predicate exists and doesn't fit."
  [{:keys [requires denies any-of]} argv]
  (let [requires (or requires #{})
        denies   (or denies   #{})
        any-of   (or any-of   #{})]
    (and (every?    #(match-pred? % argv) requires)
         (not-any?  #(match-pred? % argv) denies)
         (or (empty? any-of)
             (some  #(match-pred? % argv) any-of)))))
