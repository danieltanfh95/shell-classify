(ns shell-classify.getopt-specs
  "v0.10.0 (P2) — per-program getopt specs that drive
  `argv-shape/normalize-argv`. Specs are pure data; the normalizer is
  the only consumer.

  ---

  ## Data model

  A getopt spec is a map of options describing how raw argv tokens
  fold into the `{:flags {...} :positionals [...]}` shape consumed by
  `argv-shape/match?`:

  ```
  {:opts-with-arg #{<flag-name-str> ...}   ; flags whose VALUE is the next token
   :long-eq?      <bool>                    ; --flag=value supported
   :bundled?      <bool>                    ; -abc → -a -b -c (short flags only)
   :stop-token    <str-or-nil>              ; e.g. \"--\" forces remainder positional
   :subcommand-at <int-or-nil>              ; positional index treated as subcommand
   :flag-prefix?  <fn-token-→-bool>}        ; predicate; default: starts-with `-`
  ```

  ## Built-in specs

  | Name                    | Notes |
  |---|---|
  | `:gnu`                  | Default GNU getopt — flags `-x` / `--x[=y]`, `--` stop, bundling on. |
  | `:gnu-with-subcommand`  | Same; positional[0] is treated as subcommand (e.g. git, docker, kubectl). |
  | `:tar-bundled`          | First arg may be a flag bundle without `-` (e.g. `tar cvf out src`). |
  | `:find-predicate`       | find's pattern: paths first, then `-` tokens are predicates (not GNU flags). |
  | `:dd-key-value`         | dd's `key=value` args; no traditional flags. |
  | `:bsd-getopt`           | BSD-style (sed, date) — no `--flag=value`, no bundling. |

  Per-program overrides (`:opts-with-arg`) are attached at the
  program-registry call site; this file ships only the abstract
  shapes. The witness's effects.clj registry composes
  `(spec-of <kw>)` with per-program `:opts-with-arg` to produce the
  final spec passed to `normalize-argv`."
  (:require [clojure.string :as str]))

;; ---- Built-in specs ----------------------------------------------------

(def gnu
  {:opts-with-arg #{}
   :long-eq?      true
   :bundled?      true
   :stop-token    "--"
   :subcommand-at nil})

(def gnu-with-subcommand
  (assoc gnu :subcommand-at 0))

(def tar-bundled
  ;; tar cvf out.tar src → 'cvf' decomposed as -c -v -f; -f consumes next token
  (assoc gnu-with-subcommand
         :tar-bundle? true
         :opts-with-arg #{"-f" "-T" "-C" "-X" "-K" "-N"}))

(def find-predicate
  ;; find's argv: <paths>... <predicates>... where predicates START with -
  ;; but are POSITIONAL semantically (not GNU flags). The shell-shape
  ;; classifier already handles find specifically — this spec is for
  ;; uniformity if argv-shape needs to predicate on -delete etc.
  {:opts-with-arg #{}
   :long-eq?      false
   :bundled?      false
   :stop-token    nil
   :subcommand-at nil
   :find-predicate? true})

(def dd-key-value
  ;; dd if=src of=dst bs=1M → all positionals, no flags
  {:opts-with-arg #{}
   :long-eq?      false
   :bundled?      false
   :stop-token    nil
   :subcommand-at nil
   :dd-key-value? true})

(def bsd-getopt
  (assoc gnu
         :long-eq? false
         :bundled? false))

;; ---- Registry ----------------------------------------------------------

(def by-keyword
  "Lookup table from spec keyword to spec map."
  {:gnu                 gnu
   :gnu-with-subcommand gnu-with-subcommand
   :tar-bundled         tar-bundled
   :find-predicate      find-predicate
   :dd-key-value        dd-key-value
   :bsd-getopt          bsd-getopt})

(defn resolve-spec
  "Spec-or-keyword → spec map. Keyword → looks up in [[by-keyword]];
   map → returned as-is so per-program overrides compose cleanly via
   `(merge (resolve-spec :gnu-with-subcommand) {:opts-with-arg #{...}})`."
  [spec-or-kw]
  (cond
    (map?     spec-or-kw) spec-or-kw
    (keyword? spec-or-kw) (or (by-keyword spec-or-kw)
                              (throw (ex-info "unknown getopt spec"
                                              {:spec spec-or-kw})))
    :else (throw (ex-info "invalid getopt spec — must be keyword or map"
                          {:spec spec-or-kw}))))

;; ---- normalize-argv ----------------------------------------------------

(defn- flag-prefix? [^String tok]
  (and (string? tok)
       (>= (count tok) 2)
       (= \- (.charAt tok 0))))

(defn- long-flag? [^String tok]
  (and (string? tok)
       (>= (count tok) 3)
       (= \- (.charAt tok 0))
       (= \- (.charAt tok 1))))

(defn- short-flag? [^String tok]
  (and (flag-prefix? tok) (not (long-flag? tok))))

(defn- decompose-tar-bundle
  "tar's first arg may be a bundle of flags WITHOUT a leading `-`
   (e.g. `cvf` → `-c -v -f`). Returns a vector of `-x` tokens."
  [^String tok]
  (mapv #(str "-" %) tok))

(defn- split-long-eq
  "`--flag=value` → [`--flag` `value`], else [`tok` nil]."
  [^String tok]
  (let [idx (.indexOf tok "=")]
    (if (pos? idx)
      [(subs tok 0 idx) (subs tok (inc idx))]
      [tok nil])))

(defn- decompose-bundle
  "`-abc` → [`-a` `-b` `-c`]. Each char becomes its own short flag."
  [^String tok]
  (mapv #(str "-" %) (.substring tok 1)))

(defn normalize-argv
  "Input:  spec (keyword or map; see [[by-keyword]])
           argv (vector of strings — raw argv tokens after the program name)
   Output: {:flags {<flag-str> <true-or-value-str>}
            :positionals [<arg-str> ...]
            :raw-argv [<arg-str> ...]}

   Implements GNU-style getopt by default; per-program specs override
   via `:opts-with-arg` (flags that consume the next token),
   `:tar-bundle?` (first arg may be unprefixed flag bundle),
   `:dd-key-value?` (all `k=v` tokens are positionals), and
   `:find-predicate?` (find's special syntax — leave `-` tokens as
   positionals)."
  [spec argv]
  (let [{:keys [opts-with-arg long-eq? bundled?
                stop-token tar-bundle?
                find-predicate? dd-key-value?]
         :or {opts-with-arg #{}}}
        (resolve-spec spec)
        opts-with-arg (set opts-with-arg)
        argv (vec argv)]
    (cond
      ;; find: every token is a positional; flag-prefix? doesn't apply.
      find-predicate?
      {:flags {} :positionals (vec argv) :raw-argv (vec argv)}

      ;; dd: every token is a positional (key=value or path).
      dd-key-value?
      {:flags {} :positionals (vec argv) :raw-argv (vec argv)}

      :else
      (loop [tokens (if (and tar-bundle? (seq argv)
                             (let [t (first argv)]
                               (and (string? t)
                                    (not (str/starts-with? t "-"))
                                    (every? #(or (Character/isLetter ^char %)
                                                 (Character/isDigit  ^char %))
                                            t))))
                      ;; tar's leading bundle: explode `cvf` into `-c -v -f`
                      ;; then the rest of argv
                      (into (decompose-tar-bundle (first argv)) (rest argv))
                      argv)
             flags {}
             positionals []]
        (cond
          (empty? tokens)
          {:flags flags
           :positionals positionals
           :raw-argv (vec argv)}

          (and stop-token (= (first tokens) stop-token))
          ;; everything after `--` is positional
          {:flags flags
           :positionals (into positionals (rest tokens))
           :raw-argv (vec argv)}

          (long-flag? (first tokens))
          (let [tok (first tokens)
                [name eq-val] (if long-eq? (split-long-eq tok) [tok nil])]
            (cond
              eq-val
              (recur (rest tokens) (assoc flags name eq-val) positionals)

              (and (contains? opts-with-arg name) (next tokens))
              (recur (drop 2 tokens)
                     (assoc flags name (second tokens))
                     positionals)

              :else
              (recur (rest tokens) (assoc flags name true) positionals)))

          (short-flag? (first tokens))
          (let [tok (first tokens)]
            (cond
              (contains? opts-with-arg tok)
              (if (next tokens)
                (recur (drop 2 tokens)
                       (assoc flags tok (second tokens))
                       positionals)
                (recur (rest tokens) (assoc flags tok true) positionals))

              (and bundled? (> (count tok) 2))
              ;; -abc → -a -b -c (each gets `true`)
              (recur (into (vec (decompose-bundle tok)) (rest tokens))
                     flags positionals)

              :else
              (recur (rest tokens) (assoc flags tok true) positionals)))

          :else
          (recur (rest tokens) flags (conj positionals (first tokens))))))))
