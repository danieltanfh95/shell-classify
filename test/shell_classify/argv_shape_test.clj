(ns shell-classify.argv-shape-test
  "Predicate-set DSL: position-invariance, predicate composition, and
   the fail-closed posture on unknown predicate types."
  (:require [clojure.test :refer [deftest is testing]]
            [shell-classify.argv-shape :as as]))

;; ---- Position-invariance (Section 2 of the working note) ---------------

(deftest has-flag-position-invariant
  (testing "a `:has-flag` predicate fires regardless of where the flag
            appears among the flags — set semantics."
    (let [p (as/has-flag "--force")
          argvs [{:flags {"--force" true} :positionals ["push" "origin" "main"]}
                 {:flags {"--force" true "--mirror" true}
                  :positionals ["push" "origin" "main"]}
                 {:flags {"-c" "x=1" "--force" true}
                  :positionals ["push" "origin" "main"]}]]
      (doseq [a argvs] (is (as/match-pred? p a))))))

(deftest no-flag-is-pure-negation
  (let [p (as/no-flag "--force")]
    (is (as/match-pred? p {:flags {} :positionals []}))
    (is (as/match-pred? p {:flags {"--mirror" true} :positionals []}))
    (is (not (as/match-pred? p {:flags {"--force" true} :positionals []})))))

(deftest flag-equals-discriminates-value
  (let [p (as/flag-equals "-c" "x=1")
        a {:flags {"-c" "x=1"} :positionals []}]
    (is (as/match-pred? p a))
    (is (not (as/match-pred? (as/flag-equals "-c" "x=2") a)))
    ;; Bare flag (no value) doesn't match flag-equals
    (is (not (as/match-pred?
              p {:flags {"-c" true} :positionals []})))))

(deftest positional-is-position-sensitive
  (let [p (as/positional 0 "push")
        a {:flags {} :positionals ["push" "origin" "main"]}]
    (is (as/match-pred? p a))
    (is (not (as/match-pred? p {:flags {} :positionals ["pull" "origin"]})))
    (is (not (as/match-pred? p {:flags {} :positionals []})))))

(deftest positional-starts-with-prefix
  (let [p (as/positional-starts-with 1 "origin/")]
    (is (as/match-pred? p {:flags {} :positionals ["push" "origin/main"]}))
    (is (not (as/match-pred? p {:flags {} :positionals ["push" "upstream/main"]})))))

(deftest positional-matches-regex
  (let [p (as/positional-matches 0 "^[a-z]+$")]
    (is (as/match-pred? p {:flags {} :positionals ["push"]}))
    (is (not (as/match-pred? p {:flags {} :positionals ["PUSH"]})))
    (is (not (as/match-pred? p {:flags {} :positionals ["123"]})))))

(deftest positional-count-comparator
  (is (as/match-pred? (as/positional-count := 2)
                     {:flags {} :positionals ["a" "b"]}))
  (is (as/match-pred? (as/positional-count :>= 2)
                     {:flags {} :positionals ["a" "b" "c"]}))
  (is (not (as/match-pred? (as/positional-count :>= 5)
                          {:flags {} :positionals ["a"]}))))

;; ---- Predicate composition ---------------------------------------------

(deftest any-of-disjunction
  (let [p (as/any-of [(as/has-flag "--force")
                      (as/has-flag "--mirror")])]
    (is (as/match-pred? p {:flags {"--force" true} :positionals []}))
    (is (as/match-pred? p {:flags {"--mirror" true} :positionals []}))
    (is (not (as/match-pred? p {:flags {"-q" true} :positionals []})))))

(deftest none-of-negation
  (let [p (as/none-of [(as/has-flag "--force")
                       (as/has-flag "--mirror")])]
    (is (as/match-pred? p {:flags {} :positionals []}))
    (is (not (as/match-pred? p {:flags {"--force" true} :positionals []})))))

(deftest all-of-conjunction
  (let [p (as/all-of [(as/positional 0 "push")
                      (as/no-flag "--force")])]
    (is (as/match-pred? p {:flags {} :positionals ["push"]}))
    (is (not (as/match-pred? p {:flags {"--force" true} :positionals ["push"]})))
    (is (not (as/match-pred? p {:flags {} :positionals ["pull"]})))))

;; ---- match? — shape evaluation ----------------------------------------

(deftest shape-requires-conjunction
  (let [shape {:requires #{(as/positional 0 "push")
                           (as/no-flag "--force")}}]
    (is (as/match? shape {:flags {} :positionals ["push" "origin"]}))
    (is (not (as/match? shape {:flags {"--force" true}
                              :positionals ["push" "origin"]})))
    (is (not (as/match? shape {:flags {} :positionals ["pull"]})))))

(deftest shape-denies-fires-on-any-hit
  (let [shape {:denies #{(as/has-flag "--force")
                         (as/has-flag "--mirror")}}]
    (is (as/match? shape {:flags {} :positionals []}))
    (is (not (as/match? shape {:flags {"--force" true} :positionals []})))
    (is (not (as/match? shape {:flags {"--mirror" true} :positionals []})))))

(deftest shape-any-of-requires-at-least-one
  (let [shape {:any-of #{(as/has-flag "-q")
                         (as/has-flag "--quiet")}}]
    (is (as/match? shape {:flags {"-q" true} :positionals []}))
    (is (not (as/match? shape {:flags {} :positionals []})))))

(deftest empty-shape-is-vacuously-true
  (is (as/match? {} {:flags {} :positionals []}))
  (is (as/match? {} {:flags {"--x" true} :positionals ["a" "b"]})))

;; ---- Position-invariance under reorder (the load-bearing test) --------

(deftest force-deny-fires-across-flag-reorderings
  (let [shape {:requires #{(as/positional 0 "push")}
               :denies   #{(as/has-flag "--force")}}
        permutations
        [{:flags {"--force" true} :positionals ["push" "origin" "main"]}
         {:flags {"--force" true} :positionals ["push" "main" "origin"]}
         ;; With a `--git-dir` global before subcommand, normalize-argv puts
         ;; --git-dir in :flags; subcommand stays at positional[0].
         {:flags {"--git-dir" "/tmp/x" "--force" true}
          :positionals ["push" "origin" "main"]}]]
    (doseq [a permutations]
      (is (not (as/match? shape a))
          (str "permutation must be denied: " a)))))

(deftest non-force-permutations-are-allowed
  (let [shape {:requires #{(as/positional 0 "push")}
               :denies   #{(as/has-flag "--force") (as/has-flag "--mirror")}}
        ok-perms
        [{:flags {} :positionals ["push" "origin" "main"]}
         {:flags {"-q" true} :positionals ["push" "origin" "main"]}]]
    (doseq [a ok-perms]
      (is (as/match? shape a)
          (str "permutation must be allowed: " a)))))

;; ---- Fail-closed posture on unknown predicates ------------------------

(deftest unknown-predicate-type-fails-closed
  (is (not (as/match-pred? {:type :nonsense :x 1} {:flags {} :positionals []}))))
