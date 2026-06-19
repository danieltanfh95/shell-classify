(ns shell-classify.getopt-specs-test
  "Per-program getopt normalization: GNU getopt, --flag=value, opts-with-arg
   consumption, bundling, `--` stop-token, tar's leading bundle, find's
   predicate syntax, dd's key=value, BSD-getopt restrictions."
  (:require [clojure.test :refer [deftest is testing]]
            [shell-classify.getopt-specs :as gs]))

;; ---- Basic GNU getopt --------------------------------------------------

(deftest gnu-long-flag-bare
  (let [r (gs/normalize-argv :gnu ["--force"])]
    (is (= {"--force" true} (:flags r)))
    (is (= [] (:positionals r)))))

(deftest gnu-long-eq-value
  (let [r (gs/normalize-argv :gnu ["--name=John"])]
    (is (= {"--name" "John"} (:flags r)))))

(deftest gnu-short-flag-bare
  (let [r (gs/normalize-argv :gnu ["-v"])]
    (is (= {"-v" true} (:flags r)))))

(deftest gnu-short-flag-bundled
  (testing "`-abc` → `-a -b -c` under `:bundled?` true"
    (let [r (gs/normalize-argv :gnu ["-abc"])]
      (is (= {"-a" true "-b" true "-c" true} (:flags r))))))

(deftest gnu-positionals-pass-through
  (let [r (gs/normalize-argv :gnu ["push" "origin" "main"])]
    (is (= {} (:flags r)))
    (is (= ["push" "origin" "main"] (:positionals r)))))

(deftest gnu-stop-token-double-dash
  (testing "everything after `--` is positional"
    (let [r (gs/normalize-argv :gnu ["--verbose" "--" "--not-a-flag" "x"])]
      (is (= {"--verbose" true} (:flags r)))
      (is (= ["--not-a-flag" "x"] (:positionals r))))))

;; ---- opts-with-arg consumption ----------------------------------------

(deftest opts-with-arg-consumes-next-token
  (let [spec (assoc gs/gnu :opts-with-arg #{"-c"})
        r (gs/normalize-argv spec ["-c" "receive.denyDeletes=true" "push"])]
    (is (= {"-c" "receive.denyDeletes=true"} (:flags r)))
    (is (= ["push"] (:positionals r)))))

(deftest opts-with-arg-long-eq-form-takes-precedence
  (testing "long-eq form binds the value even when also in :opts-with-arg"
    (let [spec (assoc gs/gnu :opts-with-arg #{"--git-dir"})
          r (gs/normalize-argv spec ["--git-dir=/tmp/x" "push"])]
      (is (= {"--git-dir" "/tmp/x"} (:flags r)))
      (is (= ["push"] (:positionals r))))))

;; ---- Position-invariance among flags (key load-bearing test) ----------

(deftest flag-reorder-yields-same-flags-set
  (testing "flag position among themselves is irrelevant: the normalized
            :flags map is the same regardless of source order."
    (let [a (gs/normalize-argv :gnu ["--force" "push" "origin" "main"])
          b (gs/normalize-argv :gnu ["push" "--force" "origin" "main"])
          c (gs/normalize-argv :gnu ["push" "origin" "main" "--force"])]
      (is (= (:flags a) (:flags b) (:flags c)))
      ;; Positionals stay in their order, MODULO the flag positions
      (is (= ["push" "origin" "main"] (:positionals a) (:positionals b) (:positionals c))))))

;; ---- tar's leading bundle ---------------------------------------------

(deftest tar-bundled-first-arg-decomposes
  (testing "`tar cvf out.tar src` → `tar -c -v -f out.tar src`"
    (let [r (gs/normalize-argv :tar-bundled ["cvf" "out.tar" "src"])]
      (is (contains? (:flags r) "-c"))
      (is (contains? (:flags r) "-v"))
      ;; -f consumes the next token per :opts-with-arg
      (is (= "out.tar" (get (:flags r) "-f")))
      (is (= ["src"] (:positionals r))))))

(deftest tar-bundled-leading-dash-still-works
  (testing "`tar -xvf x.tar` still normalizes correctly (no bundle decomposition needed)"
    (let [r (gs/normalize-argv :tar-bundled ["-xvf" "out.tar" "src"])]
      (is (contains? (:flags r) "-x"))
      (is (contains? (:flags r) "-v"))
      (is (= "out.tar" (get (:flags r) "-f")))
      (is (= ["src"] (:positionals r))))))

;; ---- find's predicate syntax ------------------------------------------

(deftest find-predicate-leaves-dash-tokens-positional
  (testing "find's `-delete`, `-name X`, `-type f` are POSITIONAL semantically.
            normalize-argv hands the raw argv through; find's classifier
            in effects.clj does the structural parsing."
    (let [r (gs/normalize-argv :find-predicate
                              ["/tmp" "-type" "f" "-name" "*.log" "-delete"])]
      (is (= {} (:flags r)))
      (is (= ["/tmp" "-type" "f" "-name" "*.log" "-delete"] (:positionals r))))))

;; ---- dd's key=value syntax --------------------------------------------

(deftest dd-key-value-all-positional
  (let [r (gs/normalize-argv :dd-key-value ["if=src" "of=dst" "bs=1M"])]
    (is (= {} (:flags r)))
    (is (= ["if=src" "of=dst" "bs=1M"] (:positionals r)))))

;; ---- BSD-getopt restrictions ------------------------------------------

(deftest bsd-getopt-no-long-eq
  (testing "BSD-getopt doesn't split `--name=value`; the equals char is
            part of the flag name."
    (let [r (gs/normalize-argv :bsd-getopt ["--name=John"])]
      (is (= {"--name=John" true} (:flags r))))))

(deftest bsd-getopt-no-bundling
  (testing "BSD-getopt keeps `-abc` as a single flag (no decomposition)."
    (let [r (gs/normalize-argv :bsd-getopt ["-abc"])]
      (is (= {"-abc" true} (:flags r))))))

;; ---- Subcommand-at-0 specs ---------------------------------------------

(deftest gnu-with-subcommand-positional-zero-is-subcommand
  (testing "`:gnu-with-subcommand` doesn't change normalize-argv semantics —
            it only signals to consumers that positional[0] is the subcommand.
            (Discrimination happens at the predicate level.)"
    (let [r (gs/normalize-argv :gnu-with-subcommand ["push" "origin" "main"])]
      (is (= ["push" "origin" "main"] (:positionals r))))))

;; ---- resolve-spec invariants ------------------------------------------

(deftest resolve-spec-keyword-lookup
  (is (= gs/gnu (gs/resolve-spec :gnu)))
  (is (= gs/gnu-with-subcommand (gs/resolve-spec :gnu-with-subcommand))))

(deftest resolve-spec-passes-through-map
  (let [spec {:opts-with-arg #{"--mine"} :long-eq? true}]
    (is (= spec (gs/resolve-spec spec)))))

(deftest resolve-spec-unknown-keyword-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (gs/resolve-spec :nonsense))))

(deftest resolve-spec-rejects-non-keyword-non-map
  (is (thrown? clojure.lang.ExceptionInfo
               (gs/resolve-spec "string"))))

;; ---- Empty argv corner case -------------------------------------------

(deftest empty-argv-yields-empty-shape
  (let [r (gs/normalize-argv :gnu [])]
    (is (= {} (:flags r)))
    (is (= [] (:positionals r)))))
