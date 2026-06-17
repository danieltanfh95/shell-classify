(ns shell-shape-classify.bindings-test
  "Unit tests for P7a same-string binding extraction and substitution
  helpers in shell-shape-classify.bindings. The classify-side integration
  (eval $VAR → :fs-delete /tmp/x and friends) is covered in
  resolved-eval-test."
  (:require
   [clojure.test :refer [deftest is testing]]
   [shell-shape-classify.bindings :as bind]
   [shell-shape.core :as ss]))

;; ---- assignment-shape? -----------------------------------------------------

(deftest assignment-shape?-recognises-literal-assignments
  (is (true?  (bind/assignment-shape? "FOO=bar")))
  (is (true?  (bind/assignment-shape? "FOO=")))
  (is (true?  (bind/assignment-shape? "_X=y")))
  (is (true?  (bind/assignment-shape? "F1=v")))
  (is (false? (bind/assignment-shape? "rm /tmp/x")))
  (is (false? (bind/assignment-shape? "1FOO=bar")))
  (is (false? (bind/assignment-shape? "FOO BAR=baz")))
  (is (false? (bind/assignment-shape? nil)))
  (is (false? (bind/assignment-shape? ""))))

;; ---- command-binding -------------------------------------------------------

(defn- first-command [s]
  (-> (ss/parse s) :nodes first :stages first))

(deftest command-binding-standalone-literal-rhs
  (let [cmd (first-command "FOO=bar")
        b   (bind/command-binding cmd)]
    (is (= :standalone (:kind b)))
    (is (= "FOO" (:name b)))
    (is (= "bar" (:value b)))))

(deftest command-binding-standalone-quoted-rhs
  ;; Quotes are stripped at the tokenizer; the literal is the inner string.
  (let [cmd (first-command "FOO=\"rm /tmp/x\"")
        b   (bind/command-binding cmd)]
    (is (= :standalone (:kind b)))
    (is (= "FOO" (:name b)))
    (is (= "rm /tmp/x" (:value b)))))

(deftest command-binding-standalone-empty-value
  (let [cmd (first-command "FOO=")
        b   (bind/command-binding cmd)]
    (is (= :standalone (:kind b)))
    (is (= "FOO" (:name b)))
    (is (= "" (:value b)))))

(deftest command-binding-standalone-non-literal-rhs
  ;; RHS contains a $BAR var — binding shape recognised, value=nil.
  (let [cmd (first-command "FOO=$BAR")
        b   (bind/command-binding cmd)]
    (is (= :standalone (:kind b)))
    (is (= "FOO" (:name b)))
    (is (nil? (:value b)))))

(deftest command-binding-prefix-style
  (let [cmd (first-command "FOO=bar BAZ=qux echo hi")
        b   (bind/command-binding cmd)]
    (is (= :prefix (:kind b)))
    (is (= [{:name "FOO" :value "bar"}
            {:name "BAZ" :value "qux"}]
           (:bindings b)))
    (is (= "echo" (:program (:stripped-cmd b))))))

(deftest command-binding-nil-when-not-assignment-shape
  (is (nil? (bind/command-binding (first-command "rm /tmp/x"))))
  (is (nil? (bind/command-binding (first-command "echo hi")))))

;; ---- resolve-token / resolve-args / resolve-program-token -----------------

(defn- arg-token [s]
  ;; Extract the first arg-token from `echo <s>`.
  (-> (ss/parse (str "echo " s)) :nodes first :stages first :args first))

(deftest resolve-token-substitutes-bound-var
  (let [tok    (arg-token "$DANGER")
        bnds   {"DANGER" "rm /tmp/x"}
        result (bind/resolve-token bnds tok)]
    (is (some? result))
    (is (= "rm /tmp/x" (:literal result)))
    (is (= [:literal] (mapv :kind (:parts result))))))

(deftest resolve-token-returns-nil-when-var-unbound
  (let [tok (arg-token "$DANGER")]
    (is (nil? (bind/resolve-token {} tok)))))

(deftest resolve-token-mixed-literal-and-var
  ;; `prefix$VAR` is one token with two parts.
  (let [tok  (arg-token "prefix-$VAR-suffix")
        bnds {"VAR" "X"}
        result (bind/resolve-token bnds tok)]
    (is (some? result))
    (is (= "prefix-X-suffix" (:literal result)))))

(deftest resolve-token-fails-on-subst-part
  ;; `$(date)` is a :subst part — we do not execute substitutions, so
  ;; resolution must fail (caller falls back to opaque).
  (let [tok (arg-token "$(date)")]
    (is (nil? (bind/resolve-token {"date" "Tue"} tok)))))

(deftest resolve-program-token-substitutes-var-program
  ;; In `$CMD args`, the :program is a token with :var parts.
  (let [cmd  (first-command "$CMD /tmp/x")
        bnds {"CMD" "rm"}]
    (is (= "rm" (bind/resolve-program-token bnds (:program cmd))))))

(deftest resolve-program-token-nil-when-unbound
  (let [cmd (first-command "$CMD /tmp/x")]
    (is (nil? (bind/resolve-program-token {} (:program cmd))))))

(deftest resolve-args-substitutes-var-args
  (let [cmd  (first-command "echo $A and $B")
        bnds {"A" "alpha" "B" "beta"}
        new-args (bind/resolve-args bnds (:args cmd))]
    (is (= "alpha" (:literal (nth new-args 0))))
    (is (= "and"   (:literal (nth new-args 1))))
    (is (= "beta"  (:literal (nth new-args 2))))))

(deftest args->joined-literal-concatenates-literal-args
  (let [cmd    (first-command "echo rm /tmp/x")
        joined (bind/args->joined-literal (:args cmd))]
    (is (= "rm /tmp/x" joined))))

(deftest args->joined-literal-nil-on-unresolved-var
  (let [cmd    (first-command "echo $UNBOUND")
        joined (bind/args->joined-literal (:args cmd))]
    (is (nil? joined))))

;; ---- args-fully-resolvable? / invocation-resolvable? ---------------------

(deftest args-fully-resolvable?-true-when-all-vars-bound
  (let [cmd (first-command "echo $A $B")]
    (is (true?  (bind/args-fully-resolvable? {"A" "x" "B" "y"} (:args cmd))))
    (is (false? (bind/args-fully-resolvable? {"A" "x"}        (:args cmd))))
    (is (false? (bind/args-fully-resolvable? {}               (:args cmd))))))
