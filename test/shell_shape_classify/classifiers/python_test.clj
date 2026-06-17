(ns shell-shape-classify.classifiers.python-test
  "Per-language stdlib effect classifier for Python (P5.1 / v0.13.0).

  Tests verify the end-to-end pipeline from shell-shape's :script
  :dialect :python tree → witness's effect-records, with coordinate
  axes (:module, :function, :program) populated for the policy
  surface.

  Direct ns tests use shell-shape.dialect.python to produce the input;
  classify-tree dispatches to classifiers/python via the :dialect
  discriminator."
  (:require [clojure.test :refer [deftest is testing]]
            [shell-shape-classify.classify :as cls]
            [shell-shape.dialect.python :as py]))

(defn- effects-of [py-src]
  (let [tree (py/parse-script (py/tokenize py-src) {:dialect :python})]
    (cls/classify-tree tree)))

(defn- effects-of-bash [bash-src]
  (cls/classify-tree (shell-shape.core/parse bash-src)))

;; ---- Core stdlib mappings ---------------------------------------------

(deftest os-remove-emits-fs-delete
  (let [effs (effects-of "import os\nos.remove('/tmp/x')\n")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "/tmp/x"   (:scope %))
                    (= "os"       (:module %))
                    (= "os.remove" (:function %)))
              effs))))

(deftest os-unlink-emits-fs-delete
  (let [effs (effects-of "import os\nos.unlink('/tmp/x')\n")]
    (is (some #(= :fs-delete (:class %)) effs))))

(deftest os-mkdir-emits-fs-write
  (let [effs (effects-of "import os\nos.mkdir('/tmp/newdir')\n")]
    (is (some #(and (= :fs-write (:class %))
                    (= "/tmp/newdir" (:scope %)))
              effs))))

(deftest os-rename-emits-both-axes
  (let [effs (effects-of "import os\nos.rename('/tmp/a', '/tmp/b')\n")]
    (is (some #(and (= :fs-read  (:class %)) (= "/tmp/a" (:scope %))) effs))
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/b" (:scope %))) effs))))

(deftest os-system-recursively-classifies-shell-string
  (testing "os.system('rm -rf /tmp/x') → :fs-delete via shell-shape's
            bash dialect, with python's :module/:function coordinates
            propagated onto the spawned effect."
    (let [effs (effects-of "import os\nos.system('rm -rf /tmp/x')\n")]
      (is (some #(and (= :fs-delete (:class %))
                      (= "/tmp/x"   (:scope %))
                      (= "os"       (:module %))
                      (= "os.system" (:function %)))
                effs)))))

;; ---- subprocess: recursive descent via :spawned-commands -------------

(deftest subprocess-run-with-list-classifies-recursively
  (let [effs (effects-of "import subprocess\nsubprocess.run(['rm', '-rf', '/tmp/x'])\n")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "subprocess" (:module %))
                    (= "subprocess.run" (:function %)))
              effs))))

(deftest subprocess-run-shell-true-classifies-via-shell
  (let [effs (effects-of "import subprocess\nsubprocess.run('curl https://x.com', shell=True)\n")]
    (is (some #(and (= :net-out (:class %))
                    (= "subprocess" (:module %)))
              effs))))

(deftest subprocess-run-variable-argv-is-opaque
  (let [effs (effects-of "import subprocess\nsubprocess.run(cmd)\n")]
    (is (some #(and (= :opaque (:class %))
                    (= :variable-args (-> % :provenance :reason)))
              effs))))

;; ---- urllib / requests / socket --------------------------------------

(deftest urllib-urlopen-emits-net-out
  (let [effs (effects-of "import urllib.request\nurllib.request.urlopen('https://x.com/api')\n")]
    (is (some #(and (= :net-out (:class %))
                    (= "https://x.com/api" (:scope %)))
              effs))))

(deftest requests-get-emits-net-out
  (let [effs (effects-of "import requests\nrequests.get('https://x.com/api')\n")]
    (is (some #(and (= :net-out (:class %))
                    (= "requests" (:module %)))
              effs))))

;; ---- Open + mode discrimination --------------------------------------

(deftest open-default-mode-is-read
  (let [effs (effects-of "open('/tmp/x')")]
    (is (some #(and (= :fs-read  (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest open-mode-w-is-write
  (let [effs (effects-of "open('/tmp/x', 'w')")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest open-mode-a-is-write
  (let [effs (effects-of "open('/tmp/x', 'a')")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest open-mode-r-plus-is-write
  (let [effs (effects-of "open('/tmp/x', 'r+')")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %))) effs))))

;; ---- eval / exec → opaque :dynamic-eval ------------------------------

(deftest eval-emits-opaque-dynamic-eval
  (let [effs (effects-of "eval('1 + 1')")]
    (is (some #(and (= :opaque (:class %))
                    (= :dynamic-eval (-> % :provenance :reason)))
              effs))))

(deftest exec-emits-opaque-dynamic-eval
  (let [effs (effects-of "exec('print(1)')")]
    (is (some #(and (= :opaque (:class %))
                    (= :dynamic-eval (-> % :provenance :reason)))
              effs))))

;; ---- Binding-table resolution ----------------------------------------

(deftest from-import-resolves-bare-call
  (testing "`from os import remove; remove('/tmp/x')` → :fs-delete.
            The binding-table prepass joins remove → [os remove]."
    (let [effs (effects-of "from os import remove\nremove('/tmp/x')\n")]
      (is (some #(and (= :fs-delete (:class %))
                      (= "/tmp/x" (:scope %))
                      (= "os.remove" (:function %)))
                effs)))))

(deftest from-import-with-alias-resolves
  (let [effs (effects-of "from os import remove as rm\nrm('/tmp/x')\n")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest import-with-alias-resolves
  (let [effs (effects-of "import os as o\no.remove('/tmp/x')\n")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

;; ---- Unknown callables → :unknown-effect-for-program -----------------

(deftest unknown-callable-is-opaque-with-reason
  (testing "third-party calls not in the stdlib table emit a
            discriminated :opaque record carrying the dotted callable
            in :scope and :reason :unknown-effect-for-program."
    (let [effs (effects-of "import flask\nflask.run(app)\n")]
      (is (some #(and (= :opaque (:class %))
                      (= :unknown-effect-for-program (-> % :provenance :reason)))
                effs)))))

;; ---- End-to-end via shell-shape: `python -c "..."` --------------------

(deftest python-dash-c-end-to-end
  (let [effs (effects-of-bash "python3 -c \"import os; os.remove('/tmp/x')\"")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))
    (is (some #(= "os.remove" (:function %)) effs))))

(deftest python-heredoc-end-to-end
  (let [effs (effects-of-bash "python3 <<EOF\nimport os\nos.remove('/tmp/x')\nEOF\n")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

;; ---- Control-flow flattening: calls inside if-branches ---------------

(deftest call-inside-if-still-classified
  (testing "Python's shell-shape parser flattens control flow; the
            classifier emits the effect anyway — conservative."
    (let [effs (effects-of "import os\nif False:\n    os.remove('/tmp/x')\n")]
      (is (some #(= :fs-delete (:class %)) effs)))))

;; ---- :program coordinate ----------------------------------------------

(deftest python-effects-carry-python-program-coordinate
  (let [effs (effects-of "import os\nos.remove('/tmp/x')\n")]
    (is (every? #(= "python" (-> % :provenance :program))
                (filter #(= "os.remove" (:function %)) effs)))))
