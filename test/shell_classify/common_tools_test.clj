(ns shell-classify.common-tools-test
  "Classifier coverage for the common-tool entries added in v0.1.0
   alongside the witness hoist: process tools (pkill/pgrep), filesystem
   inventory (df/du/free/uptime/whereis), pure (sleep/yes), tee, gh,
   and JVM launchers (bb/clojure/clj). Each test pins the
   `[class scope]` pairs the substrate must emit so a future
   classifier rewrite cannot silently downgrade these to opaque."
  (:require
   [clojure.test :refer [deftest is testing]]
   [shell-classify.classify :as cls]
   [shell-classify.effects :as eff]
   [shell-shape.core :as ss]))

(defn- pairs-of [cmd-str]
  (cls/effect-set (cls/classify-tree {:registry eff/default-registry}
                                     (ss/parse cmd-str))))

(defn- has-pair? [cmd-str class scope]
  (contains? (pairs-of cmd-str) [class scope]))

;; ---- Process — signal by name ------------------------------------------

(deftest pkill-emits-proc-signal
  (is (has-pair? "pkill firefox" :proc-signal "?")
      "pkill targets by name pattern — scope is variable, model as ?"))

(deftest pgrep-is-env-read
  (is (has-pair? "pgrep firefox" :env-read "?")
      "pgrep walks /proc; treated as env-read like ps"))

;; ---- Filesystem inventory ---------------------------------------------

(deftest df-is-env-read
  (is (has-pair? "df -h" :env-read "?")))

(deftest du-emits-fs-read-on-each-path
  (testing "du -sh /tmp /var — fs-read on each starting path"
    (is (has-pair? "du -sh /tmp /var" :fs-read "/tmp"))
    (is (has-pair? "du -sh /tmp /var" :fs-read "/var"))))

(deftest du-bare-reads-cwd
  (is (has-pair? "du" :fs-read ".")
      "du with no positional args walks cwd"))

(deftest free-is-env-read
  (is (has-pair? "free -m" :env-read "?")))

(deftest uptime-is-env-read
  (is (has-pair? "uptime" :env-read "?")))

(deftest whereis-is-env-read
  (is (has-pair? "whereis clojure" :env-read "?")
      "whereis scans $PATH/$MANPATH; same envelope as which"))

;; ---- Pure / idle / stdout-only ----------------------------------------

(deftest sleep-is-pure
  ;; `pure` factory emits :stdout-emit "?" (idle is observationally pure).
  (is (has-pair? "sleep 5" :stdout-emit "?")))

(deftest yes-is-stdout-emit-only
  (is (has-pair? "yes hello" :stdout-emit "?")))

;; ---- tee --------------------------------------------------------------

(deftest tee-writes-each-positional-and-consumes-stdin
  (testing "tee a.log b.log — fs-write on each path PLUS :stdin-consume"
    (is (has-pair? "tee a.log b.log" :fs-write "a.log"))
    (is (has-pair? "tee a.log b.log" :fs-write "b.log"))
    (is (has-pair? "tee a.log b.log" :stdin-consume "stdin")
        "tee unconditionally consumes stdin — distinct from classify-fs-write")))

(deftest tee-bare-is-just-stdin-consume
  (is (has-pair? "tee" :stdin-consume "stdin")
      "bare `tee` mirrors stdin → stdout; no fs-write"))

;; ---- gh ---------------------------------------------------------------

(deftest gh-emits-static-net-out-to-github-api
  (testing "every gh subcommand collapses to a single net-out scope"
    (is (has-pair? "gh issue create --title 'x'" :net-out "api.github.com"))
    (is (has-pair? "gh repo view owner/repo" :net-out "api.github.com"))
    (is (has-pair? "gh pr list" :net-out "api.github.com"))))

;; ---- JVM launchers ----------------------------------------------------

(deftest bb-emits-proc-spawn
  (is (has-pair? "bb -e '(println :hi)'" :proc-spawn "bb")))

(deftest clojure-emits-proc-spawn
  (is (has-pair? "clojure -M:test" :proc-spawn "clojure")))

(deftest clj-emits-proc-spawn
  (is (has-pair? "clj -A:dev" :proc-spawn "clj")))
