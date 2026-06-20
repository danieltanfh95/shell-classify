(ns shell-classify.overlay-test
  "Operator-facing classifier overlay tests.

   Covers:
     - happy-path instantiation for every :kind in `valid-kinds`
     - validation errors (unknown :kind, missing :dialect, bad shapes)
     - missing-file → no-op load
     - file-read errors → fail-closed
     - merge override semantics + summary reporting
     - install! actually swaps the root binding such that
       classify-tree picks up the new program

   These exercise the substrate at the API surface — `parse-overlay`,
   `install!`, then `(eff/active-registry)` — not the dynamic-var
   internals."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [shell-classify.overlay :as overlay]
   [shell-classify.call :as call]
   [shell-classify.effects :as eff]
   [shell-shape.core :as ss])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- tempdir ^java.io.File [^String prefix]
  (.toFile (Files/createTempDirectory prefix (into-array FileAttribute []))))

(defn- delete-recursively! [^java.io.File f]
  (when (.exists f)
    (when (.isDirectory f)
      (doseq [^java.io.File c (.listFiles f)] (delete-recursively! c)))
    (.delete f)))

;; install! is process-global via alter-var-root. Each test that exercises
;; install! must restore the original (nil) root afterward so other tests
;; in the suite see default-registry.
(defn- with-restored-registry [f]
  (let [orig (.getRawRoot #'eff/*registry-override*)]
    (try (f)
         (finally
           (alter-var-root #'eff/*registry-override* (constantly orig))))))

(use-fixtures :each with-restored-registry)

(defn- spec [overrides]
  (merge {:doc "test program" :kind :pure :rule :test}
         overrides))

;; ---- parse-overlay: happy path for every :kind -------------------------

(deftest parse-overlay-instantiates-every-kind
  (testing "every :kind in valid-kinds dispatches to a real classifier fn"
    (doseq [kind overlay/valid-kinds]
      (let [extra (case kind
                    :shell-interpret  {:dialect :clojure}
                    :interp-interpret {:dialect :perl}
                    {})
            data {"testprog" (spec (merge {:kind kind} extra))}
            result (overlay/parse-overlay data)
            entry  (get (:specs result) "testprog")]
        (is (fn? (:classify entry))
            (format "kind %s should yield a callable classifier" kind))
        (is (= kind (:kind entry)))
        (is (= :test (:rule entry)))))))

(deftest parse-overlay-classifies-correctly-by-kind
  ;; Classifiers receive a normalized-call (see `shell-classify.call`).
  ;; Construct via `from-shell-shape-command` (the test exercises the
  ;; classifier function directly, not the full classify-tree pipeline
  ;; which would normalize for us).
  (testing ":pure classifier emits stdout-emit"
    (let [data {"sleep" (spec {:kind :pure :rule :sleep})}
          result (overlay/parse-overlay data)
          classify (-> result :specs (get "sleep") :classify)
          effs (classify (call/from-shell-shape-command
                          {:program "sleep" :args []}) {})]
      (is (= 1 (count effs)))
      (is (= :stdout-emit (-> effs first :class)))))

  (testing ":proc-signal classifier honors :scope override"
    (let [data {"pkill" (spec {:kind :proc-signal :rule :pkill :scope "?"})}
          result (overlay/parse-overlay data)
          classify (-> result :specs (get "pkill") :classify)
          effs (classify (call/from-shell-shape-command
                          {:program "pkill" :args []}) {})]
      (is (= :proc-signal (-> effs first :class)))
      (is (= "?" (-> effs first :scope)))))

  (testing ":shell-interpret classifier honors :dialect"
    (let [data {"bb" (spec {:kind :shell-interpret :rule :bb :dialect :clojure})}
          result (overlay/parse-overlay data)
          classify (-> result :specs (get "bb") :classify)
          effs (classify (call/from-shell-shape-command
                          {:program "bb" :args []}) {})]
      (is (= :shell-interpret (-> effs first :class)))
      (is (= "clojure" (-> effs first :scope)))))

  (testing ":fs-write classifier emits fs-write per positional"
    (let [data {"tee" (spec {:kind :fs-write :rule :tee})}
          result (overlay/parse-overlay data)
          classify (-> result :specs (get "tee") :classify)
          cmd (ss/parse "tee out.txt")
          cmd-node (-> cmd :nodes first :stages first)
          effs (classify (call/from-shell-shape-command cmd-node) {})]
      (is (some #(= :fs-write (:class %)) effs)))))

;; ---- parse-overlay: validation errors ----------------------------------

(deftest parse-overlay-rejects-unknown-kind
  (let [data {"weird" {:doc "x" :kind :no-such-kind :rule :x}}]
    (is (thrown? clojure.lang.ExceptionInfo (overlay/parse-overlay data)))
    (let [info (try (overlay/parse-overlay data)
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= :overlay-malformed (:reason info)))
      (is (some #(= :unknown (:reason %)) (:errors info))))))

(deftest parse-overlay-rejects-missing-dialect
  (let [data {"bb" {:doc "needs dialect" :kind :shell-interpret :rule :bb}}
        info (try (overlay/parse-overlay data)
                  (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :overlay-malformed (:reason info)))
    (is (some #(= :dialect (:field %)) (:errors info)))))

(deftest parse-overlay-rejects-bad-value-flags-shape
  (let [data {"head" {:doc "x" :kind :fs-read :rule :head
                      :value-flags ["-n"]}}  ; vector instead of set
        info (try (overlay/parse-overlay data)
                  (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :overlay-malformed (:reason info)))
    (is (some #(= :value-flags (:field %)) (:errors info)))))

(deftest parse-overlay-rejects-empty-program-name
  (let [data {"" {:doc "x" :kind :pure :rule :empty}}
        info (try (overlay/parse-overlay data)
                  (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :overlay-malformed (:reason info)))
    (is (some #(= :program (:field %)) (:errors info)))))

(deftest parse-overlay-rejects-missing-doc-and-rule
  (let [data {"bad" {:kind :pure}}
        info (try (overlay/parse-overlay data)
                  (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :overlay-malformed (:reason info)))
    (is (some #(= :doc (:field %))  (:errors info)))
    (is (some #(= :rule (:field %)) (:errors info)))))

(deftest parse-overlay-rejects-non-map-input
  (let [info (try (overlay/parse-overlay [])
                  (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :overlay-malformed (:reason info)))))

;; ---- merge semantics ---------------------------------------------------

(deftest parse-overlay-reports-overrides-vs-added
  (testing "an entry for an existing program reports as :override; a new one as :added"
    ;; `my-custom-tool` is a synthetic name with no possibility of
    ;; collision against the lib's growing default-registry. Using a
    ;; real program here would force this test to chase the registry's
    ;; evolution.
    (let [data {"ls"             (spec {:kind :pure :rule :weakened-ls})
                "my-custom-tool" (spec {:kind :shell-interpret
                                        :rule :my-custom-tool
                                        :dialect :clojure})}
          {:keys [overrides added]} (overlay/parse-overlay data)]
      (is (contains? overrides "ls"))
      (is (contains? added "my-custom-tool"))
      (is (not (contains? added "ls")))
      (is (not (contains? overrides "my-custom-tool"))))))

(deftest merge-registry-overlay-wins-on-conflict
  (let [overlay-specs (-> {"ls" (spec {:kind :pure :rule :weakened-ls})}
                          overlay/parse-overlay
                          :specs)
        merged (overlay/merge-registry eff/default-registry overlay-specs)]
    (is (= :weakened-ls (get-in merged ["ls" :rule]))
        "overlay entry must replace the default-registry entry for the same key")))

;; ---- load-overlay-from-file! -------------------------------------------

(deftest load-overlay-from-file-returns-nil-when-absent
  (let [d (tempdir "cw-overlay-")
        path (str (.getAbsolutePath d) "/no-such-file.edn")]
    (try
      (is (nil? (overlay/load-overlay-from-file! path)))
      (finally
        (delete-recursively! d)))))

(deftest load-overlay-from-file-throws-on-bad-edn
  (let [d (tempdir "cw-overlay-")
        f (io/file d "classifier-overlay.edn")]
    (try
      (spit f "{:not-balanced")  ; truncated EDN → reader error
      (let [info (try (overlay/load-overlay-from-file! (.getAbsolutePath f))
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :overlay-read-error (:reason info))))
      (finally
        (delete-recursively! d)))))

(deftest load-overlay-from-file-parses-valid-edn
  (let [d (tempdir "cw-overlay-")
        f (io/file d "classifier-overlay.edn")]
    (try
      (spit f (pr-str {"my-custom-tool" {:doc "pure timing"
                                         :kind :pure
                                         :rule :my-custom-tool}}))
      (let [result (overlay/load-overlay-from-file! (.getAbsolutePath f))]
        (is (contains? (:specs result) "my-custom-tool"))
        (is (contains? (:added result) "my-custom-tool")))
      (finally
        (delete-recursively! d)))))

;; ---- install! end-to-end -----------------------------------------------

(deftest install-no-file-leaves-default-registry
  (let [d (tempdir "cw-overlay-")
        path (str (.getAbsolutePath d) "/missing.edn")]
    (try
      (let [summary (overlay/install! path)]
        (is (false? (:loaded? summary)))
        (is (nil? eff/*registry-override*))
        (is (= eff/default-registry (eff/active-registry))))
      (finally
        (delete-recursively! d)))))

(deftest install-merges-and-classifies-new-program
  (testing "after install!, the active registry contains the new program
            and its classifier emits the configured kind. Tests the lib
            seam directly (no classify-tree orchestrator); the integration
            with classify-tree is covered by consumers."
    (let [d (tempdir "cw-overlay-")
          f (io/file d "classifier-overlay.edn")]
      (try
        (spit f (pr-str {"my-custom-tool"
                         {:doc "synthetic — Clojure runtime"
                          :kind :shell-interpret
                          :rule :my-custom-tool
                          :dialect :clojure}}))
        ;; baseline: my-custom-tool absent from default-registry
        (is (nil? (eff/lookup eff/default-registry "my-custom-tool"))
            "before install!, my-custom-tool is unknown")
        (let [summary (overlay/install! (.getAbsolutePath f))]
          (is (true? (:loaded? summary)))
          (is (contains? (:added summary) "my-custom-tool")))
        ;; after install!: my-custom-tool resolves through active-registry
        ;; to a shell-interpret:clojure classifier
        (let [spec (eff/lookup (eff/active-registry) "my-custom-tool")]
          (is (some? spec) "my-custom-tool is now in the active registry")
          (let [effs ((:classify spec) {:program "my-custom-tool" :args []} {})]
            (is (some #(and (= :shell-interpret (:class %))
                            (= "clojure" (:scope %)))
                      effs))))
        (finally
          (delete-recursively! d))))))

(deftest install-malformed-throws
  (let [d (tempdir "cw-overlay-")
        f (io/file d "classifier-overlay.edn")]
    (try
      (spit f (pr-str {"bb" {:doc "missing kind" :rule :bb}}))
      (is (thrown? clojure.lang.ExceptionInfo
                   (overlay/install! (.getAbsolutePath f))))
      (finally
        (delete-recursively! d)))))

(deftest active-overlay-summary-reflects-last-install
  (let [d (tempdir "cw-overlay-")
        f (io/file d "classifier-overlay.edn")]
    (try
      (spit f (pr-str {"my-custom-tool" {:doc "pure timing"
                                         :kind :pure
                                         :rule :my-custom-tool}}))
      (let [_ (overlay/install! (.getAbsolutePath f))
            s (overlay/active-overlay-summary)]
        (is (true? (:loaded? s)))
        (is (contains? (:added s) "my-custom-tool")))
      (finally
        (delete-recursively! d)))))

(deftest format-summary-renders-human-readable
  (let [d (tempdir "cw-overlay-")
        f (io/file d "classifier-overlay.edn")
        nope (str (.getAbsolutePath d) "/none.edn")]
    (try
      (spit f (pr-str {"my-custom-tool" {:doc "x"
                                         :kind :pure
                                         :rule :my-custom-tool}
                       "ls"              {:doc "weakened"
                                          :kind :pure
                                          :rule :weakened-ls}}))
      (let [s1 (overlay/install! (.getAbsolutePath f))
            txt1 (overlay/format-summary s1)]
        (is (str/includes? txt1 "1 added"))
        (is (str/includes? txt1 "1 overridden"))
        (is (str/includes? txt1 "my-custom-tool")))
      (let [s2 (overlay/install! nope)
            txt2 (overlay/format-summary s2)]
        (is (str/includes? txt2 "no file")))
      (finally
        (delete-recursively! d)))))
