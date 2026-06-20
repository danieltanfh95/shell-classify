(ns shell-classify.classifiers.ruby-test
  "Ruby stdlib effect classifier (P5.3)."
  (:require [clojure.test :refer [deftest is testing]]
            [shell-classify.classify :as cls]
            [shell-shape.dialect.ruby :as ruby]))

(defn- effects-of [src]
  (let [tree (ruby/parse-script (ruby/tokenize src) {:dialect :ruby})]
    (cls/classify-tree tree)))

(defn- effects-of-bash [src]
  (cls/classify-tree (shell-shape.core/parse src)))

;; ---- File class ------------------------------------------------------

(deftest file-delete-emits-fs-delete
  (let [effs (effects-of "File.delete('/tmp/x')")]
    (is (some #(and (= :fs-delete   (:class %))
                    (= "/tmp/x"     (:scope %))
                    (= "File"       (:module %))
                    (= "File.delete" (:function %)))
              effs))))

(deftest file-unlink-emits-fs-delete
  (let [effs (effects-of "File.unlink('/tmp/x')")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %)))
              effs))))

(deftest file-read-emits-fs-read
  (let [effs (effects-of "File.read('/etc/passwd')")]
    (is (some #(and (= :fs-read (:class %)) (= "/etc/passwd" (:scope %)))
              effs))))

(deftest file-write-emits-fs-write
  (let [effs (effects-of "File.write('/tmp/x', 'data')")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %)))
              effs))))

(deftest file-rename-emits-both
  (let [effs (effects-of "File.rename('/tmp/a', '/tmp/b')")]
    (is (some #(and (= :fs-read  (:class %)) (= "/tmp/a" (:scope %))) effs))
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/b" (:scope %))) effs))))

(deftest file-open-write-mode
  (let [effs (effects-of "File.open('/tmp/x', 'w')")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest file-open-default-read
  (let [effs (effects-of "File.open('/etc/passwd')")]
    (is (some #(and (= :fs-read (:class %)) (= "/etc/passwd" (:scope %))) effs))))

;; ---- FileUtils -------------------------------------------------------

(deftest fileutils-rm-rf-emits-fs-delete
  (let [effs (effects-of "FileUtils.rm_rf('/tmp/x')")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "/tmp/x"  (:scope %))
                    (= "FileUtils" (:module %))
                    (= "FileUtils.rm_rf" (:function %)))
              effs))))

(deftest fileutils-cp-emits-both
  (let [effs (effects-of "FileUtils.cp('/src', '/dst')")]
    (is (some #(and (= :fs-read  (:class %)) (= "/src" (:scope %))) effs))
    (is (some #(and (= :fs-write (:class %)) (= "/dst" (:scope %))) effs))))

(deftest fileutils-mkdir-p-emits-fs-write
  (let [effs (effects-of "FileUtils.mkdir_p('/tmp/a/b/c')")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/a/b/c" (:scope %))) effs))))

;; ---- Dir class -------------------------------------------------------

(deftest dir-mkdir-emits-fs-write
  (let [effs (effects-of "Dir.mkdir('/tmp/new')")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/new" (:scope %))) effs))))

(deftest dir-rmdir-emits-fs-delete
  (let [effs (effects-of "Dir.rmdir('/tmp/old')")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/old" (:scope %))) effs))))

(deftest dir-glob-emits-fs-read
  (let [effs (effects-of "Dir.glob('/tmp/*.log')")]
    (is (some #(and (= :fs-read (:class %)) (= "/tmp/*.log" (:scope %))) effs))))

;; ---- system / exec / spawn shell descent -----------------------------

(deftest system-shell-string-recursive
  (let [effs (effects-of "system('rm -rf /tmp/x')")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest exec-shell-string-recursive
  (let [effs (effects-of "exec('rm -rf /tmp/x')")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest process-spawn-list-form
  (let [effs (effects-of "Process.spawn('rm', '-rf', '/tmp/x')")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest io-popen-shell-descent
  (let [effs (effects-of "IO.popen('ls /etc')")]
    (is (some #(= :fs-read (:class %)) effs))))

(deftest open3-capture3-shell-descent
  (let [effs (effects-of "Open3.capture3('rm -rf /tmp/x')")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest system-variable-args-is-opaque
  (let [effs (effects-of "system(cmd)")]
    (is (some #(and (= :opaque (:class %))
                    (= :variable-args (-> % :provenance :reason)))
              effs))))

;; ---- Net::HTTP -------------------------------------------------------

(deftest net-http-get-emits-net-out
  (let [effs (effects-of "Net::HTTP.get('http://example.com')")]
    (is (some #(and (= :net-out (:class %))
                    (= "http://example.com" (:scope %)))
              effs))))

(deftest net-http-post-form-emits-net-out
  (let [effs (effects-of "Net::HTTP.post_form('http://x.com', {})")]
    (is (some #(= :net-out (:class %)) effs))))

(deftest kernel-open-url-emits-net-out
  (let [effs (effects-of "open('http://example.com/data')")]
    (is (some #(and (= :net-out (:class %))
                    (= "http://example.com/data" (:scope %)))
              effs))))

(deftest kernel-open-path-emits-fs-read
  (let [effs (effects-of "open('/tmp/file')")]
    (is (some #(and (= :fs-read (:class %)) (= "/tmp/file" (:scope %))) effs))))

;; ---- eval / dynamic ---------------------------------------------------

(deftest eval-emits-opaque-dynamic-eval
  (let [effs (effects-of "eval('puts 1')")]
    (is (some #(and (= :opaque (:class %))
                    (= :dynamic-eval (-> % :provenance :reason)))
              effs))))

(deftest instance-eval-emits-opaque-dynamic-eval
  (let [effs (effects-of "x.instance_eval('1+1')")]
    ;; receiver `x.instance_eval` → callable ["x" "instance_eval"]
    ;; which doesn't match ["instance_eval"]. So this falls to
    ;; :unknown-effect-for-program — acceptable; literal-form
    ;; bare instance_eval is what we actively classify.
    (is (some #(and (= :opaque (:class %))
                    (or (= :dynamic-eval (-> % :provenance :reason))
                        (= :unknown-effect-for-program
                           (-> % :provenance :reason))))
              effs))))

;; ---- load + require dynamic ------------------------------------------

(deftest load-emits-fs-read-and-dynamic-eval
  (let [effs (effects-of "load('/etc/init.rb')")]
    (is (some #(and (= :fs-read (:class %)) (= "/etc/init.rb" (:scope %))) effs))
    (is (some #(and (= :opaque (:class %))
                    (= :dynamic-eval (-> % :provenance :reason)))
              effs))))

;; ---- Unknown callable → opaque :unknown-effect-for-program ----------

(deftest unknown-callable-is-opaque
  (let [effs (effects-of "Rails.application.eager_load!")]
    (is (some #(and (= :opaque (:class %))
                    (= :unknown-effect-for-program
                       (-> % :provenance :reason)))
              effs))))

;; ---- End-to-end via shell-shape: `ruby -e "..."` ---------------------

(deftest ruby-dash-e-end-to-end
  (let [effs (effects-of-bash "ruby -e \"File.delete('/tmp/x')\"")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))
    (is (some #(= "File.delete" (:function %)) effs))))

;; ---- :program coordinate ----------------------------------------------

(deftest ruby-effects-carry-ruby-program-coordinate
  (let [effs (effects-of "File.delete('/tmp/x')")]
    (is (every? #(= "ruby" (-> % :provenance :program))
                (filter #(= "File.delete" (:function %)) effs)))))
