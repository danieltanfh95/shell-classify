(ns shell-classify.classifiers.perl-test
  "Perl stdlib effect classifier (P5.4 / v0.16.0)."
  (:require [clojure.test :refer [deftest is testing]]
            [shell-classify.classify :as cls]
            [shell-shape.dialect.perl :as perl]))

(defn- effects-of [src]
  (let [tree (perl/parse-script (perl/tokenize src) {:dialect :perl})]
    (cls/classify-tree tree)))

(defn- effects-of-bash [src]
  (cls/classify-tree (shell-shape.core/parse src)))

;; ---- Core file builtins ----------------------------------------------

(deftest unlink-emits-fs-delete
  (let [effs (effects-of "unlink('/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "/tmp/x"   (:scope %))
                    (= "unlink"   (:function %)))
              effs))))

(deftest rmdir-emits-fs-delete
  (let [effs (effects-of "rmdir('/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest mkdir-emits-fs-write
  (let [effs (effects-of "mkdir('/tmp/x');")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest rename-emits-both
  (let [effs (effects-of "rename('/tmp/a', '/tmp/b');")]
    (is (some #(and (= :fs-read  (:class %)) (= "/tmp/a" (:scope %))) effs))
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/b" (:scope %))) effs))))

;; ---- open() polymorphism ---------------------------------------------

(deftest open-3arg-read
  (let [effs (effects-of "open(my $fh, '<', '/etc/passwd');")]
    (is (some #(and (= :fs-read (:class %)) (= "/etc/passwd" (:scope %))) effs))))

(deftest open-3arg-write
  (let [effs (effects-of "open(my $fh, '>', '/tmp/x');")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest open-3arg-append
  (let [effs (effects-of "open(my $fh, '>>', '/tmp/log');")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/log" (:scope %))) effs))))

;; ---- system / exec ---------------------------------------------------

(deftest system-shell-string-recursive
  (let [effs (effects-of "system('rm -rf /tmp/x');")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest system-list-form-recursive
  (let [effs (effects-of "system('rm', '-rf', '/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest exec-shell-string-recursive
  (let [effs (effects-of "exec('rm -rf /tmp/x');")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest system-variable-args-is-opaque
  (let [effs (effects-of "system($cmd);")]
    (is (some #(and (= :opaque (:class %))
                    (= :variable-args (-> % :provenance :reason)))
              effs))))

;; ---- Backtick / qx → shell descent ----------------------------------

(deftest backtick-shell-descent
  (let [effs (effects-of "`rm -rf /tmp/x`;")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest qx-shell-descent
  (let [effs (effects-of "qx{rm -rf /tmp/x};")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

;; ---- File::Path with fully-qualified callable ------------------------

(deftest file-path-rmtree-fully-qualified
  (let [effs (effects-of "File::Path::rmtree('/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "/tmp/x"   (:scope %))
                    (= "File::Path" (:module %))
                    (= "File::Path::rmtree" (:function %)))
              effs))))

;; ---- use-imports binding-table (qw) ---------------------------------

(deftest file-path-rmtree-via-qw-import
  (let [effs (effects-of "use File::Path qw(rmtree);\nrmtree('/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "/tmp/x"   (:scope %))
                    (= "File::Path::rmtree" (:function %)))
              effs))))

(deftest lwp-simple-get-via-qw-import
  (let [effs (effects-of "use LWP::Simple qw(get);\nget('http://example.com');")]
    (is (some #(and (= :net-out (:class %))
                    (= "http://example.com" (:scope %)))
              effs))))

;; ---- LWP::Simple fully-qualified ------------------------------------

(deftest lwp-simple-get-fully-qualified
  (let [effs (effects-of "my $body = LWP::Simple::get('http://example.com');")]
    (is (some #(and (= :net-out (:class %))
                    (= "http://example.com" (:scope %)))
              effs))))

;; ---- eval / dynamic --------------------------------------------------

(deftest eval-emits-opaque-dynamic-eval
  (let [effs (effects-of "eval('print 1');")]
    (is (some #(and (= :opaque (:class %))
                    (= :dynamic-eval (-> % :provenance :reason)))
              effs))))

;; ---- Unknown callable → opaque :unknown-effect-for-program ----------

(deftest unknown-callable-is-opaque
  (let [effs (effects-of "MyApp::do_stuff();")]
    (is (some #(and (= :opaque (:class %))
                    (= :unknown-effect-for-program
                       (-> % :provenance :reason)))
              effs))))

;; ---- End-to-end via shell-shape: `perl -e "..."` ---------------------

(deftest perl-dash-e-end-to-end
  (let [effs (effects-of-bash "perl -e \"unlink('/tmp/x');\"")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))
    (is (some #(= "unlink" (:function %)) effs))))

;; ---- :program coordinate ----------------------------------------------

(deftest perl-effects-carry-perl-program-coordinate
  (let [effs (effects-of "unlink('/tmp/x');")]
    (is (every? #(= "perl" (-> % :provenance :program))
                (filter #(= "unlink" (:function %)) effs)))))
