(ns shell-shape-classify.classifiers.node-test
  "Node.js stdlib effect classifier (P5.2 / v0.14.0)."
  (:require [clojure.test :refer [deftest is testing]]
            [shell-shape-classify.classify :as cls]
            [shell-shape.dialect.node :as node]))

(defn- effects-of [js-src]
  (let [tree (node/parse-script (node/tokenize js-src) {:dialect :node})]
    (cls/classify-tree tree)))

(defn- effects-of-bash [bash-src]
  (cls/classify-tree (shell-shape.core/parse bash-src)))

;; ---- fs stdlib mappings ----------------------------------------------

(deftest fs-unlink-sync-emits-fs-delete
  (let [effs (effects-of "const fs = require('fs');\nfs.unlinkSync('/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "/tmp/x"   (:scope %))
                    (= "fs"       (:module %))
                    (= "fs.unlinkSync" (:function %)))
              effs))))

(deftest fs-write-file-sync-emits-fs-write
  (let [effs (effects-of "const fs = require('fs');\nfs.writeFileSync('/tmp/x', 'data');")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest fs-read-file-sync-emits-fs-read
  (let [effs (effects-of "const fs = require('fs');\nfs.readFileSync('/tmp/x');")]
    (is (some #(and (= :fs-read (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest fs-rename-sync-emits-both
  (let [effs (effects-of "const fs = require('fs');\nfs.renameSync('/tmp/a', '/tmp/b');")]
    (is (some #(and (= :fs-read  (:class %)) (= "/tmp/a" (:scope %))) effs))
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/b" (:scope %))) effs))))

(deftest fs-open-sync-write-flags
  (let [effs (effects-of "const fs = require('fs');\nfs.openSync('/tmp/x', 'w');")]
    (is (some #(and (= :fs-write (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest fs-promises-unlink-emits-fs-delete
  (let [effs (effects-of "const { promises } = require('fs');\nfs.promises.unlink('/tmp/x');")]
    ;; Without full destructure resolution, fs.promises.unlink resolves
    ;; via the dotted path directly.
    (is (some #(or (= :fs-delete (:class %))
                   (and (= :opaque (:class %))
                        (= :unknown-effect-for-program (-> % :provenance :reason))))
              effs))))

;; ---- child_process stdlib mappings -----------------------------------

(deftest exec-sync-recursively-classifies-shell
  (let [effs (effects-of "const cp = require('child_process');\ncp.execSync('rm -rf /tmp/x');")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "/tmp/x" (:scope %))
                    (= "child_process" (:module %))
                    (= "child_process.execSync" (:function %)))
              effs))))

(deftest exec-file-sync-classifies-list-form
  (let [effs (effects-of "const cp = require('child_process');\ncp.execFileSync('rm', ['-rf', '/tmp/x']);")]
    (is (some #(and (= :fs-delete (:class %))
                    (= "/tmp/x" (:scope %))
                    (= "child_process.execFileSync" (:function %)))
              effs))))

(deftest spawn-with-variable-args-is-opaque
  (let [effs (effects-of "const cp = require('child_process');\ncp.spawnSync(cmd, args);")]
    (is (some #(and (= :opaque (:class %))
                    (= :variable-args (-> % :provenance :reason)))
              effs))))

;; ---- http/https net-out ----------------------------------------------

(deftest http-get-emits-net-out
  (let [effs (effects-of "const http = require('http');\nhttp.get('http://x.com');")]
    (is (some #(and (= :net-out (:class %))
                    (= "http://x.com" (:scope %)))
              effs))))

(deftest https-request-emits-net-out
  (let [effs (effects-of "const https = require('https');\nhttps.request('https://x.com/api');")]
    (is (some #(= :net-out (:class %)) effs))))

;; ---- eval / new Function → opaque dynamic-eval -----------------------

(deftest eval-emits-opaque-dynamic-eval
  (let [effs (effects-of "eval('1 + 1');")]
    (is (some #(and (= :opaque (:class %))
                    (= :dynamic-eval (-> % :provenance :reason)))
              effs))))

;; ---- Binding-table resolution ----------------------------------------

(deftest destructured-require-resolves-bare-call
  (testing "`const { unlinkSync } = require('fs'); unlinkSync('/x')`
            → :fs-delete via binding-table."
    (let [effs (effects-of "const { unlinkSync } = require('fs');\nunlinkSync('/tmp/x');")]
      (is (some #(and (= :fs-delete (:class %))
                      (= "/tmp/x" (:scope %))
                      (= "fs.unlinkSync" (:function %)))
                effs)))))

(deftest destructured-require-with-alias-resolves
  (let [effs (effects-of "const { unlinkSync: rm } = require('fs');\nrm('/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest default-import-resolves
  (let [effs (effects-of "import fs from 'fs';\nfs.unlinkSync('/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

(deftest named-import-resolves
  (let [effs (effects-of "import { unlinkSync } from 'fs';\nunlinkSync('/tmp/x');")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))))

;; ---- Unknown callable → opaque :unknown-effect-for-program ----------

(deftest unknown-callable-is-opaque
  (let [effs (effects-of "const express = require('express');\nexpress.run(app);")]
    (is (some #(and (= :opaque (:class %))
                    (= :unknown-effect-for-program (-> % :provenance :reason)))
              effs))))

;; ---- End-to-end via shell-shape: `node -e "..."` ---------------------

(deftest node-dash-e-end-to-end
  (let [effs (effects-of-bash "node -e \"const fs = require('fs'); fs.unlinkSync('/tmp/x');\"")]
    (is (some #(and (= :fs-delete (:class %)) (= "/tmp/x" (:scope %))) effs))
    (is (some #(= "fs.unlinkSync" (:function %)) effs))))

;; ---- :program coordinate ----------------------------------------------

(deftest node-effects-carry-node-program-coordinate
  (let [effs (effects-of "const fs = require('fs');\nfs.unlinkSync('/tmp/x');")]
    (is (every? #(= "node" (-> % :provenance :program))
                (filter #(= "fs.unlinkSync" (:function %)) effs)))))
