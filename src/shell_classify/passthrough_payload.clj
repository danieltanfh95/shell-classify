(ns shell-classify.passthrough-payload
  "P7b — payload extraction for pipe-to-file content classification.

  When `echo \"rm -rf /\" > /tmp/danger.sh` is parsed, the witness
  needs to surface what the written content *would do if executed*.
  This namespace extracts literal stdout content from passthrough
  producers (echo, printf, cat <<HEREDOC) so classify-redirect can
  hand it to ss/parse under a dialect derived from the target file's
  extension.

  This is the existing pipeline-composition `producer | interpreter`
  re-classification (shell-shape composition.clj) generalised to
  `producer > file` where the file is an interpreter at a future
  unknown moment. The default policy treats `:provenance :source
  :fs-write-content` effects as equivalent to direct execution
  effects — the LLM writing rm-rf-/ into any file is intent to
  execute it eventually.

  Stays parser-not-actor: we don't read non-literal file content
  (`cat file.sh > target` returns no payload); we don't execute
  substitutions (`echo $(date) > target` returns no payload). Only
  literal content reachable from the parse tree is extracted."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Dialect-by-extension for the content of the written file.
;; ---------------------------------------------------------------------------

(def ^:private extension->dialect
  "File extension → shell-shape dialect for content classification.
   Unknown extension defaults to :bash — most LLM-emitted scripts that
   land on disk are shell scripts; over-classifying (treating a .txt
   as bash) yields no false positives because non-shell content
   parses to no effects."
  {"sh"   :bash
   "bash" :bash
   "zsh"  :bash
   "py"   :python
   "pl"   :perl
   "rb"   :ruby
   "js"   :node
   "mjs"  :node
   "cjs"  :node})

(defn dialect-for-target
  "Pick the dialect for re-parsing content destined for `path`. Returns
   a dialect kw (defaults to :bash) suitable for `shell-shape.core/parse`'s
   `:dialect` opt."
  [path]
  (let [base (some-> path (str/split #"/") last)
        ext  (when base
               (let [i (.lastIndexOf base ".")]
                 (when (pos? i)
                   (str/lower-case (subs base (inc i))))))]
    (or (get extension->dialect ext) :bash)))

;; ---------------------------------------------------------------------------
;; Literal-content predicate / extractor for token args.
;; ---------------------------------------------------------------------------

(defn- token-literal-string
  "Concatenated `:value` of every `:literal` part. Returns nil when any
   part is `:var` / `:subst` / `:backtick` — the content can't be
   resolved from the parse tree alone."
  [tok]
  (when (and (= :token (:kind tok))
             (every? #(= :literal (:kind %)) (:parts tok)))
    (apply str (mapv :value (:parts tok)))))

(defn- args-fully-literal? [args]
  (every? token-literal-string args))

;; ---------------------------------------------------------------------------
;; Per-program extractors.
;; ---------------------------------------------------------------------------

(defn- echo-payload
  "`echo a b c` → \"a b c\\n\". `echo -n a b` → \"a b\" (no trailing
   newline). Returns nil if any arg is non-literal."
  [cmd]
  (let [raw-args (:args cmd)]
    (when (args-fully-literal? raw-args)
      (let [args (mapv token-literal-string raw-args)
            [opts rest] (split-with #(re-matches #"-[neE]+" %) args)
            no-newline? (some #(str/includes? % "n") opts)
            body (str/join " " rest)]
        (if no-newline? body (str body "\n"))))))

(defn- printf-payload
  "`printf FORMAT ARGS...`. We only handle the literal-only no-format-
   specifier case (`printf 'rm -rf /\\n'`). If FORMAT contains `%` or
   any arg is non-literal, return nil — runtime formatting isn't
   something we evaluate statically."
  [cmd]
  (let [raw-args (:args cmd)]
    (when (and (seq raw-args) (args-fully-literal? raw-args))
      (let [args (mapv token-literal-string raw-args)
            fmt  (first args)]
        (when (and (not (str/includes? fmt "%"))
                   (= 1 (count args)))
          ;; Interpret \n / \t etc in the literal format string. shell-shape's
          ;; tokenizer strips quotes but leaves escape sequences in the literal.
          (-> fmt
              (str/replace "\\n" "\n")
              (str/replace "\\t" "\t")
              (str/replace "\\r" "\r")
              (str/replace "\\\\" "\\")))))))

;; ---------------------------------------------------------------------------
;; Heredoc extractor.
;; ---------------------------------------------------------------------------

(defn- heredoc-stdin-from-redirects
  "When a `:command` has a `<<TAG` redirect, return its `:body` (the
   heredoc body string). Returns nil if no heredoc redirect or if the
   body is missing. cat <<EOF > target  — heredoc is the stdin source,
   redirected stdout goes to target."
  [cmd]
  (some (fn [rd]
          (when (and (= :<< (:op rd))
                     (= :heredoc (:kind (:target rd))))
            (:body (:target rd))))
        (:redirects cmd)))

;; ---------------------------------------------------------------------------
;; Public API.
;; ---------------------------------------------------------------------------

(defn extract-payload
  "Inspect `cmd` and return `{:body <string> :origin <kw>}` when the
   command emits a literal stdout payload reachable through the parse
   tree. Recognised producers:

   - `echo ARGS...`     → :echo-args
   - `printf FORMAT`    → :printf-args  (no `%` specifiers; single literal arg)
   - `cat <<TAG`        → :heredoc-body (heredoc body, body-quoted or stripped per shell-shape)

   Returns nil if the producer is unrecognised, args are non-literal,
   or the heredoc body is absent. Caller then suppresses content
   classification — only the existing `:fs-write` lands."
  [cmd]
  (let [prog (:program cmd)]
    (cond
      (not (string? prog)) nil

      (= prog "echo")
      (when-let [b (echo-payload cmd)] {:body b :origin :echo-args})

      (= prog "printf")
      (when-let [b (printf-payload cmd)] {:body b :origin :printf-args})

      (= prog "cat")
      (when-let [b (heredoc-stdin-from-redirects cmd)]
        {:body b :origin :heredoc-body})

      :else nil)))
