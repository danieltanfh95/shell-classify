(ns shell-classify.env-resolve
  "P7c — env-read resolution (allowlist + deny-pattern + hash audit).

  When a `:var` part isn't resolvable from the same-string binding
  table (P7a `bindings.clj`), the witness optionally consults its
  process env to substitute. This namespace gates that read with two
  filters and a hashed-audit shape:

  - **Allowlist** (default empty): explicit list of var-NAMES or
    glob patterns that may flow through resolution. `.env` files
    contribute their declared names (via `dotenv/read-names`) to the
    union. Default empty makes the substrate fail-closed — vars must
    be explicitly enrolled.

  - **Deny-pattern** (final filter): even when on the allowlist, a
    var whose name matches any deny-pattern is refused (e.g.
    `*KEY*`, `*TOKEN*`). Belt-and-suspenders for the
    `SECRET_API_KEY=…` accidentally-allowlisted case.

  - **Audit hash**: when an env-read resolution fires, the substituted
    value's SHA-256 (base64url, truncated to 12 chars) is recorded.
    The value itself never reaches disk; the hash is a forensic
    marker for reproducibility.

  This namespace is a pure data API; the integration glue
  (substituting `:var` parts, marking provenance, binary disclosure
  on the wire) lives in `bindings.clj` / `classify.clj` /
  `socket.handlers`."
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util Base64)))

;; ---------------------------------------------------------------------------
;; Pattern matching — glob with `*` wildcard.
;; ---------------------------------------------------------------------------

(defn- glob-pattern->regex
  "Translate a glob-style pattern (`*` matches any char-sequence
   including empty) to a Java Pattern matching the full string."
  [^String pat]
  (let [escaped (-> pat
                    (str/replace "\\" "\\\\")
                    (str/replace "." "\\.")
                    (str/replace "+" "\\+")
                    (str/replace "?" "\\?")
                    (str/replace "(" "\\(")
                    (str/replace ")" "\\)")
                    (str/replace "[" "\\[")
                    (str/replace "]" "\\]")
                    (str/replace "{" "\\{")
                    (str/replace "}" "\\}")
                    (str/replace "^" "\\^")
                    (str/replace "$" "\\$")
                    (str/replace "|" "\\|")
                    (str/replace "*" ".*"))]
    (re-pattern (str "^" escaped "$"))))

(defn- any-glob-match?
  "True when `name` matches at least one glob pattern in `patterns`."
  [patterns ^String name]
  (boolean
   (some (fn [pat]
           (re-find (glob-pattern->regex pat) name))
         patterns)))

(defn allowlist-match?
  "Is `name` covered by the allowlist? Empty allowlist means
   fail-closed — no var is resolvable."
  [allowlist ^String name]
  (any-glob-match? allowlist name))

(defn deny-pattern-match?
  "Does `name` match any deny-pattern? Empty deny-pattern means no
   names are refused at this layer (allowlist still applies)."
  [deny-pattern ^String name]
  (any-glob-match? deny-pattern name))

(defn env-resolvable?
  "Composed gate: name is on allowlist AND not blocked by deny-pattern.
   `cfg` is a map with `:allowlist :deny-pattern` (vectors of glob
   strings)."
  [cfg ^String name]
  (and (allowlist-match? (or (:allowlist cfg) []) name)
       (not (deny-pattern-match? (or (:deny-pattern cfg) []) name))))

;; ---------------------------------------------------------------------------
;; Hash function — SHA-256 → base64url → truncate to 12 chars.
;; Forensic marker only; truncation is acceptable because the hash
;; is not a cryptographic commitment, just a reproducibility token.
;; ---------------------------------------------------------------------------

(defn value-hash
  "SHA-256 of `value` (UTF-8 bytes), base64url-encoded, truncated to
   12 characters. nil-safe: returns nil for nil input."
  [^String value]
  (when (some? value)
    (let [bytes (.getBytes value StandardCharsets/UTF_8)
          md    (MessageDigest/getInstance "SHA-256")
          digest (.digest md bytes)
          b64u  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) digest)]
      (subs b64u 0 12))))

;; ---------------------------------------------------------------------------
;; Look-up — given an env-resolve config and a var name, return
;; the literal value (or nil) PLUS its hash (when value present).
;; ---------------------------------------------------------------------------

(defn lookup
  "Try to resolve `name` from `cfg`. Returns `{:value <str> :hash
   <12-char>}` if the var is enabled + on the allowlist + not in
   deny-pattern + present in the env-source. Otherwise nil.

   `cfg` shape:
     {:enabled?     true
      :env          {name → value}   ; process-env snapshot
      :allowlist    [\"PATH\" \"BUILD_*\" ...]
      :deny-pattern [\"*KEY*\" \"*TOKEN*\" ...]}"
  [cfg ^String name]
  (when (and cfg
             (:enabled? cfg)
             (env-resolvable? cfg name))
    (let [value (get (or (:env cfg) {}) name)]
      (when (some? value)
        {:value value
         :hash  (value-hash value)}))))

;; ---------------------------------------------------------------------------
;; Default deny-pattern — names that look like credentials. Belt-and-
;; suspenders for misconfigured allowlists.
;; ---------------------------------------------------------------------------

(def default-deny-pattern
  ["*KEY*" "*TOKEN*" "*SECRET*" "*PASSWORD*"
   "*AUTH*" "*PRIVATE*" "*CREDENTIAL*" "*PASSWD*"
   "*PASSPHRASE*"])
