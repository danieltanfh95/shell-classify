(ns shell-classify.overlay
  "Operator-facing program-classifier overlay.

   Operators can extend the program-classifier registry by placing a
   `classifier-overlay.edn` file under `$CONTINUITY_WITNESS_HOME`
   (typically `~/.config/continuity-witness/classifier-overlay.edn`).
   The file is pure data — a closed set of `:kind` discriminators
   dispatches each operator-authored spec to one of effects.clj's
   vetted factory functions. There is no `eval` and no Clojure-code
   load path; an operator cannot smuggle arbitrary side-effecting
   code into the witness JVM through this seam.

   Lifecycle:

     1. Daemon boot reads the file (if present) via
        `load-overlay-from-file!`.
     2. `parse-overlay` validates every spec — unknown `:kind`,
        malformed fields, or reserved program names abort the boot
        with a clear diagnostic. Failing closed at boot is the
        intended behavior: a malformed overlay would otherwise be
        silently ignored at exercise time, which would surprise the
        operator.
     3. `install!` instantiates each spec into the
        `{:doc :classify :stdin :stdout :kind :rule}` shape that
        `effects/default-registry` uses, merges over the default
        (overlay wins on conflict), and `alter-var-root`s
        `effects/*registry-override*`. The existing
        `active-registry` resolution then routes every classifier
        call through the merged map.

   Overlay overrides are logged loudly — an operator who weakens
   `rm` to `:pure` should see it in the boot log. See the doctor
   `:classifier-overlay` check for steady-state verification.

   Spec shape (data-first):

       {<program-name-string>
        {:doc      <string>             ; required
         :kind     <kind-keyword>        ; required, see `valid-kinds`
         :rule     <keyword>             ; required (provenance tag)
         ;; per-kind keys — see `validate-spec` for what each kind
         ;; requires / permits:
         :scope             <string>     ; :net-out, :proc-signal default scope
         :dialect           <kw-or-str>  ; :shell-interpret, :interp-interpret
         :value-flags       <set-of-str> ; :fs-read, :stdin-or-fs-read,
                                         ; :script-then-files
         :path-value-flags  <set-of-str> ; :fs-read
         :script-via-flags  <set-of-str> ; :script-then-files
         :script-file-flags <set-of-str> ; :script-then-files
         :two-value-flags   <set-of-str> ; :script-then-files
         :inplace-flags     <set-of-str> ; :script-then-files
         :file-class        <keyword>    ; :script-then-files default :fs-read
         :emit-stdout?      <bool>       ; :script-then-files
         :stdin             <keyword>    ; metadata, default :data
         :stdout            <keyword>}}  ; metadata, default :data"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [shell-classify.effects :as eff]))

;; ---- Data: closed kinds & per-kind required fields ---------------------

(def valid-kinds
  "Closed set of `:kind` values an operator-authored spec may use.
   Each kind dispatches in `spec->classifier` to one of effects.clj's
   public factory functions. Adding a kind here is the only path to
   broadening operator authority — by design."
  #{:fs-read
    :stdin-or-fs-read
    :fs-write
    :fs-delete
    :fs-read-write
    :script-then-files
    :net-out
    :proc-signal
    :proc-spawn
    :env-read
    :stdout-emit
    :pure
    :shell-interpret
    :interp-interpret})

(def ^:private kinds-needing-dialect
  #{:shell-interpret :interp-interpret})

(def ^:private string-set?-fields
  "Spec keys whose value must be a (set of strings) — used by argv
   walkers in effects.clj that look up flag membership. Validating
   set-ness up front catches the common operator mistake of
   `[:-n :-N]` (keyword vector) instead of `#{\"-n\" \"--lines\"}`."
  #{:value-flags :path-value-flags
    :script-via-flags :script-file-flags
    :two-value-flags :inplace-flags})

(def ^:private valid-stream-modes
  "Recognized values for the `:stdin` / `:stdout` metadata fields. Not
   load-bearing on classification (the classifier emits effects, not
   stream descriptors), but constrained so operators can self-document
   their entries consistently with `default-registry`."
  #{:none :data :shell-source :interp-source :file-contents :passthrough})

;; ---- Validation --------------------------------------------------------

(defn- non-empty-string? [x]
  (and (string? x) (pos? (count x))))

(defn- ^:private set-of-strings? [x]
  (and (set? x) (every? string? x)))

(defn- ^:private validate-spec
  "Returns `nil` if the spec is well-formed, or a vector of error maps
   `[{:field <kw> :reason <kw> :detail <str>} …]`. Each error names
   the offending field so the operator can find and fix it. Errors
   accumulate — one malformed spec can surface multiple problems in
   one pass."
  [program spec]
  (let [errs (volatile! [])
        flag! (fn [field reason detail]
                (vswap! errs conj {:program program
                                   :field   field
                                   :reason  reason
                                   :detail  detail}))]
    (when-not (non-empty-string? program)
      (flag! :program :invalid-program-name
             "program name must be a non-empty string"))
    (when-not (map? spec)
      (flag! nil :spec-not-map
             (str "spec must be a map, got " (type spec))))
    (when (map? spec)
      (let [{:keys [doc kind rule dialect file-class
                    scope emit-stdout?]} spec]
        (when-not (non-empty-string? doc)
          (flag! :doc :missing-or-empty
                 "spec must include a non-empty :doc string"))
        (when-not (keyword? rule)
          (flag! :rule :missing-or-bad-type
                 "spec must include a :rule keyword (provenance tag)"))
        (cond
          (nil? kind)
          (flag! :kind :missing
                 (str "spec must declare :kind from " valid-kinds))

          (not (contains? valid-kinds kind))
          (flag! :kind :unknown
                 (str "unknown :kind " kind
                      " — valid kinds are " valid-kinds)))
        (when (and (contains? kinds-needing-dialect kind)
                   (not (or (keyword? dialect) (string? dialect))))
          (flag! :dialect :missing-or-bad-type
                 (str ":kind " kind " requires a :dialect (keyword or string)")))
        (when (and (some? scope) (not (string? scope)))
          (flag! :scope :bad-type ":scope must be a string when present"))
        (when (and (some? file-class) (not (keyword? file-class)))
          (flag! :file-class :bad-type ":file-class must be a keyword"))
        (when (and (some? emit-stdout?) (not (boolean? emit-stdout?)))
          (flag! :emit-stdout? :bad-type ":emit-stdout? must be a boolean"))
        (doseq [k string-set?-fields]
          (when-let [v (get spec k)]
            (when-not (set-of-strings? v)
              (flag! k :bad-type
                     (str (name k) " must be a set of strings, got " (type v))))))
        (doseq [k [:stdin :stdout]]
          (when-let [v (get spec k)]
            (when-not (contains? valid-stream-modes v)
              (flag! k :unknown
                     (str (name k) " must be one of " valid-stream-modes
                          " — got " v)))))))
    (when (seq @errs) @errs)))

;; ---- Instantiation -----------------------------------------------------

(defn- ^:private spec->classifier
  "Dispatch the `:kind` discriminator to one of effects.clj's public
   factory functions, threading the spec's per-kind config. Returns
   the classifier function `(fn [cmd ctx] [<effect-instance>])`.

   PRE: the spec has passed `validate-spec` — this fn does not
   re-check shapes."
  [{:keys [kind rule scope dialect file-class emit-stdout?
           value-flags path-value-flags
           script-via-flags script-file-flags
           two-value-flags inplace-flags]}]
  (case kind
    :fs-read
    (eff/classify-fs-read rule
                          (cond-> {}
                            value-flags      (assoc :value-flags value-flags)
                            path-value-flags (assoc :path-value-flags path-value-flags)))

    :stdin-or-fs-read
    (eff/classify-stdin-or-fs-read rule
                                   (cond-> {}
                                     value-flags (assoc :value-flags value-flags)))

    :fs-write        (eff/classify-fs-write rule)
    :fs-delete       (eff/classify-fs-delete rule)
    :fs-read-write   (eff/classify-fs-read-write rule)

    :script-then-files
    (eff/classify-script-then-files
     {:rule              rule
      :script-via-flags  (or script-via-flags #{})
      :script-file-flags (or script-file-flags #{})
      :value-flags       (or value-flags #{})
      :two-value-flags   (or two-value-flags #{})
      :inplace-flags     (or inplace-flags #{})
      :file-class        (or file-class :fs-read)
      :emit-stdout?      (boolean emit-stdout?)})

    :net-out      (eff/classify-net-out rule)
    :env-read     (eff/env-read-only rule)
    :stdout-emit  (eff/stdout-emit-only rule)
    :pure         (eff/pure rule)
    :proc-signal  (eff/proc-signal-classifier rule (or scope "?"))
    :proc-spawn   (eff/proc-spawn-classifier rule)

    :shell-interpret
    (eff/shell-interpret-classifier
     (if (keyword? dialect) dialect (keyword dialect))
     rule)

    :interp-interpret
    (eff/interp-interpret-classifier
     (if (keyword? dialect) dialect (keyword dialect))
     rule)))

(defn- ^:private instantiate-spec
  "Convert a validated operator-authored spec into the internal
   registry entry shape `{:doc :classify :stdin :stdout :kind :rule}`.
   The `:kind` and `:rule` are preserved so the doctor and any
   `classifier-overlay status`-style introspection can report what was
   loaded."
  [{:keys [doc kind rule stdin stdout] :as spec}]
  {:doc      doc
   :kind     kind
   :rule     rule
   :classify (spec->classifier spec)
   :stdin    (or stdin :data)
   :stdout   (or stdout :data)})

;; ---- Public parse / load -----------------------------------------------

(defn parse-overlay
  "Validate and instantiate an overlay map. Returns
   `{:specs <program → instantiated-spec>
     :overrides #{program …} ; programs that exist in default-registry
     :added     #{program …} ; programs new to the registry}`.

   Throws `ex-info {:reason :overlay-malformed :errors [<err> …]}`
   when any spec fails validation. The errors vector contains one map
   per problem (a single spec can yield multiple errors), each with
   `:program :field :reason :detail` so the operator can fix the
   offending entry by name."
  [data]
  (when-not (map? data)
    (throw (ex-info "overlay must be a top-level EDN map"
                    {:reason :overlay-malformed
                     :errors [{:program nil
                               :field   :root
                               :reason  :not-a-map
                               :detail  (str "expected a map, got " (type data))}]})))
  (let [errors (->> data
                    (mapcat (fn [[program spec]] (validate-spec program spec)))
                    vec)]
    (when (seq errors)
      (throw (ex-info "classifier-overlay validation failed"
                      {:reason :overlay-malformed
                       :errors errors}))))
  (let [specs (reduce-kv
               (fn [acc program spec]
                 (assoc acc program (instantiate-spec spec)))
               {}
               data)
        default-progs (set (keys eff/default-registry))
        loaded        (set (keys specs))]
    {:specs     specs
     :overrides (into (sorted-set) (filter default-progs loaded))
     :added     (into (sorted-set) (remove default-progs loaded))}))

(defn load-overlay-from-file!
  "Read an overlay EDN file from `path` and return the parsed-and-
   instantiated map (same shape as `parse-overlay`). Returns nil if
   the file does not exist — operators who haven't authored an overlay
   are the common case and shouldn't pay a boot-time error.

   Throws `ex-info {:reason :overlay-read-error}` if the file exists
   but cannot be read or parsed as EDN. Throws `ex-info
   {:reason :overlay-malformed}` if it parses but fails validation
   (propagated from `parse-overlay`)."
  [^String path]
  (let [f (io/file path)]
    (when (.exists f)
      (let [data (try (-> f slurp edn/read-string)
                      (catch Throwable t
                        (throw (ex-info "classifier-overlay file unreadable or not valid EDN"
                                        {:reason :overlay-read-error
                                         :path   path
                                         :message (.getMessage t)}
                                        t))))]
        (parse-overlay data)))))

;; ---- Installation seam -------------------------------------------------

(defn merge-registry
  "Merge `overlay-specs` over `default`. Overlay wins on conflict —
   operators can intentionally override the witness's default
   classifier for a program (e.g. tighten `git` to only allow specific
   subcommands, or weaken `rm` for a development sandbox). Overrides
   are loud at boot via the `install!` summary, not silent."
  [default overlay-specs]
  (merge default overlay-specs))

(defonce ^:private installed-state
  ;; Holds the most recent install summary so the doctor / introspection
  ;; can report what's loaded without re-reading from disk. nil before
  ;; the first install! call.
  (atom nil))

(defn install!
  "Top-level seam — read the overlay from `path`, validate +
   instantiate, alter-var-root the registry hook in effects.clj. Idempotent;
   calling twice with the same path performs a fresh load each time
   (operators editing the overlay file followed by a daemon restart is
   the supported reload path; in-process hot reload is intentionally
   not offered).

   Returns the install summary `{:added <#{…}> :overrides <#{…}>
   :path <path> :loaded? <bool>}`. The `:loaded?` flag is false when
   the file was absent — the daemon proceeds with the default registry
   unchanged.

   Throws if the file is malformed (see `load-overlay-from-file!`).
   Callers should treat that as a boot-time fail-closed condition."
  [^String path]
  (let [parsed (load-overlay-from-file! path)]
    (if parsed
      (let [{:keys [specs overrides added]} parsed
            merged (merge-registry eff/default-registry specs)
            summary {:added     added
                     :overrides overrides
                     :path      path
                     :loaded?   true}]
        (alter-var-root #'eff/*registry-override* (constantly merged))
        (reset! installed-state summary)
        summary)
      (let [summary {:added     #{}
                     :overrides #{}
                     :path      path
                     :loaded?   false}]
        (reset! installed-state summary)
        summary))))

(defn active-overlay-summary
  "Most recent install! summary, or nil if install! hasn't been called
   yet in this process. Used by the doctor's classifier-overlay check
   and by introspection."
  []
  @installed-state)

(defn format-summary
  "Human-readable single-line summary of an install! result. Used by
   the daemon boot log and by the doctor."
  [{:keys [added overrides loaded? path]}]
  (cond
    (not loaded?)
    (format "classifier-overlay: no file at %s — default registry only" path)

    (and (empty? added) (empty? overrides))
    (format "classifier-overlay: %s loaded (no entries)" path)

    :else
    (format "classifier-overlay: %s loaded — %d added (%s), %d overridden (%s)"
            path
            (count added)    (str/join " " added)
            (count overrides) (str/join " " overrides))))
