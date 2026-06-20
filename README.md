# shell-classify

Parsed shell commands → effects. Parser-neutral: the registry
consumes a normalized-call shape, so any bash parser plugs in via a
small adapter. [shell-shape](../shell-shape) is the reference
parser.

```clojure
(require '[shell-shape.core :as ss])
(require '[shell-classify.classify :as cls])
(require '[shell-classify.effects :as eff])

(cls/classify-tree
 {:registry eff/default-registry}
 (ss/parse "find . | xargs rm /tmp/x"))
;; => ({:class :fs-read   :scope "."                    :provenance {:rule :find  ...}}
;;     {:class :fs-delete :scope "/tmp/x"               :provenance {:rule :rm    ...}}
;;     {:class :opaque    :scope "xargs-stdin-fed-argv" :provenance {:rule :xargs ...}})
;;
;; The :opaque marker (v0.2.1) records that xargs's actual operands
;; come from stdin, not the static argv — policies that grant
;; :fs-delete:** alone won't clear this; the operator must also
;; grant opaque:xargs-stdin-fed-argv (or defer).
```

The effect-class taxonomy (`:fs-read`, `:fs-write`, `:fs-delete`,
`:net-out`, `:proc-spawn`, `:env-mutate`, `:env-read`,
`:stdout-emit`, `:stdin-consume`, `:proc-signal`, `:opaque`, …)
covers what a shell program can do *intrinsically* — what
`fs-delete /tmp/x` does is the same regardless of who calls it.
Agent-graph effects (`:mcp-call`, `:skill-invoke`, …) are added
at the consumer layer; this lib stays scoped to shell programs.

## Relationship to continuity-witness

shell-classify is the parser-neutral classifier registry. It's
used by [continuity-witness](../continuity-witness) and reusable
by non-witness consumers (e.g. [muschel](../muschel)) without
pulling biscuit-java + the witness daemon.

The witness keeps a thin shim at `continuity-witness.effects`
that re-exports this lib's surface and layers two witness-
specific additions: the agent-graph effect classes and the
`continuity-witness` self-classifier (closes the self-mint hole
on the witness's own CLI).

## Surface

- `shell-classify.effects/default-registry` — `program-name →
  {:doc :classify :stdin :stdout}` map; the canonical authoritative
  set of program classifiers.
- `shell-classify.effects/active-registry` — returns the default-
  registry, or the overlay-merged registry if
  `shell-classify.overlay/install!` has been called.
- `shell-classify.effects/classify-call` — apply the registry to a
  parser-neutral *normalized-call* (see `shell-classify.call`).
  Non-shell-shape parsers (e.g. muschel) use this directly.
- `shell-classify.effects/classify-command` — the shell-shape shim;
  translates a shell-shape `:command` into a normalized-call and
  delegates to `classify-call`. ssc's walker uses this seam.
- `shell-classify.call` — the normalized-call shape + helpers. The
  ns docstring is the contract external parsers read to write
  their adapter.
- `shell-classify.classify/classify-tree` — walks the shell-shape
  parse tree, descending through subshells / pipelines / wrappers /
  process-substitutions, applying each program's classifier from
  the registry, and returning the union of effect-instances.
- `shell-classify.classify/effect-set` — dedupes effect-instances
  to `#{[class scope] ...}` for policy comparison.
- `shell-classify.overlay` — operator-facing classifier overlay;
  declarative EDN of `program-name → {:kind …}` validated at boot,
  merged into the active-registry via a dynamic-var seam. Closed
  `:kind` set (no `eval`, no code-load path) so the overlay can't
  compromise the substrate.

## Status

v0.2.1. See [`CHANGELOG.md`](CHANGELOG.md) for per-version detail.

The classifier substrate. The taxonomy is closed (the
`:effect-classes` set). Adding a new program follows one of:

1. The operator overlay (no source patch) — pick a `:kind` from
   the closed set and ship an EDN entry; works for shapes already
   modelled by an existing factory (`:fs-read`, `:fs-write`,
   `:fs-delete`, `:fs-read-write`, `:stdin-or-fs-read`,
   `:script-then-files`, `:net-out`, `:env-read`, `:stdout-emit`,
   `:pure`, `:proc-signal`, `:proc-spawn`, `:shell-interpret`,
   `:interp-interpret`).
2. A patch to `default-registry` — for programs whose argv
   discrimination needs a new factory (e.g. `git` /
   `tar` use `argv-shape-classifier` with a per-program shape
   table).

## Tests

```
bb test:unit       # standard suite
bb test:all        # + adversarial gauntlet
```

## License

MIT — see [LICENSE](LICENSE).
