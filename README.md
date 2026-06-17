# shell-shape-classify

Per-program effect classification on top of
[shell-shape](../shell-shape) trees. Walks the parsed command tree
and emits coordinate-rich `:class :scope` effect-instance records
that downstream policy can predicate on:

```clojure
(require '[shell-shape.core :as ss])
(require '[shell-shape-classify.classify :as cls])
(require '[shell-shape-classify.effects :as eff])

(cls/classify-tree
 {:registry eff/default-registry}
 (ss/parse "find . | xargs rm /tmp/x"))
;; => ({:class :fs-read   :scope "."      :provenance {:rule :find ...}}
;;     {:class :fs-delete :scope "/tmp/x" :provenance {:rule :rm   ...}})
```

The effect-class taxonomy (`:fs-read`, `:fs-write`, `:fs-delete`,
`:net-out`, `:proc-spawn`, `:env-mutate`, `:env-read`,
`:stdout-emit`, `:stdin-consume`, `:proc-signal`, `:opaque`, …)
covers what a shell program can do *intrinsically* — what
`fs-delete /tmp/x` does is the same regardless of who calls it.
Agent-graph effects (`:mcp-call`, `:skill-invoke`, …) are added
at the consumer layer; this lib stays scoped to shell programs.

## Why split this out from continuity-witness

shell-shape-classify is the classifier registry, hoisted out of
[continuity-witness](../continuity-witness) in v0.29.0 of the
witness so non-witness consumers (e.g. [muschel](../muschel)) can
reuse it without pulling biscuit-java + the witness daemon.

The witness keeps a thin shim at `continuity-witness.effects` that
re-exports this lib's surface and layers two witness-specific
additions: the agent-graph effect classes and the
`continuity-witness` self-classifier that closes the v0.24.0
self-mint hole.

## Surface

- `shell-shape-classify.effects/default-registry` — `program-name →
  {:doc :classify :stdin :stdout}` map; the canonical
  authoritative set of program classifiers.
- `shell-shape-classify.effects/active-registry` — returns the
  default-registry, or the overlay-merged registry if
  `shell-shape-classify.overlay/install!` has been called.
- `shell-shape-classify.classify/classify-tree` — walks the
  shell-shape parse tree, descending through subshells / pipelines
  / wrappers / process-substitutions, applying each program's
  classifier from the registry, and returning the union of
  effect-instances.
- `shell-shape-classify.classify/effect-set` — dedupes effect-
  instances to `#{[class scope] ...}` for policy comparison.
- `shell-shape-classify.overlay` — operator-facing classifier
  overlay; declarative EDN of `program-name → {:kind …}` validated
  at boot, merged into the active-registry via a dynamic-var seam.
  Closed `:kind` set (no `eval`, no code-load path) so the overlay
  can't compromise the substrate.

## Status

v0.1.0 — initial release.

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
