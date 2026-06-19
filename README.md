# shell-classify

> **Renamed from `shell-shape-classify` in v0.2.0.** The old GitHub
> URL auto-forwards; deps.edn coordinate is now
> `io.github.danieltanfh95/shell-classify` and the Clojure
> namespaces are `shell-classify.*`. v0.1.x history under the old
> name is preserved in `CHANGELOG.md`.

Per-program effect classification, parser-neutral. The reference
parser is [shell-shape](../shell-shape) (v0.2.0+ ships a small
adapter for it); other bash parsers translate their command shape
into a normalized-call and run the same registry.

```clojure
(require '[shell-shape.core :as ss])
(require '[shell-classify.classify :as cls])
(require '[shell-classify.effects :as eff])

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

shell-classify is the classifier registry, hoisted out of
[continuity-witness](../continuity-witness) in v0.29.0 (where it
shipped as `shell-shape-classify`) so non-witness consumers
(e.g. [muschel](../muschel)) can reuse it without pulling
biscuit-java + the witness daemon. v0.2.0 finished the
decoupling by making the registry parser-neutral and renaming
accordingly.

The witness keeps a thin shim at `continuity-witness.effects` that
re-exports this lib's surface and layers two witness-specific
additions: the agent-graph effect classes and the
`continuity-witness` self-classifier that closes the v0.24.0
self-mint hole.

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

v0.2.0 — **renamed from `shell-shape-classify` to `shell-classify`**;
per-program classifier registry decomplected from any specific
parse-tree shape. The registry now consumes a parser-neutral
*normalized-call* (`shell-classify.call`), so non-shell-shape
parsers (e.g. muschel's bash AST) can reuse the taxonomy by
translating their command shape into a normalized-call and calling
`effects/classify-call`. The tree walker (`classify.clj`) remains
shell-shape-coupled by design — that is how this lib earns its keep
inside a shell-shape pipeline. Breaking for consumers that
registered custom classifiers via `active-registry` swap
(mechanical migration; see `CHANGELOG.md [0.2.0]`).

v0.1.1 — publication-readiness cut. Git-coordinate pin against
shell-shape v0.8.0; lint config tracked; minor dead-code cleanup.
(Shipped under the old name `shell-shape-classify`.)

v0.1.0 — initial release. (Shipped under the old name
`shell-shape-classify`.)

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
