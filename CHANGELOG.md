# Changelog

All notable changes to this project are documented in this file.
The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] — 2026-06-17

**Initial release.** Hoisted out of
[continuity-witness](https://github.com/danieltanfh95/continuity-witness)
v0.29.0 so non-witness consumers can reuse the classifier registry
without pulling biscuit-java + the witness daemon. The witness
itself becomes a thin shim — `continuity-witness.effects` re-exports
this lib's surface and layers the agent-graph effect classes + the
`continuity-witness` self-classifier on top.

### Provided

- **`shell-shape-classify.effects`** — closed effect-class taxonomy
  (`:fs-read`, `:fs-write`, `:fs-delete`, `:fs-read-write`,
  `:net-out`, `:net-in`, `:proc-spawn`, `:proc-signal`, `:env-read`,
  `:env-mutate`, `:stdout-emit`, `:stdin-consume`, `:privilege-elevate`,
  `:shell-interpret`, `:interp-interpret`, `:opaque`), classifier
  factories (`classify-fs-read`, `classify-fs-write`,
  `classify-fs-delete`, `classify-fs-read-write`,
  `classify-stdin-or-fs-read`, `classify-script-then-files`,
  `classify-net-out`, `env-read-only`, `stdout-emit-only`, `pure`,
  `proc-signal-classifier`, `proc-spawn-classifier`,
  `shell-interpret-classifier`, `interp-interpret-classifier`), and
  `default-registry` covering ~60 common programs.
- **`shell-shape-classify.classify`** — `classify-tree` walks a
  parsed shell-shape command tree (chains, pipelines, subshells,
  wrappers, process-substitutions, embedded interpreter bodies)
  and emits the union of effect-instances. `effect-set` dedupes
  to `#{[class scope] ...}`.
- **`shell-shape-classify.overlay`** — operator-facing classifier
  overlay. Declarative EDN of `program-name → {:kind …}` validated
  at boot, merged into `active-registry` via the
  `*registry-override*` dynamic-var seam. Closed `:kind` set with
  no `eval`/no code-load path; fail-closed on unknown kinds or
  malformed dialect/value-flags shapes.
- **Per-language CST classifiers** — `python`, `node`, `ruby`,
  `perl` modules ship `tree-sitter`-backed CST → effect-set
  mappings consumed by the shell-side `:interp-interpret`
  composition rule.
- **Pure-data binding/env-resolve substrate** —
  `shell_shape_classify.bindings` (same-string env-binding
  resolution: `FOO=...; eval $FOO`),
  `shell_shape_classify.env_resolve` (allowlist + deny-pattern
  + hash-audit machinery for the env-read narrowing path),
  `shell_shape_classify.passthrough_payload` (echo/printf/cat
  content extraction for `> file.sh` writes).

### Common-tool entries (v0.1.0 additions over the v0.28.0 carve-out)

- **`pkill`** / **`pgrep`** — proc-signal-classifier / env-read-only
- **`df`** / **`du`** / **`free`** / **`uptime`** / **`whereis`** —
  env-read-only (du is `:fs-read` per starting path; the rest
  read kernel-maintained state, not filesystem contents)
- **`sleep`** / **`yes`** — pure / stdout-emit-only
- **`tee`** — stdin-consume + fs-write per positional (custom
  classifier; standard classify-fs-write only emits stdin-consume
  when literal `-` is in argv)
- **`gh`** — static `:net-out` to `api.github.com` (subcommand
  semantics aren't statically resolvable; overlay can refine)
- **`bb`** / **`clojure`** / **`clj`** — conservative `:proc-spawn`
  scoped on the program name (full Clojure-body classification
  would require an analyzer; out of scope)

### Promoted to public surface

- `arg-literals`, `option?`, `mk` — needed by the witness shim's
  self-classifier (and any consumer extending the registry with
  argv-introspecting classifiers).

### Tests

694 tests, 1414 assertions, 0 failures.

[0.1.0]: https://github.com/danieltanfh95/shell-shape-classify/releases/tag/v0.1.0
