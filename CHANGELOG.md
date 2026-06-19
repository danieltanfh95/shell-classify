# Changelog

All notable changes to this project are documented in this file.
The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.1] — 2026-06-19

**Fix: `xargs` own-effect closes the stdin-fed-argv hole.** The
`xargs` registry entry shipped as `(wrapper-classifier nil)` — no
own-effects, transparent delegation to the wrapped command.
Operationally that's wrong: xargs reads its actual operands from
**stdin** and substitutes them into the wrapped argv at run-time,
so the static argv the classifier sees (e.g. `xargs sudo rm` shows
`rm` with no path operands) systematically under-represents what
will execute. Witness's nested-wrapper-corpus had 5 failing entries
documenting this — every case with `xargs` in the wrapper chain
silently dropped the indeterminacy marker.

Adds `xargs-own-effects` emitting:

```
{:class :opaque
 :scope "xargs-stdin-fed-argv"
 :provenance {:rule :xargs :program "xargs"}}
```

The inner-command family signal still flows through
`effects-of-invokes` — `xargs rm` continues to emit
`[:fs-delete "?"]` from v0.24.3's design (informative family
classification with unknown scope). The new own-effect is
**additional**, not replacing: both must be present, and granting
either alone is insufficient to clear the gate. Closes the
`fs-delete:**` over-approximation hole where a broad delete grant
would otherwise pass `xargs sudo rm` despite stdin-fed targets.

### Changed

- **`src/shell_classify/effects.clj`** — adds `xargs-own-effects`;
  `xargs` registry entry switches from `(wrapper-classifier nil)` to
  `(wrapper-classifier xargs-own-effects)`. No other wrapper entry
  affected.
- **`test/shell_classify/classify_test.clj`** — `xargs-delegates-
  to-wrapped-family` updated: was `(is (not (has-class? "xargs rm"
  :opaque)))` reflecting the pre-fix design; now asserts both the
  family signal AND the new `:opaque "xargs-stdin-fed-argv"` own-
  effect. Docstring/rationale updated.

### Policy impact

This is a security-positive widening: the substrate previously
under-approximated xargs's effect surface, so policies that gated
on `:fs-delete` (or any inner family) cleared `xargs <cmd>` even
though the actual operands were stdin-fed. After v0.2.1, policies
need to explicitly grant `opaque:xargs-stdin-fed-argv` (or grant
the broad `:opaque` axis) in addition to the inner family scope.
Standard witness policy templates do NOT grant
`opaque:xargs-stdin-fed-argv`, so xargs invocations now defer by
default — operator must approve out-of-band. Pre-v0.2.1 policies
relying on the under-approximation should explicitly add the new
scope if they want to preserve the prior auto-allow behavior; the
recommended path is to leave the new default in place.

## [0.2.0] — 2026-06-19

**Decomplection + rename. Two coordinated breaking changes shipping
together:**

1. **Per-program classifier registry decomplected from any specific
   parse-tree shape.** Closes the structural complecting where the
   program-classifier API took a `shell-shape` `:command` node
   directly, forcing any consumer to either use shell-shape as their
   parser OR fork the lib to retarget the registry against their own
   parse-tree shape.
2. **Renamed `shell-shape-classify` → `shell-classify`.** The
   `shell-shape-` prefix described the lib's parser dep, but post-
   decomplection the registry is parser-neutral; the prefix was
   actively misleading about what the lib couples to. GitHub repo
   renamed to [`danieltanfh95/shell-classify`](https://github.com/danieltanfh95/shell-classify);
   the old URL auto-forwards. The tools.deps coordinate is now
   `io.github.danieltanfh95/shell-classify`. All Clojure namespaces
   move from `shell-shape-classify.*` to `shell-classify.*`. v0.1.x
   history below remains under the original name — it's where the lib
   shipped from.

v0.2.0 introduces `shell-classify.call` — a small, parser-neutral
*normalized-call* shape — and routes the registry through it. The
shape is documented in detail in the `call` ns docstring; the short
form is `{:program <str> :argv [<arg>] :assigns [...] :redirs [...]
:invokes [...] :program-sources [...] :raw <opaque>}`, with the per-
arg `:kind :token / :process-sub` and per-part `:kind :literal /
:var / :subst / :backtick` shape unchanged from shell-shape (so per-
language and per-token semantics carry over without translation).

Architecturally:

- **ssc's tree walker (`classify.clj`) stays shell-shape-coupled.**
  It still walks `:script` / `:pipeline` / `:command` / `:group` /
  `:program-sources` natively — that's how ssc earns its keep
  inside a shell-shape pipeline.
- **The per-program classifier registry is parser-neutral.**
  `classify-fs-read`, `classify-net-out`, `classify-script-then-
  files`, `ssh-classifier`, `argv-shape-classifier` (used for git/
  tar getopt-shape discrimination), etc. now receive a normalized-
  call rather than a shell-shape `:command`. Every helper they read
  (`arg-literals`, `non-option-positional-literals`, `option?`,
  `host-from-url`, `net-targets-from-args`, `literal-token?`,
  `token-literal-value`) moved to `shell-classify.call`.

Two entry points are exposed:

- `effects/classify-call` — the parser-neutral dispatch:
  `(registry, normalized-call, ctx) → effects`. New consumers
  (muschel, other bash parsers) call this with a normalized-call
  they constructed via their own adapter.
- `effects/classify-command` — the shell-shape shim, preserved for
  backward compatibility. Internally translates `:command` to a
  normalized-call via `call/from-shell-shape-command` and delegates
  to `classify-call`. ssc's walker uses this seam directly.

**Adapter contract.** Non-shell-shape parsers (e.g. muschel's bash
AST) write their own translator that produces a normalized-call.
Universal classifiers (`rm`, `mv`, `cp`, `curl`, `wget`, `grep`,
`sed`, `awk`, `jq`, `git`, `tar`, `ls`, …) run unchanged — they
only read `:program` / `:argv` / `:assigns`. A small coupled
minority (ssh / trap body-recursion, find expression scanning,
source / shell-interpret) reaches back through `:raw` for shape-
specific access; muschel can either populate `:raw` with a shell-
shape-compatible bag for the few coupling spots or curate their
registry to exclude those classifiers.

### Breaking

- **Program-classifier function signature** changed from
  `(fn [shell-shape-command ctx] → effects)` to
  `(fn [normalized-call ctx] → effects)`. Consumers that registered
  custom classifiers via `eff/active-registry` swap (operator
  overlay, witness self-classifier) update their fns to read
  `:argv` instead of `:args` and use `shell-classify.call/*`
  helpers in place of the previously private (now removed) helpers
  on `effects`. The migration is mechanical — see
  `continuity-witness v0.30.0`'s `effects.clj` for the canonical
  example.
- **Removed from `shell-classify.effects`** (moved to
  `shell-classify.call`): `arg-literals`, `option?`,
  `literal-token?`, `token-literal-value`, `non-option-positional-
  literals`, `host-from-url`, `net-targets-from-args`,
  `arg-literal-scope`, `process-sub-fd-scope`. The public surface
  on `effects` shrinks to: the effect-class taxonomy, classify-*
  factories, `default-registry`, `*registry-override*`,
  `active-registry`, `classify-call`, `classify-command` (shim),
  `mk` (still the canonical effect-instance constructor; the
  `cmd`-arg now expects a normalized-call's `:program`).

### Added

- **`shell-classify.call`** — normalized-call shape + helpers. The
  ns docstring is the contract muschel-and-friends read to write
  their adapter. `from-shell-shape-command` translates a shell-shape
  `:command` to a normalized-call; external parsers write their own.
- **`shell-classify.effects/classify-call`** — parser-neutral entry
  point. New consumers use this.

### Changed

- **`shell-classify.effects`** — every classify-* factory's inner
  fn now consumes the normalized-call shape via `call/` helpers.
  The factory signatures, the registry-spec map shape, and the
  emitted effect-instances are unchanged.
- **`shell-classify.classify`** — `classify-command-resolved`
  builds the normalized-call once at the dispatch site (via
  `call/from-shell-shape-command`) and delegates to
  `effects/classify-call`. The walker's shell-shape coupling is
  unchanged; only the per-command dispatch is parser-neutral.

### Migration

A consumer that registered `(fn [cmd ctx] (when (= "myprog"
(:program cmd)) [(eff/mk :fs-read (first (eff/arg-literals cmd))
:my-prog cmd)]))` updates to
`(fn [norm-call ctx] (when (= "myprog" (:program norm-call))
[(eff/mk :fs-read (first (call/arg-literals norm-call)) :my-prog
norm-call)]))` — three callsite changes: `eff/arg-literals` →
`call/arg-literals`, `(:args cmd)` → `(:argv norm-call)`
(if it was reading `:args` directly), and the `cmd` param rename
is cosmetic. See `continuity-witness/src/continuity_witness/
effects.clj` post-v0.30.0 for a real-world before/after.

### Tests

- **694 tests, 1414 assertions, 0 failures.** Overlay tests now
  exercise classifiers through a normalized-call constructed via
  `call/from-shell-shape-command` (4 tests refit to the new
  signature, no semantic change). All other suites (built-in
  registry, classify-tree pipeline, per-language classifiers,
  property tests, adversarial corpora) pass unchanged.

## [0.1.1] — 2026-06-19

**Publication-readiness cut.** Switches the upstream `shell-shape`
dep from `:local/root "../shell-shape"` to a git-coordinate pin
against the freshly-cut [v0.8.0](https://github.com/danieltanfh95/shell-shape/releases/tag/v0.8.0)
so the lib resolves cleanly for downstream consumers without a
sister checkout. Bundles four dead-code lint cleanups in `src/`
(no behavior change) and tooling tidy. No public API change.

### Changed

- **`deps.edn`**: `io.github.danieltanfh95/shell-shape`
  `:local/root` → `:git/url "https://github.com/danieltanfh95/shell-shape" :git/tag "v0.8.0" :git/sha "5cfff3e"`.
  Picks up shell-shape v0.8.0's spawned-commands walker descent —
  cross-dialect interpreter spawns (`subprocess.run(["git","push"])`,
  Node `execFileSync`, Ruby `Process.spawn`, Perl `system`
  list-form) now surface their inner commands to `ss/roots`, which
  the ssc classifier walks for the standard descent.

### Tooling

- **`.clj-kondo/config.edn` tracked** — matches the sibling
  shell-shape + continuity-witness configs. `bb lint` is
  0 errors / 0 warnings. Includes a one-time
  `:unresolved-namespace [shell-shape.core]` exclusion that
  suppresses noise on first-time `bb lint` before `clj -Spath`
  populates the gitlib cache.
- **`.gitignore`** — adds `.replsh/` + `.succession/`; narrows
  `.clj-kondo/` to `.clj-kondo/.cache/` so the new config tracks.
- **Dead-code cleanups in `src/`**:
  - `classifiers/perl.clj` — drops an unused `path` binding in
    the open-mode branch (was shadowed by `path-2arg`/`path-3arg`).
  - `classify.clj` — deletes the unused `classify-stage` wrapper
    fn (callers already used the underlying `classify-pipeline`).
  - `effects.clj` — deletes the backwards-compat `path-args`
    accessor (no in-repo callers; pre-v0.1.0 shim).
  - `overlay.clj` — drops `stdin` / `stdout` from the spec
    destructure since validation reads them via `(get spec k)`.

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
