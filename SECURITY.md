# Security policy

## Reporting a vulnerability

Email **danieltanfh95@gmail.com** with the subject prefix
`[shell-classify security]`. Please do not open a public GitHub
issue for a suspected vulnerability.

Include:

- a description of the failure mode,
- a parse input that reproduces it (and the parser used, if not
  `shell-shape`),
- the shell-classify version you ran against,
- and, if you have one, your assessment of severity.

Acknowledgement within 72 hours. Coordinated disclosure timing is
negotiable.

## Scope

In scope:

- **Effect-set under-approximation.** A program invocation whose
  classifier emits a strictly smaller `(class, scope)` set than what
  the program actually does. The canonical failure mode is a
  registered classifier missing an effect that lets a grant
  authorizing the visible effects accidentally cover an unauthorized
  side effect (e.g., `xargs stdin-fed-argv`, dynamic-eval reductions
  that leak past the `:opaque` gate).
- **Scope-string injection.** A classifier emitting a scope string
  derived from attacker-controlled input without the appropriate
  prefix discriminator (e.g., losing the `ssh:<host>:` prefix on a
  remote-shell effect).
- **Registry-mutation routes.** A code path that mutates the global
  registry (`active-registry`) from attacker-controlled input.

Out of scope:

- Parser bugs upstream of the classifier — those belong to
  [shell-shape](https://github.com/danieltanfh95/shell-shape) or
  whichever parser the consumer chose.
- Policy-decision logic — the classifier emits effects, the consumer
  decides; policy-side gaps belong to the consumer (e.g.,
  [continuity-witness](https://github.com/danieltanfh95/continuity-witness)).

## Consumers

shell-classify is a parser-neutral classifier registry. The
downstream consumer that wires it into a signed-grant decision
substrate is
[continuity-witness](https://github.com/danieltanfh95/continuity-witness);
end-to-end vulnerability classes (forged grants, signed-ledger
tampering, replay) are tracked there.
