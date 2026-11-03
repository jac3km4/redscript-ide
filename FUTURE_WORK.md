# Future work

Prioritized follow-ups from the LSP/code review. Keep fixes focused and add regression tests.

## Reliability


- Reconcile file identities across symlink creation/retargeting (ordinary creation/deletion and missing parents are handled).
- Isolate non-missing-root discovery/read failures so healthy files keep updating (missing roots are handled).

## LSP correctness

- Scope fix-all to its originating workspace analysis or standalone document.
- Offer code actions for exact diagnostic ranges, diagnostic starts and overlapping selections.
- Preserve existing action properties during resolution; resolve only negotiated properties.
- Negotiate snippets, documentation formats and other client-dependent response features.
- Resolve identifier/type-name collisions using semantic binding information.
- Apply a consistent ignore policy to discovery and open-document overlays.

## Architecture and quality

- Move analysis off the protocol loop; coalesce edits, support cancellation and reject stale publications.
- Extend protocol integration coverage to capability negotiation and dynamic registration.
- Preserve generic specialization and parameter qualifiers in hover/completion signatures.
- Pin an immutable compatible compiler revision for reproducible releases.
- Replace brittle diagnostic-message assertions with structured checks where possible.
