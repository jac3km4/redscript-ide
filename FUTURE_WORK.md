# Future work

Prioritized follow-ups from the LSP/code review. Keep fixes focused and add regression tests.

## Reliability

- Guarantee one response per request, including invalid parameters, unavailable documents and unknown methods.
- Terminate on transport disconnection; enforce shutdown/exit lifecycle rules.
- Stabilize file identities across creation/deletion, especially Windows canonical paths and missing parent directories.
- Treat missing source roots as temporarily empty with warnings; isolate discovery/read failures so healthy files keep updating.

## LSP correctness

- Scope fix-all to its originating workspace analysis or standalone document.
- Offer code actions for exact diagnostic ranges, diagnostic starts and overlapping selections.
- Preserve existing action properties during resolution; resolve only negotiated properties.
- Negotiate snippets, documentation formats and other client-dependent response features.
- Resolve identifier/type-name collisions using semantic binding information.
- Apply a consistent ignore policy to discovery and open-document overlays.

## Architecture and quality

- Move analysis off the protocol loop; coalesce edits, support cancellation and reject stale publications.
- Add protocol integration tests for request errors, disconnection, shutdown and capability negotiation.
- Preserve generic specialization and parameter qualifiers in hover/completion signatures.
- Pin an immutable compatible compiler revision for reproducible releases.
- Replace brittle diagnostic-message assertions with structured checks where possible.
