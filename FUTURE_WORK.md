# Future work

Prioritized follow-ups from the LSP/code review. Keep fixes focused and add regression tests.

## Reliability

- Reconcile file identities across symlink creation/retargeting. Non-symlink creation/deletion
  and missing parents are handled; `dialect::identity` is now memoized and the cache is cleared
  on filesystem notifications, so symlink retargeting is observed after a refresh but not while
  an identity is cached.
- Isolate non-missing-root discovery/read failures so healthy files keep updating (missing roots
  are handled).

## LSP correctness

- Negotiate remaining client-dependent response features (for example, workspace edit document
  changes and disabled code actions).
- Preserve generic specialization in full hover/completion signatures. Receiver-bound instance
  methods, static methods with derivable type arguments, and generic field types are
  specialized; static-method completions without derivable type arguments still fall back to
  the declared signature.

## Architecture and quality

- Replace remaining brittle diagnostic-message assertions with structured checks where practical
  (protocol integration tests now assert on structured fields).

## Completed

- Moved analysis off the protocol loop: workspace parsing/lowering/diagnostics now run on a
  dedicated worker thread (enabled by the upstream `redscript` `sync`/`parallel` features), so the
  protocol loop never blocks. `$/cancelRequest` cancels in-flight and queued requests cooperatively,
  and diagnostics computed for a superseded document state are rejected before publication.
- Unified diagnostic state into a single `DiagnosticTracker`; removed the fragmented
  per-category published sets and the associated `RefCell` fields.
- Scoped fix-all code actions to their originating workspace or standalone document, and made fix
  validity content-based so unrelated edits no longer invalidate cached fixes.
- Resolved identifier/type-name collisions using semantic binding information.
- Applied a consistent ignore policy to discovery and open-document overlays.
- Split the language server module into cohesive units (`analysis`, `code_actions`, `workspace`,
  expanded `diagnostics`/`completions`).
- Honored the client-provided formatting tab size when `.redscript` does not configure one.
- Extended protocol integration coverage to capability negotiation and dynamic watcher
  registration, and pinned immutable compiler revisions for reproducible releases.
