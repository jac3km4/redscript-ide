# Future work

Prioritized follow-ups from the LSP/code review. Keep fixes focused and add regression tests.

## Reliability

- Reconcile file identities across symlink creation/retargeting. Non-symlink creation/deletion
  and missing parents are handled; `dialect::identity` is now memoized and the cache is cleared
  on filesystem notifications, so symlink retargeting is observed after a refresh but not while
  an identity is cached.
- Isolate non-missing-root discovery/read failures so healthy files keep updating (missing roots
  are handled).

## Performance

- The base symbol table is now reused (see Completed), but each snapshot still clones it: the clone
  is proportional to the number of base type definitions (~3k `HashMap` entries, each with a
  `Box<[Shared<CtxVar>]>`/`Box<[&str]>`), which profiling has not yet shown to matter. Pooling the
  table would remove even that clone.

## LSP correctness

- Negotiate remaining client-dependent response features (for example, falling back to `Command`
  in `code_action` when `codeActionLiteralSupport` is absent; `workspaceEdit.documentChanges`,
  `disabled` code actions, and presentation capabilities are now negotiated).
- Preserve generic specialization in full hover/completion signatures. Receiver-bound instance
  methods, static methods with derivable type arguments, and generic field types are
  specialized; static-method completions without derivable type arguments still fall back to
  the declared signature.

## Architecture and quality

- Replace remaining brittle diagnostic-message assertions with structured checks where practical
  (protocol integration tests now assert on structured fields).

## Completed

- Reused the decoded base-game bundle and the whole base `Symbols` table across snapshots
  (`BaseEnv`), eliminating both the per-snapshot `ScriptBundle` decode and the ~30k-entry base
  symbol rebuild on every edit and speculative query. Each snapshot forks a disposable child
  `TypeInterner` from the shared base, so its own user-defined types and typos stay out of the
  base and off its lock, and `BaseEnv` now owns its bytes and interner in a self-referential
  value instead of leaking them. Enabled by the upstream forkable `TypeInterner` and covariant,
  copy-on-write `Symbols`.
- Lower only the function enclosing an IDE query in speculative query snapshots, skipping every
  unrelated function body and the whole-program validation passes.
- Cut member-completion latency: skip the wasted REDmod recovery parse for
  already-terminated statements, reuse the capability-adapted response across prefix
  edits, resolve documentation lazily through `completionItem/resolve`, poll
  cancellation while formatting members, abort a cancelled speculative query before
  parsing, and run queued interactive requests ahead of background diagnostics.
  Per-phase analysis timings log under `--features verbose-logs`, and the ignored
  `completion_latency_benchmark` test prints end-to-end latency.
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
