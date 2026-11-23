# Future work

Prioritized follow-ups from the LSP/code review. Keep fixes focused and add regression tests.

## Reliability

- Reconcile file identities across symlink creation/retargeting. Non-symlink creation/deletion
  and missing parents are handled; `dialect::identity` is memoized and the cache is cleared
  on filesystem notifications, so symlink retargeting is observed after a refresh but not while
  an identity is cached.

## Performance

- The base symbol table is reused across snapshots, but each snapshot still clones it: the clone
  is proportional to the number of base type definitions (~3k `HashMap` entries, each with a
  `Box<[Shared<CtxVar>]>`/`Box<[&str]>`), which profiling has not yet shown to matter. Pooling the
  table would remove even that clone.

## LSP correctness

- Fall back to `Command` in `code_action` when `codeActionLiteralSupport` is absent.
  `workspaceEdit.documentChanges`, `disabled` code actions, and presentation capabilities are
  already negotiated.
- Specialize static-method completions whose type arguments are not derivable, instead of
  falling back to the declared signature. Receiver-bound instance methods, static methods with
  derivable type arguments, and generic field types are already specialized.

## Architecture and quality

- Replace remaining brittle diagnostic-message assertions with structured checks where practical.
