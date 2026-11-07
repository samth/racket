# CI-only Chez session key prototype

The automatic default change has been removed. Chez's configure, session-key
implementation, and BUILDING documentation match the upstream baseline again.
Normal builds therefore keep ordinary fresh session identities. The dependency
fixes and native .so/.wpo output declarations remain.

The replacement is an explicit CI opt-in: a pure-Zuo helper generates a seed from
Chez/build/PB inputs; a small Racket adapter forwards RACKET_CHEZ_SESSION_KEY to
Chez's existing option. A separate manual workflow tests the normal PB bootstrap
and desktop tests, with a seeded/default boolean control. See
[CI-BOOT-SESSION-KEY.md](CI-BOOT-SESSION-KEY.md).

The earlier automatic-default experiment's boot hash JSON files are retained as
historical evidence, not validation of the current policy. Current explicit-key
and default-control evidence is in `prototype-evidence/ci-session-key/`.
Dependency-audit regression evidence remains in
`prototype-evidence/dependency-fixes-bundled.json`.
