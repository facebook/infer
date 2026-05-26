# RETAIN_CYCLE_UNACTIONABLE

Internal companion to `RETAIN_CYCLE`, disabled by default. Raised when
the retain-cycle report's headline location resolves to a
`<bitcode_id>:compiler-generated` sentinel sourcefile — i.e. the cycle
was detected inside a compiler-generated procedure (Swift partial-apply
forwarder, autoclosure body, async continuation thunk, witness-table
accessor, overlay initializer, or ObjC bridging thunk) with no user
source file to anchor the report.

The cycle itself is real; the report just has no actionable file:line
for a product engineer. Disabled by default so production drops these.
Re-enable with `--enable-issue-type RETAIN_CYCLE_UNACTIONABLE` for
debugging or to measure prevalence in Scuba.
