This checker detects opportunities to hoist function calls that are invariant to outside of loop bodies. The hoisting analysis relies on [purity](/docs/next/checker-purity) analysis to determine whether a function is pure or not.

It has an additional mode that reports [loop-invariant functions that are expensive](/docs/next/all-issue-types#expensive_loop_invariant_call) (i.e. at least linear). This is enabled by the flag `--hoisting-report-only-expensive`.
