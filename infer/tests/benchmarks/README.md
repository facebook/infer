# Pure calls and writes: performance follow-up

PR #2202 removes obsolete `ret = f(actuals)` equalities when a write can change
the result. Arithmetic facts about the *old value* remain valid, and entry-state
`f@pre` equalities remain necessary for callers. Removing either to save space
would change analysis results.

The forgetting patch does not bound summary size. `DeadVariables` retains the
whole arithmetic component connected to a live value; chained queries, mutations,
and arithmetic can therefore carry a new group of existential variables through
every caller. This includes comparisons, scaled differences, remainder, shifts,
and division. Aggregate database growth also includes call histories and should
not be mistaken for formula size alone.

## Reproduce

Build the original PR and its parent with identical compiler, frontend, and
optimization settings. This benchmark needs GNU `time` and `timeout`, Python 3,
and an Infer-compatible Clang. It captures each input once and serially runs both
analyzers against that capture. Binaries must use compatible database formats.

```sh
python3 infer/tests/benchmarks/pulse_pure_calls.py \
  --base /path/to/base/infer/bin/infer \
  --candidate /path/to/candidate/infer/bin/infer \
  --clang /path/to/clang \
  --output /path/to/new-results-directory \
  --depths 20 40 80 --timeout 30
```

The output includes generated C, commands, versions, binary SHA-256 digests, logs,
and JSONL measurements.
Times exclude capture and include analysis/reporting with `--pulse-only -j 1`.
The default single sample is a screening run; use `--repeats 3` for repeatability.
The runner alternates binary order on repeated runs. Exit 124 denotes a timeout,
not a completed measurement. The source shapes are independent reconstructions
of the review report, not the unavailable original generators.

`db_live_bytes` counts live SQLite pages including committed WAL contents;
`db_bytes` alone can omit WAL data or include free pages. `pulse_summary_bytes`
sums serialized Pulse payloads, which still include more than the formula.
Inspect individual formulas separately before claiming a per-disjunct bound.

`kdiv`, `kmod`, and `kmul` contain no unknown calls and are controls against
introducing regressions in unrelated code. `modr2` uses a non-const unknown call.

## Original-PR screening results (2026-10-04)

Compared original PR `cc8b6100cb` against its parent `d303f3f87a`, not against a
newer main or the review stack. Both binaries used OCaml 5.4.1, the `opt` profile,
and Clang 21.1.6 on the same host. No production-code changes were added to the
PR for these measurements. The unavailable six projection revisions were not
reconstructed or benchmarked.

All numbers below are user CPU seconds for analysis only, except timeout entries.
This is one sample per binary/shape/depth: 108 attempts, 106 completed, two timed
out. Small baseline timings are especially sensitive to rounding. These establish
regressions on the tested inputs; they do not establish a universal growth bound.

| Shape | Base at N=80 | PR N=20 | PR N=40 | PR N=80 |
| --- | ---: | ---: | ---: | ---: |
| eq | 0.19 | 0.10 | 0.30 | 0.92 |
| gt | 0.07 | 0.19 | 1.33 | 13.57 |
| gt2 | 0.08 | 0.59 | 2.68 | 14.37 |
| two | 0.26 | 1.29 | 6.87 | timeout (30 s wall) |
| ret | 0.08 | 0.32 | 2.01 | 14.00 |
| ret2x | 0.08 | 0.42 | 2.60 | 18.43 |
| r2d | 0.07 | 0.39 | 2.28 | 14.63 |
| m23 | 0.10 | 1.04 | 10.04 | timeout (30 s wall) |
| mod | 0.07 | 0.23 | 0.87 | 4.34 |
| shr | 0.07 | 0.23 | 0.87 | 4.34 |
| div | 0.08 | 0.16 | 0.62 | 2.32 |
| band | 0.08 | 0.18 | 0.65 | 3.32 |
| half | 0.08 | 0.23 | 1.48 | 14.36 |
| modr | 0.08 | 0.04 | 0.10 | 0.40 |
| modr2 | 0.19 | 0.05 | 0.08 | 0.19 |
| kdiv | 0.20 | 0.04 | 0.07 | 0.22 |
| kmod | 0.38 | 0.04 | 0.09 | 0.39 |
| kmul | 0.21 | 0.04 | 0.06 | 0.22 |

For example, `gt2` at N=80 used 494.9 MiB peak RSS and about 15.7 MiB of live
SQLite pages with the PR, versus 132.7 MiB RSS and 1.84 MiB of live pages on the
base. This storage includes histories and other summary data, not just formulas.
The no-call controls and the non-const `modr2` control stayed close to baseline.
These controls do not compensate for the large pure-call regressions above.

A three-run follow-up on `gt2` at N=80 gave median user CPU 0.08 s (base) versus
14.27 s (PR), with PR samples 14.28/14.11/14.27 s. A deeper no-call `kdiv`
control at N=320 took 8.50 versus 8.94 s (single samples); serialized Pulse
payloads were identical in size at 25,051,782 bytes. Thus the original patch
avoids the reported projection-induced explosion on that control, but still has
large and repeatable regressions on the pure-call chains.

## Implemented follow-up: conservative summary compaction

`PulseFormula.SummaryProjection` now runs only on C-family summaries retaining an
entry-state function application (`f@pre`). It leaves the forgetting mechanism
unchanged. The scope is deliberately limited:

- Substitute existing canonical linear definitions for non-kept, unrestricted
  variables. The canonical map has disjoint domains and ranges. The pass does
  not choose new pivots or introduce/reorient equations.
- Protect precondition and heap/return vocabulary, congruences, interval and type
  constraints, and variables in nonlinear terms or function applications.
- Preserve `IsInt(t, ikind)` with its **finite type range**, conditions with their
  depths, restricted-variable bounds, and divisibility. A map collision, capped
  coefficient overflow, or a binding that would need a new equation skips the
  substitution instead of dropping facts.
- For integer translates of the same affine expression, retain the original
  `IsInt` atoms giving the tightest lower and upper bounds. This represents the
  same intersection, including integrality; it does not widen the integer kind.
- Use one pass, a 4096-occurrence/fact input budget, at most 16 variables per
  substituted definition, a pre-expansion estimate, and a final check that the
  counted formula size has not increased. On rejection, retain the original
  constraints. There is no pairwise lattice search or fixpoint loop.

This is partial compaction, **not a summary-size bound or a guarantee of linear
analysis time**. The budget limits optimization work; larger formulas retain the
old behavior. Nonlinear relations and coupled integer constraints can still
keep existential variables alive. Exact projection of arbitrary formulas cannot
be assumed to have a small representation. A hard cap would require a separate,
explicit policy for precision or coverage; this patch does not implement one.

An experimental endpoint-witness elimination was removed before the final
version: on the two-query case at depth 40, its resulting inequalities made
analysis take about 17 seconds, although compaction itself took only 0.14
seconds. Retaining only canonical substitution took about 3.7 seconds. This is
why local expression-size checks alone are not treated as a bound on downstream
solver cost.

The new unit tests check caller import, finite signed and unsigned ranges,
intersection of different integer kinds, divisibility, and actual removal of a
canonical definition. The C tests also exercise bounded query results in callers.

## Follow-up validation (2026-10-04)

The final compaction was screened on 24 reconstructed shapes at depths 40, 80,
and 160 against the PR parent, using matching optimized builds. Of 144 analysis
attempts, 136 finished and eight candidate runs reached the 30-second wall limit.
Those eight were `two`, `ret`, `ret2x`, `ret3x`, `r2d`, `r2c`, `r6`, and `m23`,
all at depth 160. The table below compares the original-PR screen above with this
follow-up screen; these are separate single runs, not paired medians.

| Shape, N=80 | Original PR user s | With compaction user s |
| --- | ---: | ---: |
| eq | 0.92 | 0.41 |
| gt | 13.57 | 0.57 |
| gt2 | 14.37 | 2.55 |
| two | timeout (30 s wall) | 27.08 |
| ret | 14.00 | 3.22 |
| ret2x | 18.43 | 3.01 |
| r2d | 14.63 | 3.61 |
| m23 | timeout (30 s wall) | 15.42 |
| mod | 4.34 | 2.38 |
| shr | 4.34 | 2.41 |
| div | 2.32 | 0.96 |
| band | 3.32 | 1.28 |
| half | 14.36 | 0.81 |
| modr | 0.40 | 0.56 |
| modr2 | 0.19 | 0.19 |
| kdiv | 0.22 | 0.21 |
| kmod | 0.39 | 0.37 |
| kmul | 0.22 | 0.21 |

The modest regression on `modr` is included deliberately: preserving semantics
and reducing counted summary size do not guarantee faster downstream analysis.
The expensive query chains remain much slower than the unsound parent, which
incorrectly equates queries across writes and prunes feasible branches.

The no-query controls (`kdiv`, `kmod`, `kmul`) at depth 160 finished in
1.17/2.76/1.18 user seconds respectively. Mixed controls additionally query and
write an object before the arithmetic chain, exercising the optimization gate
in the presence of nonlinear keys. Their depth-160 times were 1.47/0.65/1.36
seconds. These finite measurements do not establish an asymptotic guarantee.

A paired before/after run of all 24 shapes at depth 40 completed without
timeouts: total user CPU across this artificial set was 41.50 seconds for the
original PR and 13.25 seconds with compaction. A separate three-run paired test
of `ret2x` at depth 80 gave median user CPU **18.47 → 3.07 seconds** and median
peak RSS **670.1 → 343.6 MiB**. Original CPU samples were 18.44/18.47/18.78;
compaction samples were 3.07/3.11/3.07. These runs alternate analyzer order and
reuse the same capture.

Paired mixed-control results expose both benefits and costs. At depth 80,
`mixed_kdiv` took 0.23 → 0.49 seconds and `mixed_kmul` 0.22 → 0.49 seconds.
At depth 320:

| Shape | Original PR user s | Compaction user s | Peak RSS change |
| --- | ---: | ---: | ---: |
| modr | 18.18 | 12.34 | +18% |
| mixed_kdiv | 8.95 | 6.55 | +27% |
| mixed_kmod | 29.28 | 2.14 | −43% |
| mixed_kmul | 8.95 | 6.36 | +25% |

In particular, local formula compaction can increase aggregate serialized
summary size and peak RSS after caller processing. Do not claim a global
no-growth property from the local cost check. The arithmetic-only controls are
excluded by the entry-state-call gate; the mixed controls exercise that gate.

Real-code comparison: zlib 1.3.1, commit
`51b7f2abdade71cd9bb0e7a373ef2610ec6f9daf`, static `-O0` capture with 227
procedures. All three optimized analyzers reused that capture, with one warmup
and three measured runs each, rotating analyzer order. Analysis used
`--pulse-only -j 1`:

| Version | Median user+system CPU | Median peak RSS |
| --- | ---: | ---: |
| PR parent | 8.34 s | 555.0 MiB |
| Original PR | 8.40 s | 541.0 MiB |
| With compaction | 8.40 s | 548.7 MiB |

All twelve runs reported the same one issue, matching file, procedure, line,
issue type, and qualifier. This supports neutrality on **this project**, not a
claim about a general corpus or every C++ workload.

Final native `opt` build, OCaml `@runtest`, c/pulse, cpp/pulse, and cpp/pulse-11
passed. The C/C++ expectations add four distinct reports from the new files
(including the documented field-write false positive); existing reports and
traces are unchanged. ObjC (which needs the Xcode SDK) and the multi-PR integration stack remain
untested. These results support keeping the soundness fix and this conservative
compaction for review, with the documented performance exception; they do not
satisfy a universal summary-size-bound landing condition.

## Other limitations and integration checks

- Invalidation follows whole reachable objects. An unrelated field write can
  decorrelate queries. `FP_correlated_after_field_write_ok` records that case;
  a non-const unknown mutation is tested separately as a true bug.
- Hidden global dependencies are not represented by pointer actuals. The three
  `FN_*` cases in `pure_calls_and_writes.c` record zero-argument queries across
  direct writes, callee writes, and an unknown mutator in a drain loop. A future
  global/effect model must account for entry-state queries too.
- The review's requested integration measurements with #2158, #2176, #2178, and
  #2197 and the #2202-before-#2177 landing order still apply. The isolated tests
  here do not validate that stack or #2177's composition changes.
