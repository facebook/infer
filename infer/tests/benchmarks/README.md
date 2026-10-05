# Pure calls and writes: performance notes

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

The summary compaction described below is a constant-factor optimisation on some
of these shapes. It is **not a summary-size bound**: on the shapes it helps, time
still grows superlinearly with depth, and on several shapes it gains almost
nothing.

## Reproduce

Build two analyzers with identical compiler, frontend, and optimization settings,
for example the PR and its parent, or the PR and the forgetting-only commit
`cc8b6100cb`. This benchmark needs GNU `time` and `timeout`, Python 3, and an
Infer-compatible Clang. It captures each input once and serially runs both
analyzers against that capture. Binaries must use compatible database formats.

```sh
python3 infer/tests/benchmarks/pulse_pure_calls.py \
  --base /path/to/base/infer/bin/infer \
  --candidate /path/to/candidate/infer/bin/infer \
  --clang /path/to/clang \
  --output /path/to/new-results-directory \
  --depths 20 40 80 --timeout 30 --repeats 3
```

The output includes generated C, commands, versions, binary SHA-256 digests, logs,
and JSONL measurements.
Times exclude capture and include analysis/reporting with `--pulse-only -j 1`.
A single sample (the default) is a screening run; use `--repeats 3` for
repeatability. The runner alternates binary order on repeated runs. Exit 124
denotes a timeout, not a completed measurement.

Each pure-call shape is a chain of N functions. Level `i` queries `q(s)`, an unknown
function taking `const struct S*`, calls level `i+1`, writes `s->x`, queries `q(s)`
again, and combines the two results as listed in `TAILS` in the script (`two` and
`m23` also query `p(s)`).

`db_live_bytes` counts live SQLite pages including committed WAL contents;
`db_bytes` alone can omit WAL data or include free pages. `pulse_summary_bytes`
sums serialized Pulse payloads, which still include more than the formula.
Inspect individual formulas separately before claiming a per-disjunct bound.

`kdiv`, `kmod`, and `kmul` contain no unknown calls and are controls against
introducing regressions in unrelated code. `modr2` uses a non-const unknown call.
The `mixed_*` controls query and write the object before the arithmetic chain.

## Implemented follow-up: conservative summary compaction

`PulseFormula.SummaryProjection` now runs only on C-family summaries retaining an
entry-state function application (`f@pre`). It leaves the forgetting mechanism
unchanged. The scope is deliberately limited:

- Substitute existing canonical linear definitions for non-kept, unrestricted
  variables. The canonical map has disjoint domains and ranges. The pass does
  not choose new pivots or introduce/reorient equations.
- Protect precondition and heap/return vocabulary, congruences, interval and type
  constraints, and variables in nonlinear terms or function applications.
- Leave the summary's `conditions` untouched: every variable occurring in them is
  blocked from substitution (see the next section).
- Preserve `IsInt(t, ikind)` with its **finite type range**, restricted-variable
  bounds, and divisibility. A map collision, capped coefficient overflow, or a
  binding that would need a new equation skips the substitution instead of
  dropping facts.
- For integer translates of the same affine expression, retain the original
  `IsInt` atoms giving the tightest lower and upper bounds. This represents the
  same intersection, including integrality; it does not widen the integer kind.
- Use one pass, a 4096-occurrence/fact input budget, at most 16 variables per
  substituted definition, a pre-expansion estimate, and a final check that the
  counted formula size has not increased. On rejection, retain the original
  constraints. There is no pairwise lattice search or fixpoint loop.
- Return the union of the live variables of both `DeadVariables` passes.

This is partial compaction, **not a summary-size bound or a guarantee of linear
analysis time**. The budget limits optimization work; larger formulas retain the
old behavior. Nonlinear relations and coupled integer constraints can still
keep existential variables alive. Exact projection of arbitrary formulas cannot
be assumed to have a small representation. A hard cap would require a separate,
explicit policy for precision or coverage; this patch does not implement one.

An experimental endpoint-witness elimination was removed during development: on
the two-query case it produced inequalities that made downstream analysis several
times slower than canonical substitution alone, although the compaction itself
was cheap. This is why local expression-size checks alone are not treated as a
bound on downstream solver cost.

The new unit tests check caller import, finite signed and unsigned ranges,
intersection of different integer kinds, divisibility, and actual removal of a
canonical definition. The C tests also exercise bounded query results in callers.

## Why conditions are not compacted

The first version of the compaction (`cd55eb43e3`) also substituted definitions into
`conditions`, the callee path conditions that keep an issue latent. Latent versus
manifest classification and caller feasibility read those conditions syntactically.
After substitution, a condition such as `10 < v9` became `10 < [a1+11]` over a slack
variable; the caller dropped it as trivially true and reported the issue as manifest
in itself, instead of leaving it latent for its own caller that triggers it. The
same rewriting could remove the only explicit copy of a bound and make an infeasible
path feasible. Separately, returning only the live set of the second `DeadVariables`
pass lost the arithmetic liveness that the leak check needs, giving a
`MEMORY_LEAK_C` false positive on allocators that return `h + 1`.

`a161a2cc4a` blocks every variable of `conditions` and keeps both live sets. On
700 generated C programs with `NULLPTR_DEREFERENCE_LATENT` enabled, the
forgetting-only commit reports 1496 manifest and 2790 latent issues, `cd55eb43e3`
2125 and 2474, and the current branch 1495 and 2790. On 600 generated allocator
programs, `cd55eb43e3` adds 238 `MEMORY_LEAK_C` reports; the current branch
reports exactly what the forgetting-only commit reports. Across these and 2000
further generated programs, the current branch differs from the forgetting-only
commit in 9: in 7 it is more precise (4 false positives on paths that cannot
happen go away, 3 real issues are found or become manifest), and in 2 a
manifest report becomes latent.

That last effect remains and comes from the compaction itself (`cd55eb43e3` has
it too): after substitution in the formula, a caller may no longer discharge a
callee condition that always holds on its path, so the report is only visible
with `NULLPTR_DEREFERENCE_LATENT`. It is a missed report, not a false positive.
The codetoanalyze tests do not yet check latent status after compaction.

Keeping conditions costs most of the speed of `cd55eb43e3` on `gt`, `gt2`,
`half`, and `two`: there, most of the gain came from reducing an O(N) chain of
conditions to tautologies. Four narrower rules that allow some condition rewriting
were tried, and none was both sound and faster: three were no faster and still added
a few unjustified reports, and the one as fast as `cd55eb43e3` added the most.

## Measured results (2026-10-04)

Builds: PR parent `d303f3f87a` ("base"), forgetting only `cc8b6100cb`,
compaction without the condition fix `cd55eb43e3`, and the current branch
`a161a2cc4a`, all optimized builds from the same toolchain with the bundled
Clang 21.1. Numbers are user CPU seconds of analysis only (`--pulse-only -j 1`),
the median of three runs that alternate the two binaries on one capture (for base,
the median of all nine base runs of the three invocations).
"> 30" or "> 300" means that all three runs hit that timeout in seconds. The host
was shared and heavily loaded, and the benchmark invocations ran at the same time.
Repeats of one configuration differ by up to about 50% (more than 15% in a quarter
of the configurations of 1 s or more), so small differences are noise and absolute
times are not comparable with other machines.

At N=40, from `--depths 20 40 80 --timeout 30 --repeats 3` with each build as
`--candidate` against base. "Kept" is the share of the gain of `cd55eb43e3` over
the forgetting-only commit that the current branch keeps; "-" means there is no
gain beyond noise to keep. For `m23` the forgetting-only runs timed out, so the
share is a lower bound.

| Shape | Base | Forgetting only | `cd55eb43e3` | Current | Kept |
| --- | ---: | ---: | ---: | ---: | ---: |
| eq | 0.19 | 0.93 | 0.64 | 0.61 | all |
| gt | 0.16 | 4.72 | 0.83 | 4.18 | 14% |
| gt2 | 0.18 | 9.93 | 2.42 | 9.06 | 12% |
| two | 0.38 | 26.51 | 14.19 | 25.51 | 8% |
| half | 0.12 | 4.70 | 0.93 | 3.82 | 23% |
| ret | 0.19 | 7.85 | 2.48 | 5.18 | 50% |
| ret2x | 0.15 | 8.86 | 2.24 | 5.48 | 51% |
| ret3x | 0.18 | 10.71 | 2.48 | 6.01 | 57% |
| r6 | 0.14 | 9.75 | 2.63 | 6.51 | 46% |
| r2c | 0.15 | 8.11 | 2.67 | 4.39 | 68% |
| r2d | 0.16 | 7.69 | 3.17 | 3.92 | 83% |
| m23 | 0.15 | > 30 | 3.40 | 7.54 | > 84% |
| div | 0.11 | 1.79 | 1.30 | 1.04 | all |
| band | 0.11 | 1.85 | 1.40 | 1.21 | all |
| mod | 0.11 | 2.38 | 2.37 | 2.00 | - |
| shr | 0.12 | 2.65 | 2.42 | 1.90 | - |
| modr | 0.11 | 0.25 | 0.40 | 0.36 | - |
| mixed_kdiv | 0.16 | 0.16 | 0.29 | 0.26 | - |
| mixed_kmul | 0.17 | 0.18 | 0.32 | 0.25 | - |

`kdiv`, `kmod`, `kmul`, and `modr2` are within noise of base on all builds at
N=20/40/80: these chains never reach the compaction. `mixed_kmod` is within noise
at N=20/40; deeper results for it are below.
Summed over all 24 shapes with a timeout counted as 30 s, base / forgetting only /
`cd55eb43e3` / current take 2.4 / 23.9 / 11.9 / 19.5 s at N=20 (37% of the gain
kept) and 4.0 / 140.0 / 47.6 / 90.1 s at N=40 (54% kept).

At N=80, nine shapes time out at 30 s on the current branch (eleven on the
forgetting-only commit, two on `cd55eb43e3`). Those were rerun with the
forgetting-only commit as `--base` and the current branch as `--candidate`,
`--depths 80 --timeout 300 --repeats 3`; the other columns come from the 30 s run.

| Shape, N=80 | Base | Forgetting only | `cd55eb43e3` | Current | Current / base |
| --- | ---: | ---: | ---: | ---: | ---: |
| eq | 0.70 | 3.29 | 1.46 | 1.51 | 2x |
| gt | 0.27 | 52.17 | 2.12 | 46.97 | 174x |
| gt2 | 0.29 | 48.86 | 8.50 | 36.68 | 126x |
| half | 0.17 | 54.75 | 2.62 | 48.13 | 283x |
| two | 0.92 | 119.42 | > 30 | 111.83 | 122x |
| ret | 0.28 | 50.92 | 12.46 | 32.23 | 115x |
| ret2x | 0.29 | 59.18 | 11.57 | 42.31 | 146x |
| ret3x | 0.30 | 60.22 | 11.54 | 43.05 | 144x |
| r6 | 0.21 | 67.77 | 13.11 | 47.07 | 224x |
| r2d | 0.24 | > 30 | 14.21 | 14.89 | 62x |
| r2c | 0.29 | > 30 | 11.77 | 17.53 | 60x |
| m23 | 0.24 | > 300 | > 30 | 189.10 | 788x |
| mod | 0.18 | 11.84 | 9.87 | 6.89 | 38x |
| shr | 0.19 | 11.66 | 10.07 | 7.55 | 40x |
| div | 0.21 | 6.45 | 3.41 | 2.97 | 14x |
| band | 0.19 | 8.94 | 4.34 | 3.70 | 19x |

So at N=80 the current branch is 14x (`div`) to about 790x (`m23`) slower than base
on the pure-call shapes other than `eq`. Base times here are a fraction of a second,
so the ratios are approximate. `ret2x` at N=80, 59.2 s with forgetting only, takes
42.3 s on the current branch, against about 11.6 s for `cd55eb43e3`; the roughly 6x
speedup previously reported for `cd55eb43e3` on this shape does not hold with the
condition fix.

One further shape is not in the script; it is the slowest one found and the
compaction does not help it. Level `i` of `br` is
`int a = q(s); int r = level(i-1)(s); s->f++; int b = q(s); if (a > b) r++;
return r + a - b;`. At N=40 with a 300 s timeout (median of three, measured with a
separate script that runs all four builds back to back on one capture),
base takes 0.19 s, forgetting only 84 s, `cd55eb43e3` 64 s, and the current
branch 91 s; at N=80 the three PR builds time out.

The compaction has costs on chains that mix pure calls with arithmetic. At N=80,
`modr` takes 1.04 s with forgetting only and 1.61 s on the current branch, and
`mixed_kdiv`/`mixed_kmul` take 0.57/0.56 s against 1.40/1.35 s, while `mixed_kmod`
drops from 1.01 to 0.60 s. The deeper runs below used forgetting only as `--base`
and the current branch as `--candidate` (`--timeout 300 --repeats 3`). Summary MiB
is the serialized Pulse payload, forgetting only / current; peak RSS is the change
on the current branch.

| Shape | N | Forgetting only | Current | Summary MiB (both) | Peak RSS |
| --- | ---: | ---: | ---: | ---: | ---: |
| modr | 160 | 9.21 | 6.60 | 7.2 / 9.5 | +17% |
| modr | 320 | 61.26 | 37.84 | 28.8 / 34.2 | +16% |
| mixed_kdiv | 160 | 3.28 | 4.54 | 6.1 / 8.4 | +18% |
| mixed_kdiv | 320 | 22.04 | 17.96 | 24.5 / 30.7 | +24% |
| mixed_kmod | 160 | 7.63 | 1.85 | 7.4 / 6.3 | -27% |
| mixed_kmod | 320 | 93.59 | 6.43 | 32.6 / 25.2 | -50% |
| mixed_kmul | 160 | 3.39 | 4.27 | 6.1 / 8.4 | +15% |
| mixed_kmul | 320 | 22.84 | 19.11 | 24.3 / 30.7 | +25% |

Local formula compaction can therefore increase aggregate serialized summary size
and peak RSS after caller processing. Do not claim a global no-growth property
from the local cost check.

## Real code

Ten open-source C++ projects (tinyxml2, pugixml, jsoncpp, yaml-cpp, fmt, snappy,
leveldb, re2, glog, prometheus-cpp) were analyzed with the default checkers plus
RacerD, Starvation, and BufferOverrun at `-j 1`. The forgetting-only commit,
`cd55eb43e3`, and the current branch report identical issues, and their total
instruction counts are within 0.03% of each other (0.006% between `cd55eb43e3` and
the current branch): the synthetic speedups do not show up on this code. Against
base, all three execute about 1.1% more instructions in total (yaml-cpp 2.9%) and
make the same five issue changes: two `USE_AFTER_DELETE` false positives disappear
(leveldb `~InMemoryEnv`, re2 `~NFA`), a real copy in prometheus-cpp
`Family<Counter>::Collect`, which base reported for one template instantiation only,
is no longer reported, a real copy in yaml-cpp `Scanner::ScanTag` is newly reported,
and a new `PULSE_RESOURCE_LEAK` in glog `LogFileObject::CreateLogfile` is likely a
false positive.

Eight single-procedure summaries, four in yaml-cpp and four in leveldb, grow far
more than the rest with all three versions. The largest is leveldb
`DBIter::SeekToLast`: 5,414 bytes on base, 341,655 with forgetting only, 332,022
with `cd55eb43e3`, and 334,876 on the current branch. These blowups come from the
forgetting itself; the compaction trims them by 2-3%.

## Other limitations and integration checks

- Invalidation follows whole reachable objects. An unrelated field write can
  decorrelate queries. `FP_correlated_after_field_write_ok` records that case;
  a non-const unknown mutation is tested separately as a true bug.
- Hidden global dependencies are not represented by pointer actuals. The three
  `FN_*` cases in `pure_calls_and_writes.c` record zero-argument queries across
  direct writes, callee writes, and an unknown mutator in a drain loop. A future
  global/effect model must account for entry-state queries too.
- These measurements do not cover ObjC/ObjC++ (which needs the Xcode SDK) or the
  combination with #2158, #2176, #2178, and #2197, nor #2177's composition
  changes.
