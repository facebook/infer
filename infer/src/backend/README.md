# Back End

The back end is responsible for the analysis of a project starting from an intermediate representation stored in the results directory, typically `infer-out`.

The main entry point is module [InferAnalyze](InferAnalyze.re) which produces the back-end executable `InferAnalyze`.

Module [InferPrint](InferPrint.re) produces the executable `InferPrint`, which is used to export analysis results.

Module [Infer](infer.ml) is the top-level driver that orchestrates build system integration, frontends, and backend.

Module [DB](DB.ml)

## Inferanalyze

- This module is the main module for analysis after the capture-phase.
- Fetches all clusters of source directories and runs analysis on each of them.
- Gets Call Graph from the execution environment, which is created from the cluster.
- Runs in 2 modes:
  1. Full Analysis - Performs complete inter-procedural analysis.
  2. Run checkers only - Only enabled checkers are run on the Call Graph.


## Infer

- Sets environment for Clang wrapper.
- Forks a process that calls `infer.py` with the commandline arguments.
