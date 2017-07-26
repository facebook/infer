# Back End

The back end is responsible for the analysis of a project starting from an intermediate representation stored in the results directory, typically `infer-out`.

The main entry point for infer binary is [infer.ml](infer.ml).

Entry point for the analysis is module [InferAnalyze](InferAnalyze.ml).

Module [InferPrint](InferPrint.ml) is used to export the analysis results.
