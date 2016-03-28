# Back End

The back end is responsible for the analysis of a project starting from an intermediate representation stored in the results directory, typically `infer-out`.

The main entry point is module [Inferanalyze](inferanalyze.ml) which produces the back-end executable `InferAnalyze`.

Module [Inferprint](inferprint.ml) produces the executable `InferPrint`, which is used to export analysis results.

