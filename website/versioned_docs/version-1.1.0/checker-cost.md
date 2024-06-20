---
title: "Cost: Runtime Complexity Analysis"
description: "Computes the time complexity of functions and methods. Can be used to detect changes in runtime complexity with `infer reportdiff`."
---

Computes the time complexity of functions and methods. Can be used to detect changes in runtime complexity with `infer reportdiff`.

Activate with `--cost`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes
- C#/.Net: Yes

Cost analysis statically estimates an upper bound on the worst-case execution cost of a program (WCET). This page gives an overview of how the analysis works for *Java* code. The analyser also has limited support for C/C++ and Objective-C.

To run the analysis, you can use run `infer --cost` (which will run cost analysis along with other
analyses that are run by default) or `infer --cost-only` (which will only run cost analysis).

For example, the command `infer --cost-only -- javac File.java` will run
cost analysis on File.java.


## How the analysis works


Most ideas behind this analysis are based on Stefan Bydge's PhD thesis [Static WCET Analysis based on Abstract Interpretation and Counting of Elements](https://www.semanticscholar.org/paper/Static-WCET-Analysis-Based-on-Abstract-and-Counting-Bygde/ee5157164d497725c1f42dc6c475a59a87c99957).

The analysis computes two things for each node in the CFG:
- the cost of its instructions, i.e. how much one execution of this node costs,
- how many times it can be executed.

The total cost of the node is the scalar product of these two vectors. Then, these are passed to a constraint solver that computes the execution cost of the procedure based on the incoming/outgoing edges.


At a high level, the analysis has three steps:
- Choose control variables that allude to "how many times a loop may iterate".
- Get abstract ranges of the control variables from [InferBO](/docs/1.1.0/checker-bufferoverrun) (a numerical analysis that infers symbolic intervals)
- Construct complexity polynomials for loops and functions by via a constraint solving algorithm.



## Examples

Infer’s cost analysis statically estimates the execution cost of a
program without running the code. For instance, assume that we had the
following program:

```java
void loop(ArrayList<Integer> list){
  for (int i = 0; i <= list.size(); i++){
  }
}
```

For this program, Infer statically infers a polynomial (e.g. `8|list|+16`) for the execution cost of this program by giving each instruction in Infer's intermediate language a symbolic cost (where `|.|` refers to the length of a list). Here---overlooking the actual constants---the analysis infers that this program’s asymptotic complexity is `O(|list|)`, that is loop is linear in the size of its input list. Then, at diff time, if a developer modifies this code to,

```java
void loop(ArrayList<Integer> list){
  for (int i = 0; i <= list.size(); i++){
    foo(i); // newly added function call
  }
}
```

where `foo` has a linear cost in its parameter, then Infer automatically detects that the complexity of loop has increased from `O(|list|)` to `O(|list|^2)` and then reports an [`EXECUTION_TIME_COMPLEXITY_INCREASE`](/docs/1.1.0/all-issue-types#execution_time_complexity_increase) issue.



Unlike other Infer analyses (which report found issues/bugs when running infer once), cost analysis only reports an issue for differential analysis (i.e. when comparing the analysis results on the original and the modified files). Instead, infer writes the execution cost of the program into `infer-out/costs-report.json` file. For each procedure, `costs-report.json` includes the actual polynomial (for the execution cost) along with the degree of the polynomial, the procedure name, line number etc.

Differential cost analysis in action:
- first run infer's cost analysis on `File.java` and copy `inter-out/costs-report.json` to `previous-costs-report.json` (Note that the file should be copied outside the result directory because the directory will be removed in the second infer run.)
- modify `File.java` as shown above
- re-run infer on `File.java` and copy `infer-out/costs-report.json` to `current-costs-report.json`
- run `infer reportdiff --costs-current current-costs-report.json --costs-previous previous-costs-report.json`.
- Inspect `infer-out/differential/introduced.json` to see the newly found complexity increase issue(s).


## Limitations

There are a number of known limitations to the design of the static cost analysis:

- InferBo's intervals are limited to affine expressions, not full-blown polynomials. Hence, we can automatically infer bounds involving square roots.

- We do not handle recursion.

- If the execution cost of a program depends on an unknown call (e.g. an unmodeled library calls), we can't compute a static upper bound and return T (unknown cost).


## List of Issue Types

The following issue types are reported by this checker:
- [AUTORELEASEPOOL_SIZE_COMPLEXITY_INCREASE](/docs/1.1.0/all-issue-types#autoreleasepool_size_complexity_increase)
- [AUTORELEASEPOOL_SIZE_COMPLEXITY_INCREASE_UI_THREAD](/docs/1.1.0/all-issue-types#autoreleasepool_size_complexity_increase_ui_thread)
- [AUTORELEASEPOOL_SIZE_UNREACHABLE_AT_EXIT](/docs/1.1.0/all-issue-types#autoreleasepool_size_unreachable_at_exit)
- [EXECUTION_TIME_COMPLEXITY_INCREASE](/docs/1.1.0/all-issue-types#execution_time_complexity_increase)
- [EXECUTION_TIME_COMPLEXITY_INCREASE_UI_THREAD](/docs/1.1.0/all-issue-types#execution_time_complexity_increase_ui_thread)
- [EXECUTION_TIME_UNREACHABLE_AT_EXIT](/docs/1.1.0/all-issue-types#execution_time_unreachable_at_exit)
- [EXPENSIVE_AUTORELEASEPOOL_SIZE](/docs/1.1.0/all-issue-types#expensive_autoreleasepool_size)
- [EXPENSIVE_EXECUTION_TIME](/docs/1.1.0/all-issue-types#expensive_execution_time)
- [INFINITE_AUTORELEASEPOOL_SIZE](/docs/1.1.0/all-issue-types#infinite_autoreleasepool_size)
- [INFINITE_EXECUTION_TIME](/docs/1.1.0/all-issue-types#infinite_execution_time)
