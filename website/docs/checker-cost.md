---
title: "Cost: Complexity Analysis"
description: "Computes the asymptotic complexity of functions with respect to execution cost or other user defined resources. Can be used to detect changes in the complexity with `infer reportdiff`."
---

Computes the asymptotic complexity of functions with respect to execution cost or other user defined resources. Can be used to detect changes in the complexity with `infer reportdiff`.

Activate with `--cost`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: No
- Erlang: No
- Hack: Experimental
- Java: Yes
- Python: No

Cost analysis statically estimates an upper bound on the worst-case execution cost of a program (WCET). This page gives an overview of how the analysis works for *Java* code. The analyser also has limited support for C/C++ and Objective-C.

To run the analysis, you can use run `infer --cost` (which will run cost analysis along with other
analyses that are run by default) or `infer --cost-only` (which will only run cost analysis).

For example, the command `infer --cost-only -- javac File.java` will run
cost analysis on `File.java`.


## How the analysis works

The analysis computes symbolic upper bounds on the resource usage of programs—-execution cost being the main resource we consider. These costs are expressed in terms of polynomials describing the asymptotic complexity of procedures with respect to their input sizes. The main input of the analysis is the source file which is then translated to an intermediate language along with the control-flow graph of the program. The analysis then operates on this intermediate language in several phases:
- 1) a numerical value analysis based on [InferBo](/docs/checker-bufferoverrun) computes value ranges for instructions accessing memory
- 2) a loop bound analysis determines upper bounds for the number of iterations of loops and generates constraints for nodes in the control-flow graph
- 3) a constraint solving step resolves the constraints generated in the second step and computes an upper bound on the execution cost.

Most ideas behind this analysis are based on Stefan Bydge's PhD thesis [Static WCET Analysis based on Abstract Interpretation and Counting of Elements](https://www.semanticscholar.org/paper/Static-WCET-Analysis-Based-on-Abstract-and-Counting-Bygde/ee5157164d497725c1f42dc6c475a59a87c99957).

The analysis computes two things for each node in the CFG:
- the cost of its instructions, i.e. how much one execution of this node costs,
- how many times it can be executed (part 2 above)

The total cost of the node is the scalar product of these two vectors. Then, these are passed to a constraint solver (part 3 above) that computes the execution cost of the procedure based on the incoming/outgoing edges.


The results of the analysis are written into `costs-report.json` where for each procedure, we record the actual polynomial (for the execution cost) along with the degree of the polynomial, the procedure name, line number etc.



## Types of resources/costs

Although the analysis was initially designed to reason about the execution cost, it is not limited to inferring bounds for just execution cost. In order to statically detect regressions in other types of resource usage, we have generalized the analysis to account costs for different types of resources such as Objective-C's autorelease pool size or memory allocations.


Currently, there are three types of resources/costs the analysis operates on:
- 1) execution cost
- 2) allocation cost
- 3) autoreleasepool size

For 1), the analysis assumes a simple sequential model with an abstract cost semantics: each primitive instruction in the intermediate language (SIL) is assumed to incur a unit execution cost. 

For 2), the analysis only incurs costs for primitive operations that allocate memory (e.g. `new`). This is in experimental mode and hence the results are not written into `costs-report.json`. 

For 3), the analysis incurs a cost when objects are added to Objective-C's `@autoreleasepool`. This usually happens in two cases: 1) when `autorelease` is called explicitly in non-ARC compiled code and 2) when an (autoreleased) object pointer is returned from non-ARC compiled callee to ARC compiled caller, and vice-versa. 


## Examples (execution cost)

For instance, assume that we had the following program:

```java
void loop(ArrayList<Integer> list){
  for (int i = 0; i <= list.size(); i++){
  }
}
```

Infer statically infers a polynomial (e.g. `8 · |list|+16`) for the execution cost of this program by giving each instruction in Infer's intermediate language a symbolic cost (where `|...|` refers to the length of a list). Here---overlooking the actual constants---the analysis infers that this program’s asymptotic complexity is `O(|list|)`, that is loop is linear in the size of its input list. Then, at diff time, if a developer modifies this code to,

```java
void loop(ArrayList<Integer> list){
  for (int i = 0; i <= list.size(); i++){
    foo(i); // newly added function call
  }
}
```

where `foo` has a linear cost in its parameter, then Infer automatically detects that the complexity of loop has increased from `O(|list|)` to `O(|list|^2)` and then reports an [`EXECUTION_TIME_COMPLEXITY_INCREASE`](/docs/next/all-issue-types#execution_time_complexity_increase) issue.

## Differential mode
Unlike other Infer analyses (which on reports found issues/bugs in `report.json` when running infer once), cost analysis also has a special mode that reports an issue for differential analysis (i.e. when comparing the analysis results on the original and the modified files). For each procedure, `costs-report.json` includes the actual polynomial (for the execution cost) along with the degree of the polynomial, the procedure name, line number etc. Then, in the differential mode, these `costs-report.json` files are compared. 

Differential cost analysis in action:
- first run infer's cost analysis on `File.java` and copy `inter-out/costs-report.json` to `previous-costs-report.json` (Note that the file should be copied outside the result directory because the directory will be removed in the second infer run.)
- modify `File.java` as shown above
- re-run infer on `File.java` and copy `infer-out/costs-report.json` to `current-costs-report.json`
- run `infer reportdiff --costs-current current-costs-report.json --costs-previous previous-costs-report.json`.
- Inspect `infer-out/differential/introduced.json` to see the newly found complexity increase issue(s).


## Limitations

There are a number of known limitations to the design of the static cost analysis:

- [InferBo](/docs/checker-bufferoverrun) 's intervals are limited to affine expressions, not full-blown polynomials. Hence, we can not automatically infer bounds involving square roots.

- We do not handle recursion.

- If the execution cost of a program depends on an unknown call (e.g. due to iterating over an unmodeled library call), we can't compute a static upper bound and return T (unknown cost). See [INFINITE_EXECUTION_COST](/docs/next/all-issue-types#infinite_execution_time) for details.


## List of Issue Types

The following issue types are reported by this checker:
- [EXECUTION_TIME_COMPLEXITY_INCREASE](/docs/next/all-issue-types#execution_time_complexity_increase)
- [EXECUTION_TIME_COMPLEXITY_INCREASE_UI_THREAD](/docs/next/all-issue-types#execution_time_complexity_increase_ui_thread)
- [EXECUTION_TIME_UNREACHABLE_AT_EXIT](/docs/next/all-issue-types#execution_time_unreachable_at_exit)
- [EXPENSIVE_EXECUTION_TIME](/docs/next/all-issue-types#expensive_execution_time)
- [INFINITE_EXECUTION_TIME](/docs/next/all-issue-types#infinite_execution_time)
