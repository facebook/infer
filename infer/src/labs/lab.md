This is a lab exercise designed to take the participant through the basics of using the Infer.AI framework for building compositional abstract interpreters. We provide the skeleton for a simple intraprocedural resource leak analysis. During this exercise, you will identify limitations in the existing approach and work on extending it to a create a more powerful interprocedural analysis.

This document assumes that you have already [installed](https://github.com/facebook/infer/blob/master/INSTALL.md) Infer from source (you only need Infer for Java for this lab: `./build-infer.sh java`), successfully compiled it, and [set up](https://github.com/facebook/infer/blob/master/CONTRIBUTING.md#hacking-on-the-code) your OCaml dev environment.

The `infer/src/labs/ResourceLeaks.ml` and `infer/src/labls/ResourceLeaksDomain.ml` files have comments containing hints for which parts of the code you need to change in order to accomplish a particular task (e.g., 1(e) in `ResourceLeaks.ml`).

## (1) Warm up: running, testing, and debugging Infer

(a) Change to the test directory (`cd infer/tests/codetoanalyze/java/lab`) and run the checker on a single file: `infer --resource-leak-only -- javac Leaks.java`. Infer should report 3 bugs.

(b) Run the tests with `make test`. The tests work by producing an output file `issues.exp.test`, which is compared against the expected output file `issues.exp`. Tests pass when the two files are identical. You should see no output, which indicates that the tests have passed.

(c) Add a new test method containing a resource leak to `Leaks.java`, then run the tests again. The tests should now fail, and the error message should indicate that the failure is due to the new lreeak that you added. As the message suggests, run `make replace` to update the expected output (this simply copies the actual output `issues.exp.test` to the expected output `issues.exp`). Re-run the tests; they should now pass again.

(d) Run the analyzer on a single test file to produce the debug HTML: `infer --debug --resource-leak-only -- javac Leaks.java`. Then, open the debug HTML: `open infer-out/captured/*.html`. This helpful artifact shows the Infer warnings alongside the code they are complaining about. It also displays the CFG node(s) associated with each line of code. Clicking a CFG node shows the Infer IR associated with the node, and the pre/post state computed while analyzing the instruction. Come back to the debug HTML early and often when you can't understand what your analysis is doing--it will help!

(e) The `Logging.d_str`/`Logging.d_strln` functions print to the debug HTML. The logging is contextual: it will print to the log for the CFG node that's being analyzed at the time the logging statement execute. Try adding `Logging.d_strln "Hi Infer";` inside of the case for `Call`, recompiling/re-running. You should see the text you printed inside the appropriate CFG node log in the debug HTML.

(f) The `Logging.progress` function prints to stderr. This can be useful for printing information that doesn't happen in the context of a particular CFG node (e.g., performing post-processing on a summary). Try adding `Logging.progress "Hi Infer;"` to the `compute_post` function, recompile/re-run and see your text printed on the command line.

## (2) Reasoning about abstract domains

(a) Look at the `ResourceLeakDomain.ml`. There's a termination bug in the abstract domain--do you see it?.
- Write a test method that makes the analyzer diverge (Note: this may manifest as a stack overflow that crashes Infer rather than geniune divergence).
- Fix the domain to make your test pass and ensure that the analyzer will always terminate. Make sure your fix is sufficiently general (remember: the allocation count can be negative :)).
- Hint: `AbstractDomain.TopLifted` may be useful for this.

(b) Think about the concretization of the resource count. What does a resource count of zero mean? Is there a concrete state in the concretization of "Resource count zero" that leaks a resource? Write a simple test method `FN_leakBad` that will produce this concrete state (that is, a false negative test where the program leaks a resource, but the analyzer doesn't catch it)?

(c) In addition, there are programs that do not leak resources that the analyzer will flag. Write a simple test method `FP_noLeakOk` that exhibits this problem (that is, a false positive test that demonstrates the imprecision of the analyzer)?

(d) Do your false negative and false positive examples have a common underlying cause? Can the domain be improved to address them?

## (3) Improving the domain

(a) Change the simple counting domain to a domain that overapproximates the set of storage locations that hold a resource. As a concrete goal, the new domain should allow you to print the name of the resource(s) that leak in the error message (rather than just the number of resources). The new domain should also allow your analysis to get the correct answer on your false negative and false positive tests from 2(b) and 2(c). Think about the following questions when designing your new domain:
- How should we abstract storage locations? Is abstracting the stack program variables (`Var.t`)'s enough, or do we need an abstraction of the heap as well?
- How will we handle aliasing (storage locations that store the same address?)
- Will it be easy to extend the domain to incorporate information from the callee summaries/represent state that will be instantiated by callers (see 4)?
- Some modules that might be useful in creating your new domain (depending on what approach you choose): `AbstractDomain.FiniteSet`, `AbstractDomain.Map`, `AccessPath`, `Var`.

(b) Write some tests that demonstrate the limitations of your new domain: both false positives (names prefixed with `FP_` and false negatives (prefixed with `FN_`).

## (4) Interprocedural analysis
At this point, you likely have a fairly advanced intraprocedural analysis that is capable of finding resource leaks in real programs. Feel free to skip to 5 if you are eager to get started with bugfinding; many real resource leak bugs are intraprocedural. Alternatively, continue on with 4 to make your analysis interprocedural.

(a) Augment the summary type with state to indicate when the current procedure returns a resource. Allowing a resource to escape to the caller should not be considered a leak. This improvement should allow your analysis to pass the following tests:

```
FileInputStream returnResourceOk() {
  return new FileInputStream("file.txt");
}

FileInputStream returnResourceWrapperOk() {
  return returnResourceOk();
}

void returnResourceThenCloseOk() {
  returnResourceWrapperOk().close();
}

void returnResourceThenLeakBad() {
  returnResourceWrapperOk(); // warning
}
```

(b) Augment the summary type with state to indicate formals that are closed by the current function. This should allow your analysis to pass the following tests:

```
void closeResourceOk(Closeable c) {
  c.close();
}

void closeResourceWrapperOk(c) {
  closeResourceOk(c);
}

void closeResourceDirectOK() {
  closeResourceOk(new FileInputStream("file.txt"));
}

void closeResourceTransitiveOk()
  closeResourceOk(new FileInputStream("file.txt"));
}

void closeOne(Closeable c1, Closeable c2) {
  c2.close();
}

void closeOnlyOneBad() {
  closeOne(new FileInputStream("1.txt"), new FileInputStream("2.txt")); // warning
}
```

Hint: you might find the `FormalMap.t` stored in `proc_data.extras` useful for this. This module lets you go back and forth between the index of a formal and its name. This utility module is also used in the `RacerD` and `TaintAnalysis` modules.

## (5) Making it practical

(a) Real resource leaks frequently involve failing to close resources along exceptional control-flow paths. For simplicity, the initial version of the current analysis uses a filtered view of the CFG that skips exceptional edges (`ProcCfg.Normal`). To find more bugs, you might want to switch to using `ProcCfg.Exceptional` and make sure that your analysis gets the right answer on some realistic exception examples like:

```
void tryWithResourcesOk() {
  // this is syntactic sugar that makes sure stream gets closed
  try (FileInputStream stream = new FileInputStream("file.txt")) {
    ...
  }
}

void closeInFinallyOk() {
  FileInputStream stream = null;
  try {
    stream = new FileInputStream("file.txt");
  } finally {
    if (stream != null) {
      stream.close();
    }
  }
}

void closeInCatchBad() {
  FileInputStream stream = null;
  try {
    stream = new FileInputStream("file.txt");
  } catch {
    // ok not to close here, since we never acquired the resource
  }
  if (stream != null) {
    stream.close();
  }
}

```

(b) Try running on real code! The instructions [here](http://fm.csl.sri.com/SSFT17/infer-instr.html) have several suggestions for open-source Android apps to point your analysis at. Try `./gradlew assembleDebug -x test` first to make sure everything builds correctly without Infer (if not, you are probably missing some dependencies--the error messages should guide you). Once that's working, try
`./gradlew clean; infer run --resource-leak-only -- ./gradlew assembleDebug -x test`.
- Found a real bug? Bonus points! Send a pull request to fix it! Very frequently, the best fix is to use try-with-resources.
- Found a false positive in your analysis? Try re-running Infer with `--debug` and see if you can narrow down the root cause/fix it.
- How does your analysis compare to Infer's production resource leak analysis? Run with `infer -- <gradle command>` to see if your analysis finds bugs that Infer misses, or vice versa.
