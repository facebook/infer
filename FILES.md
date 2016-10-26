#Files in infer/bin/

## Top-level commands

*infer* : Main command to run Infer. Check out the docs for instructions on how to use it.

*inferTest* : Shell script for running Infer's tests. Uses Buck for running the tests.
Usage: inferTest {c, objc, java} for the tests about the analysis of C, Objective-C, or Java files.

*inferTraceBugs* : Python script to explore the error traces in Infer reports

## Helper commands

The rest of the commands in infer/bin/ are not meant to be called directly, but are used by the top-level commands above.

*InferJava* : Binary containing the Java frontend.

*InferClang* : Binary containing the clang frontend.

*InferAnalyze* : Binary containing the backend of Infer that performs the analysis.

*InferPrint* : Binary that prints reports about the analysis such as the specs of methods and a list of bugs found.
