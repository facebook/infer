---
id: advanced-features
title: Advanced usage
layout: docs
permalink: /docs/advanced-features.html
section: User Guide
section_order: 01
order: 06
---

In this section we discuss how to use Infer if you wish to make contributions to it or just look under the hood
to learn more about how it is working.
There are, for instance, debug options and ways to obtain the specs from the methods.

## Structure of the results folder

After a successful Infer run, a directory is created to store the
results of the analysis. By default this directory is called
`infer-out`.

```
infer-out
├── captured/
├── log/
├── multicore/
├── sources/
├── specs/
├── bugs.txt
├── procs.csv
├── report.csv
├── report.json
└── stats.json
```

- `captured/` contains information for each file analyzed by Infer. See [below](docs/advanced-features.html#captured-folder) for more information.
- The `log/`, `multicore/`, and `sources/` folders are used internally to drive the analyzer.
- `specs/` contains the [specs](docs/advanced-features.html#print-the-specs) of each function that was analyzed, as inferred by Infer.
- `bugs.txt`, `report.csv`, and `report.json` contain the Infer reports in three different formats.
- `procs.csv` and `stats.json` contain debug information about the run.


### Captured folder

Inside the folder `infer-out/captured` there is a folder for each captured file. Assume we captured a file called `example.c`. Then, Infer creates the following files inside the folder `infer-out/captured/example.c/`:

- `example.c.cfg`
- `example.c.cg`
- `example.c.tenv`

The files `.cfg`, `.cg` and `.tenv` contain the intermediate representation of that file. This data is passed to the backend of Infer, which then performs the analysis. The files contain serialized OCaml data structures. The `.cfg` file contains a control flow graph for each function or method implemented in the file. The file `.cg` contains the call graph of the functions defined or called from that file. Finally, the file `.tenv` contains all the types that are defined or used in the file.



## Debug mode

With the debug option enabled `infer --debug -- <build command>`, Infer outputs debug information. When using `make` or `clang`, one needs an extra debug argument for the frontend:

```bash
infer --frontend_debug --debug -- make example.c
```

In each captured folder, we obtain the file `icfg.dot`, which is the graphical representation of the file `.cfg` and the file
`call_graph.dot`, that is the graphical representation of the call graph.


Moreover, we obtain an html page for each captured file inside `infer-out/captured`. This html file contains the source file. In each line of the file there are links to the nodes of the control flow graph that correspond to that line of code. So one can see what the translation looks like. Moreover, when you click on those links you can see details of the symbolic execution of that particular node. If the option `--no_test` is also passed to `infer`, then the page pointed to from the nodes contains the printout of the whole symbolic execution.

## Print the specs

It is also possible to print the specs created by Infer using the command `InferPrint`. You can print one particular spec that corresponds to one method, or you can print all the specs in the results directory. Let us look at an example:

```java
class Hello {
    int x;
    void setX(int newX) {
	    this.x = newX;
    }
}
```

We run Infer on this example with:

```bash
	infer -- javac Hello.java
```

Infer saves the spec for the method `setX` in `infer-out/specs` and we can print it with the command:

```bash
	InferPrint infer-out/specs/Hello.setX{98B5}:void.specs
```

The convention for method names in Java is `<class name>.<method name>`. This outputs the following:

```bash
Procedure: void Hello.setX(int)
void void Hello.setX(int)(class Hello *this, int newX)
Timestamp: 1
Status: INACTIVE
Phase: RE_EXECUTION
Dependency_map:
TIME:0.006893 s TIMEOUT:N SYMOPS:34 CALLS:0,0
ERRORS:
--------------------------- 1 of 1 [nvisited: 4 5 6] ---------------------------
PRE:
this = val$1: ;
newX = val$3: ;
this|->{Hello.x:val$2}:
POST 1 of 1:
this = val$1: ;
return = val$4: ;
newX = val$3: ;
this|->{Hello.x:newX}:
----------------------------------------------------------------
```

which expresses the fact that `this` needs to be allocated at the beginning of the method, and that at the end of the method the field `x` is equal to `newX`.


Moreover, you can print all the specs in the results directory with the command:

```bash
InferPrint -results_dir infer-out
```


## Run internal tests

There are many tests in the Infer code base that check that Infer behaves correctly on small program examples. The tests use [Buck](http://buckbuild.com/), another Facebook's open source tool for building projects. We provide the script `inferTest` to run the tests, which requires buck to be in your PATH.

```bash
inferTest java    # Run the tests about Java analysis
inferTest clang   # Run the tests about C and Objective-C analysis
inferTest c       # Run the tests about C analysis
inferTest objc    # Run the tests about Objective-C analysis
```
