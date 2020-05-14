---
id: advanced-features
title: Advanced usage
---

In this section we discuss how to use Infer if you wish to make contributions to
it or just look under the hood to learn more about how it is working. There are,
for instance, debug options and ways to obtain the specs from the methods.

## Structure of the results folder

After a successful Infer run, a directory is created to store the results of the
analysis. By default this directory is called `infer-out`.

```
infer-out
├── captured/
├── log/
├── specs/
├── report.json
├── report.txt
├── toplevel.log
└── ...
```

- `captured/` contains information for each file analyzed by Infer. See
  [below](advanced-features#captured-folder) for more information.
- `specs/` contains the [specs](advanced-features#print-the-specs) of each
  function that was analyzed, as inferred by Infer.
- `log/` and toplevel.log contains logs
- `report.txt` and `report.json` contain the Infer reports in text and JSON
  formats
- there are other folders reserved for Infer's internal workings

### Captured folder

Inside the folder `infer-out/captured` there is a folder for each captured file.
Assume we captured a file called `example.c`. Then, Infer creates the following
files inside the folder `infer-out/captured/example.c/`:

- `example.c.cfg`
- `example.c.cg`
- `example.c.tenv`

The files `.cfg`, `.cg` and `.tenv` contain the intermediate representation of
that file. This data is passed to the backend of Infer, which then performs the
analysis. The files contain serialized OCaml data structures. The `.cfg` file
contains a control flow graph for each function or method implemented in the
file. The file `.cg` contains the call graph of the functions defined or called
from that file. Finally, the file `.tenv` contains all the types that are
defined or used in the file.

## Debug mode

With the debug option enabled `infer run --debug -- <build command>`, Infer
outputs debug information in infer-out/log/. The option `--stats` provides only
light debug information, and `--print-logs` outputs every message on the console
as well as in the log files.

In each captured folder, we obtain the file `icfg.dot`, which is the graphical
representation of the file `.cfg` and the file `call_graph.dot`, that is the
graphical representation of the call graph.

Moreover, we obtain an HTML page for each captured file inside
`infer-out/captured`. This HTML file contains the source file. In each line of
the file there are links to the nodes of the control flow graph that correspond
to that line of code. So one can see what the translation looks like. Moreover,
when you click on those links you can see details of the symbolic execution of
that particular node.

## Print the specs

It is also possible to print the specs created by Infer using the subcommand
`infer report`. You can print one particular spec that corresponds to one
method, or you can print all the specs in the results directory. Let us look at
an example:

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
infer run -- javac Hello.java
```

Infer saves the spec for the method `setX` in `infer-out/specs` and we can print
it with the command:

```bash
infer report infer-out/specs/Hello.setX{98B5}:void.specs
```

The convention for method names in Java is `<class name>.<method name>`. This
outputs the following:

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

which expresses the fact that `this` needs to be allocated at the beginning of
the method, and that at the end of the method the field `x` is equal to `newX`.

Moreover, you can print all the specs in the results directory with the command:

```bash
infer report
```

## Run internal tests

There are many tests in the Infer code base that check that Infer behaves
correctly on small program examples. This is how you'd typically run the tests;
you can adapt the figure `8` depending on the number of cores available on your
machine:

```bash
make -j8 test
```
