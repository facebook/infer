This directory contains a model of the semantics of LLVM, of LLAIR, and of the
LLVM to LLAIR translation, along with some theorems about them. It is written
in the HOL4 theorem prover.

# Building

To build the formalisation, which just gets HOL4 to check the proofs, run the
`Holmake` command in this directory. This requires having HOL4 installed, which
itself depends on PolyML.

The directions for installing HOL4 are here
<https://hol-theorem-prover.org/#get>, but essentially it is

    git clone https://github.com/HOL-Theorem-Prover/HOL.git
    cd HOL
    poly < tools/smart-configure.sml
    bin/build

PolyML is available here <https://www.polyml.org/download.html>, but it is
widely available from various package managers and easiest to install that way,
e.g., on MacOS `brew install polyml`.

This has been tested on PolyML 5.8, and on HOL4 Github commit e2d0fafdb3ccdd098d2bd6e0aaaacbc9468ae4b1.

# What is modelled

The model of the translation is not complete: function calls and exceptions are
two notable omissions. However, it covers the other important aspects of the
translation:

 - the SSA restrictions on LLVM,

 - building compound expressions to reduce the number of temporary stores in
   LLAIR,

 - and how the SSA restrictions imply that a simple linear traversal of the
   (topologically sorted) CFG suffices for the translation,

 - the treatment of LLVM's bitwise 2s complement arithmetic on LLAIR's
   mathematical integers.

The semantics of LLVM and LLAIR are given as small step abstract state
machines, and the translation between them as a pure recursive function.

The semantics of LLVM uses a concrete memory model, i.e., indices into an array
of bytes. This means that it does not account for all possible forms of
undefined behaviour that could occur with LLVM's more complicated pointer
provenance model.

The proof that if an LLVM program doesn't get stuck and has a certain
observable behaviour, then the translation to LLAIR also doesn't get stuck and
has the same behaviour is complete, except for a few minor cheats. The proof in
the other direction, from LLAIR behaviour to LLVM behaviour, is not attempted.

# Contents

`llairScript.sml` : the syntax and semantics of LLAIR

`llair_propScript.sml` : theorems about various properties of LLAIR's semantics

`llvmScript.sml` : the syntax and semantics of LLVM

`llvm_propScript.sml` : theorems about various properties of LLVM's semantics

`llvm_ssaScript.sml` : the definition of SSA form for LLVM, along with theorems
about SSA, dominance, and variable liveness

`llvm_to_llairScript.sml` : the translation function's definition

`llvm_to_llair_propScript.sml` : theorems about the LLVM to LLAIR translation
that don't mention the semantics, so just those that establish invariants about
the translation functions themselves.

`llvm_to_llair_sem_propScript.sml` : the overall correctness theorems for the
translation

`memory_modelScript.sml` : an abstract model of memory as an array of bytes. It
includes reading and writing multi-byte words in little-endian encoding

`miscScript.sml` : various generic theorems that could in principle be moved
into HOL4's libraries

`settingsScript.sml` : set up HOL4, replacing many of it's ugly UPPERCASE
constants with lower case ones
