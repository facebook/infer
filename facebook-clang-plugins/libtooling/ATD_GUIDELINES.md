Guidelines for writing ATD annotations in ASTExporter.cpp
=========================================================

The ATD specifications inlined in ASTExporter.cpp are used to generate Ocaml parsers using `atdgen`. Those annotations must reflect
closely the Yojson/Json/Biniou emitted by the C++ plugin.

The ATD language and the parser generating tool `atdgen` are documented here:
  https://atd.readthedocs.io/en/latest/atdgen.html

ATD basics
----------

The definition of object in ASTExporter.cpp typically look like this:
```
//@atd type record = {
//@atd  mandatory_field : string
//@atd  ?optional_int : int option
//@atd  ~string_empty_by_default : string
//@atd } <ocaml field_prefix="r_">
```

Each line of ATD definitions must start with `//@atd`-style comments.

The `?` symbols mean that an absent field is ok and maps to the ocaml value `None`.
The `~` symbols mean that an absent field is ok and maps to some default value for this type.

The `<ocaml field_prefix="r_">` annotations are currently required to disambiguate records on the ocaml side. The prefix should be
made the first letters of the C++ types, except for a few exceptions (e.g. `CXX` is mapped to `x`).

Valid Yojson values for this specification are for instance:
```
{ "mandatory_field" : "foo" }
{ "mandatory_field" : "foo", "optional_int" : 3 }
```

Simple example
--------------

```
//@atd type source_location = {
//@atd   ?file : string option;
//@atd   ?line : int option;
//@atd   ?column : int option;
//@atd } <ocaml field_prefix="sl_">
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpSourceLocation(SourceLocation Loc) {

  SourceLocation SpellingLoc = SM.getSpellingLoc(Loc);
  PresumedLoc PLoc = SM.getPresumedLoc(SpellingLoc);
  if (PLoc.isInvalid()) {
    // Outputs an object. The closing brace will be added when 'Scope' is destroyed.
    // This is typical in C++ : http://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization
    // Second argument encodes size of an object. In this case there are 0 tags exported
    ObjectScope Scope(OF, 0);
    // Early return is ok because all fields are optional.
    // return will destroy Scope and add closing brace
    return;
  }

  // There are at most 3 tags exported in this case. It's ok to specify upper bound
  // on the size of the object. It will be correct, but inefficient.
  ObjectScope Scope(OF, 3);

  if (strcmp(PLoc.getFilename(), LastLocFilename) != 0) {
    // emits the tag of the field 'file'
    OF.emitTag("file");
    // emits a string (since we do emit something, the ocaml value is 'Some (...)')
    OF.emitString(PLoc.getFilename());
    OF.emitTag("line");
    OF.emitInteger(PLoc.getLine());
    OF.emitTag("column");
    OF.emitInteger(PLoc.getColumn());
  }
} // Scope gets destroyed here and adds closing brace
```

Note that parser expects the C++ code to emit the corresponding fields in the same order.

More complex example
--------------------

To get types of AST nodes, exporter relies on generated header files from clang.
```
//@atd type decl_ref = {
//@atd   kind : decl_kind;                (* ATD type declared below *)
//@atd   ?name : string;
//@atd   ~is_hidden : bool;
//@atd   ?qual_type : qual_type option
//@atd } <ocaml field_prefix="dr_">
//@atd
//@atd type decl_kind = [

// define macros used in clang/AST/DeclNodes.inc to produce ATD annotations
#define DECL(DERIVED, BASE) //@atd | DERIVED
#define ABSTRACT_DECL(DECL) DECL
// clang/AST/DeclNodes.inc file contains all possible Decl nodes
#include <clang/AST/DeclNodes.inc>

//@atd ]

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpBareDeclRef(const Decl &D) {
  const NamedDecl *ND = dyn_cast<NamedDecl>(&D);
  bool isHidden = ND ? ND->isHidden() : false;
  const ValueDecl *VD = dyn_cast<ValueDecl>(&D);

  // specify exact size of the scope.
  ObjectScope Scope(OF, 1 + (bool)ND + isHidden + (bool)VD);
  OF.emitTag("kind");
  // case of an algebraic datatype that carries no value (like a C enum field)
  OF.emitSimpleVariant(D.getDeclKindName());

  // ok not to output anything because the field is optional
  if (ND) {
    OF.emitTag("name");
    OF.emitString(ND->getNameAsString());
    // flags correspond to ATD fields of the form "~tag : bool"
    OF.emitFlag("is_hidden", ND->isHidden());
  }
  // ok not to output anything because the field is optional
  if (VD) {
    OF.emitTag("qual_type");
    dumpQualType(VD->getType()); // will emit value for a QualType
  }
}
```

The complex definition for decl_kind is processed in several stages.

First we use an adequate node of the clang preprocessor and expand the `#include <clang/AST/DeclNodes.inc>` to following code:
```
//@atd type decl_kind = [
//@atd | AccessSpec
//@atd | Block
//@atd | Captured
//@atd (* ... *)
//@atd ]
```

Then we extract the ATD specifications by looking for `//@atd`-style comments
```
type decl_kind = [
| AccessSpec
| Block
| Captured
(* ... *)
]
```

After calling `atdgen`, the final Ocaml type is:
```
type decl_kind = `AccessSpec | `Block | `Captured | `ClassScopeFunctionSpecialization  (* ... *)
```

Testing
-------

Compiling with `DEBUG=1` will make the ATDWriter enforce the general well-formedness of the emitted Yojson/Json/Biniou. For instance, a missing tag will trigger an assert failure.

Discrepancies between annotations and emitted values are detected by the tests in `../clang-ocaml`.

It's is important to test both exporter (`make test`) and atd annotations (`make -C ../clang-ocaml test`)

When changing the exporter, sometimes tests will fail due to exporting new information. To record this fact, run `make record-test-outputs`

Mapping clang AST nodes to ocaml values
---------------------------------------

Clang AST entities of a given type are typically represented by a cluster of classes.

For instance, here is the cluster for declarations: http://clang.llvm.org/doxygen/classclang_1_1Decl.html
Function declaration: https://clang.llvm.org/doxygen/classclang_1_1FunctionDecl.html

To map these entities to a flat algebraic data type of Ocaml (serialized as a "variant" by ATDWriter), as seen
before, we heavily rely on a (hacky) C-preprocessing stage and several scripts. They can be found in `atdlib` directory.

Let us study how declarations are handled more precisely. Handling for statement and type nodes is very similar.

##### Default values for node tuples
```
#define DECL(DERIVED, BASE) //@atd #define @DERIVED@_decl_tuple @BASE@_tuple
#define ABSTRACT_DECL(DECL) DECL
#include <clang/AST/DeclNodes.inc>
```

After one step of preprocessing + ATD-extraction, this creates the following intermediate code (see `build/ast_inline.atd.inc`)
```
#define access_spec_decl_tuple decl_tuple
#define block_decl_tuple decl_tuple
#define captured_decl_tuple decl_tuple
// ...
#define named_decl_tuple decl_tuple
#define decl_context_tuple decl list * decl_context_info
```

This defines the default value of each `xxxx_decl_tuple` to be that of the base class.

The `@...@` signs are processed by python macros in `libtooling/atdlib`. For instance, `@CaptureDecl@` gives `capture_decl`.

##### Overriding node tuples when outputting data

When the visiting method for nodes of given type is effectively written, it is expected that the
corresponding `#define xxxx_decl_tuple` is overwritten to add the specific information of the kind of nodes.
It is important to name tuples correctly. For example, for `SomeNewNodeDecl`, tuple needs to be named as `some_new_node_decl_tuple`. Failure to name them exactly like this will result in picking up default value for tuple node instead of overridden one.

It is also required to specify how many fields tuple for given node has via `XxxTupleSize()` method

```

//@atd #define decl_tuple decl_info
//@atd type decl_info = {
//@atd    (* ... *)
//@atd } <ocaml field_prefix="di_">

template <class ATDWriter>
int ASTExporter<ATDWriter>::DeclTupleSize() {
  return 1; // decl_tuple has size of 1 (decl_info)
}

// Decl is the top class. Everything here concerns all declarations nodes.
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDecl(const Decl *D) {
// ...
}


//@atd #define named_decl_tuple decl_tuple * string   (* must start with the tuple of the base class *)
template <class ATDWriter>
int ASTExporter<ATDWriter>::NamedDeclTupleSize() {
  return DeclTupleSize() + 1; // named_decl_tuple has size of decl_tuple + 1 (for string field)
}

// Some important intermediate abstract class NamedDecl.
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitNamedDecl(const NamedDecl *D) {
  // Must visit the base class to output its tuple of information.
  VisitDecl(D);

  // Extra information for the derived class.
  OF.emitString(D->getNameAsString());
}
```

##### Putting everything together

The final definitions of the `xxx_decl_tuple` are meant to be inlined in the declaration of the actual sum type for all declarations.

```
// main variant for declarations
//@atd type decl = [
#define DECL(DERIVED, BASE)   //@atd   | DERIVED@@Decl of (@DERIVED@_decl_tuple)
#define ABSTRACT_DECL(DECL)
#include <clang/AST/DeclNodes.inc>
//@atd ]

```

This expands first to: (see `build/ast_inline.atd.p`)
```
type decl = [
| AccessSpecDecl of (access_spec_decl_tuple)
| BlockDecl of (block_decl_tuple)
| CapturedDecl of (captured_decl_tuple)
| ClassScopeFunctionSpecializationDecl of (class_scope_function_specialization_decl_tuple)
(* ... *)
]
```

Then after a last stage of preprocessing: (see `build/clang_ast.atd`)
```
type decl = [
    AccessSpecDecl of (decl_info)
  | BlockDecl
      of (decl_info * decl list * decl_context_info * block_decl_info)
  | CapturedDecl of (decl_info * decl list * decl_context_info)
  | ClassScopeFunctionSpecializationDecl of (decl_info)
(* ... *)
]
```
