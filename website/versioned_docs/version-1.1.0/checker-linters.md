---
title: "AST Language (AL)"
description: "Declarative linting framework over the Clang AST."
---

Declarative linting framework over the Clang AST.

**\*\*\*DEPRECATED\*\*\*** On end-of-life support, may be removed in the future.

Activate with `--linters`.

Supported languages:
- C/C++/ObjC: Yes
- Java: No
- C#/.Net: No

For C/C++ and Objective-C languages, we provide a linters framework. These are
checks about the syntax of the program; it could be about a property, or about
code inside one method, or that a class or method have certain properties. We
provide [a few checks by default](#list-of-issue-types) and we have developed a domain
specific language (DSL) to make it easier to write checks.

## AL: A declarative language for writing linters in Infer

One of the major advantage of Infer when compared with other static analyzers is
the fact it performs sophisticated inter-procedural/inter-file analysis. That
is, Infer can detect bugs which involve tracking values through many procedure
calls and the procedures may live in different files. These may be very subtle
bugs and designing static analyses to do that is quite involved and normally
requires deep static analysis expertise.

However, there are many important software bugs that are confined in the code of
a single procedure (called intra-procedural). To detect these bugs simpler
analyses may suffice which do not require deep technical expertise in static
analysis. Often these bugs can be expressed by referring to the syntax of the
program, or the types of certain expressions. We have defined a new language to
easily design checkers which identify these kind of bugs. The language is called
AL (AST Language) and its main feature is the ability to reason about the
Abstract Syntax Tree of a program in a concise declarative way. AL's checkers
are interpreted by Infer to analyze programs. Thus, to detect new kind of bugs
in Infer one can just write a check in AL. We will see in more detail later,
that for writing AL formulas we also need predicates: simple functions that
check a property of the AST. Predicates are written in OCaml inside Infer, thus
it requires a bit of OCaml knowledge and getting familiar with the OCaml data
structure for the clang AST.

## Getting the clang AST

When you write a linter that traverses the AST of some programs to check some
property, you probably need to understand what the AST looks like. You can get
the AST of programs using clang directly, or using Infer.

If you have a clang command `clang <clang arguments> File.m` then you can get
the AST with

```bash
clang <clang arguments> -Xclang -ast-dump -fsyntax-only File.m
```

You can also get the AST using Infer. One advantage of this is that you don't
need to know the speicifc clang command, just the general build command.
Moreover, what you get here is exactly the form of the AST that Infer has as
input.

For this you need to install an OCaml package `biniou` with
`opam install biniou`. See [the opam website](https://opam.ocaml.org/) for
instructions on how to install opam.

Then, the AST can be created by Infer in debug mode. Call Infer with

```bash
infer --debug -- <build command>
```

This will, among other things, generate a file `/path/to/File.m.ast.sh` for
every file `/path/to/File.m` that is being analyzed. Run this script with
`bash File.m.ast.sh` and a file `/path/to/File.m.ast.bdump` will be generated,
that contains the AST of the program in `bdump` format (similar to json). If you
get an error about `bdump` not being found you may need to run
`eval $(opam env)` to get the `bdump` executable (provided by the biniou opam
package) into your `PATH`.

For general info on the clang AST, you can check out
[clang's website](http://clang.llvm.org/docs/IntroductionToTheClangAST.html).

## Using AL to write linters

Let's start with an example. Suppose we want to write the following
Objective-C's linter:

_"a property containing the word 'delegate', but not containing the word 'queue'
should not be declared strong"_.

We can write this property in the following way:

```bash
DEFINE-CHECKER STRONG_DELEGATE_WARNING = {

    LET name_contains_delegate =
        declaration_has_name(REGEXP("[dD]elegate"));

    LET name_does_not_contain_queue =
        NOT declaration_has_name(REGEXP("[qQ]ueue"));

    SET report_when =
        WHEN
           name_contains_delegate
           AND name_does_not_contain_queue
           AND is_strong_property()
        HOLDS-IN-NODE ObjCPropertyDecl;

    SET message = "Property or ivar %decl_name% declared strong";
    SET suggestion = "In general delegates should be declared weak or assign";
    SET severity = "WARNING"
  };
```

The linter definition starts with the keyword `DEFINE-CHECKER` followed by the
checker's name. The first `LET` clause defines the _formula variable_
`name_contains_delegate` using the predicate `declaration_has_name` which return
true/false depending whether the property's name contains a word in the language
of the regular expression `[dD]elegate`. In general a predicate is a simple
atomic formula evaluated on an AST node. The list of available predicates is in
the module
[`cPredicates.mli`](https://github.com/facebook/infer/blob/main/infer/src/clang/cPredicates.mli)
(this list is continuously growing and if you need a new predicate you can add
it in ocaml). Formula variables can be used to simplify other definitions. The
`SET report_when` is mandatory and defines a formula that, when evaluates to
true, will tell Infer to report an error. In the case above, the formula is
saying that we should report when visiting an `ObjCPropertyDecl` (that is the
AST node declaring a property in Objective-C) where it holds that: the name
contains "delegate/Delegate" (`name_contains_delegate`) and the name doesn't
contain "queue/Queue" (`name_does_not_contain_queue`) and the node is defining a
"strong" property (`is_strong_property()`).

The `SET message` clause defines the error message that will be displayed to the
user. Notice that the message can include placeholders like `%decl_name%`.
Placeholders are evaluated by Infer and substituted by their current value when
the error message is reported. In this case the name of the declaration. The
`SET suggestion` clause define an optional hint to give to programmer on how to
fix the problem.

The general structure of a checker is the following:

```bash
DEFINE-CHECKER id_of_the_checker = {

     LET formula = <formula definition>;
     LET ….

     SET report_when = <formula definition>;

     SET name = <optional name>;
     SET message = <error message to show the user>;
     SET suggestion = <optional suggestion to the user>;
     SET doc_url = <optional URL to documentation of the issue>;
     SET severity = INFO | LIKE | ADVICE | WARNING | ERROR;
     SET mode = ON | OFF
     SET whitelist_path = {path1, path2, ..., pathn };
     SET blacklist_path = {path1, path2, ..., pathn };
  };
```

The default severity is `WARNING` and the default mode is `ON`, so these are
optional. If the check is `OFF` it will only be available in debug mode (flags
`--debug` or `--linters-developer-mode`). `INFOs` are generally also not
reported, except with some specialzed flags. `name` and `doc_url` are used only
for CI comments at the moment (in Phabricator).

## Defining Paths

`whitelist_path` and `blacklist_path` are optional, by default the rule is
enabled everywhere. For specifying paths, one can use either string constants
(`"File.m"`) or regexes (`REGEXP("path/to/.*")`) or variables. The variables
stand for a list of paths, and are defined in a separate block:

```bash
 GLOBAL-PATHS {
     path1 = {"A.m", REGEXP("path/to/.*")};
  };
```

## Defining Macros

It is possible to define macros that can be used in several checkers. This is
done in the following way:

```bash
GLOBAL-MACROS {
  LET is_subclass_of(x) =
        is_class(x) HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;
 };
```

`GLOBAL-MACROS` is the section of an AL specification where one can define a
list of global macros. In the example we are defining the macro `is_subclass(x)`
which can now be used in checkers instead of its complex definition.

It is possible to import a library of macros and paths with the following
command:

```
#IMPORT <library.al>
```

In an AL file, the command above import and make available all the macros and
paths defined in the `library.al` file.

## AL Predicates

The simplest formulas we can write are predicates. They are defined inside
Infer. We provide a
[library](https://github.com/facebook/infer/blob/main/infer/src/clang/cPredicates.mli),
but if the predicate that you require is not available, you will need to extend
the library. Here are the some of the currently defined predicates:

```
call_class_method ("class_name", "method_name")
call_function ("method_name")
call_instance_method ("class_name", "method_name")
call_method ("method_name")
captures_cxx_references ()
context_in_synchronized_block ()
declaration_has_name ("decl_name")
declaration_ref_name ("decl_ref_name")
decl_unavailable_in_supported_ios_sdk ()
has_cast_kind("cast_kind") // useful in a cast node
has_type ("type") // only builtin types, pointers and Objective-C classes available at the moment
isa ("class_name")
is_assign_property ()
is_binop_with_kind ("kind")
is_class ("class_name")
is_const_var ()
is_global_var ()
is_ivar_atomic ()
is_ivar_readonly ()
is_method_property_accessor_of_ivar ()
is_node ("node_name")
is_objc_constructor ()
is_objc_dealloc ()
is_objc_extension ()
is_objc_interface_named ("name")
is_property_pointer_type ()
is_strong_property ()
is_weak_property ()
is_unop_with_kind ("kind")
method_return_type ("type") // only builtin type, pointers, and Objective-C classes available at the moment
objc_method_has_nth_parameter_of_type("type")
using_namespace("namespace")
within_responds_to_selector_block ()
```

In general, the parameters of predicates can be constants, or variables, or
regular expressions. Variables are used in macros, see below. The syntax for
using regexes is `REGEX("your_reg_exp_here")`.

**NOTE:** The predicates that expect types, such as `has_type` or
`method_return_type` or `objc_method_has_nth_parameter_of_type` also accept
regexes, but the syntax is a bit different: `REGEX('your_reg_exp_here')`, and
this regex can be embedded inside another string, for example:
`has_type("REGEXP('NS.+')*" )` which stands for pointer to a class of name
starting with NS.

If you need to add a new predicate, write the predicate in
[cPredicates.ml](https://github.com/facebook/infer/blob/main/infer/src/clang/cPredicates.ml)
and then register it in
[CTL.ml](https://github.com/facebook/infer/blob/main/infer/src/clang/cTL.ml#L728).

## AL Formulas

Formulas are defined using a variation of the
[_CTL temporal logic_](https://en.wikipedia.org/wiki/Computation_tree_logic).
CTL is a logic expressing properties of a tree model. In the case of AL, the
tree is the AST of the program. Formulas are defined according to the following
grammar:

```
formula ::= predicate
          | NOT formula
          | formula1 OR formula2
          | formula1 AND formula2
          | formula1 IMPLIES formula2
          | formula1 HOLDS-UNTIL formula2
          | formula1 HOLDS-EVERYWHERE-UNTIL formula2
          | formula HOLDS-EVENTUALLY
          | formula HOLDS-EVERYWHERE-EVENTUALLY
          | formula HOLDS-NEXT
          | formula HOLDS-EVERYWHERE-NEXT
          | formula HOLDS-ALWAYS
          | formula HOLDS-EVERYWHERE-ALWAYS
          | WHEN formula HOLDS-IN-NODE node-name-list
          | IN-NODE node-name-list WITH-TRANSITION transition-name
               formula HOLDS-EVENTUALLY
```

The first four cases (`NOT`, `OR`, `AND`, `IMPLIES`) are classic boolean
operators with the usual semantics. The others are temporal operators describing
how the truth-value of a formula is evaluated in a tree. Let's consider case by
case.

| Formula             |                                       Semantic meaning                                        |
| ------------------- | :-------------------------------------------------------------------------------------------: |
| F1 _HOLDS-UNTIL_ F2 | from the current node, there exists a path where F1 holds at every node until F2 becomes true |

An example is depicted in the following tree. When `F1` or `F2` hold in a node
this is indicated between square brackets. The formula `F1 HOLDS-UNTIL F2` holds
in the green nodes.

![](/img/AL/holds_until.jpeg)

---

| Formula              |                               Semantic meaning                               |
| -------------------- | :--------------------------------------------------------------------------: |
| F _HOLDS-EVENTUALLY_ | from the current node there exists a path where at some point F becomes true |

In the picture below, as `F` holds in `n10`, then `F HOLDS-EVENTUALLY` holds in
the green nodes `n1`, `n7`, `n10`. This is because from these nodes there is a
path reaching `n10` where `F` holds. Note that it holds for `n10` as well
because there exists a trivial path of length 0 from `n1` to itself.

![](/img/AL/holds_eventually.jpeg)

---

| Formula                       |                             Semantic meaning                              |
| ----------------------------- | :-----------------------------------------------------------------------: |
| F HOLDS-EVERYWHERE-EVENTUALLY | in every path starting from the current node at some point F becomes true |

For example, in the tree below, the formula holds in every green node because
every paths starting from each of them eventually reaches a node where F holds.

![](/img/AL/holds_everywhere_eventually.jpeg)

---

| Formula      |                               Semantic meaning                               |
| ------------ | :--------------------------------------------------------------------------: |
| F HOLDS-NEXT | from the current node (we are visiting) there exists a child where F is true |

In the tree below, the formula `F HOLDS-NEXT` it is true only in n1 as it's the
only node with a child where `F` holds (node n3). In AL, `NEXT` is synonym of
child as, in terms of a path in the tree, a child is the next node.

![](/img/AL/holds_next.jpeg)

---

| Formula                 |                    Semantic meaning                     |
| ----------------------- | :-----------------------------------------------------: |
| F HOLDS-EVERYWHERE-NEXT | from the current node in every existing child F is true |

In the tree below, the formula `F HOLDS-EVERYWHERE-NEXT` it is true in n1 as
it's the only node for which in every child `F` holds (node n2, n3, and n7).

![](/img/AL/holds_everywhere_next.jpeg)

---

| Formula        |                           Semantic meaning                            |
| -------------- | :-------------------------------------------------------------------: |
| F HOLDS-ALWAYS | from the current node there exists a path where F holds at every node |

In the tree below `F HOLDS-ALWAYS` holds in `n1`, `n2`, `n8` because for each of
these nodes there exists a path where `F` holds at each node in the path.

![](/img/AL/always_holds.jpeg)

---

| Formula                   |                      Semantic meaning                      |
| ------------------------- | :--------------------------------------------------------: |
| F HOLDS-EVERYWHERE-ALWAYS | from the current node, in every path F holds at every node |

`F HOLDS-EVERYWHERE-ALWAYS` holds in `n2`, `n4`, `n5`, and `n8` because when we
visit those nodes in every path that start from them `F` holds in every node.

![](/img/AL/always_holds_everywhere.jpeg)

---

| Formula                            |                 Semantic meaning                 |
| ---------------------------------- | :----------------------------------------------: |
| WHEN F HOLDS-IN-NODE node1,…,nodeK | we are in a node among node1,…,nodeK and F holds |

`WHEN F HOLDS-IN-NODE` `n2`, `n7`, `n6` holds only in node `n2` as it is the
only node in the list `n2`, `n7`, `n6` where F holds.

![](/img/AL/holds_in_node.jpeg)

Let's consider an example of checker using formula
`WHEN F HOLDS-IN-NODE node1,…,nodeK` for checking that a property with pointer
type should not be declared _"assign"_:

```
DEFINE-CHECKER ASSIGN_POINTER_WARNING = {

      SET report_when =
          WHEN
            is_assign_property() AND is_property_pointer_type()
          HOLDS-IN-NODE ObjCPropertyDecl;

      SET message = "Property `%decl_name%` is a pointer type marked with the `assign` attribute";
      SET suggestion = "Use a different attribute like `strong` or `weak`.";
      SET severity = "WARNING";
  };
```

The checker uses two predefined predicates `is_assign_property()` and
`is_property_pointer_type()` which are true if the property being declared is
assign and has a pointer type respectively. We want to check both conditions
only on nodes declaring properties, i.e., `ObjCPropertyDecl`.

---

| Formula                                                     |                                                                  Semantic meaning                                                                  |
| ----------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------: |
| IN-NODE node1,…, nodeK WITH-TRANSITION t F HOLDS-EVENTUALLY | from the current node there exists a path which eventually reaches a node among “node1,…,nodeK” with a transition t reaching a child where F holds |

The following tree explain the concept:

![](/img/AL/in_node_with_transition.jpeg)

The concept of transition is needed because of the special structure of the
clang AST. Certain kind of nodes, for example statements, have a list of
children that are statements as well. In this case there is no special tag
attached to the edge between the node and the children. Other nodes have
records, where some of the fields point to other nodes. For example a node
representing a function declaration will have a record where one of the fields
is body. This is pointing to a statement representing the function's body. For
records, sometimes we need to specify that we need a particular node reachable
via a particular field (i.e., a transition).

**Hint** A good way to learn how to write checkers is looking at existing
checkers in the file
[linters.al](https://github.com/facebook/infer/blob/main/infer/lib/linter_rules/linters.al).

## Example checks

In the following we show a few examples of simple checks you may wish to write
and the corresponding formulas:

- A check for flagging a Objective-C class that inherits from a class that
  shouldn't be subclassed.

```
DEFINE-CHECKER SUBCLASSING_TEST_EXAMPLE = {
   SET report_when = is_class("A") HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;
   SET message = "This is subclassing A. Class A should not be subclassed.";
};
```

- A check for flagging an Objective-C instance method call:

```
DEFINE-CHECKER CALL_INSTANCE_METHOD = {
  SET report_when = call_instance_method("A", "foo:");
  SET message = "Do not call this method";
};
```

- A check for flagging an Objective-C instance method call of any method of a
  class:

```
DEFINE-CHECKER CALL_ANY_INSTANCE_METHODS = {
  SET report_when = call_instance_method(A, REGEXP("*"));
  SET message = "Do not call any method of class A";
};
```

- A check for flagging an Objective-C class method call:

```
DEFINE-CHECKER CALL_CLASS_METHOD = {
  SET report_when = call_class_method("A", "foo:");
  SET message = "Do not call this method";
};
```

- A check for flagging an Objective-C method call of a method with int return
  type:

```
DEFINE-CHECKER TEST_RETURN_METHOD = {
  SET report_when = WHEN method_return_type("int")
        HOLDS-IN-NODE ObjCMethodDecl;
  SET message = "Method return int";
};
```

- A check for flagging a variable declaration with type long

```
DEFINE-CHECKER TEST_VAR_TYPE_CHECK = {
  SET report_when = WHEN has_type("long")
	    HOLDS-IN-NODE VarDecl;
  SET message = "Var %name% has type long";
};
```

- A check for flagging a method that has a parameter of type A\*

```
DEFINE-CHECKER TEST_PARAM_TYPE_CHECK = {
  LET method_has_a_parameter_with_type(x) =
        WHEN HOLDS-NEXT WITH-TRANSITION Parameters (has_type(x))
        HOLDS-IN-NODE ObjCMethodDecl;
  SET report_when =
           method_has_a_parameter_with_type("A*" );
  SET message = "Found a method with a parameter of type A";
};
```

- A check for flagging a method that has all the parameters of type A\* (and at
  least one)

```
DEFINE-CHECKER TEST_PARAM_TYPE_CHECK2 = {
  LET method_has_at_least_a_parameter =
      WHEN HOLDS-NEXT WITH-TRANSITION Parameters (TRUE)
       HOLDS-IN-NODE ObjCMethodDecl;

   LET method_has_all_parameter_with_type(x) =
        WHEN HOLDS-EVERYWHERE-NEXT WITH-TRANSITION Parameters (has_type(x))
        HOLDS-IN-NODE ObjCMethodDecl;

  SET report_when = method_has_at_least_a_parameter AND
                    method_has_all_parameter_with_type("int");
  SET message = "All the parameters of the method have type int";
};
```

- A check for flagging a method that has the 2nd parameter of type A\*

```
DEFINE-CHECKER TEST_NTH_PARAM_TYPE_CHECK = {
  SET report_when =
    WHEN objc_method_has_nth_parameter_of_type("2", "A*")
    HOLDS-IN-NODE ObjCMethodDecl;
  SET message = "Found a method with the 2nd parameter of type A*";
  SET severity = "LIKE";
};
```

- A check for flagging a protocol that inherits from a given protocol.
  `HOLDS-EVENTUALLY WITH-TRANSITION Protocol` means follow the `Protocol` branch
  in the AST until the condition holds.

```
DEFINE-CHECKER TEST_PROTOCOL_DEF_INHERITANCE = {
  LET is_subprotocol_of(x) = declaration_has_name(x) HOLDS-EVENTUALLY WITH-TRANSITION Protocol;
  SET report_when =
    WHEN is_subprotocol_of("P")
    HOLDS-IN-NODE ObjCProtocolDecl;
  SET message = "Do not inherit from Protocol P";
};
```

- A check for flagging when a constructor is defined with a parameter of a type
  that implements a given protocol (or that inherits from it).
  `HOLDS-NEXT WITH-TRANSITION Parameters` means, starting in the
  `ObjCMethodDecl` node, follow the `Parameters` branch in the AST and check
  that the condition holds there.

```
DEFINE-CHECKER TEST_PROTOCOL_TYPE_INHERITANCE = {

  LET method_has_parameter_subprotocol_of(x) =
            WHEN
             HOLDS-NEXT WITH-TRANSITION Parameters
                (has_type_subprotocol_of(x))
             HOLDS-IN-NODE ObjCMethodDecl;

  SET report_when =
      WHEN
        declaration_has_name(REGEXP("^newWith.*:$")) AND
        method_has_parameter_subprotocol_of("P")
      HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "Do not define parameters of type P.";
};
```

- A check for flagging a variable declaration of type NSArray applied to A.

```
DEFINE-CHECKER TEST_GENERICS_TYPE = {
  SET report_when =
      WHEN has_type("NSArray<A>*")
      HOLDS-IN-NODE VarDecl;
  SET message = "Do not create arrays of type A";
};
```

- A check for flagging using a property or variable that is not available in the
  supported API. decl_unavailable_in_supported_ios_sdk is a predicate that works
  on a declaration, checks the available attribute from the declaration and
  compares it with the supported iOS SDK. Notice that we flag the occurrence of
  the variable or property, but the attribute is in the declaration, so we need
  the transition `PointerToDecl` that follows the pointer from the usage to the
  declaration.

```
DEFINE-CHECKER UNAVAILABLE_API_IN_SUPPORTED_IOS_SDK = {
  SET report_when =
	WHEN HOLDS-NEXT WITH-TRANSITION PointerToDecl
	 (decl_unavailable_in_supported_ios_sdk() AND
	HOLDS-IN-NODE DeclRefExpr;
  SET message = "%name% is not available in the required iOS SDK version";
};
```

- A check for flagging using a given namespace

```
DEFINE-CHECKER TEST_USING_NAMESPACE = {
  SET report_when = using_namespace("N");
  SET message = "Do not use namespace N";
};
```

- A check for flagging the use of given enum constants

```
DEFINE-CHECKER ENUM_CONSTANTS = {
  SET report_when = is_enum_constant(REGEXP("MyName.*"));
  SET message = "Do not use the enum MyName";
};
```

## AST info in messages

When you write the message of your rule, you may want to specify which
particular AST items were involved in the issue, such as a type or a variable
name. We have a mechanism for that, we specified a few placeholders that can be
used in rules with the syntax `%placeholder%` and it will be substituted by the
correct AST info. At the moment we have `%type%`, `%child_type%` and `%name%`
that print the type of the node, the type of the node's child, and a string
representation of the node, respectively. As with predicates, we can add more as
needed.

## Testing your rule

To test your rule you need to run it with Infer. If you are adding a new linter
you can test it in a separate al file that you can pass to Infer with the option
`--linters-def-file file.al`. Pass the option
`--linters-developer-mode --linter <LINTER_NAME>` to Infer to print debug
information and only run the linter you are developing, so it will be faster and
the debug info will be only about your linter.

To test your code, write a small example that triggers the rule. Then, run your
code with

```
infer --linters-developer-mode --linters-def-file file.al -- clang -c Test.m
```

the bug should be printed in the screen, like, for instance:

```
infer/tests/codetoanalyze/objcpp/linters/global-var/B.mm:34: warning: GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL
  Global variable kLineSize is initialized using a function or method call at line 34, column 1. If the function/method call is expensive,
  it can affect the starting time of the app.
  32.   static float kPadding = [A bar] ? 10.0 : 11.0; // Error
  33.
  34. > static const float kLineSize = 1 / [A scale]; // Error
  35.
  36.   static const float ok = 37;
  37.
```

Moreover, the bug can be found in the file `infer-out/report.json` where
`infer-out` is the results directory where Infer operates, that is created in
the current directory. You can specify a different directory with the option
`-o`.

## Debugging

If there are syntax errors or other parsing errors with your al file, you will
get an error message when testing the rule, remember to use
`linters-developer-mode` when you are developing a rule. If the rule gets parsed
but still doesn't behave as you expect, you can debug it, by adding the
following line to a test source file in the line where you want to debug the
rule: `//INFER_BREAKPOINT`. Then run infer again in linters developer mode, and
it will stop the execution of the linter on the line of the breakpoint. Then you
can follow the execution step by step. It shows the current formula that is
being evaluated, and the current part of the AST that is being checked. A red
node means that the formula failed, a green node means that it succeeded.

## Demo

<iframe src="https://www.facebook.com/plugins/video.php?href=https%3A%2F%2Fwww.facebook.com%2Finferstaticanalyzer%2Fvideos%2F810308939133850%2F&show_text=0&width=400" width="500" height="500" scrolling="no" frameborder="0" allowTransparency="true" allowFullScreen="true"></iframe>

## Command line options for linters

The linters are run by default when you run Infer. However, there is a way of
running only the linters, which is faster than also running Infer. This is by
adding the option `--linters` to the analysis command as in this example:

```bash
infer run --linters -- clang -c Test.m
```

There are a few other command-line options that are useful for using or
developing new linters in Infer. Read about them in the [`infer capture` manual](/docs/1.1.0/man-infer-capture).


## List of Issue Types

The following issue types are reported by this checker:
- [ASSIGN_POINTER_WARNING](/docs/1.1.0/all-issue-types#assign_pointer_warning)
- [BAD_POINTER_COMPARISON](/docs/1.1.0/all-issue-types#bad_pointer_comparison)
- [COMPONENT_WITH_MULTIPLE_FACTORY_METHODS](/docs/1.1.0/all-issue-types#component_with_multiple_factory_methods)
- [CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK](/docs/1.1.0/all-issue-types#cxx_reference_captured_in_objc_block)
- [DIRECT_ATOMIC_PROPERTY_ACCESS](/docs/1.1.0/all-issue-types#direct_atomic_property_access)
- [DISCOURAGED_WEAK_PROPERTY_CUSTOM_SETTER](/docs/1.1.0/all-issue-types#discouraged_weak_property_custom_setter)
- [GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL](/docs/1.1.0/all-issue-types#global_variable_initialized_with_function_or_method_call)
- [MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE](/docs/1.1.0/all-issue-types#mutable_local_variable_in_component_file)
- [POINTER_TO_CONST_OBJC_CLASS](/docs/1.1.0/all-issue-types#pointer_to_const_objc_class)
- [STRONG_DELEGATE_WARNING](/docs/1.1.0/all-issue-types#strong_delegate_warning)
