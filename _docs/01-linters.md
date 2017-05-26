---
docid: linters
title: "Infer : AL"
layout: docs
permalink: /docs/linters.html
---

For iOS apps, we provide a linters framework. These are checks about the syntax of the program; it could be about a property, or about code inside one method, or that a class or method have certain properties. We provide [a few checks](/docs/linters-bug-types.html) and we have developed a domain specific language (DSL) to make it easier to write checks.

- [AL: A declarative language for writing linters in Infer](/docs/linters.html#al_intro)
- [Background on the clang AST](/docs/linters.html#clang_ast)
- [Using AL to write linters](/docs/linters.html#write_linters) 
  - [AL Predicates](/docs/linters.html#predicates) 
  - [AL Formulas](/docs/linters.html#formulas) 
  - [Defining Macros](/docs/linters.html#macros) 
  - [Testing your rule](/docs/linters.html#testing)  
  - [Debugging](/docs/linters.html#debugging) 
  - [Command line options for linters](/docs/linters.html#command_line) 


<a name="al_intro">**AL: A declarative language for writing linters in Infer**</a>

One of the major advantage of Infer when compared with other static analyzers is the fact it performs sophisticated inter-procedural/inter-file analysis. That is, Infer can detect bugs which involve tracking values through many procedure calls and the procedures may live in different files. These may be very subtle bugs and designing static analyses to do that is quite involved and normally requires deep static analysis expertise.

However, there are many important software bugs that are confined in the code of a single procedure (called intra-procedural). To detect these bugs simpler analyses may suffice which do not require deep technical expertise in static analysis. Often these bugs can be expressed by referring to the syntax of the program, or the types of certain expressions. We have defined a new language to easily design checkers which identify these kind of bugs. The language is called AL (AST Language) and its main feature is the ability to reason about the Abstract Syntax Tree of a program in a concise declarative way. AL's checkers are interpreted by Infer to analyze programs. Thus, to detect new kind of bugs in Infer one can just write a check in AL without any knowledge of the internal of Infer.

Once the new linter is added to the linters' file it will then work out of the box without the need to recompile Infer. Moreover to modify and/or debug your linters is enough to just update the linters' file.


<a name="clang_ast">**Background on the clang AST**</a>  

First of all, get familiar with the `decl` and `stmt` data structures of the ast in infer/infer/src/clang/clang_ast_t.mli. This is a generated file. `decl` is the type for declarations and  contains items such as `ObjCInterfaceDecl`, `ObjCPropertyDecl`, `ObjCMethodDecl`, etc. `stmt` is a type for statements and contains items such as `ObjCMessageExpr`, `IfStmt`, etc. For information on those names, you can google them, and you'll find the clang docs.

More important is to be able to map source code to its ast components. You can do this in two ways. Say your file is called Test.m. The first one is with the command

```bash
clang -Xclang -ast-dump -fsyntax-only Test.m
```
and the other one is using Infer. First, call Infer with

```bash
infer --stats -- clang -c Test.m
```

where the part after the `--` is the clang command you would use to compile the code. This will, among other things, generate a file Test.o.sh in the current directory. Run this script with bash Test.o.sh and a file Test.o.bdump will be generated, that contains the ast of the program in a readable format.


<a name="write_linters">**Using AL to write linters**</a>

Let's start with an example. Suppose we want to write the following Objective-C's linter:

  *"a property containing the word 'delegate', but not containing the word 'queue' should not be declared strong"*. 

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
  };
```

The linter definition starts with the keyword `DEFINE-CHECKER` followed by the checker's name. The first `LET` clause defines the *formula variable* `name_contains_delegate` using the predicate `declaration_has_name` which return true/false depending whether the property's name contains a word in the language of the regular expression `[dD]elegate`. In general a predicate is a simple atomic formula evaluated on an AST node. The list of available predicates is in the module `Predicates.mli` (this list is continuously growing and if you need a new predicate you can add it in ocaml). Formula variables can be used to simplify other definitions. The `SET report_when` is mandatory and defines a formula that, when evaluates to true, will tell Infer to report an error. In the case above, the formula is saying that we should report when visiting an `ObjCPropertyDecl` (that is the AST node declaring a property in Objective-C) where it holds that: the name contains "delegate/Delegate" (`name_contains_delegate`) and the name doesn't contain "queue/Queue" (`name_does_not_contain_queue`) and the node is defining a "strong" property (`is_strong_property()`).

The `SET message` clause defines the error message that will be displayed to the user. Notice that the message can include placeholders like `%decl_name%`. Placeholders are evaluated by Infer and substituted by their current value when the error message is reported. In this case the name of the declaration. The `SET suggestion` clause define an optional hint to give to programmer on how to fix the problem. 

The general structure of a checker is the following:

```bash
DEFINE-CHECKER name_of_the_checker = {

     LET formula = <formula definition>;
     LET ….

     SET report_when = <formula definition>;
    
     SET message = <error message to show the user>;
     SET suggestion = <optional suggestion to the user>;

  };
```

Formulas are defined using a variation of the *CTL temporal logic*. CTL is a logic expressing properties of a tree model. In the case of AL, the tree is the AST of the program. Formulas are defined according to the following grammar:

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

<a name="predicates">**AL Predicates**</a> 

The predicates are defined inside Infer. We provide a library, and can add more as needed. Here are the currently defined predicates:

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
has_type ("type") // only builtin types, pointers and Objective-C classes available at the moment
isa ("class_name")
is_assign_property ()
is_binop_with_kind ("kind")
is_class ("class_name")
is_const_var ()
is_global_var ()
is_ivar_atomic ()
is_method_property_accessor_of_ivar ()
is_node ("node_name")
is_objc_constructor ()
is_objc_dealloc ()
is_objc_extension ()
is_objc_interface_named ("name")
is_property_pointer_type ()
is_strong_property ()
is_unop_with_kind ("kind")
method_return_type ("type") // only builtin type, pointers, and Objective-C classes available at the moment
within_responds_to_selector_block ()
```

In general, the parameters of predicates can be constants, or variables, or regular expressions. Variables are used in macros, see below. The syntax for using regexes is `REGEX("your_reg_exp_here")`.


<a name="formulas">**AL Formulas**</a>

The first four cases (`NOT`, `OR`, `AND`, `IMPLIES`) are classic boolean operators with the usual semantics. The others are temporal operators describing how the truth-value of a formula is evaluated in a tree. Let's consider case by case.


| Formula | Semantic meaning |
| ------- |:----------------:|
|F1 *HOLDS-UNTIL* F2 | from the current node, there exists a path where F1 holds at every node until F2 becomes true |

An example is depicted in the following tree. When `F1` or `F2` hold in a node this is indicated between square brackets. The formula `F1 HOLDS-UNTIL F2` holds in the green nodes.

![](static/images/AL/holds_until.jpeg)

<hr>

| Formula | Semantic meaning |
| ------- |:----------------:|
| F *HOLDS-EVENTUALLY* | from the current node there exists a path where at some point F becomes true |

In the picture below, as `F` holds in `n10`, then `F HOLDS-EVENTUALLY` holds in the green nodes `n1`, `n7`, `n10`. This is because from these nodes there is a path reaching `n10` where `F` holds. Note that it holds for `n10` as well because there exists a trivial path of length 0 from `n1` to itself.

![](static/images/AL/holds_eventually.jpeg)

<hr>

| Formula | Semantic meaning |
| ------- |:----------------:|
| F HOLDS-EVERYWHERE-EVENTUALLY | in every path starting from the current node at some point F becomes true |

For example, in the tree below, the formula holds in every green node because every paths starting from each of them eventually reaches a node where F holds.

![](static/images/AL/holds_everywhere_eventually.jpeg)


<hr>

| Formula | Semantic meaning |
| ------- |:----------------:|
| F HOLDS-NEXT | from the current node (we are visiting) there exists a child where F is true |

In the tree below, the formula `F HOLDS-NEXT` it is true only in n1 as it's the only node with a child where `F` holds (node n3). In AL, `NEXT` is synonym of child as, in terms of a path in the tree, a child is the next node.

![](static/images/AL/holds_next.jpeg)

<hr>

| Formula | Semantic meaning |
| ------- |:----------------:|
| F HOLDS-EVERYWHERE-NEXT | from the current node in every existing child F is true |

In the tree below, the formula `F HOLDS-EVERYWHERE-NEXT` it is true in n1 as it's the only node for which in every child `F` holds (node n2, n3, and n7).


![](static/images/AL/holds_everywhere_next.jpeg)

<hr>

| Formula | Semantic meaning |
| ------- |:----------------:|
| F HOLDS-ALWAYS  | from the current node there exists a path where F holds at every node |

In the tree below `F HOLDS-ALWAYS` holds in `n1`, `n2`, `n8` because for each of these nodes there exists a path where `F` holds at each node in the path.


![](static/images/AL/always_holds.jpeg)

<hr>

| Formula | Semantic meaning |
| ------- |:----------------:|
| F HOLDS-EVERYWHERE-ALWAYS | from the current node, in every path F holds at every node |

`F HOLDS-EVERYWHERE-ALWAYS` holds in `n2`, `n4`, `n5`, and `n8` because when we visit those nodes in every path that start from them `F` holds in every node.


![](static/images/AL/always_holds_everywhere.jpeg)

<hr>

| Formula | Semantic meaning |
| ------- |:----------------:|
| WHEN F HOLDS-IN-NODE node1,…,nodeK | we are in a node among node1,…,nodeK and F holds |

`WHEN F HOLDS-IN-NODE` `n2`, `n7`, `n6` holds only in node `n2` as it is the only node in the list `n2`, `n7`, `n6` where F holds.

![](static/images/AL/holds_in_node.jpeg)

Let's consider an example of checker using formula `WHEN F HOLDS-IN-NODE node1,…,nodeK` for checking that a property with pointer type should not be declared *"assign"*:

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

The checker uses two predefined predicates `is_assign_property()` and `is_property_pointer_type()` which are true if the property being declared is assign and has a pointer type respectively. We want to check both condition only on nodes declaring properties, i.e., `ObjCPropertyDecl`.

<hr>

| Formula | Semantic meaning |
| ------- |:----------------:|
| IN-NODE node1,…, nodeK WITH-TRANSITION t F HOLDS-EVENTIALLY | from the current node there exists a path which eventually reaches a node among “node1,…,nodeK” with a transition t reaching a child where F holds |

The following tree explain the concept:

![](static/images/AL/in_node_with_transition.jpeg)

The concept of transition is needed because of the special structure of the clang AST. Certain kind of nodes, for example statements, have a list of children that are statements as well. In this case there is no special tag attached to the edge between the node and the children. Other nodes have records, where some of the fields point to other nodes. For example a node representing a function declaration will have a record where one of the fields is body. This is pointing to a statement representing the function's body. For records, sometimes we need to specify that we need a particular node reachable via a particular field (i.e., a transition).

<a name="macros">**Defining Macros**</a> 

It is possible to define macros that can be used in several checkers. This is done in the following way:

```
GLOBAL-MACROS {

  LET is_subclass_of(x) =
        is_class(x) HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;

 };
```

`GLOBAL-MACROS` is the section of an AL specification where one can define a list of global macros. In the example we are defining the macro `is_subclass(x)` which can now use in checkers instead of its complex definition. It's possible to import library of macros with the following command:

```
#IMPORT <library.al>
```

In an AL file, the command above import and make available all the macros defined in the `library.al` file.

**Hint**
A good way to learn how to write checkers is looking at existing checkers in the file `linters.al`.


<a name="testing">**Testing your rule**</a>

To test your rule you need to run it with Infer. If you are adding a new linter you can test it in a separate al file that you can pass to Infer with the option `--linters-def-file file.al`. Pass the option `--linters-developer-mode` to Infer that will print debug information and only take the linters from that file into account in the execution, it will ignore the default linters, so it will be faster and debug info will be only 
about your linter.

To test your code, write a small example that triggers the rule. Then, run your code with

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

Moreover, the bug can be found in the file `infer-out/report.json` where `infer-out` is the results directory where Infer operates, that is created in the current directory. You can specify a different directory with the option `-o`.


<a name="debugging">**Debugging**</a>

If there are syntax errors or other parsing errors with your al file, you will get an error message when testing the rule, remember to use `linters-developer-mode` when you are developing a rule. If the rule gets parsed but still doesn't behave as you expect, you can debug it, by adding the following line to a test source file in the line where yo want to debug the rule: `//INFER_BREAKPOINT`. Then run infer again in linters developer mode, and it will stop the execution of the linter in the line of the breakpoint. Then you can follow the execution step by step. It shows the current formula that is being evaluated, and the current part of the AST that is being checked. A red node means that the formula failed, a green node means that it succeeded.

<a name="command_line">**Command line options for linters**</a>

The linters are run by default when you run Infer. However, there is a way of running only the linters, which is faster than also running Infer. This is by adding the option `-a linters` to the analysis command as in this example:

```bash
infer run -a linters -- clang -c Test.m
```

There are a few other command line options that are useful for using or developing new linters in Infer. You can get those options with the command `infer-capture --help`:

<img style="float: left;" src="static/images/AL/linters_help.png">



