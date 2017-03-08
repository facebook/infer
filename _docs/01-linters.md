---
docid: linters
title: "Infer : Linters"
layout: docs
permalink: /docs/linters.html
---

For iOS apps, we provide a linters framework. These are checks about the syntax of the program; it could be about a property, or about code inside one method, or that a class or method have certain properties. We provide [a few checks](/docs/linters-bug-types.html) and the framework is also easy to extend. Soon we will have a domain specific language (DSL) to make it even easier to write checks.


The linters are run by default when you run Infer. However, there is a way of running only the linters, which is faster than also running Infer. This is by adding the option `-a linters` to the analysis command as in this example:

```bash
infer -a linters -- clang -c Test.m
```

**AL: a Domain Specific Language to write AST linters**

Infer has now a DSL to specify new linters. The purpose of this language is to simplify as much as possible the process of adding new linters. By using the DSL is now possible to specify linters in a declarative way and just add it to a linters' file. The new linters will then work out of the box without the need to recompile Infer. Moreover to modify and/or debug your linters is enough to just update the linters' file.

Let's start with an example. Suppose we want to write the following Objectvie-C's linter:

  *"a property containing the word 'delegate', but not containing the word 'queue' should not be declared strong"*. 

We can write this property in the following way:


```bash
  DEFINE-CHECKER STRONG_DELEGATE_WARNING = {

  LET name_contains_delegate = property_name_contains_word(delegate);
  LET name_does_not_contains_queue = NOT property_name_contains_word(queue);

  SET report_when =
	    WHEN
               name_contains_delegate 
               AND name_does_not_contains_queue 
               AND is_strong_property()
	    HOLDS-IN-NODE ObjCPropertyDecl;

  SET message = "Property or ivar %decl_name% declared strong";
  SET suggestion = "In general delegates should be declared weak or assign";
  
  };
  ```
  
  
  
  
  
