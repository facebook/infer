---
id: analyzing-ios-app
title: Analyzing an iOS app
layout: docs
permalink: /docs/analyzing-ios-app.html
section: User Guide
section_order: 01
order: 01
---
## Run infer on a sample iOS app
Download the example iOS app that contains some bugs and run infer on it:

```bash
wget localhost:4000/downloads/InferiOSExample.tar.gz
tar xvfz InferiOSExample.tar.gz
cd HelloWorldApp
xcodebuild -target HelloWorldApp -configuration Debug -sdk iphonesimulator clean
infer -- xcodebuild -target HelloWorldApp -configuration Debug -sdk iphonesimulator
```

Infer will output the list of found bugs:

```bash
/Users/dulmarod/works/InferAnalyzer/clang_examples/HelloWorldApp/HelloWorldApp/AppDelegate.m:22: error: NULL_DEREFERENCE
  [B1] pointer hello last assigned on line 21 could be null and is dereferenced at line 22, column 13

/Users/dulmarod/works/InferAnalyzer/clang_examples/HelloWorldApp/HelloWorldApp/AppDelegate.m:26: error: MEMORY_LEAK
  [CF] memory dynamically allocated to shadowPath by call to CGPathCreateWithRect() at line 26, column 28 is not reachable after line 26, column 5

/Users/dulmarod/works/InferAnalyzer/clang_examples/HelloWorldApp/HelloWorldApp/AppDelegate.m:31: error: RESOURCE_LEAK
   resource acquired to fp by call to fopen() at line 31, column 8 is not released after line 31, column 5

/Users/dulmarod/works/InferAnalyzer/clang_examples/HelloWorldApp/HelloWorldApp/AppDelegate.m:50: error: NULL_DEREFERENCE
  [B1] pointer str last assigned on line 49 could be null and is dereferenced at line 50, column 24

/Users/dulmarod/works/InferAnalyzer/clang_examples/HelloWorldApp/HelloWorldApp/AppDelegate.m:55: error: PREMATURE_NIL_TERMINATION_ARGUMENT
  [B1] pointer str last assigned on line 54 could be nil which results in a call to arrayWithObjects: with 1 arguments instead of 3 (nil indicates that the last argument of this variadic method has been reached) at line 55, column 24
  ```

## Running infer using xcodebuild

```bash
$> xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator clean
$> infer -- xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator
```

## Viewing results
A list of bugs will appear in the standard output.
