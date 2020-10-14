## Version 0.17.0

- There's a new  `--inefficient-keyset-iterator`  checker for finding inefficient uses of Java's keyset iterators that retrieve both key and value (on by default).
- Complete the set of Android thread annotations and Java nullability annotations. Updated artifacts are available on [Maven Central](https://search.maven.org/artifact/com.facebook.infer.annotation/infer-annotation/0.17.0/jar).
- `--starvation` is now on by default. This analysis catches problems with threads not being able to make progress due to locking issues, incorrect scheduling priorities, etc. For instance, on Android calling Future.get from a UiThread without a sensible timeout will be flagged as a starvation issue.
- New Objective-C linter for calls to `@optional` methods: `UNSAFE_CALL_TO_OPTIONAL_METHOD`, enabled by default.
- A new call-graph scheduler (`--call-graph-schedule`) improves performance of the analysis phase of Infer, especially when the number of files to analyze is less than available CPUs.
- A new flag `--oom-threshold` allows to throttle the analysis when the amount of free memory is below the provided threshold.
- New genrule based Buck/Java integration is much faster than the previous one, use with `--genrule-master-mode`.
- Infer's internal clang is now in version 8.0.0.
- Update to javalib 3.1 provides better compatibility with Java 9 and Java 11. Refer to [their change log](https://github.com/javalib-team/javalib/blob/master/CHANGELOG) for more details.
- Infer can now be built and run on MacOS Mojave without fiddling with `SDKROOT` (although you still might need it with non-standard toolchain setup).
- [β] __Pulse__ is a new experimental lifetime analysis for C++, give it a try with `--pulse`. Beware that it doesn't report much yet.
- `--ownership` checker was superseded by Pulse and removed.


## Version 0.16.0

Backend analyses:
- A brand new analysis to compute the runtime cost of methods and functions: passing `--cost` (off by default) to Infer will output a costs-report.json file describing, among others, the computational complexity of each function in the code using the big-O notation, eg `O(1)`, `O(list.length)`, ...
- The deadlock detection analysis has been ported to C++ and Objective-C and mainly focuses on self-deadlocks (taking a mutex twice). Activate with `--starvation` (off by default).
- The data race detector RacerD has been ported to Objective-C and detects races on fields protected by a C++ mutex. It reports "Thread Safety Violation" and "GuardedBy Violation" errors on Java and "Lock Consistency Violation" on C++ and Objective-C. Activate with `--racerd` (on by default).
- A progress bar is displayed while the analysis is running
- Countless improvements and tweaks, in particular in RacerD and in analyses for C++.

Frontends:
- Infer now ships with clang version 7.0.1
- Support for Java up to version 11


## Version 0.15.0

- switch infer license to MIT
- publish binaries
- [clang] lots of improvements to the frontend


## Version 0.14.0

- New checker: `--ownership` detects a subset of use-after-free issues due to bad manual memory management. This is a rough prototype of Rust-style borrow checker for C++. (enabled by default, C++)
- New checker: `--uninit` detects uses of uninitialized values (enabled by default, C/C++/Objective-C)
- New checker: `--racerd` now also detects inconsistent lock usage in C++. Also improved the lock domain to reduce false positives for all languages.
- Improved C++ support: destructors are now properly translated; addresses and pointers are handled more precisely
- Improved retain cycles detection (Objective-C)
- Upgraded the internal clang to clang 7
- [internal] SQLite is being used to store some of infer's analysis artefacts instead of storing them in files on disk. This improves analysis speed and reduces load on the OS.


## Version 0.13.1

This is a fix for the 0.13.0 release, whose build broke due to changes in opam.


## Version 0.13.0

- Infer now runs multiple checkers at the same time by default, including the biabduction analysis that was the previous and only default. In particular, we are pleased to introduce [RacerD](http://fbinfer.com/docs/racerd.html) for race detection in Java. The following checkers are activated by default: annotation reachability (Java), biabduction (C/C++/ObjC, Java), fragment retains view (Java), immutable cast (Java), liveness (C/C++/ObjC), printf args (Java), quandary (C/C++/ObjC, Java), RacerD (C/C++/ObjC, Java), SIOF (C/C++/ObjC). Each checker may report several issue types.
- Upgraded to clang 5.0
- Richer DSL for writing linters ([AL](http://fbinfer.com/docs/linters.html)), and a new default linter for `const` pointers in Objective-C
- Lots of perf improvements and bug fixes, and improved logging

Please note the following breaking changes:
- `-a eradicate` is now simply `--eradicate` and can run alongside other checkers
- `inferTraceBugs` is now the `explore` subcommand: `infer explore --help`
- infer now depends on sqlite


## Version 0.12.1

Hotfix release to update infer's opam dependencies to cope with upgrades of cppo in opam (in particular, #718).


## Version 0.12.0

- introduces *subcommands* and man pages for all subcommands
- This release introduces **AL**, a language for writing linters against the clang AST. AL lets you check [syntactic properties](http://fbinfer.com/docs/linters-bug-types.html) of source code by traversing the AST of the program. Using the included domain-specific language (DSL), you can [write your own set of checks](http://fbinfer.com/docs/linters.html).


## Version 0.11.0

- [Java] ThreadSafety analyzer is now on by default; run it with `infer -a checkers ...`. This checker will try and detect races (unprotected concurrent accesses with at least one write) in classes bearing the `@ThreadSafe` annotation.
- Infer now builds using OCaml 4.04.0.


## Version 0.10.0

- [Clang] C++ support.
- [Clang] Improved support for cmake and Xcode compilation databases. Use with `infer --compilation-database compile_commands.json` (for cmake and Buck), or with `infer --compilation-database-escaped compile_commands.json` (for xcbuild and xcpretty).
- [C++] New [SIOF Checker](http://fbinfer.com/docs/experimental-checkers.html).
- [iOS] New [linter for target SDK version](http://fbinfer.com/docs/linters-bug-types.html#UNAVAILABLE_API_IN_SUPPORTED_IOS_SDK). Use with `infer --iphoneos-target-sdk-version <min version you support> ...` or with `infer -a linters --iphoneos-target-sdk-version <min version you support> ...`.
- [Java] New [Thread Safety Checker](http://fbinfer.com/docs/experimental-checkers.html).
- [Java] Smarter analysis of dynamic dispatch.
- [Java] Improved Maven integration.
- [Java] `@SuppressWarnings` support removed. Use `@SuppressLint` instead. `android.annotation.SuppressLint` is only available on Android, but do let us know if that is an issue for you.


## Version 0.9.5

fix for #577


## Version 0.9.4.1

hotfix: https://github.com/facebook/infer/commit/9393c4f533c153e4c6f984c0752d1b08a4fa1e50


## Version 0.9.4

- [Java] preliminary support for Java 8: infer no longer skips methods containing Java 8 code
- [clang] support for clang compilation databases
- [Xcode] more robust integration using the compilation database (requires [xcpretty](https://github.com/supermarin/xcpretty))
- [iOS] added checks for some of the [ComponentKit best practices](http://componentkit.org/docs/never-subclass-components.html)
- lots of under-the-hood improvements, including perf improvements and bug fixes


## Version 0.9.3

- Fix issues with using Infer with Clang 4.0 and Xcode 8
- Various fixes and performance improvements


## Version 0.9.2

No changelog.


## Version 0.9.1

- enable packaging via Homebrew
- fix an issue with locales

## Version 0.9.0

- lots of perf improvements and fixes across all analyses (thanks to everyone who reported issues and made pull requests!)
- [experimental] C++ language support. See `--cxx` in `infer --help`. This is still in heavy development and only includes a few bug types. Feedback welcome!


## Version 0.8.1

- [Objective-C and C] upgrade clang to version 3.8.0
- [all] bugfixes

## Version 0.8.0

- New `--reactive` mode to rapidly analyze the effects of a code change. Be sure to check out [the documentation of the new workflow](http://fbinfer.com/docs/infer-workflow.html#global-default-and-differential-workflows). As a result, the incremental mode (`--incremental`) is now deprecated.
- New XML output, compatible with output from other static analyzers, eg [PMD](https://pmd.github.io/). To use it, pass the `--pmd-xml` flag to Infer (see `infer --help`).
- Use `@SuppressWarnings("infer")` in your Java projects to annotate methods or classes where Infer shouldn't report.
- This release incorporates a number of contributions (#284 #289 #300 #301) and addresses a number of issues (#279 #281 #283 #288 #291 #294).


## Version 0.7.0

- addresses a number of bugs, eg #270 #274 #275 #276
- [all] add summary of the analysis results at the end of the console output
- [android] new ["Fragment retains View" checker](http://fbinfer.com/docs/checkers-bug-types.html)


## Version 0.6.0

- [android] fixed intermittent infinite loop
- [iOS] new check for [capturing](http://fbinfer.com/docs/infer-bug-types.html#CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK) a C++ reference in an Objective-C block


## Version 0.5.0

- [android] detection of context leaks
- [android] support for `@PerformanceCritical` and `@Expensive` method annotations. Infer will check that an expensive method is never called during the execution of a performance critical method (run it with `infer -a checkers -- ...`).
- [iOS] new check to catch strong delegate properties, likely to create retain cycles
- [iOS] new check to catch direct accesses to atomic properties, which can cause race conditions
- [all] performance improvements all-around


## Version 0.4.0

No changelog.


## Version 0.3.0

No changelog.


## Version 0.2.0

- [java] analyze class files with $$ in the name (closes #3 more)
- [java] don't fail on compilation warnings (closes #18)
- [clang] support __nullable et al. (closes #4)
- add an Infer:Checkers for printf arguments


## Version 0.1.1

- [java] no more crash on class names containing "$$" (closes #3)
- [java] model for `assert` (closes #68)
- [objective-c] support for `@import` (closes #2)
- [c family] Infer now always reports on the right line numbers (closes #31)
- [c family] fix c++ compilation errors (closes #37)


## Version 0.1.0

Initial release
