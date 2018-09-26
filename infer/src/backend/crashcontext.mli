(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(**
   CRASHCONTEXT Intro [experimental]:

   Crashcontext is an experimental analysis (in the future: a family of
   analyses). It takes one or more stacktraces representing crashes
   corresponding to the codebase being analyzed and expands them into
   crashcontext.json files. These files incorporate further information about
   the code that might have executed before the crash.

   This analysis is run with '-a crashcontext' and must take either of the
   following extra arguments:

    --stacktrace stacktrace.json (a single stacktrace, output defaults to
                                  crashcontext/crashcontext.json)
    --stacktraces-dir dir (will expand every json stacktace in dir, output
                           crashcontext files will be saved to dir as well)

   For further information, take a look at tests under:

    infer/tests/codetoanalyze/java/crashcontext/ and
    infer/tests/endtoend/java/crashcontext/
*)

val crashcontext_epilogue : in_buck_mode:bool -> unit
(**
   Runs crashcontext epilogue code, which takes the per-method summaries
   produced by crashcontext related analysis (future: analyses) and stitches
   them together into a final crashcontext.json output file.
   This code should run after all checkers when running with '--crashcontext'.
   When running with buck, summaries are stitched across multiple buck targets,
   so this runs at the end of the parent buck infer process only.
   TODO: Similar integration with build systems other than buck.
*)

val pp_stacktree : Format.formatter -> Stacktree_t.stacktree -> unit
