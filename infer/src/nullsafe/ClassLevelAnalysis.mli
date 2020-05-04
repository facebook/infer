(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** This stage is called for each Java class after all corresponding proc summaries are calculated. *)

val analyze_class :
  Tenv.t -> SourceFile.t -> AggregatedSummaries.ClassInfo.t -> IssueLog.t -> IssueLog.t
(** Given aggregated summary for a class, analyze it, and return updated issue log, if necessary.
    This function will be called for each non-trivial{^ 1} anonymous class in the file, including
    nested classes. Order of calls is not specified.

    {^ 1}The class is non-trivial if it has at least one procedure, or contains at least one nested
    non-trivial class.

    (Note that [IssueLog.t] is a mutable type so it can be actually mutated by this function:
    returning [IssueLog.t] is done for convenient chaining.) *)
