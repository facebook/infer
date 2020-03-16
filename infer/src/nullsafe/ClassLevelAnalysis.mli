(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** This stage is called for each Java class after all corresponding proc summaries are calculated. *)

val analyze_class :
  SourceFile.t -> JavaClassName.t -> AggregatedSummaries.ClassInfo.t -> IssueLog.t -> IssueLog.t
(** Given all summaries for given class, analyze them in combination, and update issue log, if
    necessary *)
