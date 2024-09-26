(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type target =
  | Procname of {proc_name: Procname.t; specialization: Specialization.t option}
  | File of SourceFile.t

type analysis_result =
  | Ok  (** Analysis finished normally. *)
  | RaceOn of {dependency_filenames: string list}
      (** Analysis stopped when trying to access the summary of a callee and that callee is being
          analyzed by another worker. [dependency_filenames] are in the path to the callee's lock
          file. *)
