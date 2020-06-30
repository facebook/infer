(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Main modes of operation for infer *)
type t =
  | Analyze  (** analyze previously captured source files *)
  | Capture
      (** capture compilation commands and translate source files into infer's intermediate language *)
  | Compile
      (** set up the infer environment then run the compilation commands without capturing the
          source files *)
  | Debug  (** print information about internal structures *)
  | Explore  (** explore infer reports *)
  | Help  (** documentation about various aspects of infer *)
  | Report  (** post-process infer results and reports *)
  | ReportDiff  (** compute the difference of two infer reports *)
  | Run  (** orchestrate the capture, analysis, and reporting of a compilation command *)
[@@deriving compare]

val of_string : string -> t

val to_string : t -> string

val equal : t -> t -> bool

val all_commands : t list

val infer_exe_name : string

val to_exe_name : t -> string

val of_exe_name : string -> t option
