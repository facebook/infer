(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Error : sig
  type kind

  type t = Logging.error * kind

  val ffi : FFI.Error.t -> t [@@warning "-unused-value-declaration"]

  val textual_parser : Textual.SourceFile.t -> t [@@warning "-unused-value-declaration"]

  val pp_kind : Format.formatter -> kind -> unit [@@warning "-unused-value-declaration"]
end

val to_module : sourcefile:Textual.SourceFile.t -> FFI.Code.t -> (Textual.Module.t, Error.t) result
(** Translate a Python code object into its Textual counter part *)
