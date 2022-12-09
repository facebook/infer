(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val proc_decl_to_sil : Textual.Lang.t -> Textual.ProcDecl.t -> Procname.t
  [@@warning "-unused-value-declaration"]

val module_to_sil : line_map:LineMap.t option -> Textual.Module.t -> Cfg.t * Tenv.t

val from_java : filename:string -> Tenv.t -> Cfg.t -> unit
(** generate a .sil file with name [filename] containing all the functions in the given cfg *)
