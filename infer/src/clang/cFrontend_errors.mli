(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type exception_details =
  { msg: string
  ; position: Logging.ocaml_pos
  ; source_range: Clang_ast_t.source_range
  ; ast_node: string option }

exception Unimplemented of exception_details

exception IncorrectAssumption of exception_details

exception Invalid_declaration

val unimplemented :
     Logging.ocaml_pos
  -> Clang_ast_t.source_range
  -> ?ast_node:string
  -> ('a, Format.formatter, unit, _) format4
  -> 'a
(** Raise Unimplemented. This is caught at the level of translating a method and makes the frontend
    give up on that method. *)

val incorrect_assumption :
     Logging.ocaml_pos
  -> Clang_ast_t.source_range
  -> ?ast_node:string
  -> ('a, Format.formatter, unit, _) format4
  -> 'a
(** Used to mark places in the frontend that incorrectly assume something to be impossible.
    TODO(t21762295) get rid of all instances of this. *)

val protect :
     f:(unit -> unit)
  -> recover:(unit -> unit)
  -> pp_context:(Format.formatter -> unit -> unit)
  -> unit
(** Catch frontend errors in [f] to avoid crashing due to bugs in the frontend. Upon error [recover]
    is run and [pp_context] is used to provide more info to the user. *)
