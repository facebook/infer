(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
module ModuleState = Llair2TextualState.ModuleState
module ProcState = Llair2TextualState.ProcState
module IdentMap = Textual.Ident.Map

(* --- Dependency Injection Types --- *)

type f_to_textual_exp =
     proc_state:ProcState.t
  -> Textual.Location.t
  -> Exp.t
  -> Textual.Exp.t * Textual.Typ.t option * Textual.Instr.t list

type f_add_deref =
     proc_state:ProcState.t
  -> Textual.Exp.t
  -> Textual.Location.t
  -> Textual.Instr.t list * Textual.Exp.t

(* --- Constants & Simple Checks --- *)

val swift_weak_assign : Textual.ProcName.t

val llvm_dynamic_call : Textual.ProcName.t

val derived_enum_equals : string

val functions_to_skip : Textual.ProcName.t list

val boxed_opaque_existentials : string list

val llvm_init_tuple : string

val swift_metadata_equals : string

val builtin_qual_proc_name : string -> Textual.QualifiedProcName.t

val is_protocol_witness_optional_deinit_copy : Textual.Lang.t -> string -> bool

(* --- Models --- *)

module Metadata : sig
  val is_metadata_field : Textual.qualified_fieldname -> bool

  val propagate : proc_state:ProcState.t -> Textual.Ident.t -> Textual.Exp.t -> is_load:bool -> unit

  val try_translate_swift_metadata_call :
       proc_state:ProcState.t
    -> Textual.QualifiedProcName.t
    -> Exp.t list
    -> Textual.Ident.t option
    -> Textual.Location.t
    -> (Textual.Exp.t * Textual.Instr.t list) option
end

val translate_optional_protocol_witness :
     proc_state:ProcState.t
  -> Textual.Exp.t
  -> Textual.Typ.t option
  -> Textual.TypeName.t
  -> int
  -> Textual.TypeName.t * int

val translate_boxed_opaque_existential :
     f_to_textual_exp:f_to_textual_exp
  -> f_add_deref:f_add_deref
  -> proc_state:ProcState.t
  -> Textual.Location.t
  -> Exp.t list
  -> (Textual.Exp.t * Textual.Instr.t list) option

val translate_protocol_witness_optional_deinit_copy :
     f_to_textual_exp:f_to_textual_exp
  -> f_add_deref:f_add_deref
  -> proc_state:ProcState.t
  -> Exp.t
  -> Exp.t
  -> Textual.Location.t
  -> Textual.Instr.t list

val resolve_objc_msgSend :
  proc_state:ProcState.t -> Textual.Exp.t -> Textual.Typ.t option list -> Textual.Exp.t

val save_metadata_type :
  proc_state:ProcState.t -> 'a call -> Exp.t list -> Textual.QualifiedProcName.t -> unit

val get_alloc_class_name :
     proc_state:ProcState.t
  -> Textual.ProcName.t
  -> Exp.t list
  -> (Textual.TypeName.t * Textual.QualifiedProcName.t) option

val update_selector_metadata :
  proc_state:ProcState.t -> IdentMap.key option -> Textual.Exp.t -> label -> unit

val try_rewrite_call_to_instrs :
     proc_state:ProcState.t
  -> f_to_textual_exp:
       (   proc_state:ProcState.t
        -> Textual.Location.t
        -> 'a
        -> Textual.Exp.t * 'b * Textual.Instr.t list )
  -> f_add_deref:
       (   proc_state:ProcState.t
        -> Textual.Exp.t
        -> Textual.Location.t
        -> Textual.Instr.t list * Textual.Exp.t )
  -> loc:Textual.Location.t
  -> call_exp:Textual.Exp.t
  -> llair_args:'a list
  -> args_instrs:Textual.Instr.t list
  -> id:IdentMap.key option
  -> Textual.Instr.t list option

val try_propagate_objc_class :
  proc_state:ProcState.t -> label -> IdentMap.key option -> Llair.Exp.t list -> unit

val try_capture_objc_metadata : proc_state:ProcState.t -> IdentMap.key -> Llair.Exp.t -> unit
