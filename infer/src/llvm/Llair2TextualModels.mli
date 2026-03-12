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

type f_reg_to_textual_var = proc_state:ProcState.t -> Reg.t -> Textual.Exp.t * Textual.Typ.t option

type f_reg_to_var_name = Reg.t -> Textual.VarName.t

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
     f_reg_to_textual_var:f_reg_to_textual_var
  -> f_to_textual_exp:f_to_textual_exp
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
     f_reg_to_textual_var:f_reg_to_textual_var
  -> f_reg_to_var_name:f_reg_to_var_name
  -> proc_state:ProcState.t
  -> 'a call
  -> Exp.t list
  -> Textual.QualifiedProcName.t
  -> unit

val get_alloc_class_name :
     f_reg_to_var_name:f_reg_to_var_name
  -> proc_state:ProcState.t
  -> Textual.ProcName.t
  -> Exp.t list
  -> (Textual.TypeName.t * Textual.QualifiedProcName.t) option
