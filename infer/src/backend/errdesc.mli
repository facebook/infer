(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Create descriptions of analysis errors *)

(** find the dexp, if any, where the given value is stored
    also return the type of the value if found *)
val vpath_find : 'a Prop.t -> Sil.exp -> Sil.vpath * Sil.typ option

(** Return true if [id] is assigned to a program variable which is then nullified *)
val id_is_assigned_then_dead : Cfg.Node.t -> Ident.t -> bool

(** Check whether the hpred is a |-> representing a resource in the Racquire state *)
val hpred_is_open_resource : 'a Prop.t -> Sil.hpred -> Sil.resource option

(** Find the function call instruction used to initialize normal variable [id],
    and return the function name and arguments *)
val find_normal_variable_funcall :
  Cfg.Node.t -> Ident.t -> (Sil.exp * (Sil.exp list) * Location.t * Sil.call_flags) option

(** Find a program variable assignment in the current node or straightline predecessor. *)
val find_program_variable_assignment : Cfg.Node.t -> Pvar.t -> (Cfg.Node.t * Ident.t) option

(** Find a program variable assignment to id in the current node or predecessors. *)
val find_ident_assignment : Cfg.Node.t -> Ident.t -> (Cfg.Node.t * Sil.exp) option

(** Find a boolean assignment to a temporary variable holding a boolean condition.
    The boolean parameter indicates whether the true or false branch is required. *)
val find_boolean_assignment : Cfg.Node.t -> Pvar.t -> bool -> Cfg.Node.t option

(** describe rvalue [e] as a dexp *)
val exp_rv_dexp : Cfg.Node.t -> Sil.exp -> Sil.dexp option

(** Produce a description of a persistent reference to an Android Context *)
val explain_context_leak : Procname.t -> Sil.typ -> Ident.fieldname ->
  (Ident.fieldname option * Sil.typ) list -> Localise.error_desc

(** Produce a description of a pointer dangerously coerced to a boolean in a comparison *)
val explain_bad_pointer_comparison : Sil.exp -> Cfg.Node.t -> Location.t -> Localise.error_desc

(** Produce a description of a mismatch between an allocation function and a deallocation function *)
val explain_allocation_mismatch : Sil.res_action -> Sil.res_action -> Localise.error_desc

(** Produce a description of the array access performed in the current instruction, if any. *)
val explain_array_access : Localise.deref_str -> 'a Prop.t -> Location.t -> Localise.error_desc

(** explain a class cast exception *)
val explain_class_cast_exception :
  Procname.t option -> Sil.exp -> Sil.exp -> Sil.exp ->
  Cfg.Node.t -> Location.t -> Localise.error_desc

(** Explain a deallocate stack variable error *)
val explain_deallocate_stack_var : Pvar.t -> Sil.res_action -> Localise.error_desc

(** Explain a deallocate constant string error *)
val explain_deallocate_constant_string : string -> Sil.res_action -> Localise.error_desc

(** Produce a description of which expression is dereferenced in the current instruction, if any. *)
val explain_dereference :
  ?use_buckets:bool -> ?is_nullable:bool -> ?is_premature_nil:bool ->
  Localise.deref_str -> 'a Prop.t -> Location.t -> Localise.error_desc

(** return a description explaining value [exp] in [prop] in terms of a source expression
    using the formal parameters of the call *)
val explain_dereference_as_caller_expression :
  ?use_buckets:bool ->
  Localise.deref_str -> 'a Prop.t -> 'b Prop.t -> Sil.exp ->
  Cfg.Node.t -> Location.t -> Pvar.t list -> Localise.error_desc

(** explain a division by zero *)
val explain_divide_by_zero : Sil.exp -> Cfg.Node.t -> Location.t -> Localise.error_desc

(** explain a return expression required *)
val explain_return_expression_required : Location.t -> Sil.typ -> Localise.error_desc

(** explain a comparing floats for equality *)
val explain_comparing_floats_for_equality : Location.t -> Localise.error_desc

(** explain a condition is an assignment *)
val explain_condition_is_assignment : Location.t -> Localise.error_desc

(** explain a condition which is always true or false *)
val explain_condition_always_true_false :
  Sil.Int.t -> Sil.exp -> Cfg.Node.t -> Location.t -> Localise.error_desc

(** explain the escape of a stack variable address from its scope *)
val explain_stack_variable_address_escape :
  Location.t -> Pvar.t -> Sil.dexp option -> Localise.error_desc

(** explain frontend warning *)
val explain_frontend_warning : string -> string -> Location.t -> Localise.error_desc

(** explain a return statement missing *)
val explain_return_statement_missing : Location.t -> Localise.error_desc

(** explain a retain cycle *)
val explain_retain_cycle :
  Prop.normal Prop.t -> ((Sil.strexp * Sil.typ) * Ident.fieldname * Sil.strexp) list ->
  Location.t -> string option -> Localise.error_desc

(** explain unary minus applied to unsigned expression *)
val explain_unary_minus_applied_to_unsigned_expression :
  Sil.exp -> Sil.typ -> Cfg.Node.t -> Location.t -> Localise.error_desc

(** Explain a tainted value error *)
val explain_tainted_value_reaching_sensitive_function :
  Prop.normal Prop.t -> Sil.exp -> Sil.taint_info -> Procname.t -> Location.t -> Localise.error_desc

(** Produce a description of a leak by looking at the current state.
    If the current instruction is a variable nullify, blame the variable.
    If it is an abstraction, blame any variable nullify at the current node.
    If there is an alloc attribute, print the function call and line number. *)
val explain_leak :
  Tenv.t -> Sil.hpred -> 'a Prop.t -> Sil.attribute option -> string option ->
  Exceptions.exception_visibility * Localise.error_desc

(** Produce a description of the memory access performed in the current instruction, if any. *)
val explain_memory_access : Localise.deref_str -> 'a Prop.t -> Location.t -> Localise.error_desc

(** explain a test for NULL of a dereferenced pointer *)
val explain_null_test_after_dereference :
  Sil.exp -> Cfg.Node.t -> int -> Location.t -> Localise.error_desc

(** Check whether the program variable is a temporary one generated by CIL *)
val pvar_is_cil_tmp : Pvar.t -> bool

(** Check whether the program variable is a temporary generated by the front-end *)
val pvar_is_frontend_tmp : Pvar.t -> bool

(** Print a warning to the err stream at the given location (note: only prints in developer mode) *)
val warning_err : Location.t -> ('a, Format.formatter, unit) format -> 'a

(* offset of an expression found following a program variable *)
type pvar_off =
  | Fpvar  (* value of a pvar *)
  | Fstruct of Ident.fieldname list (* value obtained by dereferencing the pvar and following a sequence of fields *)

(** Find a program variable whose value is [exp] or pointing to a struct containing [exp] *)
val find_with_exp : 'a Prop.t -> Sil.exp -> (Pvar.t * pvar_off) option
