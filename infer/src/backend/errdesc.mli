(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Create descriptions of analysis errors *)

(** find the dexp, if any, where the given value is stored
    also return the type of the value if found *)
val vpath_find : Tenv.t -> 'a Prop.t -> Exp.t -> DecompiledExp.vpath * Typ.t option

(** Return true if [id] is assigned to a program variable which is then nullified *)
val id_is_assigned_then_dead : Procdesc.Node.t -> Ident.t -> bool

(** Check whether the hpred is a |-> representing a resource in the Racquire state *)
val hpred_is_open_resource : Tenv.t -> 'a Prop.t -> Sil.hpred -> PredSymb.resource option

(** Find the function call instruction used to initialize normal variable [id],
    and return the function name and arguments *)
val find_normal_variable_funcall :
  Procdesc.Node.t -> Ident.t -> (Exp.t * (Exp.t list) * Location.t * CallFlags.t) option

(** Find a program variable assignment in the current node or straightline predecessor. *)
val find_program_variable_assignment :
  Procdesc.Node.t -> Pvar.t -> (Procdesc.Node.t * Ident.t) option

(** Find a program variable assignment to id in the current node or predecessors. *)
val find_ident_assignment : Procdesc.Node.t -> Ident.t -> (Procdesc.Node.t * Exp.t) option

(** Find a boolean assignment to a temporary variable holding a boolean condition.
    The boolean parameter indicates whether the true or false branch is required. *)
val find_boolean_assignment : Procdesc.Node.t -> Pvar.t -> bool -> Procdesc.Node.t option

(** describe rvalue [e] as a dexp *)
val exp_rv_dexp : Tenv.t -> Procdesc.Node.t -> Exp.t -> DecompiledExp.t option

(** Produce a description of a persistent reference to an Android Context *)
val explain_context_leak : Procname.t -> Typ.t -> Ident.fieldname ->
  (Ident.fieldname option * Typ.t) list -> Localise.error_desc

(** Produce a description of a mismatch between an allocation function and a deallocation function *)
val explain_allocation_mismatch :
  PredSymb.res_action -> PredSymb.res_action -> Localise.error_desc

(** Produce a description of the array access performed in the current instruction, if any. *)
val explain_array_access : Tenv.t -> Localise.deref_str -> 'a Prop.t -> Location.t -> Localise.error_desc

(** explain a class cast exception *)
val explain_class_cast_exception :
  Tenv.t -> Procname.t option -> Exp.t -> Exp.t -> Exp.t ->
  Procdesc.Node.t -> Location.t -> Localise.error_desc

(** Explain a deallocate stack variable error *)
val explain_deallocate_stack_var : Pvar.t -> PredSymb.res_action -> Localise.error_desc

(** Explain a deallocate constant string error *)
val explain_deallocate_constant_string : string -> PredSymb.res_action -> Localise.error_desc

(** Produce a description of which expression is dereferenced in the current instruction, if any. *)
val explain_dereference :
  Tenv.t -> ?use_buckets:bool -> ?is_nullable:bool -> ?is_premature_nil:bool ->
  Localise.deref_str -> 'a Prop.t -> Location.t -> Localise.error_desc

(** return a description explaining value [exp] in [prop] in terms of a source expression
    using the formal parameters of the call *)
val explain_dereference_as_caller_expression :
  Tenv.t -> ?use_buckets:bool ->
  Localise.deref_str -> 'a Prop.t -> 'b Prop.t -> Exp.t ->
  Procdesc.Node.t -> Location.t -> Pvar.t list -> Localise.error_desc

(** explain a division by zero *)
val explain_divide_by_zero : Tenv.t -> Exp.t -> Procdesc.Node.t -> Location.t -> Localise.error_desc

(** explain a return expression required *)
val explain_return_expression_required : Location.t -> Typ.t -> Localise.error_desc

(** explain a comparing floats for equality *)
val explain_comparing_floats_for_equality : Location.t -> Localise.error_desc

(** explain a condition is an assignment *)
val explain_condition_is_assignment : Location.t -> Localise.error_desc

(** explain a condition which is always true or false *)
val explain_condition_always_true_false :
  Tenv.t -> IntLit.t -> Exp.t -> Procdesc.Node.t -> Location.t -> Localise.error_desc

(** explain the escape of a stack variable address from its scope *)
val explain_stack_variable_address_escape :
  Location.t -> Pvar.t -> DecompiledExp.t option -> Localise.error_desc

(** explain frontend warning *)
val explain_frontend_warning : string -> string option -> Location.t -> Localise.error_desc

(** explain a return statement missing *)
val explain_return_statement_missing : Location.t -> Localise.error_desc

(** explain a retain cycle *)
val explain_retain_cycle :
  ((Sil.strexp * Typ.t) * Ident.fieldname * Sil.strexp) list ->
  Location.t -> string option -> Localise.error_desc

(** explain unary minus applied to unsigned expression *)
val explain_unary_minus_applied_to_unsigned_expression :
  Tenv.t -> Exp.t -> Typ.t -> Procdesc.Node.t -> Location.t -> Localise.error_desc

(** Explain a tainted value error *)
val explain_tainted_value_reaching_sensitive_function :
  Prop.normal Prop.t -> Exp.t -> PredSymb.taint_info -> Procname.t -> Location.t ->
  Localise.error_desc

(** Produce a description of a leak by looking at the current state.
    If the current instruction is a variable nullify, blame the variable.
    If it is an abstraction, blame any variable nullify at the current node.
    If there is an alloc attribute, print the function call and line number. *)
val explain_leak :
  Tenv.t -> Sil.hpred -> 'a Prop.t -> PredSymb.t option -> string option ->
  Exceptions.visibility * Localise.error_desc

(** Produce a description of the memory access performed in the current instruction, if any. *)
val explain_memory_access : Tenv.t -> Localise.deref_str -> 'a Prop.t -> Location.t -> Localise.error_desc

(** explain a test for NULL of a dereferenced pointer *)
val explain_null_test_after_dereference :
  Tenv.t -> Exp.t -> Procdesc.Node.t -> int -> Location.t -> Localise.error_desc

(** Print a warning to the err stream at the given location (note: only prints in developer mode) *)
val warning_err : Location.t -> ('a, Format.formatter, unit) format -> 'a

(* offset of an expression found following a program variable *)
type pvar_off =
  | Fpvar  (* value of a pvar *)
  | Fstruct of Ident.fieldname list (* value obtained by dereferencing the pvar and following a sequence of fields *)

(** Find a program variable whose value is [exp] or pointing to a struct containing [exp] *)
val find_with_exp : 'a Prop.t -> Exp.t -> (Pvar.t * pvar_off) option
