(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Create descriptions of analysis errors *)

val vpath_find : Tenv.t -> 'a Prop.t -> Exp.t -> DecompiledExp.vpath * Typ.t option
(** find the dexp, if any, where the given value is stored
    also return the type of the value if found *)

val hpred_is_open_resource : Tenv.t -> 'a Prop.t -> Sil.hpred -> PredSymb.resource option
(** Check whether the hpred is a |-> representing a resource in the Racquire state *)

val find_normal_variable_funcall :
  Procdesc.Node.t -> Ident.t -> (Exp.t * Exp.t list * Location.t * CallFlags.t) option
(** Find the function call instruction used to initialize normal variable [id],
    and return the function name and arguments *)

val find_program_variable_assignment :
  Procdesc.Node.t -> Pvar.t -> (Procdesc.Node.t * Ident.t) option
(** Find a program variable assignment in the current node or straightline predecessor. *)

val find_ident_assignment : Procdesc.Node.t -> Ident.t -> (Procdesc.Node.t * Exp.t) option
(** Find a program variable assignment to id in the current node or predecessors. *)

val find_boolean_assignment : Procdesc.Node.t -> Pvar.t -> bool -> Procdesc.Node.t option
(** Find a boolean assignment to a temporary variable holding a boolean condition.
    The boolean parameter indicates whether the true or false branch is required. *)

val exp_rv_dexp : Tenv.t -> Procdesc.Node.t -> Exp.t -> DecompiledExp.t option
(** describe rvalue [e] as a dexp *)

val explain_allocation_mismatch : PredSymb.res_action -> PredSymb.res_action -> Localise.error_desc
(** Produce a description of a mismatch between an allocation function and a deallocation function *)

val explain_array_access :
  Typ.Procname.t -> Tenv.t -> Localise.deref_str -> 'a Prop.t -> Location.t -> Localise.error_desc
(** Produce a description of the array access performed in the current instruction, if any. *)

val explain_class_cast_exception :
     Tenv.t
  -> Typ.Procname.t option
  -> Exp.t
  -> Exp.t
  -> Exp.t
  -> Procdesc.Node.t
  -> Location.t
  -> Localise.error_desc
(** explain a class cast exception *)

val explain_deallocate_stack_var : Pvar.t -> PredSymb.res_action -> Localise.error_desc
(** Explain a deallocate stack variable error *)

val explain_deallocate_constant_string : string -> PredSymb.res_action -> Localise.error_desc
(** Explain a deallocate constant string error *)

val explain_dereference :
     Typ.Procname.t
  -> Tenv.t
  -> ?use_buckets:bool
  -> ?is_nullable:bool
  -> ?is_premature_nil:bool
  -> Localise.deref_str
  -> 'a Prop.t
  -> Location.t
  -> Localise.error_desc
(** Produce a description of which expression is dereferenced in the current instruction, if any. *)

val explain_dereference_as_caller_expression :
     Typ.Procname.t
  -> Tenv.t
  -> ?use_buckets:bool
  -> Localise.deref_str
  -> 'a Prop.t
  -> 'b Prop.t
  -> Exp.t
  -> Procdesc.Node.t
  -> Location.t
  -> Pvar.t list
  -> Localise.error_desc
(** return a description explaining value [exp] in [prop] in terms of a source expression
    using the formal parameters of the call *)

val explain_divide_by_zero :
  Tenv.t -> Exp.t -> Procdesc.Node.t -> Location.t -> Localise.error_desc
(** explain a division by zero *)

val explain_condition_always_true_false :
  Tenv.t -> IntLit.t -> Exp.t -> Procdesc.Node.t -> Location.t -> Localise.error_desc
(** explain a condition which is always true or false *)

val explain_stack_variable_address_escape :
  Location.t -> Pvar.t -> DecompiledExp.t option -> Localise.error_desc
(** explain the escape of a stack variable address from its scope *)

val explain_frontend_warning : string -> string option -> Location.t -> Localise.error_desc
(** explain frontend warning *)

val explain_unary_minus_applied_to_unsigned_expression :
  Tenv.t -> Exp.t -> Typ.t -> Procdesc.Node.t -> Location.t -> Localise.error_desc
(** explain unary minus applied to unsigned expression *)

val explain_leak :
     Tenv.t
  -> Sil.hpred
  -> 'a Prop.t
  -> PredSymb.t option
  -> string option
  -> Exceptions.visibility * Localise.error_desc
(** Produce a description of a leak by looking at the current state.
    If the current instruction is a variable nullify, blame the variable.
    If it is an abstraction, blame any variable nullify at the current node.
    If there is an alloc attribute, print the function call and line number. *)

val explain_null_test_after_dereference :
  Tenv.t -> Exp.t -> Procdesc.Node.t -> int -> Location.t -> Localise.error_desc
(** explain a test for NULL of a dereferenced pointer *)

val warning_err : Location.t -> ('a, Format.formatter, unit) format -> 'a
(** warn at the given location *)

val find_outermost_dereference : Tenv.t -> Procdesc.Node.t -> Exp.t -> DecompiledExp.t option

val access_opt : ?is_nullable:bool -> Sil.inst -> Localise.access option
