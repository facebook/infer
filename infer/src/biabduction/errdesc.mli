(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Create descriptions of analysis errors *)

val vpath_find : Tenv.t -> 'a Prop.t -> Exp.t -> DecompiledExp.vpath * Typ.t option
(** find the dexp, if any, where the given value is stored also return the type of the value if
    found *)

val hpred_is_open_resource : Tenv.t -> 'a Prop.t -> Predicates.hpred -> PredSymb.resource option
(** Check whether the hpred is a |-> representing a resource in the Racquire state *)

val explain_array_access :
  Procname.t -> Tenv.t -> Localise.deref_str -> 'a Prop.t -> Location.t -> Localise.error_desc
(** Produce a description of the array access performed in the current instruction, if any. *)

val explain_class_cast_exception :
     Tenv.t
  -> Procname.t option
  -> Exp.t
  -> Exp.t
  -> Exp.t
  -> Procdesc.Node.t
  -> Location.t
  -> Localise.error_desc
(** explain a class cast exception *)

val explain_dereference :
     Procname.t
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
     Procname.t
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
(** return a description explaining value [exp] in [prop] in terms of a source expression using the
    formal parameters of the call *)

val explain_divide_by_zero : Tenv.t -> Exp.t -> Procdesc.Node.t -> Location.t -> Localise.error_desc
(** explain a division by zero *)

val explain_leak :
     Tenv.t
  -> Predicates.hpred
  -> 'a Prop.t
  -> PredSymb.t option
  -> string option
  -> bool (* should the leak be reported to the user? *) * Localise.error_desc
(** Produce a description of a leak by looking at the current state. If the current instruction is a
    variable nullify, blame the variable. If it is an abstraction, blame any variable nullify at the
    current node. If there is an alloc attribute, print the function call and line number. *)

val warning_err : Location.t -> ('a, Format.formatter, unit) format -> 'a
(** warn at the given location *)

val find_outermost_dereference : Tenv.t -> Procdesc.Node.t -> Exp.t -> DecompiledExp.t option

val access_opt : ?is_nullable:bool -> Predicates.inst -> Localise.access option
