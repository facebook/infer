(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Attribute manipulation in Propositions (i.e., Symbolic Heaps) *)

val is_pred : Sil.atom -> bool
(** Check whether an atom is used to mark an attribute *)

val add :
     Tenv.t
  -> ?footprint:bool
  -> ?polarity:bool
  -> Prop.normal Prop.t
  -> PredSymb.t
  -> Exp.t list
  -> Prop.normal Prop.t
(** Add an attribute associated to the argument expressions *)

val add_or_replace : Tenv.t -> Prop.normal Prop.t -> Sil.atom -> Prop.normal Prop.t
(** Replace an attribute associated to the expression *)

val add_or_replace_check_changed :
     Tenv.t
  -> (PredSymb.t -> PredSymb.t -> unit)
  -> Prop.normal Prop.t
  -> Sil.atom
  -> Prop.normal Prop.t
(** Replace an attribute associated to the expression, and call the given function with new and
    old attributes if they changed. *)

val get_all : 'a Prop.t -> Sil.atom list
(** Get all the attributes of the prop *)

val get_for_exp : Tenv.t -> 'a Prop.t -> Exp.t -> Sil.atom list
(** Get the attributes associated to the expression, if any *)

val get_objc_null : Tenv.t -> 'a Prop.t -> Exp.t -> Sil.atom option
(** Get the objc null attribute associated to the expression, if any *)

val get_observer : Tenv.t -> 'a Prop.t -> Exp.t -> Sil.atom option
(** Get the observer attribute associated to the expression, if any *)

val get_resource : Tenv.t -> 'a Prop.t -> Exp.t -> Sil.atom option
(** Get the resource attribute associated to the expression, if any *)

val get_undef : Tenv.t -> 'a Prop.t -> Exp.t -> Sil.atom option
(** Get the undef attribute associated to the expression, if any *)

val get_wontleak : Tenv.t -> 'a Prop.t -> Exp.t -> Sil.atom option
(** Get the wontleak attribute associated to the expression, if any *)

val has_dangling_uninit : Tenv.t -> 'a Prop.t -> Exp.t -> bool
(** Test for existence of an Adangling DAuninit attribute associated to the exp *)

val remove : Tenv.t -> Prop.normal Prop.t -> Sil.atom -> Prop.normal Prop.t
(** Remove an attribute *)

val remove_for_attr : Tenv.t -> Prop.normal Prop.t -> PredSymb.t -> Prop.normal Prop.t
(** Remove all attributes for the given attr *)

val remove_resource :
  Tenv.t -> PredSymb.res_act_kind -> PredSymb.resource -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Remove all attributes for the given resource and kind *)

val map_resource :
     Tenv.t
  -> Prop.normal Prop.t
  -> (Exp.t -> PredSymb.res_action -> PredSymb.res_action)
  -> Prop.normal Prop.t
(** Apply f to every resource attribute in the prop *)

val replace_objc_null : Tenv.t -> Prop.normal Prop.t -> Exp.t -> Exp.t -> Prop.normal Prop.t
(** [replace_objc_null lhs rhs].
    If rhs has the objc_null attribute, replace the attribute and set the lhs = 0 *)

val nullify_exp_with_objc_null : Tenv.t -> Prop.normal Prop.t -> Exp.t -> Prop.normal Prop.t
(** For each Var subexp of the argument with an Aobjc_null attribute,
    remove the attribute and conjoin an equality to zero. *)

val mark_vars_as_undefined :
     Tenv.t
  -> Prop.normal Prop.t
  -> ret_exp:Exp.t
  -> undefined_actuals_by_ref:Exp.t list
  -> Typ.Procname.t
  -> Annot.Item.t
  -> Location.t
  -> PredSymb.path_pos
  -> Prop.normal Prop.t
(** mark Exp.Var's or Exp.Lvar's as undefined *)

(** type for arithmetic problems *)
type arith_problem =
  (* division by zero *)
  | Div0 of Exp.t
  (* unary minus of unsigned type applied to the given expression *)
  | UminusUnsigned of Exp.t * Typ.t

val find_arithmetic_problem :
     Tenv.t
  -> PredSymb.path_pos
  -> Prop.normal Prop.t
  -> Exp.t
  -> arith_problem option * Prop.normal Prop.t
(** Look for an arithmetic problem in [exp] *)

val deallocate_stack_vars :
  Tenv.t -> Prop.normal Prop.t -> Pvar.t list -> Pvar.t list * Prop.normal Prop.t
(** Deallocate the stack variables in [pvars], and replace them by normal variables.
    Return the list of stack variables whose address was still present after deallocation. *)

val find_equal_formal_path : Tenv.t -> Exp.t -> 'a Prop.t -> Exp.t option
