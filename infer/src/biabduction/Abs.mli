(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Implementation of Abstraction Functions *)

(** Abstraction rules discovered *)
type rules

val abstract : Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Abstract a proposition. *)

val abstract_junk : Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Check whether the prop contains junk.
    If it does, and [Config.allowleak] is true, remove the junk,
    otherwise raise a Leak exception. *)

val abstract_no_symop : Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Abstract a proposition but don't pay a SymOp *)

val get_current_rules : unit -> rules
(** Get the current rules discoveres *)

val lifted_abstract : Typ.Procname.t -> Tenv.t -> Propset.t -> Propset.t
(** Abstract each proposition in [propset] *)

val remove_redundant_array_elements :
  Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Remove redundant elements in an array, and check for junk afterwards *)

val reset_current_rules : unit -> unit
(** Reset the abstraction rules discovered *)

val set_current_rules : rules -> unit
(** Set the current rules discovered *)
