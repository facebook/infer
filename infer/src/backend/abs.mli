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

(** Implementation of Abstraction Functions *)

(** Abstraction rules discovered *)
type rules

(** Abstract a proposition. *)
val abstract : Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t

(** Check whether the prop contains junk.
    If it does, and [Config.allowleak] is true, remove the junk,
    otherwise raise a Leak exception. *)
val abstract_junk :
  ?original_prop:Prop.normal Prop.t ->
  Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t

(** Abstract a proposition but don't pay a SymOp *)
val abstract_no_symop : Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t

(** Get the current rules discoveres *)
val get_current_rules : unit -> rules

(** Abstract each proposition in [propset] *)
val lifted_abstract : Typ.Procname.t -> Tenv.t -> Propset.t -> Propset.t

(** Remove redundant elements in an array, and check for junk afterwards *)
val remove_redundant_array_elements :
  Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t

(** Reset the abstraction rules discovered *)
val reset_current_rules : unit -> unit

(** Set the current rules discovered *)
val set_current_rules : rules -> unit
