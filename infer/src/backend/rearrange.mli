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

(** Re-arrangement and extension of structures with fresh variables *)

(* TODO: this description is not clear *)

exception ARRAY_ACCESS

val is_only_pt_by_fld_or_param_nonnull :
  Procdesc.t -> Tenv.t -> Prop.normal Prop.t -> Exp.t -> bool

val check_dereference_error :
  Tenv.t -> Procdesc.t -> Prop.normal Prop.t -> Exp.t -> Location.t -> unit
(** Check for dereference errors: dereferencing 0, a freed value, or an undefined value *)

val check_call_to_objc_block_error :
  Tenv.t -> Procdesc.t -> Prop.normal Prop.t -> Exp.t -> Location.t -> unit
(** Check that an expression representing an objc block can be null and raise a [B1] null exception.
    It's used to check that we don't call possibly null blocks *)

val rearrange :
  ?report_deref_errors:bool -> Procdesc.t -> Tenv.t -> Exp.t -> Typ.t -> Prop.normal Prop.t
  -> Location.t -> Sil.offset list Prop.prop_iter list
(** [rearrange lexp prop] rearranges [prop] into the form [prop' * lexp|->strexp:typ].
    It returns an iterator with [lexp |-> strexp: typ] as current predicate
    and the path (an [offsetlist]) which leads to [lexp] as the iterator state. *)
