(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** returns true if [callee_pname] returns a tainted value *)
val returns_tainted : Typ.Procname.t -> ProcAttributes.t option -> PredSymb.taint_kind option

(** returns list of zero-indexed argument numbers of [callee_pname] that may be tainted *)
val accepts_sensitive_params :
  Typ.Procname.t -> ProcAttributes.t option -> (int * PredSymb.taint_kind) list

(** returns list of zero-indexed parameter numbers of [callee_pname] that should be
    considered tainted during symbolic execution *)
val tainted_params : Typ.Procname.t -> (int * PredSymb.taint_kind) list

(** returns the taint_kind of [fieldname] if it has a taint source annotation *)
val has_taint_annotation : Fieldname.t -> Typ.Struct.t -> bool

val add_tainting_attribute : Tenv.t -> PredSymb.t -> Pvar.t -> Prop.normal Prop.t -> Prop.normal Prop.t

val get_params_to_taint :
  (int * PredSymb.taint_kind) list -> Pvar.t list -> (Pvar.t * PredSymb.taint_kind) list
