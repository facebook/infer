(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module type Spec = sig
  type astate

  val initial : astate

  val exec_instr :
    astate -> Sil.instr -> Procdesc.Node.nodekind -> Typ.Procname.t -> Tenv.t -> astate

  val report : astate -> Location.t -> Typ.Procname.t -> unit

  val compare : astate -> astate -> int
end

module type S = sig
  val checker : Callbacks.proc_callback_t
end

module Make (Spec : Spec) : S
