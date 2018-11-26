(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Spec = sig
  type t

  val initial : t

  val exec_instr : t -> Sil.instr -> Procdesc.Node.nodekind -> Typ.Procname.t -> Tenv.t -> t

  val report : t -> Location.t -> Typ.Procname.t -> unit

  val compare : t -> t -> int
end

module type S = sig
  val checker : Callbacks.proc_callback_t
end

module Make (Spec : Spec) : S
