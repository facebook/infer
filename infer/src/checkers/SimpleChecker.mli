(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Spec = sig
  type t

  val initial : t

  val exec_instr : t -> Sil.instr -> Procdesc.Node.nodekind -> Procname.t -> Tenv.t -> t

  val report : t -> Location.t -> Procname.t -> unit

  val compare : t -> t -> int
end

module type S = sig
  val checker : IntraproceduralAnalysis.t -> unit
end

module Make (Spec : Spec) : S
