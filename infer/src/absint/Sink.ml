(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Kind = sig
  include TaintTraceElem.Kind

  val get : Procname.t -> HilExp.t list -> CallFlags.t -> Tenv.t -> (t * IInt.Set.t) list
end

module type S = sig
  include TaintTraceElem.S

  val get : CallSite.t -> HilExp.t list -> CallFlags.t -> Tenv.t -> t list

  val indexes : t -> IInt.Set.t

  val with_indexes : t -> IInt.Set.t -> t
end
