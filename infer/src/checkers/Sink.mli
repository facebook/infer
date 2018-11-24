(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Kind = sig
  include TraceElem.Kind

  val get : Typ.Procname.t -> HilExp.t list -> CallFlags.t -> Tenv.t -> (t * IntSet.t) list
  (** return Some kind if the given procname/actuals are a sink, None otherwise *)
end

module type S = sig
  include TraceElem.S

  val get : CallSite.t -> HilExp.t list -> CallFlags.t -> Tenv.t -> t list
  (** return Some sink if the given call site/actuals are a sink, None otherwise *)

  val indexes : t -> IntSet.t
  (** return the indexes where taint can flow into the sink *)

  val with_indexes : t -> IntSet.t -> t
end

module Make (Kind : Kind) : S with module Kind = Kind
