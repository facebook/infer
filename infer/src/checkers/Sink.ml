(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

module type Kind = sig
  include TraceElem.Kind

  val get : Typ.Procname.t -> HilExp.t list -> Tenv.t -> (t * IntSet.t) option
end

module type S = sig
  include TraceElem.S

  val get : CallSite.t -> HilExp.t list -> Tenv.t -> t option

  val indexes : t -> IntSet.t
end

module Make (Kind : Kind) = struct
  module Kind = Kind

  type t = {kind: Kind.t; site: CallSite.t; indexes: IntSet.t} [@@deriving compare]

  let kind t = t.kind

  let call_site t = t.site

  let indexes t = t.indexes

  let make ?(indexes= IntSet.empty) kind site = {kind; site; indexes}

  let get site actuals tenv =
    match Kind.get (CallSite.pname site) actuals tenv with
    | Some (kind, indexes)
     -> Some {kind; site; indexes}
    | None
     -> None

  let with_callsite t callee_site = {t with site= callee_site}

  let pp fmt s = F.fprintf fmt "%a(%a)" Kind.pp s.kind CallSite.pp s.site

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)
end
