(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module type Kind = sig
  include TraceElem.Kind

  val get : Typ.Procname.t -> HilExp.t list -> CallFlags.t -> Tenv.t -> (t * IntSet.t) list
end

module type S = sig
  include TraceElem.S

  val get : CallSite.t -> HilExp.t list -> CallFlags.t -> Tenv.t -> t list

  val indexes : t -> IntSet.t

  val with_indexes : t -> IntSet.t -> t
end

module Make (Kind : Kind) = struct
  module Kind = Kind

  type t = {kind: Kind.t; site: CallSite.t; indexes: IntSet.t} [@@deriving compare]

  let kind t = t.kind

  let call_site t = t.site

  let indexes t = t.indexes

  let make ?(indexes = IntSet.empty) kind site = {kind; site; indexes}

  let get site actuals call_flags tenv =
    Kind.get (CallSite.pname site) actuals call_flags tenv
    |> List.rev_map ~f:(fun (kind, indexes) -> {kind; site; indexes})


  let with_callsite t callee_site = {t with site= callee_site}

  let with_indexes t indexes = {t with indexes}

  let pp fmt s = F.fprintf fmt "%a(%a)" Kind.pp s.kind CallSite.pp s.site

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)
end
