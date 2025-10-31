(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module HeapPath = struct
  (** this is the subset of HilExp.access_expression that make sense in a precondition TODO: deal
      with ArrayAccess *)
  type t = Pvar of Pvar.t | FieldAccess of (Fieldname.t * t) | Dereference of t
  [@@deriving equal, compare, hash, sexp]

  let rec pp fmt = function
    | Pvar pvar ->
        Pvar.pp_value fmt pvar
    | FieldAccess (fieldname, path) ->
        F.fprintf fmt "%a -> %a " pp path Fieldname.pp fieldname
    | Dereference path ->
        F.fprintf fmt "%a -> * " pp path


  module Map = PrettyPrintable.MakeHashSexpPPMap (struct
    type nonrec t = t [@@deriving compare, hash, sexp]

    let pp = pp
  end)

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)
end

module Pulse = struct
  module Aliases = struct
    type t = HeapPath.t list list [@@deriving equal, compare, hash, sexp]

    let pp fmt aliases =
      let pp_alias fmt alias = Pp.seq ~sep:" = " HeapPath.pp fmt alias in
      Pp.seq ~sep:"@,&& " pp_alias fmt aliases
  end

  module DynamicTypes = struct
    type t = Typ.name HeapPath.Map.t [@@deriving equal, compare, hash, sexp]

    let pp fmt dtypes =
      if not (HeapPath.Map.is_empty dtypes) then
        F.fprintf fmt "@[dynamic_types: %a@]" (HeapPath.Map.pp ~pp_value:Typ.Name.pp) dtypes
  end

  type t = {aliases: Aliases.t option; dynamic_types: DynamicTypes.t}
  [@@deriving equal, compare, hash, sexp]

  let bottom = {aliases= None; dynamic_types= HeapPath.Map.empty}

  let is_bottom {aliases; dynamic_types} =
    Option.is_none aliases && HeapPath.Map.is_empty dynamic_types


  let pp_aliases fmt = function
    | None ->
        ()
    | Some aliases ->
        F.fprintf fmt "@[alias: %a@]@ " Aliases.pp aliases


  let pp fmt {aliases; dynamic_types} =
    F.fprintf fmt "@[%a%a@]" pp_aliases aliases DynamicTypes.pp dynamic_types


  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)

  module Map = PrettyPrintable.MakePPMap (struct
    type nonrec t = t

    let pp = pp

    let compare = compare
  end)

  let is_pulse_specialization_limit_reached map =
    Map.cardinal map >= Config.pulse_specialization_limit


  let has_type_in_specialization {dynamic_types} specialized_type =
    (* for Hack we don't care if type is Foo or Foo$static *)
    let get_hack_static_companion_origin typ =
      if Typ.Name.Hack.is_class typ && Typ.Name.Hack.is_static_companion typ then
        Typ.Name.Hack.static_companion_origin typ
      else typ
    in
    HeapPath.Map.exists
      (fun _ typ ->
        let typ = get_hack_static_companion_origin typ in
        let specialized_type = get_hack_static_companion_origin specialized_type in
        Typ.Name.equal typ specialized_type )
      dynamic_types
end

type t = Pulse of Pulse.t [@@deriving equal, compare, hash, sexp]

let pp fmt = function Pulse t -> F.fprintf fmt "Pulse(%a)" Pulse.pp t
