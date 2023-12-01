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
  [@@deriving equal, compare]

  let rec pp fmt = function
    | Pvar pvar ->
        Pvar.pp Pp.text fmt pvar
    | FieldAccess (fieldname, path) ->
        F.fprintf fmt "%a -> %a " pp path Fieldname.pp fieldname
    | Dereference path ->
        F.fprintf fmt "%a -> * " pp path


  module Map = PrettyPrintable.MakePPMap (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)
end

module Pulse = struct
  module Aliases = struct
    type t = Pvar.t list list [@@deriving equal, compare]

    let pp fmt aliases =
      let pp_alias fmt alias = Pp.seq ~sep:" = " Pvar.pp_value fmt alias in
      Pp.seq ~sep:"@,&& " pp_alias fmt aliases
  end

  module DynamicTypes = struct
    type t = Typ.name HeapPath.Map.t [@@deriving equal, compare]

    let pp fmt dtypes = HeapPath.Map.pp ~pp_value:Typ.Name.pp fmt dtypes

    module Set = PrettyPrintable.MakePPSet (struct
      type nonrec t = t

      let compare = HeapPath.Map.compare Typ.Name.compare_name

      let pp = pp
    end)
  end

  type t = Aliases of Aliases.t | DynamicTypes of DynamicTypes.t [@@deriving equal, compare]

  let pp fmt = function
    | Aliases aliases ->
        F.fprintf fmt "(alias) %a" Aliases.pp aliases
    | DynamicTypes dtypes ->
        F.fprintf fmt "(dynamic types) %a" DynamicTypes.pp dtypes


  module Map = PrettyPrintable.MakePPMap (struct
    type nonrec t = t

    let pp = pp

    let compare = compare
  end)

  let is_pulse_specialization_limit_not_reached map =
    Map.cardinal map < Config.pulse_specialization_limit
end

type t = Pulse of Pulse.t
