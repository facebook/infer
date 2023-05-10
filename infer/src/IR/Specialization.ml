(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Pulse = struct
  module Aliases = struct
    type t = Pvar.t list list [@@deriving equal, compare]

    let pp fmt aliases =
      let pp_alias fmt alias = Pp.seq ~sep:"=" (Pvar.pp Pp.text) fmt alias in
      Pp.seq ~sep:"^" pp_alias fmt aliases
  end

  module DynamicTypes = struct
    type t = Typ.name Pvar.Map.t [@@deriving equal, compare]

    let pp fmt dtypes =
      let pp_binding fmt (pvar, typename) =
        F.fprintf fmt "%a: %a" (Pvar.pp Pp.text) pvar Typ.Name.pp typename
      in
      Pvar.Map.bindings dtypes |> F.fprintf fmt "{%a}" (Pp.seq ~sep:"," pp_binding)
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
