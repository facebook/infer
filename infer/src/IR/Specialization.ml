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

  type t = Aliases of Aliases.t [@@deriving equal, compare]

  let pp fmt = function Aliases aliases -> Aliases.pp fmt aliases

  module Map = PrettyPrintable.MakePPMap (struct
    type nonrec t = t

    let pp = pp

    let compare = compare
  end)
end

type t = Pulse of Pulse.t
