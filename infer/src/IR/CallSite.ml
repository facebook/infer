(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = {pname: Typ.Procname.t; loc: Location.t} [@@deriving compare]

let equal = [%compare.equal: t]

let pname t = t.pname

let loc t = t.loc

let make pname loc = {pname; loc}

let dummy = make Typ.Procname.empty_block Location.dummy

let pp fmt t = F.fprintf fmt "%a at %a" Typ.Procname.pp t.pname Location.pp t.loc

module Set = PrettyPrintable.MakePPSet (struct
  type nonrec t = t

  let compare = compare

  let pp = pp
end)
