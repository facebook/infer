(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module T = struct
  type t = {procname: Procname.t; specialization: Specialization.t option}
  [@@deriving equal, compare, sexp]

  let pp fmt {procname; specialization} =
    match specialization with
    | Some specialization ->
        F.fprintf fmt "%a (specialized for %a)" Procname.pp_verbose procname Specialization.pp
          specialization
    | None ->
        Procname.pp fmt procname
end

include T
module Map = PrettyPrintable.MakePPMap (T)
module Set = PrettyPrintable.MakeSexpPPSet (T)
