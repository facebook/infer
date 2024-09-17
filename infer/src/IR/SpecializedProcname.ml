(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module T = struct
  type t = {proc_name: Procname.t; specialization: Specialization.t option}
  [@@deriving equal, compare, hash, sexp]

  let pp fmt {proc_name; specialization} =
    match specialization with
    | Some specialization ->
        F.fprintf fmt "%a (specialized for %a)" Procname.pp_verbose proc_name Specialization.pp
          specialization
    | None ->
        Procname.pp fmt proc_name
end

include T
module Map = PrettyPrintable.MakePPMap (T)
module Set = PrettyPrintable.MakeHashSexpPPSet (T)
