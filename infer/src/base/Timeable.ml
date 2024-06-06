(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type checker = Checker.t [@@deriving enumerate]

let compare_checker checker1 checker2 =
  String.compare (Checker.get_id checker1) (Checker.get_id checker2)


type t = Checker of checker | Preanalysis [@@deriving compare, enumerate]

let to_string = function Checker checker -> Checker.get_id checker | Preanalysis -> "preanalysis"

let pp fmt timeable = F.pp_print_string fmt @@ to_string timeable

module Map = Caml.Map.Make (struct
  type nonrec t = t [@@deriving compare]
end)

let mk_map_of_all ~init =
  all |> Caml.List.to_seq |> Seq.map (fun timeable -> (timeable, init)) |> Map.of_seq
