(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type 'a t = Unsat | Sat of 'a [@@deriving equal]

let pp pp_sat fmt = function Unsat -> F.pp_print_string fmt "unsat" | Sat x -> pp_sat fmt x

let sat = function Unsat -> None | Sat x -> Some x

let of_option = function None -> Unsat | Some x -> Sat x

module Types = struct
  type nonrec 'a sat_unsat_t = 'a t = Unsat | Sat of 'a
end

let map f = function Unsat -> Unsat | Sat x -> Sat (f x)

let bind f = function Unsat -> Unsat | Sat x -> f x

module Import = struct
  include Types

  let ( >>| ) x f = map f x

  let ( >>= ) x f = bind f x

  let ( let+ ) x f = map f x

  let ( let* ) x f = bind f x
end

let to_result = function Unsat -> Error () | Sat x -> Ok x

let of_result = function Error () -> Unsat | Ok x -> Sat x

let list_fold l ~init ~f =
  List.fold_result l ~init ~f:(fun accum x -> f accum x |> to_result) |> of_result


let to_list sat_unsat = sat sat_unsat |> Option.to_list

let filter l = List.filter_map l ~f:sat

let seq_fold seq ~init ~f =
  let open Import in
  Caml.Seq.fold_left
    (fun accum x ->
      let* accum in
      f accum x )
    (Sat init) seq
