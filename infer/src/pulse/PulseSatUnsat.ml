(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module Types = struct
  type unsat_info = {reason: unit -> string; source: string * int * int * int}

  type 'a sat_unsat_t = Unsat of (unsat_info[@ignore]) | Sat of 'a [@@deriving equal]
end

include Types

type nonrec 'a t = 'a sat_unsat_t = Unsat of (unsat_info[@ignore]) | Sat of 'a [@@deriving equal]

let log_source_info = ref true

let pp_unsat_info fmt {reason; source= file, lnum, cnum, enum} =
  if !log_source_info then F.fprintf fmt "UNSAT(%s:%d:%d-%d): %s" file lnum cnum enum (reason ())
  else F.fprintf fmt "UNSAT: %s" (reason ())


let pp pp_sat fmt = function
  | Unsat unsat_info ->
      pp_unsat_info fmt unsat_info
  | Sat x ->
      pp_sat fmt x


let log_unsat unsat_info = L.d_printfln "%a" pp_unsat_info unsat_info

let sat = function
  | Unsat unsat_info ->
      log_unsat unsat_info ;
      None
  | Sat x ->
      Some x


let of_option unsat_info = function None -> Unsat unsat_info | Some x -> Sat x

let map f = function Unsat _ as unsat -> unsat | Sat x -> Sat (f x)

let bind f = function Unsat _ as unsat -> unsat | Sat x -> f x

module Import = struct
  include Types

  let ( >>| ) x f = map f x

  let ( >>= ) x f = bind f x

  let ( let+ ) x f = map f x

  let ( let* ) x f = bind f x
end

let to_result = function Unsat unsat_info -> Error unsat_info | Sat x -> Ok x

let of_result = function Error unsat_info -> Unsat unsat_info | Ok x -> Sat x

let list_fold l ~init ~f =
  List.fold_result l ~init ~f:(fun accum x -> f accum x |> to_result) |> of_result


let to_list sat_unsat = sat sat_unsat |> Option.to_list

let filter l = List.filter_map l ~f:sat

let seq_fold seq ~init ~f =
  let open Import in
  Stdlib.Seq.fold_left
    (fun accum x ->
      let* accum in
      f accum x )
    (Sat init) seq
