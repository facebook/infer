(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Kind = struct
  type t = string [@@deriving compare, equal]

  type kind_info = {name: string; is_data_flow_only: bool}

  (** Taint "kinds" are user-configurable and thus represented as strings. This hash table ensures
      we only store one copy of each kind. It also identifies which kinds are designated for data
      flow reporting only. *)
  let all_kinds = Hashtbl.create (module String)

  let of_string name =
    (* use [all_kinds] to do a weak hashconsing and try to keep only one version of each string
       around. This does not ensure we always get the same representative for each string because
       kinds get marshalled in and out of summaries, which does not maintain physical equality
       between equal kinds *)
    (Hashtbl.find_or_add all_kinds name ~default:(fun () -> {name; is_data_flow_only= false})).name


  let hash kind = String.hash kind

  let sexp_of_t kind = String.sexp_of_t kind

  let mark_data_flow_only name =
    Hashtbl.update all_kinds name ~f:(fun _ -> {name; is_data_flow_only= true})


  let is_data_flow_only name =
    Hashtbl.find all_kinds name |> Option.exists ~f:(fun {is_data_flow_only} -> is_data_flow_only)


  let pp fmt kind =
    F.fprintf fmt "`%s`%s" kind (if is_data_flow_only kind then " (data flow only)" else "")
end

type origin =
  | Argument of {index: int}
  | ReturnValue
  | Allocation of {typ: string}
  | Field of {name: string; origin: origin}
[@@deriving compare, equal]

let rec pp_origin fmt = function
  | Argument {index} ->
      F.fprintf fmt "value passed as argument `#%d` to" index
  | ReturnValue ->
      F.fprintf fmt "value returned from"
  | Allocation {typ} ->
      F.fprintf fmt "allocation of type `%s` by" typ
  | Field {name; origin} ->
      F.fprintf fmt "field `%s` of %a" name pp_origin origin


type t =
  {kinds: Kind.t list; proc_name: Procname.t; origin: origin; block_passed_to: Procname.t option}
[@@deriving compare, equal]

let pp fmt {kinds; proc_name; origin; block_passed_to} =
  let proc_name_s =
    match block_passed_to with
    | Some passed_to_proc_name ->
        F.asprintf "a block passed to `%a`" Procname.pp passed_to_proc_name
    | None ->
        F.asprintf "`%a`" Procname.pp proc_name
  in
  F.fprintf fmt "%a %s with kind%s %a" pp_origin origin proc_name_s
    (match kinds with [_] -> "" | _ -> "s")
    (Pp.seq ~sep:"," Kind.pp) kinds
