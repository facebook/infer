(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Kind = struct
  type t = string [@@deriving compare]

  (** Taint "kinds" are user-configurable and thus represented as strings. This hash table is here
      to make sure we only store one copy of each kind. *)
  let kind_hashconser = Hashtbl.create (module String)

  let of_string s = Hashtbl.find_or_add kind_hashconser s ~default:(fun () -> s)

  (* [phys_equal] is enough as kinds are hashcons'd *)
  let equal k1 k2 = phys_equal k1 k2

  let pp = F.pp_print_string

  let hash = String.hash

  let sexp_of_t = String.sexp_of_t
end

type origin = Argument of {index: int} | ReturnValue [@@deriving compare, equal]

let pp_origin fmt = function
  | Argument {index} ->
      F.fprintf fmt "passed as argument #%d to" index
  | ReturnValue ->
      F.fprintf fmt "value returned from"


type t = {kinds: Kind.t list; proc_name: Procname.t; origin: origin} [@@deriving compare, equal]

let pp fmt {kinds; proc_name; origin} =
  F.fprintf fmt "%a %a with kind%s %a" pp_origin origin Procname.pp proc_name
    (match kinds with [_] -> "" | _ -> "s")
    (Pp.seq ~sep:"," Kind.pp) kinds
