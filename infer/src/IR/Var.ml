(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Single abstraction for all the kinds of variables in SIL *)

type t = LogicalVar of Ident.t | ProgramVar of Pvar.t [@@deriving compare]

let equal = [%compare.equal: t]

let compare_modulo_this x y =
  if phys_equal x y then 0
  else
    match (x, y) with
    | ProgramVar i, ProgramVar j ->
        Pvar.compare_modulo_this i j
    | _, _ ->
        compare x y


let of_id id = LogicalVar id

let of_pvar pvar = ProgramVar pvar

let of_formal_index formal_index = of_id (Ident.create_footprint Ident.name_spec formal_index)

let to_exp = function ProgramVar pvar -> Exp.Lvar pvar | LogicalVar id -> Exp.Var id

let get_ident = function ProgramVar _ -> None | LogicalVar id -> Some id

let get_pvar = function ProgramVar pvar -> Some pvar | LogicalVar _ -> None

let is_global = function ProgramVar pvar -> Pvar.is_global pvar | LogicalVar _ -> false

let is_return = function ProgramVar pvar -> Pvar.is_return pvar | LogicalVar _ -> false

let is_this = function ProgramVar pvar -> Pvar.is_this pvar | LogicalVar _ -> false

let is_footprint = function ProgramVar _ -> false | LogicalVar id -> Ident.is_footprint id

let is_none = function LogicalVar id -> Ident.is_none id | _ -> false

let get_declaring_function = function
  | LogicalVar _ ->
      None
  | ProgramVar pvar ->
      Pvar.get_declaring_function pvar


let is_local_to_procedure proc_name var =
  get_declaring_function var |> Option.exists ~f:(Typ.Procname.equal proc_name)


let get_all_vars_in_exp e =
  let acc = Exp.free_vars e |> Sequence.map ~f:of_id in
  Exp.program_vars e |> Sequence.map ~f:of_pvar |> Sequence.append acc


let appears_in_source_code = function
  | LogicalVar _ ->
      false
  | ProgramVar pvar ->
      not (Pvar.is_frontend_tmp pvar)


let is_cpp_temporary = function
  | LogicalVar _ ->
      false
  | ProgramVar pvar ->
      Pvar.is_cpp_temporary pvar


let pp fmt = function
  | ProgramVar pv ->
      F.pp_print_string fmt (Pvar.get_simplified_name pv)
  | LogicalVar id ->
      Ident.pp fmt id


let get_footprint_index t =
  match t with LogicalVar id when is_footprint t -> Some (Ident.get_stamp id) | _ -> None


module Map = PrettyPrintable.MakePPMap (struct
  type nonrec t = t

  let compare = compare

  let pp = pp
end)
