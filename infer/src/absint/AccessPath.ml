(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Raw = struct
  (* ignore types while comparing bases. we can't trust the types from all of our frontends to be
     consistent, and the variable names should already be enough to distinguish the bases. *)
  type base = Var.t * (Typ.t[@ignore]) [@@deriving compare, equal]

  type access = ArrayAccess of (Typ.t[@ignore]) * t list | FieldAccess of Fieldname.t

  and t = base * access list [@@deriving compare, equal]

  let may_pp_typ fmt typ =
    if Config.debug_level_analysis >= 3 then F.fprintf fmt ":%a" (Typ.pp Pp.text) typ


  let pp_base fmt (pvar, typ) =
    Var.pp fmt pvar ;
    may_pp_typ fmt typ


  let rec pp_access fmt = function
    | FieldAccess field_name ->
        F.pp_print_string fmt (Fieldname.get_field_name field_name)
    | ArrayAccess (typ, []) ->
        F.pp_print_string fmt "[_]" ;
        may_pp_typ fmt typ
    | ArrayAccess (typ, index_aps) ->
        F.fprintf fmt "[%a]" (PrettyPrintable.pp_collection ~pp_item:pp) index_aps ;
        may_pp_typ fmt typ


  and pp_access_list fmt accesses =
    let pp_sep fmt () = F.pp_print_char fmt '.' in
    F.pp_print_list ~pp_sep pp_access fmt accesses


  and pp fmt = function
    | base, [] ->
        pp_base fmt base
    | base, accesses ->
        F.fprintf fmt "%a.%a" pp_base base pp_access_list accesses


  let base_of_pvar pvar typ = (Var.of_pvar pvar, typ)

  let base_of_id id typ = (Var.of_id id, typ)

  let of_pvar pvar typ = (base_of_pvar pvar typ, [])

  let of_id id typ = (base_of_id id typ, [])

  let of_var var typ =
    match var with Var.LogicalVar id -> of_id id typ | Var.ProgramVar pvar -> of_pvar pvar typ


  let append (base, old_accesses) new_accesses = (base, old_accesses @ new_accesses)

  let rec chop_prefix_path ~prefix:path1 path2 =
    if phys_equal path1 path2 then Some []
    else
      match (path1, path2) with
      | [], remaining ->
          Some remaining
      | _, [] ->
          None
      | access1 :: prefix, access2 :: rest when equal_access access1 access2 ->
          chop_prefix_path ~prefix rest
      | _ ->
          None


  let chop_prefix ~prefix:((base1, path1) as ap1) ((base2, path2) as ap2) =
    if phys_equal ap1 ap2 then Some []
    else if equal_base base1 base2 then chop_prefix_path ~prefix:path1 path2
    else None


  let replace_prefix ~prefix ~replace_with access_path =
    match chop_prefix ~prefix access_path with
    | Some remaining_accesses ->
        Some (append replace_with remaining_accesses)
    | None ->
        None
end

module Abs = struct
  type raw = Raw.t

  type t = Abstracted of Raw.t | Exact of Raw.t [@@deriving compare, equal]

  let extract = function Exact ap | Abstracted ap -> ap

  let get_footprint_index_base base =
    match base with
    | Var.LogicalVar id, _ when Ident.is_footprint id ->
        Some (Ident.get_stamp id)
    | _ ->
        None


  let is_exact = function Exact _ -> true | Abstracted _ -> false

  let pp fmt = function
    | Exact access_path ->
        Raw.pp fmt access_path
    | Abstracted access_path ->
        F.fprintf fmt "%a*" Raw.pp access_path
end

include Raw

module BaseMap = PrettyPrintable.MakePPMap (struct
  type t = base [@@deriving compare]

  let pp = pp_base
end)
