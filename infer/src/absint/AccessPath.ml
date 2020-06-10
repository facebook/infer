(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Raw = struct
  type typ_ = Typ.t

  let compare_typ_ _ _ = 0

  (* ignore types while comparing bases. we can't trust the types from all of our frontends to be
     consistent, and the variable names should already be enough to distinguish the bases. *)
  type base = Var.t * typ_ [@@deriving compare]

  let equal_base = [%compare.equal: base]

  type access = ArrayAccess of typ_ * t list | FieldAccess of Fieldname.t

  and t = base * access list [@@deriving compare]

  let equal_access = [%compare.equal: access]

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


  let equal = [%compare.equal: t]

  let lookup_field_type_annot tenv base_typ field_name =
    let lookup = Tenv.lookup tenv in
    Struct.get_field_type_and_annotation ~lookup field_name base_typ


  (* Get the type of an access, or None if the type cannot be determined *)
  let get_access_type tenv base_typ = function
    | FieldAccess field_name ->
        Option.map (lookup_field_type_annot tenv base_typ field_name) ~f:fst
    | ArrayAccess (array_typ, _) ->
        Some array_typ


  (* Extract the last access of the given access path together with its base type.
   * Here the base type is defined to be the declaring class of the last accessed field,
   * or the type of base if the access list is empty.
   * For example:
   * - for x.f.g, the base type of the last access is typ(f);
   * - for x.f[][], the base type of the last access is typ(x);
   * - for x, the base type of the last access is type(x) *)
  let last_access_info ((_, base_typ), accesses) tenv =
    let rec last_access_info_impl tenv base_typ = function
      | [] ->
          (Some base_typ, None)
      | [last_access] ->
          (Some base_typ, Some last_access)
      | curr_access :: rest -> (
        match get_access_type tenv base_typ curr_access with
        | Some access_typ ->
            last_access_info_impl tenv access_typ rest
        | None ->
            (None, None) )
    in
    last_access_info_impl tenv base_typ accesses


  let get_typ ap tenv =
    match last_access_info ap tenv with
    | (Some _ as typ), None ->
        typ
    | Some base_typ, Some access ->
        get_access_type tenv base_typ access
    | _ ->
        None


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

  type t = Abstracted of Raw.t | Exact of Raw.t [@@deriving compare]

  let equal = [%compare.equal: t]

  let extract = function Exact ap | Abstracted ap -> ap

  let with_base base = function
    | Exact (_, accesses) ->
        Exact (base, accesses)
    | Abstracted (_, accesses) ->
        Abstracted (base, accesses)


  let to_footprint formal_index access_path =
    let _, base_typ = fst (extract access_path) in
    with_base (Var.of_formal_index formal_index, base_typ) access_path


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
