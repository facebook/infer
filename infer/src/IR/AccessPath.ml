(*
 * Copyright (c) 2016-present, Facebook, Inc.
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

  type access = ArrayAccess of typ_ * t list | FieldAccess of Typ.Fieldname.t

  and t = base * access list [@@deriving compare]

  let equal_access = [%compare.equal: access]

  let may_pp_typ fmt typ =
    if Config.debug_level_analysis >= 3 then F.fprintf fmt ":%a" (Typ.pp Pp.text) typ


  let pp_base fmt (pvar, typ) = Var.pp fmt pvar ; may_pp_typ fmt typ

  let rec pp_access fmt = function
    | FieldAccess field_name ->
        F.pp_print_string fmt (Typ.Fieldname.to_flat_string field_name)
    | ArrayAccess (typ, []) ->
        F.pp_print_string fmt "[_]" ; may_pp_typ fmt typ
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

  let truncate ((base, accesses) as t) =
    match List.rev accesses with
    | [] ->
        (t, None)
    | last_access :: accesses ->
        ((base, List.rev accesses), Some last_access)


  let lookup_field_type_annot tenv base_typ field_name =
    let lookup = Tenv.lookup tenv in
    Typ.Struct.get_field_type_and_annotation ~lookup field_name base_typ


  (* Get the type of an access, or None if the type cannot be determined *)
  let get_access_type tenv base_typ = function
    | FieldAccess field_name ->
        Option.map (lookup_field_type_annot tenv base_typ field_name) ~f:fst
    | ArrayAccess (array_typ, _) ->
        Some array_typ


  (* For field access, get the field name and the annotation associated with it
   * Return None if given an array access, or if the info cannot be obtained *)
  let get_access_field_annot tenv base_typ = function
    | FieldAccess field_name ->
        Option.map (lookup_field_type_annot tenv base_typ field_name) ~f:(fun (_, annot) ->
            (field_name, annot) )
    | ArrayAccess _ ->
        None


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


  let get_last_access (_, accesses) = List.last accesses

  let get_field_and_annotation ap tenv =
    match last_access_info ap tenv with
    | Some base_typ, Some access ->
        get_access_field_annot tenv base_typ access
    | _ ->
        None


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

  let of_exp ~include_array_indexes exp0 typ0 ~(f_resolve_id : Var.t -> t option) =
    (* [typ] is the type of the last element of the access path (e.g., typeof(g) for x.f.g) *)
    let rec of_exp_ exp typ accesses acc =
      match exp with
      | Exp.Var id -> (
        match f_resolve_id (Var.of_id id) with
        | Some (base, base_accesses) ->
            (base, base_accesses @ accesses) :: acc
        | None ->
            (base_of_id id typ, accesses) :: acc )
      | Exp.Lvar pvar when Pvar.is_ssa_frontend_tmp pvar -> (
        match f_resolve_id (Var.of_pvar pvar) with
        | Some (base, base_accesses) ->
            (base, base_accesses @ accesses) :: acc
        | None ->
            (base_of_pvar pvar typ, accesses) :: acc )
      | Exp.Lvar pvar ->
          (base_of_pvar pvar typ, accesses) :: acc
      | Exp.Lfield (root_exp, fld, root_exp_typ) ->
          let field_access = FieldAccess fld in
          of_exp_ root_exp root_exp_typ (field_access :: accesses) acc
      | Exp.Lindex (root_exp, index_exp) ->
          let index_access_paths =
            if include_array_indexes then of_exp_ index_exp typ [] [] else []
          in
          let array_access = ArrayAccess (typ, index_access_paths) in
          let array_typ = Typ.mk_array typ in
          of_exp_ root_exp array_typ (array_access :: accesses) acc
      | Exp.Cast (cast_typ, cast_exp) ->
          of_exp_ cast_exp cast_typ [] acc
      | Exp.UnOp (_, unop_exp, _) ->
          of_exp_ unop_exp typ [] acc
      | Exp.Exn exn_exp ->
          of_exp_ exn_exp typ [] acc
      | Exp.BinOp (_, exp1, exp2) ->
          of_exp_ exp1 typ [] acc |> of_exp_ exp2 typ []
      | Exp.Const _ | Closure _ | Sizeof _ ->
          (* trying to make access path from an invalid expression *)
          acc
    in
    of_exp_ exp0 typ0 [] []


  let of_lhs_exp ~include_array_indexes lhs_exp typ ~(f_resolve_id : Var.t -> t option) =
    match of_exp ~include_array_indexes lhs_exp typ ~f_resolve_id with
    | [lhs_ap] ->
        Some lhs_ap
    | _ ->
        None


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


  let is_prefix ap1 ap2 = chop_prefix ~prefix:ap1 ap2 |> Option.is_some

  let replace_prefix ~prefix access_path replace_with =
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

  let ( <= ) ~lhs ~rhs =
    match (lhs, rhs) with
    | Abstracted _, Exact _ ->
        false
    | Exact lhs_ap, Exact rhs_ap ->
        Raw.equal lhs_ap rhs_ap
    | (Exact lhs_ap | Abstracted lhs_ap), Abstracted rhs_ap ->
        Raw.is_prefix rhs_ap lhs_ap


  let pp fmt = function
    | Exact access_path ->
        Raw.pp fmt access_path
    | Abstracted access_path ->
        F.fprintf fmt "%a*" Raw.pp access_path
end

include Raw

module BaseMap = PrettyPrintable.MakePPMap (struct
  type t = base

  let compare = compare_base

  let pp = pp_base
end)

(* transform an access path that starts on "this" of an inner class but which breaks out to
   access outer class fields to the outermost one *)
let inner_class_normalize p =
  let open Typ in
  let is_synthetic_this pvar = Pvar.get_simplified_name pvar |> String.is_prefix ~prefix:"this$" in
  let mk_pvar_as name pvar = Pvar.get_declaring_function pvar |> Option.map ~f:(Pvar.mk name) in
  let aux = function
    (* (this:InnerClass* ).(this$n:OuterClassAccessor).f. ... -> (this:OuterClass* ).f . ... *)
    | Some
        ( ( (Var.ProgramVar pvar as root)
          , ({desc= Tptr (({desc= Tstruct name} as cls), pkind)} as ptr) )
        , FieldAccess first :: accesses )
      when Pvar.is_this pvar && Fieldname.Java.is_outer_instance first ->
        Name.Java.get_outer_class name
        |> Option.map ~f:(fun outer_name ->
               let outer_class = mk ~default:cls (Tstruct outer_name) in
               let outer_ptr = mk ~default:ptr (Tptr (outer_class, pkind)) in
               ((root, outer_ptr), accesses) )
    (* this$n.(this$m:OuterClassAccessor).f ... -> (this$m:OuterClass* ).f . ... *)
    (* happens in ctrs only *)
    | Some
        ( (Var.ProgramVar pvar, ({desc= Tptr (({desc= Tstruct name} as cls), pkind)} as ptr))
        , FieldAccess first :: accesses )
      when is_synthetic_this pvar && Fieldname.Java.is_outer_instance first ->
        Name.Java.get_outer_class name
        |> Option.bind ~f:(fun outer_name ->
               let outer_class = mk ~default:cls (Tstruct outer_name) in
               let outer_ptr = mk ~default:ptr (Tptr (outer_class, pkind)) in
               let varname = Fieldname.to_flat_string first |> Mangled.from_string in
               mk_pvar_as varname pvar
               |> Option.map ~f:(fun new_pvar ->
                      let base = base_of_pvar new_pvar outer_ptr in
                      (base, accesses) ) )
    (* this$n.f ... -> this.f . ... *)
    (* happens in ctrs only *)
    | Some ((Var.ProgramVar pvar, typ), all_accesses) when is_synthetic_this pvar ->
        mk_pvar_as Mangled.this pvar
        |> Option.map ~f:(fun new_pvar -> (base_of_pvar new_pvar typ, all_accesses))
    | _ ->
        None
  in
  let rec loop path_opt = match aux path_opt with None -> path_opt | res -> loop res in
  loop (Some p) |> Option.value ~default:p
