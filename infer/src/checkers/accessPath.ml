(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

type _typ = Typ.t

let compare__typ _ _ = 0

(* ignore types while comparing bases. we can't trust the types from all of our frontends to be
   consistent, and the variable names should already be enough to distinguish the bases. *)
type base = Var.t * _typ [@@deriving compare]

let equal_base = [%compare.equal : base]

type access = ArrayAccess of Typ.t | FieldAccess of Typ.Fieldname.t [@@deriving compare]

let equal_access = [%compare.equal : access]

let equal_access_list l1 l2 = Int.equal (List.compare compare_access l1 l2) 0

let pp_base fmt (pvar, _) = Var.pp fmt pvar

let pp_access fmt = function
  | FieldAccess field_name
   -> Typ.Fieldname.pp fmt field_name
  | ArrayAccess _
   -> F.fprintf fmt "[_]"

let pp_access_list fmt accesses =
  let pp_sep _ _ = F.fprintf fmt "." in
  F.pp_print_list ~pp_sep pp_access fmt accesses

module Raw = struct
  type t = base * access list [@@deriving compare]

  let equal = [%compare.equal : t]

  let truncate = function
    | base, [] | base, [_]
     -> (base, [])
    | base, accesses
     -> (base, List.rev (List.tl_exn (List.rev accesses)))

  let lookup_field_type_annot tenv base_typ field_name =
    let lookup = Tenv.lookup tenv in
    Typ.Struct.get_field_type_and_annotation ~lookup field_name base_typ

  (* Get the type of an access, or None if the type cannot be determined *)
  let get_access_type tenv base_typ = function
    | FieldAccess field_name
     -> Option.map (lookup_field_type_annot tenv base_typ field_name) ~f:fst
    | ArrayAccess array_typ
     -> Some array_typ

  (* For field access, get the field name and the annotation associated with it
   * Return None if given an array access, or if the info cannot be obtained *)
  let get_access_field_annot tenv base_typ = function
    | FieldAccess field_name
     -> Option.map (lookup_field_type_annot tenv base_typ field_name) ~f:(fun (_, annot) ->
            (field_name, annot) )
    | ArrayAccess _
     -> None

  (* Extract the last access of the given access path together with its base type.
   * Here the base type is defined to be the declaring class of the last accessed field,
   * or the type of base if the access list is empty.
   * For example:
   * - for x.f.g, the base type of the last access is typ(f);
   * - for x.f[][], the base type of the last access is typ(x);
   * - for x, the base type of the last access is type(x) *)
  let last_access_info ((_, base_typ), accesses) tenv =
    let rec last_access_info_impl tenv base_typ = function
      | []
       -> (Some base_typ, None)
      | [last_access]
       -> (Some base_typ, Some last_access)
      | curr_access :: rest ->
        match get_access_type tenv base_typ curr_access with
        | Some access_typ
         -> last_access_info_impl tenv access_typ rest
        | None
         -> (None, None)
    in
    last_access_info_impl tenv base_typ accesses

  let get_last_access (_, accesses) = List.last accesses

  let get_field_and_annotation ap tenv =
    match last_access_info ap tenv with
    | Some base_typ, Some access
     -> get_access_field_annot tenv base_typ access
    | _
     -> None

  let get_typ ap tenv =
    match last_access_info ap tenv with
    | (Some _ as typ), None
     -> typ
    | Some base_typ, Some access
     -> get_access_type tenv base_typ access
    | _
     -> None

  let pp fmt = function
    | base, []
     -> pp_base fmt base
    | base, accesses
     -> F.fprintf fmt "%a.%a" pp_base base pp_access_list accesses
end

type t = Abstracted of Raw.t | Exact of Raw.t [@@deriving compare]

let equal = [%compare.equal : t]

let base_of_pvar pvar typ = (Var.of_pvar pvar, typ)

let base_of_id id typ = (Var.of_id id, typ)

let of_pvar pvar typ = (base_of_pvar pvar typ, [])

let of_id id typ = (base_of_id id typ, [])

let of_exp exp0 typ0 ~(f_resolve_id: Var.t -> Raw.t option) =
  (* [typ] is the type of the last element of the access path (e.g., typeof(g) for x.f.g) *)
  let rec of_exp_ exp typ accesses acc =
    match exp with
    | Exp.Var id -> (
      match f_resolve_id (Var.of_id id) with
      | Some (base, base_accesses)
       -> (base, base_accesses @ accesses) :: acc
      | None
       -> (base_of_id id typ, accesses) :: acc )
    | Exp.Lvar pvar when Pvar.is_ssa_frontend_tmp pvar -> (
      match f_resolve_id (Var.of_pvar pvar) with
      | Some (base, base_accesses)
       -> (base, base_accesses @ accesses) :: acc
      | None
       -> (base_of_pvar pvar typ, accesses) :: acc )
    | Exp.Lvar pvar
     -> (base_of_pvar pvar typ, accesses) :: acc
    | Exp.Lfield (root_exp, fld, root_exp_typ)
     -> let field_access = FieldAccess fld in
        of_exp_ root_exp root_exp_typ (field_access :: accesses) acc
    | Exp.Lindex (root_exp, _)
     -> let array_access = ArrayAccess typ in
        let array_typ = Typ.mk (Tarray (typ, None, None)) in
        of_exp_ root_exp array_typ (array_access :: accesses) acc
    | Exp.Cast (cast_typ, cast_exp)
     -> of_exp_ cast_exp cast_typ [] acc
    | Exp.UnOp (_, unop_exp, _)
     -> of_exp_ unop_exp typ [] acc
    | Exp.Exn exn_exp
     -> of_exp_ exn_exp typ [] acc
    | Exp.BinOp (_, exp1, exp2)
     -> of_exp_ exp1 typ [] acc |> of_exp_ exp2 typ []
    | Exp.Const _ | Closure _ | Sizeof _
     -> (* trying to make access path from an invalid expression *)
        acc
  in
  of_exp_ exp0 typ0 [] []

let of_lhs_exp lhs_exp typ ~(f_resolve_id: Var.t -> Raw.t option) =
  match of_exp lhs_exp typ ~f_resolve_id with [lhs_ap] -> Some lhs_ap | _ -> None

let append (base, old_accesses) new_accesses = (base, old_accesses @ new_accesses)

let with_base base = function
  | Exact (_, accesses)
   -> Exact (base, accesses)
  | Abstracted (_, accesses)
   -> Abstracted (base, accesses)

let rec is_prefix_path path1 path2 =
  if phys_equal path1 path2 then true
  else
    match (path1, path2) with
    | [], _
     -> true
    | _, []
     -> false
    | access1 :: p1, access2 :: p2
     -> equal_access access1 access2 && is_prefix_path p1 p2

let is_prefix (base1, path1 as ap1) (base2, path2 as ap2) =
  if phys_equal ap1 ap2 then true else equal_base base1 base2 && is_prefix_path path1 path2

let extract = function Exact ap | Abstracted ap -> ap

let to_footprint formal_index access_path =
  let _, base_typ = fst (extract access_path) in
  with_base (Var.of_formal_index formal_index, base_typ) access_path

let get_footprint_index access_path =
  let raw_access_path = extract access_path in
  match raw_access_path with
  | (Var.LogicalVar id, _), _ when Ident.is_footprint id
   -> Some (Ident.get_stamp id)
  | _
   -> None

let is_exact = function Exact _ -> true | Abstracted _ -> false

let ( <= ) ~lhs ~rhs =
  match (lhs, rhs) with
  | Abstracted _, Exact _
   -> false
  | Exact lhs_ap, Exact rhs_ap
   -> Raw.equal lhs_ap rhs_ap
  | (Exact lhs_ap | Abstracted lhs_ap), Abstracted rhs_ap
   -> is_prefix rhs_ap lhs_ap

let pp fmt = function
  | Exact access_path
   -> Raw.pp fmt access_path
  | Abstracted access_path
   -> F.fprintf fmt "%a*" Raw.pp access_path

module BaseMap = PrettyPrintable.MakePPMap (struct
  type t = base

  let compare = compare_base

  let pp = pp_base
end)

module AccessMap = PrettyPrintable.MakePPMap (struct
  type t = access

  let compare = compare_access

  let pp = pp_access
end)

module RawSet = PrettyPrintable.MakePPSet (Raw)
module RawMap = PrettyPrintable.MakePPMap (Raw)
