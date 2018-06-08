(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | Base of AccessPath.base
  | FieldOffset of t * Typ.Fieldname.t
  | ArrayOffset of t * Typ.t * t list
  | AddressOf of t
  | Dereference of t
[@@deriving compare]

(** convert to an AccessPath.t, ignoring AddressOf and Dereference for now *)
let rec to_access_path t =
  let rec to_access_path_ t =
    match t with
    | Base base ->
        (base, [])
    | FieldOffset (ae, fld) ->
        let base, accesses = to_access_path_ ae in
        (base, AccessPath.FieldAccess fld :: accesses)
    | ArrayOffset (ae, typ, index_aes) ->
        let access_paths = to_access_paths index_aes in
        let base, accesses = to_access_path_ ae in
        (base, AccessPath.ArrayAccess (typ, access_paths) :: accesses)
    | AddressOf ae ->
        to_access_path_ ae
    | Dereference ae ->
        to_access_path_ ae
  in
  let base, accesses = to_access_path_ t in
  (base, List.rev accesses)


and to_access_paths ts = List.map ~f:to_access_path ts

let rec get_base = function
  | Base base ->
      base
  | FieldOffset (ae, _) | ArrayOffset (ae, _, _) | AddressOf ae | Dereference ae ->
      get_base ae


let rec replace_base ~remove_deref_after_base base_new access_expr =
  let replace_base_inner = replace_base ~remove_deref_after_base base_new in
  match access_expr with
  | Dereference (Base _) ->
      if remove_deref_after_base then Base base_new else Dereference (Base base_new)
  | Base _ ->
      Base base_new
  | FieldOffset (ae, fld) ->
      FieldOffset (replace_base_inner ae, fld)
  | ArrayOffset (ae, typ, aes) ->
      ArrayOffset (replace_base_inner ae, typ, aes)
  | AddressOf ae ->
      AddressOf (replace_base_inner ae)
  | Dereference ae ->
      Dereference (replace_base_inner ae)


let is_base = function Base _ -> true | _ -> false

let lookup_field_type_annot tenv base_typ field_name =
  let lookup = Tenv.lookup tenv in
  Typ.Struct.get_field_type_and_annotation ~lookup field_name base_typ


let rec get_typ t tenv : Typ.t option =
  match t with
  | Base (_, typ) ->
      Some typ
  | FieldOffset (ae, fld)
    -> (
      let base_typ_opt = get_typ ae tenv in
      match base_typ_opt with
      | Some base_typ ->
          Option.map (lookup_field_type_annot tenv base_typ fld) ~f:fst
      | None ->
          None )
  | ArrayOffset (_, typ, _) ->
      Some typ
  | AddressOf ae ->
      let base_typ_opt = get_typ ae tenv in
      Option.map base_typ_opt ~f:(fun base_typ -> Typ.mk (Tptr (base_typ, Pk_pointer)))
  | Dereference ae ->
      let base_typ_opt = get_typ ae tenv in
      match base_typ_opt with Some {Typ.desc= Tptr (typ, _)} -> Some typ | _ -> None


let rec pp fmt = function
  | Base (pvar, _) ->
      Var.pp fmt pvar
  | FieldOffset (Dereference ae, fld) ->
      F.fprintf fmt "%a->%a" pp ae Typ.Fieldname.pp fld
  | FieldOffset (ae, fld) ->
      F.fprintf fmt "%a.%a" pp ae Typ.Fieldname.pp fld
  | ArrayOffset (ae, _, []) ->
      F.fprintf fmt "%a[_]" pp ae
  | ArrayOffset (ae, _, index_aes) ->
      F.fprintf fmt "%a[%a]" pp ae (PrettyPrintable.pp_collection ~pp_item:pp) index_aes
  | AddressOf ae ->
      F.fprintf fmt "&(%a)" pp ae
  | Dereference ae ->
      F.fprintf fmt "*(%a)" pp ae


let equal = [%compare.equal : t]

let base_of_id id typ = (Var.of_id id, typ)

let base_of_pvar pvar typ = (Var.of_pvar pvar, typ)

let of_pvar pvar typ = AddressOf (Base (base_of_pvar pvar typ))

let of_id id typ = Base (base_of_id id typ)

(* cancel out consective *&'s *)
let rec normalize t =
  match t with
  | Base _ ->
      t
  | Dereference (AddressOf t1) ->
      normalize t1
  | FieldOffset (t1, fieldname) ->
      let t1' = normalize t1 in
      if phys_equal t1 t1' then t else normalize (FieldOffset (t1', fieldname))
  | ArrayOffset (t1, typ, tlist) ->
      let t1' = normalize t1 in
      let tlist' = IList.map_changed ~f:normalize ~equal tlist in
      if phys_equal t1 t1' && phys_equal tlist tlist' then t
      else normalize (ArrayOffset (t1', typ, tlist'))
  | Dereference t1 ->
      let t1' = normalize t1 in
      if phys_equal t1 t1' then t else normalize (Dereference t1')
  | AddressOf t1 ->
      let t1' = normalize t1 in
      if phys_equal t1 t1' then t else normalize (AddressOf t1')


(* Adapted from AccessPath.of_exp. *)
let of_exp ~include_array_indexes ~add_deref exp0 typ0 ~(f_resolve_id: Var.t -> t option) =
  let rec of_exp_ exp typ (add_accesses: t -> t) acc : t list =
    match exp with
    | Exp.Var id -> (
      match f_resolve_id (Var.of_id id) with
      | Some access_expr ->
          let access_expr' = if add_deref then Dereference access_expr else access_expr in
          add_accesses access_expr' :: acc
      | None ->
          let access_expr = of_id id typ in
          let access_expr' = if add_deref then Dereference access_expr else access_expr in
          add_accesses access_expr' :: acc )
    | Exp.Lvar pvar when Pvar.is_ssa_frontend_tmp pvar -> (
      match f_resolve_id (Var.of_pvar pvar) with
      | Some access_expr ->
          (* do not need to add deref here as it was added implicitly in the binding *)
          (* but need to remove it if add_deref is false *)
          let access_expr' =
            if not add_deref then match access_expr with Dereference ae -> ae | _ -> assert false
            else access_expr
          in
          add_accesses access_expr' :: acc
      | None ->
          let access_expr = of_pvar pvar typ in
          let access_expr' = if add_deref then Dereference access_expr else access_expr in
          add_accesses access_expr' :: acc )
    | Exp.Lvar pvar ->
        let access_expr = of_pvar pvar typ in
        let access_expr' = if add_deref then Dereference access_expr else access_expr in
        add_accesses access_expr' :: acc
    | Exp.Lfield (root_exp, fld, root_exp_typ) ->
        let add_field_access_expr access_expr = add_accesses (FieldOffset (access_expr, fld)) in
        of_exp_ root_exp root_exp_typ add_field_access_expr acc
    | Exp.Lindex (root_exp, index_exp) ->
        let index_access_exprs =
          if include_array_indexes then of_exp_ index_exp typ Fn.id [] else []
        in
        let add_array_access_expr access_expr =
          add_accesses (ArrayOffset (access_expr, typ, index_access_exprs))
        in
        let array_typ = Typ.mk_array typ in
        of_exp_ root_exp array_typ add_array_access_expr acc
    | Exp.Cast (cast_typ, cast_exp) ->
        of_exp_ cast_exp cast_typ Fn.id acc
    | Exp.UnOp (_, unop_exp, _) ->
        of_exp_ unop_exp typ Fn.id acc
    | Exp.Exn exn_exp ->
        of_exp_ exn_exp typ Fn.id acc
    | Exp.BinOp (_, exp1, exp2) ->
        of_exp_ exp1 typ Fn.id acc |> of_exp_ exp2 typ Fn.id
    | Exp.Const _ | Closure _ | Sizeof _ ->
        acc
  in
  IList.map_changed ~f:normalize ~equal (of_exp_ exp0 typ0 Fn.id [])


let of_lhs_exp ~include_array_indexes ~add_deref lhs_exp typ ~(f_resolve_id: Var.t -> t option) =
  match lhs_exp with
  | Exp.Lfield _ when not add_deref
    -> (
      let res = of_exp ~include_array_indexes ~add_deref:true lhs_exp typ ~f_resolve_id in
      match res with [lhs_ae] -> Some (AddressOf lhs_ae) | _ -> None )
  | Exp.Lindex _ when not add_deref
    -> (
      let res =
        let typ' =
          match typ.Typ.desc with
          | Tptr (t, _) ->
              t
          | _ ->
              (* T29630813 investigate cases where this is not a pointer *)
              typ
        in
        of_exp ~include_array_indexes ~add_deref:true lhs_exp typ' ~f_resolve_id
      in
      match res with [lhs_ae] -> Some (AddressOf lhs_ae) | _ -> None )
  | _ ->
      let res = of_exp ~include_array_indexes ~add_deref lhs_exp typ ~f_resolve_id in
      match res with [lhs_ae] -> Some lhs_ae | _ -> None
