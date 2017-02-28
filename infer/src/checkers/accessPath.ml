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

type _array_sensitive_typ = Typ.t

let compare__array_sensitive_typ = Typ.array_sensitive_compare

type base = Var.t * _array_sensitive_typ [@@deriving compare]

let equal_base = [%compare.equal : base]

let compare_base_untyped (base_var1, _) (base_var2, _) =
  if phys_equal base_var1 base_var2
  then 0
  else Var.compare base_var1 base_var2

type access =
  | ArrayAccess of Typ.t
  | FieldAccess of Ident.fieldname
[@@deriving compare]

let equal_access = [%compare.equal : access]


let equal_access_list l1 l2 =  Int.equal (List.compare compare_access l1 l2) 0

let pp_base fmt (pvar, _) =
  Var.pp fmt pvar

let pp_access fmt = function
  | FieldAccess field_name -> Ident.pp_fieldname fmt field_name
  | ArrayAccess _ -> F.fprintf fmt "[_]"

let pp_access_list fmt accesses =
  let pp_sep _ _ = F.fprintf fmt "." in
  F.pp_print_list ~pp_sep pp_access fmt accesses

module Raw = struct
  type t = base * access list [@@deriving compare]
  let equal = [%compare.equal : t]

  let truncate = function
    | base, []
    | base, _ :: [] -> base, []
    | base, accesses -> base, List.rev (List.tl_exn (List.rev accesses))

  let get_typ ((_, base_typ), accesses) tenv =
    let rec accesses_get_typ last_typ tenv = function
      | [] ->
          Some last_typ
      | FieldAccess field_name :: accesses ->
          let lookup = Tenv.lookup tenv in
          begin
            match Typ.Struct.get_field_type_and_annotation ~lookup field_name last_typ with
            | Some (field_typ, _) -> accesses_get_typ field_typ tenv accesses
            | None -> None
          end
      | ArrayAccess array_typ :: accesses ->
          accesses_get_typ array_typ tenv accesses in
    accesses_get_typ base_typ tenv accesses

  let pp fmt = function
    | base, [] ->  pp_base fmt base
    | base, accesses ->  F.fprintf fmt "%a.%a" pp_base base pp_access_list accesses
end

module UntypedRaw = struct
  type t = Raw.t

  (* untyped comparison *)
  let compare ((base1, accesses1) as raw1) ((base2, accesses2) as raw2) =
    if phys_equal raw1 raw2
    then
      0
    else
      let n = compare_base_untyped base1 base2 in
      if n <> 0 then n
      else List.compare compare_access accesses1 accesses2

  let pp = Raw.pp
end

type t =
  | Abstracted of Raw.t
  | Exact of Raw.t
[@@deriving compare]

let equal = [%compare.equal : t]

let base_of_pvar pvar typ =
  Var.of_pvar pvar, typ

let base_of_id id typ =
  Var.of_id id, typ

let of_pvar pvar typ =
  base_of_pvar pvar typ, []

let of_id id typ =
  base_of_id id typ, []

let of_exp exp0 typ0 ~(f_resolve_id : Var.t -> Raw.t option) =
  (* [typ] is the type of the last element of the access path (e.g., typeof(g) for x.f.g) *)
  let rec of_exp_ exp typ accesses acc =
    match exp with
    | Exp.Var id ->
        begin
          match f_resolve_id (Var.of_id id) with
          | Some (base, base_accesses) -> (base, base_accesses @ accesses) :: acc
          | None -> (base_of_id id typ, accesses) :: acc
        end
    | Exp.Lvar pvar when Pvar.is_frontend_tmp pvar ->
        begin
          match f_resolve_id (Var.of_pvar pvar) with
          | Some (base, base_accesses) -> (base, base_accesses @ accesses) :: acc
          | None -> (base_of_pvar pvar typ, accesses) :: acc
        end
    | Exp.Lvar pvar ->
        (base_of_pvar pvar typ, accesses) :: acc
    | Exp.Lfield (root_exp, fld, root_exp_typ) ->
        let field_access = FieldAccess fld in
        of_exp_ root_exp root_exp_typ (field_access :: accesses) acc
    | Exp.Lindex (root_exp, _) ->
        let array_access = ArrayAccess typ in
        let array_typ = Typ.Tarray (typ, None) in
        of_exp_ root_exp array_typ (array_access :: accesses) acc
    | Exp.Cast (cast_typ, cast_exp) ->
        of_exp_ cast_exp cast_typ [] acc
    | Exp.UnOp (_, unop_exp, _) ->
        of_exp_ unop_exp typ [] acc
    | Exp.Exn exn_exp ->
        of_exp_ exn_exp typ [] acc
    | Exp.BinOp (_, exp1, exp2) ->
        of_exp_ exp1 typ [] acc
        |> of_exp_ exp2 typ []
    | Exp.Const _ | Closure _ | Sizeof _ ->
        (* trying to make access path from an invalid expression *)
        acc in
  of_exp_ exp0 typ0 [] []

let of_lhs_exp lhs_exp typ ~(f_resolve_id : Var.t -> Raw.t option) =
  match of_exp lhs_exp typ ~f_resolve_id with
  | [lhs_ap] -> Some lhs_ap
  | _ -> None

let append (base, old_accesses) new_accesses =
  base, old_accesses @ new_accesses

let with_base_var var = function
  | Exact ((_, base_typ), accesses) -> Exact ((var, base_typ), accesses)
  | Abstracted ((_, base_typ), accesses) -> Abstracted ((var, base_typ), accesses)

let rec is_prefix_path path1 path2 =
  if phys_equal path1 path2
  then true
  else
    match path1, path2 with
    | [], _ -> true
    | _, [] -> false
    | access1 :: p1, access2 :: p2 -> equal_access access1 access2 && is_prefix_path p1 p2

let is_prefix ((base1, path1) as ap1) ((base2, path2) as ap2) =
  if phys_equal ap1 ap2
  then true
  else
    equal_base base1 base2 && is_prefix_path path1 path2

let extract = function
  | Exact ap | Abstracted ap -> ap

let is_exact = function
  | Exact _ -> true
  | Abstracted _ -> false

let (<=) ~lhs ~rhs =
  match lhs, rhs with
  | Abstracted _, Exact _ -> false
  | Exact lhs_ap, Exact rhs_ap -> Raw.equal lhs_ap rhs_ap
  | (Exact lhs_ap | Abstracted lhs_ap), Abstracted rhs_ap -> is_prefix rhs_ap lhs_ap

let pp fmt = function
  | Exact access_path -> Raw.pp fmt access_path
  | Abstracted access_path -> F.fprintf fmt "%a*" Raw.pp access_path

module BaseMap = PrettyPrintable.MakePPMap(struct
    type t = base
    let compare = compare_base
    let pp = pp_base
  end)

module AccessMap = PrettyPrintable.MakePPMap(struct
    type t = access
    let compare = compare_access
    let pp = pp_access
  end)

module RawSet = PrettyPrintable.MakePPSet(Raw)

module RawMap = PrettyPrintable.MakePPMap(Raw)

module UntypedRawSet = PrettyPrintable.MakePPSet(UntypedRaw)

module UntypedRawMap = PrettyPrintable.MakePPMap(UntypedRaw)
