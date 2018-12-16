(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type typ_ = Typ.t

let compare_typ_ _ _ = 0

module Access = struct
  type 'array_index t =
    | FieldAccess of Typ.Fieldname.t
    | ArrayAccess of typ_ * 'array_index
    | TakeAddress
    | Dereference
  [@@deriving compare]

  let pp pp_array_index fmt = function
    | FieldAccess field_name ->
        Typ.Fieldname.pp fmt field_name
    | ArrayAccess (_, index) ->
        F.fprintf fmt "[%a]" pp_array_index index
    | TakeAddress ->
        F.pp_print_string fmt "&"
    | Dereference ->
        F.pp_print_string fmt "*"
end

(** Module where unsafe construction of [access_expression] is allowed. In the rest of the code, and
   especially in clients of the whole [AccessExpression] module, we do not want to allow
   constructing access expressions directly as they could introduce de-normalized expressions of the
   form [AddressOf (Dereference t)] or [Dereference (AddressOf t)].

    We could make only the types of [AddressOf] and [Dereference] private but that proved too
   cumbersome...  *)
module T : sig
  type t =
    | AccessExpression of access_expression
    | UnaryOperator of Unop.t * t * Typ.t option
    | BinaryOperator of Binop.t * t * t
    | Exception of t
    | Closure of Typ.Procname.t * (AccessPath.base * t) list
    | Constant of Const.t
    | Cast of Typ.t * t
    | Sizeof of Typ.t * t option

  and access_expression = private
    | Base of AccessPath.base
    | FieldOffset of access_expression * Typ.Fieldname.t
    | ArrayOffset of access_expression * typ_ * t option
    | AddressOf of access_expression
    | Dereference of access_expression
  [@@deriving compare]

  module UnsafeAccessExpression : sig
    val base : AccessPath.base -> access_expression

    val field_offset : access_expression -> Typ.Fieldname.t -> access_expression

    val array_offset : access_expression -> Typ.t -> t option -> access_expression

    val address_of : access_expression -> access_expression

    val dereference : access_expression -> access_expression

    val replace_base :
      remove_deref_after_base:bool -> AccessPath.base -> access_expression -> access_expression
  end
end = struct
  type t =
    | AccessExpression of access_expression
    | UnaryOperator of Unop.t * t * Typ.t option
    | BinaryOperator of Binop.t * t * t
    | Exception of t
    | Closure of Typ.Procname.t * (AccessPath.base * t) list
    | Constant of Const.t
    | Cast of Typ.t * t
    | Sizeof of Typ.t * t option

  and access_expression =
    | Base of AccessPath.base
    | FieldOffset of access_expression * Typ.Fieldname.t
    | ArrayOffset of access_expression * typ_ * t option
    | AddressOf of access_expression
    | Dereference of access_expression
  [@@deriving compare]

  module UnsafeAccessExpression = struct
    let base base = Base base

    let field_offset t field = FieldOffset (t, field)

    let array_offset t typ index = ArrayOffset (t, typ, index)

    let address_of = function Dereference t -> t | t -> AddressOf t

    let dereference = function AddressOf t -> t | t -> Dereference t

    let rec replace_base ~remove_deref_after_base base_new access_expr =
      let replace_base_inner = replace_base ~remove_deref_after_base base_new in
      match access_expr with
      | Dereference (Base _) ->
          if remove_deref_after_base then Base base_new else Dereference (Base base_new)
      | Base _ ->
          Base base_new
      | FieldOffset (ae, fld) ->
          FieldOffset (replace_base_inner ae, fld)
      | ArrayOffset (ae, typ, x) ->
          ArrayOffset (replace_base_inner ae, typ, x)
      | AddressOf ae ->
          AddressOf (replace_base_inner ae)
      | Dereference ae ->
          Dereference (replace_base_inner ae)
  end
end

include T

let may_pp_typ fmt typ =
  if Config.debug_level_analysis >= 3 then F.fprintf fmt ":%a" (Typ.pp Pp.text) typ


let pp_array_offset_opt pp_offset fmt = function
  | None ->
      F.pp_print_string fmt "_"
  | Some offset ->
      pp_offset fmt offset


let rec pp_access_expr fmt = function
  | Base (pvar, typ) ->
      Var.pp fmt pvar ; may_pp_typ fmt typ
  | FieldOffset (Dereference ae, fld) ->
      F.fprintf fmt "%a->%a" pp_access_expr ae Typ.Fieldname.pp fld
  | FieldOffset (ae, fld) ->
      F.fprintf fmt "%a.%a" pp_access_expr ae Typ.Fieldname.pp fld
  | ArrayOffset (ae, typ, index) ->
      F.fprintf fmt "%a[%a]%a" pp_access_expr ae (pp_array_offset_opt pp) index may_pp_typ typ
  | AddressOf ae ->
      F.fprintf fmt "&(%a)" pp_access_expr ae
  | Dereference ae ->
      F.fprintf fmt "*(%a)" pp_access_expr ae


and pp fmt = function
  | AccessExpression access_expr ->
      pp_access_expr fmt access_expr
  | UnaryOperator (op, e, _) ->
      F.fprintf fmt "%s%a" (Unop.to_string op) pp e
  | BinaryOperator (op, e1, e2) ->
      F.fprintf fmt "%a %s %a" pp e1 (Binop.str Pp.text op) pp e2
  | Exception e ->
      F.fprintf fmt "exception %a" pp e
  | Closure (pname, captured) ->
      let pp_item fmt (base, exp) =
        match exp with
        | AccessExpression (Base b) when AccessPath.equal_base b base ->
            F.fprintf fmt "%a captured" AccessPath.pp_base b
        | _ ->
            F.fprintf fmt "%a captured as %a" AccessPath.pp_base base pp exp
      in
      F.fprintf fmt "closure(%a, %a)" Typ.Procname.pp pname
        (PrettyPrintable.pp_collection ~pp_item)
        captured
  | Constant c ->
      Const.pp Pp.text fmt c
  | Cast (typ, e) ->
      F.fprintf fmt "(%a) %a" (Typ.pp_full Pp.text) typ pp e
  | Sizeof (typ, length) ->
      let pp_length fmt = Option.iter ~f:(F.fprintf fmt "[%a]" pp) in
      F.fprintf fmt "sizeof(%a%a)" (Typ.pp_full Pp.text) typ pp_length length


let get_access_exprs exp0 =
  let rec get_access_exprs_ exp acc =
    match exp with
    | AccessExpression ae ->
        ae :: acc
    | Cast (_, e) | UnaryOperator (_, e, _) | Exception e | Sizeof (_, Some e) ->
        get_access_exprs_ e acc
    | BinaryOperator (_, e1, e2) ->
        get_access_exprs_ e1 acc |> get_access_exprs_ e2
    | Closure (_, captured) ->
        List.fold captured ~f:(fun acc (_, e) -> get_access_exprs_ e acc) ~init:acc
    | Constant _ | Sizeof _ ->
        acc
  in
  get_access_exprs_ exp0 []


module AccessExpression = struct
  include UnsafeAccessExpression

  type nonrec t = access_expression = private
    | Base of AccessPath.base
    | FieldOffset of access_expression * Typ.Fieldname.t
    | ArrayOffset of access_expression * typ_ * t option
    | AddressOf of access_expression
    | Dereference of access_expression
  [@@deriving compare]

  let pp = pp_access_expr

  let to_accesses_fold access_expr ~init ~f_array_offset =
    let rec aux accum accesses = function
      | Base base ->
          (accum, base, accesses)
      | FieldOffset (access_expr, fld) ->
          aux accum (Access.FieldAccess fld :: accesses) access_expr
      | ArrayOffset (access_expr, typ, index) ->
          let accum, index' = f_array_offset accum index in
          aux accum (Access.ArrayAccess (typ, index') :: accesses) access_expr
      | AddressOf access_expr ->
          aux accum (Access.TakeAddress :: accesses) access_expr
      | Dereference access_expr ->
          aux accum (Access.Dereference :: accesses) access_expr
    in
    aux init [] access_expr


  (** convert to an AccessPath.t, ignoring AddressOf and Dereference for now *)
  let rec to_access_path t =
    let rec to_access_path_ t =
      match t with
      | Base base ->
          (base, [])
      | FieldOffset (ae, fld) ->
          let base, accesses = to_access_path_ ae in
          (base, AccessPath.FieldAccess fld :: accesses)
      | ArrayOffset (ae, typ, index_opt) ->
          let access_paths =
            Option.value_map index_opt ~default:[] ~f:(fun index ->
                to_access_paths (get_access_exprs index) )
          in
          let base, accesses = to_access_path_ ae in
          (base, AccessPath.ArrayAccess (typ, access_paths) :: accesses)
      | AddressOf ae | Dereference ae ->
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


  let is_base = function Base _ -> true | _ -> false

  let lookup_field_type_annot tenv base_typ field_name =
    let lookup = Tenv.lookup tenv in
    Typ.Struct.get_field_type_and_annotation ~lookup field_name base_typ


  let rec get_typ t tenv : Typ.t option =
    match t with
    | Base (_, typ) ->
        Some typ
    | FieldOffset (ae, fld) -> (
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
    | Dereference ae -> (
        let base_typ_opt = get_typ ae tenv in
        match base_typ_opt with Some {Typ.desc= Tptr (typ, _)} -> Some typ | _ -> None )


  let equal = [%compare.equal: t]

  let base_of_id id typ = (Var.of_id id, typ)

  let base_of_pvar pvar typ = (Var.of_pvar pvar, typ)

  let of_pvar pvar typ = address_of (base (base_of_pvar pvar typ))

  let of_id id typ = base (base_of_id id typ)

  let rec fold_vars ae ~init ~f =
    match ae with
    | Base (var, _typ) ->
        f init var
    | FieldOffset (ae, _) | AddressOf ae | Dereference ae ->
        fold_vars ae ~init ~f
    | ArrayOffset (ae, _typ, exp_opt) ->
        let init = fold_vars ae ~init ~f in
        fold_vars_exp_opt exp_opt ~init ~f


  and fold_vars_exp_opt exp_opt ~init ~f =
    Option.fold exp_opt ~init ~f:(fun init ae -> fold_vars_exp ae ~init ~f)


  and fold_vars_exp exp ~init ~f =
    match exp with
    | AccessExpression ae ->
        fold_vars ae ~init ~f
    | UnaryOperator (_, exp, _) | Exception exp | Cast (_, exp) ->
        fold_vars_exp exp ~init ~f
    | BinaryOperator (_, exp1, exp2) ->
        let init = fold_vars_exp exp1 ~init ~f in
        fold_vars_exp exp2 ~init ~f
    | Closure (_, capt) ->
        List.fold capt ~init ~f:(fun init ((var, _typ), exp) ->
            let init = f init var in
            fold_vars_exp exp ~init ~f )
    | Constant _ ->
        init
    | Sizeof (_, exp_opt) ->
        fold_vars_exp_opt exp_opt ~init ~f
end

let rec get_typ tenv = function
  | AccessExpression access_expr ->
      AccessExpression.get_typ access_expr tenv
  | UnaryOperator (_, _, typ_opt) ->
      typ_opt
  | BinaryOperator ((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr), _, _) ->
      Some (Typ.mk (Typ.Tint Typ.IBool))
  | BinaryOperator (_, e1, e2) -> (
    (* TODO: doing this properly will require taking account of language-specific coercion
          semantics. Only return a type when the operands have the same type for now *)
    match (get_typ tenv e1, get_typ tenv e2) with
    | Some typ1, Some typ2 when Typ.equal typ1 typ2 ->
        Some typ1
    | _ ->
        None )
  | Exception t ->
      get_typ tenv t
  | Closure _ | Constant (Cfun _) ->
      (* We don't have a way to represent function types *)
      None
  | Constant (Cint _) ->
      (* TODO: handle signedness *)
      Some (Typ.mk (Typ.Tint Typ.IInt))
  | Constant (Cfloat _) ->
      Some (Typ.mk (Typ.Tfloat Typ.FFloat))
  | Constant (Cclass _) ->
      (* TODO: this only happens in Java. We probably need to change it to `Cclass of Typ.Name.t`
         to give a useful result here *)
      None
  | Constant (Cstr _) ->
      (* TODO: this will need to behave differently depending on whether we're in C++ or Java *)
      None
  | Cast (typ, _) ->
      Some typ
  | Sizeof _ ->
      (* sizeof returns a size_t, which is an unsigned int *)
      Some (Typ.mk (Typ.Tint Typ.IUInt))


let rec array_index_of_exp ~include_array_indexes ~f_resolve_id ~add_deref exp typ =
  if include_array_indexes then
    Some (of_sil ~include_array_indexes ~f_resolve_id ~add_deref exp typ)
  else None


(* Adapted from AccessPath.of_exp. *)
and access_exprs_of_exp ~include_array_indexes ~f_resolve_id ~add_deref exp0 typ0 =
  let rec of_exp_ exp typ (add_accesses : AccessExpression.t -> AccessExpression.t) acc :
      AccessExpression.t list =
    match exp with
    | Exp.Var id -> (
      match f_resolve_id (Var.of_id id) with
      | Some access_expr ->
          let access_expr' =
            if add_deref then AccessExpression.dereference access_expr else access_expr
          in
          add_accesses access_expr' :: acc
      | None ->
          let access_expr = AccessExpression.of_id id typ in
          let access_expr' =
            if add_deref then AccessExpression.dereference access_expr else access_expr
          in
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
          let access_expr = AccessExpression.of_pvar pvar typ in
          let access_expr' =
            if add_deref then AccessExpression.dereference access_expr else access_expr
          in
          add_accesses access_expr' :: acc )
    | Exp.Lvar pvar ->
        let access_expr = AccessExpression.of_pvar pvar typ in
        let access_expr' =
          if add_deref then AccessExpression.dereference access_expr else access_expr
        in
        add_accesses access_expr' :: acc
    | Exp.Lfield (root_exp, fld, root_exp_typ) ->
        let add_field_access_expr access_expr =
          add_accesses (AccessExpression.field_offset access_expr fld)
        in
        of_exp_ root_exp root_exp_typ add_field_access_expr acc
    | Exp.Lindex (root_exp, index_exp) ->
        let index =
          let index_typ = (* TODO: bogus *) Typ.void in
          array_index_of_exp ~include_array_indexes ~f_resolve_id ~add_deref index_exp index_typ
        in
        let add_array_access_expr access_expr =
          add_accesses (AccessExpression.array_offset access_expr typ index)
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
  of_exp_ exp0 typ0 Fn.id []


and access_expr_of_lhs_exp ~include_array_indexes ~f_resolve_id ~add_deref lhs_exp typ =
  match lhs_exp with
  | Exp.Lfield _ when not add_deref -> (
      let res =
        access_exprs_of_exp ~include_array_indexes ~f_resolve_id ~add_deref:true lhs_exp typ
      in
      match res with [lhs_ae] -> Some (AccessExpression.address_of lhs_ae) | _ -> None )
  | Exp.Lindex _ when not add_deref -> (
      let res =
        let typ' =
          match typ.Typ.desc with
          | Tptr (t, _) ->
              t
          | _ ->
              (* T29630813 investigate cases where this is not a pointer *)
              typ
        in
        access_exprs_of_exp ~include_array_indexes ~f_resolve_id ~add_deref:true lhs_exp typ'
      in
      match res with [lhs_ae] -> Some (AccessExpression.address_of lhs_ae) | _ -> None )
  | _ -> (
      let res = access_exprs_of_exp ~include_array_indexes ~f_resolve_id ~add_deref lhs_exp typ in
      match res with [lhs_ae] -> Some lhs_ae | _ -> None )


(* convert an SIL expression into an HIL expression. the [f_resolve_id] function should map an SSA
   temporary variable to the access path it represents. evaluating the HIL expression should
   produce the same result as evaluating the SIL expression and replacing the temporary variables
   using [f_resolve_id] *)
and of_sil ~include_array_indexes ~f_resolve_id ~add_deref exp typ =
  let rec of_sil_ (exp : Exp.t) typ =
    match exp with
    | Var id ->
        let ae =
          match f_resolve_id (Var.of_id id) with
          | Some access_expr ->
              if add_deref then AccessExpression.dereference access_expr else access_expr
          | None ->
              let access_expr = AccessExpression.of_id id typ in
              if add_deref then AccessExpression.dereference access_expr else access_expr
        in
        AccessExpression ae
    | UnOp (op, e, typ_opt) ->
        UnaryOperator (op, of_sil_ e typ, typ_opt)
    | BinOp (op, e0, e1) ->
        BinaryOperator (op, of_sil_ e0 typ, of_sil_ e1 typ)
    | Exn e ->
        Exception (of_sil_ e typ)
    | Const c ->
        Constant c
    | Cast (cast_typ, e) ->
        Cast (cast_typ, of_sil_ e typ)
    | Sizeof {typ; dynamic_length} ->
        Sizeof (typ, Option.map ~f:(fun e -> of_sil_ e typ) dynamic_length)
    | Closure closure ->
        let environment =
          List.map
            ~f:(fun (value, pvar, typ) ->
              (* TODO: this is a hack than can disappear once we have proper AccessExpressions (t23176430) *)
              let typ' =
                match value with
                | Exp.Lvar pvar1 when Pvar.equal pvar1 pvar ->
                    (* captured by reference, change type to pointer *)
                    {typ with Typ.desc= Typ.Tptr (typ, Typ.Pk_pointer)}
                | _ ->
                    (* capture by value *)
                    typ
              in
              (AccessPath.base_of_pvar pvar typ', of_sil_ value typ') )
            closure.captured_vars
        in
        Closure (closure.name, environment)
    | Lfield (root_exp, fld, root_exp_typ) -> (
      match access_expr_of_lhs_exp ~include_array_indexes ~f_resolve_id ~add_deref exp typ with
      | Some access_expr ->
          AccessExpression access_expr
      | None ->
          (* unsupported field expression: represent with a dummy variable *)
          of_sil_
            (Exp.Lfield
               ( Var (Ident.create_normal (Ident.string_to_name (Exp.to_string root_exp)) 0)
               , fld
               , root_exp_typ ))
            typ )
    | Lindex (Const (Cstr s), index_exp) ->
        (* indexed string literal (e.g., "foo"[1]). represent this by introducing a dummy variable
         for the string literal. if you actually need to see the value of the string literal in the
         analysis, you should probably be using SIL. this is unsound if the code modifies the
         literal, e.g. using `const_cast<char*>` *)
        of_sil_ (Exp.Lindex (Var (Ident.create_normal (Ident.string_to_name s) 0), index_exp)) typ
    | Lindex (root_exp, index_exp) -> (
      match access_expr_of_lhs_exp ~include_array_indexes ~f_resolve_id ~add_deref exp typ with
      | Some access_expr ->
          AccessExpression access_expr
      | None ->
          (* unsupported index expression: represent with a dummy variable *)
          of_sil_
            (Exp.Lindex
               ( Var (Ident.create_normal (Ident.string_to_name (Exp.to_string root_exp)) 0)
               , index_exp ))
            typ )
    | Lvar _ -> (
      match access_expr_of_lhs_exp ~include_array_indexes ~f_resolve_id ~add_deref exp typ with
      | Some access_expr ->
          AccessExpression access_expr
      | None ->
          L.(die InternalError) "Couldn't convert var expression %a to access path" Exp.pp exp )
  in
  of_sil_ exp typ


let rec is_null_literal = function
  | Constant (Cint n) ->
      IntLit.isnull n
  | Cast (_, e) ->
      is_null_literal e
  | _ ->
      false


let rec is_int_zero = function
  | Constant (Const.Cint i) ->
      IntLit.iszero i
  | Cast (_, e) ->
      is_int_zero e
  | _ ->
      false


let rec eval_arithmetic_binop op e1 e2 =
  match (eval e1, eval e2) with
  | Some (Const.Cint i1), Some (Const.Cint i2) ->
      Some (Const.Cint (op i1 i2))
  | _ ->
      None


and eval = function
  | Constant c ->
      Some c
  | Cast (_, e) ->
      eval e
  | BinaryOperator (Binop.Div, e1, e2) -> (
    try eval_arithmetic_binop IntLit.div e1 e2 with Division_by_zero -> None )
  | BinaryOperator (Binop.MinusA _, e1, e2) ->
      eval_arithmetic_binop IntLit.sub e1 e2
  | BinaryOperator (Binop.Mod, e1, e2) ->
      eval_arithmetic_binop IntLit.rem e1 e2
  | BinaryOperator (Binop.Mult _, e1, e2) ->
      eval_arithmetic_binop IntLit.mul e1 e2
  | BinaryOperator (Binop.PlusA _, e1, e2) ->
      eval_arithmetic_binop IntLit.add e1 e2
  | _ ->
      (* TODO: handle bitshifting cases, port eval_binop from RacerD.ml *)
      None


let rec ignore_cast e = match e with Cast (_, e) -> ignore_cast e | _ -> e
