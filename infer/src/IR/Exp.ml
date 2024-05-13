(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Expressions *)

open! IStd
module Hashtbl = Caml.Hashtbl
module F = Format
module L = Logging

(* reverse the natural order on Var *)
type ident_ = Ident.t [@@deriving equal, hash, normalize]

let compare_ident_ x y = Ident.compare y x

type closure = {name: Procname.t; captured_vars: captured_var list}

and captured_var = t * Pvar.t * Typ.t * CapturedVar.capture_mode

(** This records information about a [sizeof(typ)] expression.

    [nbytes] represents the result of the evaluation of [sizeof(typ)] if it is statically known.

    If [typ] is of the form [Tarray elt (Some static_length)], then [dynamic_length] is the number
    of elements of type [elt] in the array. The [dynamic_length], tracked by symbolic execution, may
    differ from the [static_length] obtained from the type definition, e.g. when an array is
    over-allocated.

    If [typ] is a struct type, the [dynamic_length] is that of the final extensible array, if any.*)
and sizeof_data =
  {typ: Typ.t; nbytes: int option; dynamic_length: t option; subtype: Subtype.t; nullable: bool}

(** Program expressions. *)
and t =
  | Var of ident_  (** Pure variable: it is not an lvalue *)
  | UnOp of Unop.t * t * Typ.t option  (** Unary operator with type of the result if known *)
  | BinOp of Binop.t * t * t  (** Binary operator *)
  | Exn of t  (** Exception *)
  | Closure of closure  (** Anonymous function *)
  | Const of Const.t  (** Constants *)
  | Cast of Typ.t * t  (** Type cast *)
  | Lvar of Pvar.t  (** The address of a program variable *)
  | Lfield of t * Fieldname.t * Typ.t
      (** A field offset, the type is the surrounding struct type *)
  | Lindex of t * t  (** An array index offset: [exp1[exp2]] *)
  | Sizeof of sizeof_data
[@@deriving compare, equal, hash, normalize]

module Set = Caml.Set.Make (struct
  type nonrec t = t [@@deriving compare]
end)

module Map = Caml.Map.Make (struct
  type nonrec t = t [@@deriving compare]
end)

module Hash = Hashtbl.Make (struct
  type nonrec t = t [@@deriving equal, hash]
end)

let is_null_literal = function Const (Cint n) -> IntLit.isnull n | _ -> false

let is_this = function Lvar pvar -> Pvar.is_this pvar | _ -> false

let is_zero = function Const (Cint n) -> IntLit.iszero n | _ -> false

let rec is_const = function Const _ -> true | Cast (_, x) -> is_const x | _ -> false

(** {2 Utility Functions for Expressions} *)

(** Turn an expression representing a type into the type it represents If not a sizeof, return the
    default type if given, otherwise raise an exception *)
let texp_to_typ default_opt = function
  | Sizeof {typ} ->
      typ
  | _ ->
      Typ.unsome "texp_to_typ" default_opt


(** Return the root of [lexp]. *)
let rec root_of_lexp lexp =
  match (lexp : t) with
  | Var _ ->
      lexp
  | Const _ ->
      lexp
  | Cast (_, e) ->
      root_of_lexp e
  | UnOp _ | BinOp _ | Exn _ | Closure _ ->
      lexp
  | Lvar _ ->
      lexp
  | Lfield (e, _, _) ->
      root_of_lexp e
  | Lindex (e, _) ->
      root_of_lexp e
  | Sizeof _ ->
      lexp


(** Checks whether an expression denotes a location by pointer arithmetic. Currently, catches
    array-indexing expressions such as a[i] only. *)
let rec pointer_arith = function
  | Lfield (e, _, _) ->
      pointer_arith e
  | Lindex _ ->
      true
  | _ ->
      false


let get_undefined footprint =
  Var (Ident.create_fresh (if footprint then Ident.kfootprint else Ident.kprimed))


(** returns true if the express operates on address of local variable *)
let rec has_local_addr e =
  match (e : t) with
  | Lvar pv ->
      Pvar.is_local pv
  | UnOp (_, e', _) | Cast (_, e') | Lfield (e', _, _) ->
      has_local_addr e'
  | BinOp (_, e0, e1) | Lindex (e0, e1) ->
      has_local_addr e0 || has_local_addr e1
  | _ ->
      false


(** Create integer constant *)
let int i = Const (Cint i)

(** Create float constant *)
let float v = Const (Cfloat v)

(** Integer constant 0 *)
let zero = int IntLit.zero

(** Null constant *)
let null = int IntLit.null

(** Integer constant 1 *)
let one = int IntLit.one

(** Integer constant -1 *)
let minus_one = int IntLit.minus_one

(** Create integer constant corresponding to the boolean value *)
let bool b = if b then one else zero

(** Create expression [e1 == e2] *)
let eq e1 e2 = BinOp (Eq, e1, e2)

(** Create expression [e1 != e2] *)
let ne e1 e2 = BinOp (Ne, e1, e2)

(** Create expression [e1 <= e2] *)
let le e1 e2 = BinOp (Le, e1, e2)

(** Create expression [e1 < e2] *)
let lt e1 e2 = BinOp (Lt, e1, e2)

let fold_captured ~f exp acc =
  let rec fold_captured_ exp captured_acc =
    match exp with
    | Cast (_, e) | UnOp (_, e, _) | Lfield (e, _, _) | Exn e | Sizeof {dynamic_length= Some e} ->
        fold_captured_ e captured_acc
    | BinOp (_, e1, e2) | Lindex (e1, e2) ->
        fold_captured_ e1 captured_acc |> fold_captured_ e2
    | Closure {captured_vars} ->
        List.fold captured_vars
          ~f:(fun acc (captured_exp, _, _, _) -> f acc captured_exp)
          ~init:captured_acc
    | Const _ | Lvar _ | Var _ | Sizeof _ ->
        captured_acc
  in
  fold_captured_ exp acc


(** Pretty print an expression. *)
let rec pp_ pe pp_t f e =
  let pp_exp = pp_ pe pp_t in
  let print_binop_stm_output e1 op e2 =
    match (op : Binop.t) with
    | Eq | Ne | PlusA _ | Mult _ ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe op) pp_exp e1
    | Lt ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Gt) pp_exp e1
    | Gt ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Lt) pp_exp e1
    | Le ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Ge) pp_exp e1
    | Ge ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Le) pp_exp e1
    | _ ->
        F.fprintf f "(%a %s %a)" pp_exp e1 (Binop.str pe op) pp_exp e2
  in
  match (e : t) with
  | Var id ->
      Ident.pp f id
  | Const c ->
      (Const.pp pe) f c
  | Cast (typ, e) ->
      F.fprintf f "(%a)%a" pp_t typ pp_exp e
  | UnOp (op, e, _) ->
      F.fprintf f "%s%a" (Unop.to_string op) pp_exp e
  | BinOp (op, Const c, e2) when Config.smt_output ->
      print_binop_stm_output (Const c) op e2
  | BinOp (op, e1, e2) ->
      F.fprintf f "(%a %s %a)" pp_exp e1 (Binop.str pe op) pp_exp e2
  | Exn e ->
      F.fprintf f "EXN %a" pp_exp e
  | Closure closure ->
      pp_closure_ pe pp_t f closure
  | Lvar pv ->
      Pvar.pp pe f pv
  | Lfield (e, fld, _) ->
      F.fprintf f "%a.%a" pp_exp e Fieldname.pp fld
  | Lindex (e1, e2) ->
      F.fprintf f "%a[%a]" pp_exp e1 pp_exp e2
  | Sizeof {typ; nbytes; dynamic_length; subtype; nullable} ->
      let pp_len f l = Option.iter ~f:(F.fprintf f "[%a]" pp_exp) l in
      let pp_size f size = Option.iter ~f:(Int.pp f) size in
      let pp_if b pp label f v = if b then F.fprintf f ";%s=%a" label pp v in
      let pp_if_some pp_opt label f opt = pp_if (Option.is_some opt) pp_opt label f opt in
      let subt_s = F.asprintf "%a" Subtype.pp subtype in
      F.fprintf f "sizeof(t=%a%a%a%a;nullable=%b)" pp_t typ (pp_if_some pp_size "nbytes") nbytes
        (pp_if_some pp_len "len") dynamic_length
        (pp_if (not (String.equal "" subt_s)) Subtype.pp "sub_t")
        subtype nullable


and pp_captured_var pe pp_t f (exp, var, typ, mode) =
  match exp with
  | Lvar evar when Pvar.equal var evar ->
      (Pvar.pp pe) f var
  | _ ->
      F.fprintf f "([%s]%a %a:%a)"
        (CapturedVar.string_of_capture_mode mode)
        (pp_ pe pp_t) exp (Pvar.pp pe) var (Typ.pp pe) typ


and pp_closure_ pe pp_t f {name; captured_vars} =
  let pp_exp = pp_ pe pp_t in
  if List.is_empty captured_vars then F.fprintf f "(%a)" pp_exp (Const (Cfun name))
  else
    F.fprintf f "(%a,%a)" pp_exp (Const (Cfun name))
      (Pp.comma_seq (pp_captured_var pe pp_t))
      captured_vars


let pp_printenv ~print_types pe f e =
  let pp_typ = if print_types then Typ.pp_full else Typ.pp in
  pp_ pe (pp_typ pe) f e


let pp f e = pp_printenv ~print_types:false Pp.text f e

let pp_closure = pp_closure_ Pp.text (Typ.pp Pp.text)

let to_string e = F.asprintf "%a" pp e

let color_wrapper ~f = if Config.print_using_diff then Pp.color_wrapper ~f else f

let pp_diff ?(print_types = false) =
  color_wrapper ~f:(fun pe f e0 ->
      let e =
        match pe.Pp.obj_sub with
        | Some sub ->
            (* apply object substitution to expression *) Obj.obj (sub (Obj.repr e0))
        | None ->
            e0
      in
      if not (equal e0 e) then match e with Lvar pvar -> Pvar.pp_value f pvar | _ -> assert false
      else pp_printenv ~print_types pe f e )


(** dump an expression. *)
let d_exp (e : t) = L.d_pp_with_pe pp_diff e

(** Pretty print a list of expressions. *)
let pp_list pe f expl = Pp.seq (pp_diff pe) f expl

(** dump a list of expressions. *)
let d_list (el : t list) = L.d_pp_with_pe pp_list el

let pp_texp pe f = function
  | Sizeof {typ; nbytes; dynamic_length; subtype} ->
      let pp_len f l = Option.iter ~f:(F.fprintf f "[%a]" (pp_diff pe)) l in
      let pp_size f size = Option.iter ~f:(Int.pp f) size in
      F.fprintf f "%a%a%a%a" (Typ.pp pe) typ pp_size nbytes pp_len dynamic_length Subtype.pp subtype
  | e ->
      pp_diff pe f e


(** Pretty print a type with all the details. *)
let pp_texp_full pe f = function
  | Sizeof {typ; nbytes; dynamic_length; subtype} ->
      let pp_len f l = Option.iter ~f:(F.fprintf f "[%a]" (pp_diff pe)) l in
      let pp_size f size = Option.iter ~f:(Int.pp f) size in
      F.fprintf f "%a%a%a%a" (Typ.pp_full pe) typ pp_size nbytes pp_len dynamic_length Subtype.pp
        subtype
  | e ->
      pp_diff ~print_types:true pe f e


(** Dump a type expression with all the details. *)
let d_texp_full (te : t) = L.d_pp_with_pe pp_texp_full te

let is_cpp_closure = function Closure {name} -> Procname.is_cpp_lambda name | _ -> false

let rec gen_free_vars =
  let open Sequence.Generator in
  function
  | Var id ->
      yield id
  | Cast (_, e) | Exn e | Lfield (e, _, _) | Sizeof {dynamic_length= Some e} | UnOp (_, e, _) ->
      gen_free_vars e
  | Closure {captured_vars} ->
      ISequence.gen_sequence_list captured_vars ~f:(fun (e, _, _, _) -> gen_free_vars e)
  | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _) | Lvar _ | Sizeof {dynamic_length= None}
    ->
      return ()
  | BinOp (_, e1, e2) | Lindex (e1, e2) ->
      gen_free_vars e1 >>= fun () -> gen_free_vars e2


let free_vars e = Sequence.Generator.run (gen_free_vars e)

let ident_mem e id = free_vars e |> Sequence.exists ~f:(Ident.equal id)

let rec gen_program_vars =
  let open Sequence.Generator in
  function
  | Lvar name ->
      yield name
  | Const _ | Var _ | Sizeof {dynamic_length= None} ->
      return ()
  | Cast (_, e) | Exn e | Lfield (e, _, _) | Sizeof {dynamic_length= Some e} | UnOp (_, e, _) ->
      gen_program_vars e
  | BinOp (_, e1, e2) | Lindex (e1, e2) ->
      gen_program_vars e1 >>= fun () -> gen_program_vars e2
  | Closure {captured_vars} ->
      ISequence.gen_sequence_list captured_vars ~f:(fun (e, _, _, _) -> gen_program_vars e)


let program_vars e = Sequence.Generator.run (gen_program_vars e)

let rec gen_closures =
  let open Sequence.Generator in
  function
  | Closure closure ->
      yield closure
  | Const _ | Lvar _ | Var _ ->
      return ()
  | UnOp (_, e1, _) | Exn e1 | Cast (_, e1) | Lfield (e1, _, _) ->
      gen_closures e1
  | Sizeof {dynamic_length= Some e1} ->
      gen_closures e1
  | Sizeof {dynamic_length= None} ->
      return ()
  | BinOp (_, e1, e2) | Lindex (e1, e2) ->
      gen_closures e1 >>= fun () -> gen_closures e2


let closures e = Sequence.Generator.run (gen_closures e)

let zero_of_type typ =
  match typ.Typ.desc with
  | Typ.Tint _ ->
      Some (Const (Cint IntLit.zero))
  | Typ.Tfloat _ ->
      Some (Const (Cfloat 0.0))
  | Typ.Tptr _ ->
      Some (Const (Cint IntLit.null))
  | _ ->
      None


let zero_of_type_exn typ = Option.value_exn (zero_of_type typ)

let rec ignore_cast e = match e with Cast (_, e) -> ignore_cast e | _ -> e

let rec ignore_integer_cast e =
  match e with Cast (t, e) when Typ.is_int t -> ignore_integer_cast e | _ -> e


let rec get_java_class_initializer tenv = function
  | Lfield (Lvar pvar, fn, typ) when Pvar.is_global pvar -> (
    match Struct.get_field_type_and_annotation ~lookup:(Tenv.lookup tenv) fn typ with
    | Some (field_typ, annot) when Annot.Item.is_final annot ->
        let java_class =
          Typ.JavaClass (JavaClassName.from_string (Mangled.to_string_full (Pvar.get_name pvar)))
        in
        Some (Procname.Java (Procname.Java.get_class_initializer java_class), pvar, fn, field_typ)
    | _ ->
        None )
  | Cast (_, e) | Lfield (e, _, _) ->
      get_java_class_initializer tenv e
  | Lvar _ | Var _ | UnOp _ | BinOp _ | Exn _ | Closure _ | Const _ | Lindex _ | Sizeof _ ->
      None
