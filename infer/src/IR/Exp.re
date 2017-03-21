/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

let module Hashtbl = Caml.Hashtbl;


/** The Smallfoot Intermediate Language: Expressions */
let module L = Logging;

let module F = Format;

/* reverse the natural order on Var */
type _ident = Ident.t;

let compare__ident x y => Ident.compare y x;

type closure = {name: Typ.Procname.t, captured_vars: list (t, Pvar.t, Typ.t)} [@@deriving compare]
/** dynamically determined length of an array value, if any */
and dynamic_length = option t [@@deriving compare]
/** Program expressions. */
and t =
  /** Pure variable: it is not an lvalue */
  | Var _ident
  /** Unary operator with type of the result if known */
  | UnOp Unop.t t (option Typ.t)
  /** Binary operator */
  | BinOp Binop.t t t
  /** Exception */
  | Exn t
  /** Anonymous function */
  | Closure closure
  /** Constants */
  | Const Const.t
  /** Type cast */
  | Cast Typ.t t
  /** The address of a program variable */
  | Lvar Pvar.t
  /** A field offset, the type is the surrounding struct type */
  | Lfield t Fieldname.t Typ.t
  /** An array index offset: [exp1\[exp2\]] */
  | Lindex t t
  /** A sizeof expression. [Sizeof (Tarray elt (Some static_length)) (Some dynamic_length)]
      represents the size of an array value consisting of [dynamic_length] elements of type [elt].
      The [dynamic_length], tracked by symbolic execution, may differ from the [static_length]
      obtained from the type definition, e.g. when an array is over-allocated.  For struct types,
      the [dynamic_length] is that of the final extensible array, if any. */
  | Sizeof Typ.t dynamic_length Subtype.t;

let equal = [%compare.equal : t];

let hash = Hashtbl.hash;

let module Set = Caml.Set.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Map = Caml.Map.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Hash = Hashtbl.Make {
  type nonrec t = t;
  let equal = equal;
  let hash = hash;
};

let rec is_array_index_of exp1 exp2 =>
  switch exp1 {
  | Lindex exp _ => is_array_index_of exp exp2
  | _ => equal exp1 exp2
  };

let is_null_literal =
  fun
  | Const (Cint n) => IntLit.isnull n
  | _ => false;

let is_this =
  fun
  | Lvar pvar => Pvar.is_this pvar
  | _ => false;

let is_zero =
  fun
  | Const (Cint n) => IntLit.iszero n
  | _ => false;


/** {2 Utility Functions for Expressions} */

/** Turn an expression representing a type into the type it represents
    If not a sizeof, return the default type if given, otherwise raise an exception */
let texp_to_typ default_opt =>
  fun
  | Sizeof t _ _ => t
  | _ => Typ.unsome "texp_to_typ" default_opt;


/** Return the root of [lexp]. */
let rec root_of_lexp lexp =>
  switch (lexp: t) {
  | Var _ => lexp
  | Const _ => lexp
  | Cast _ e => root_of_lexp e
  | UnOp _
  | BinOp _
  | Exn _
  | Closure _ => lexp
  | Lvar _ => lexp
  | Lfield e _ _ => root_of_lexp e
  | Lindex e _ => root_of_lexp e
  | Sizeof _ => lexp
  };


/** Checks whether an expression denotes a location by pointer arithmetic.
    Currently, catches array - indexing expressions such as a[i] only. */
let rec pointer_arith =
  fun
  | Lfield e _ _ => pointer_arith e
  | Lindex _ => true
  | _ => false;

let get_undefined footprint =>
  Var (Ident.create_fresh (if footprint {Ident.kfootprint} else {Ident.kprimed}));


/** Create integer constant */
let int i => Const (Cint i);


/** Create float constant */
let float v => Const (Cfloat v);


/** Integer constant 0 */
let zero = int IntLit.zero;


/** Null constant */
let null = int IntLit.null;


/** Integer constant 1 */
let one = int IntLit.one;


/** Integer constant -1 */
let minus_one = int IntLit.minus_one;


/** Create integer constant corresponding to the boolean value */
let bool b => if b {one} else {zero};


/** Create expresstion [e1 == e2] */
let eq e1 e2 => BinOp Eq e1 e2;


/** Create expresstion [e1 != e2] */
let ne e1 e2 => BinOp Ne e1 e2;


/** Create expression [e1 <= e2] */
let le e1 e2 => BinOp Le e1 e2;


/** Create expression [e1 < e2] */
let lt e1 e2 => BinOp Lt e1 e2;


/** Extract the ids and pvars from an expression */
let get_vars exp => {
  let rec get_vars_ exp vars =>
    switch exp {
    | Lvar pvar => (fst vars, [pvar, ...snd vars])
    | Var id => ([id, ...fst vars], snd vars)
    | Cast _ e
    | UnOp _ e _
    | Lfield e _ _
    | Exn e => get_vars_ e vars
    | BinOp _ e1 e2
    | Lindex e1 e2 => get_vars_ e1 vars |> get_vars_ e2
    | Closure {captured_vars} =>
      List.fold
        f::(fun vars_acc (captured_exp, _, _) => get_vars_ captured_exp vars_acc)
        init::vars
        captured_vars
    | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _ | Cptr_to_fld _) => vars
    /* TODO: Sizeof length expressions may contain variables, do not ignore them. */
    /* | Sizeof _ None _ => vars */
    /* | Sizeof _ (Some l) _ => get_vars_ l vars */
    | Sizeof _ _ _ => vars
    };
  get_vars_ exp ([], [])
};


/** Pretty print an expression. */
let rec pp_ pe pp_t f e => {
  let pp_exp = pp_ pe pp_t;
  let print_binop_stm_output e1 op e2 =>
    switch (op: Binop.t) {
    | Eq
    | Ne
    | PlusA
    | Mult => F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe op) pp_exp e1
    | Lt => F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Gt) pp_exp e1
    | Gt => F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Lt) pp_exp e1
    | Le => F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Ge) pp_exp e1
    | Ge => F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Le) pp_exp e1
    | _ => F.fprintf f "(%a %s %a)" pp_exp e1 (Binop.str pe op) pp_exp e2
    };
  switch (e: t) {
  | Var id => (Ident.pp pe) f id
  | Const c => F.fprintf f "%a" (Const.pp pe) c
  | Cast typ e => F.fprintf f "(%a)%a" pp_t typ pp_exp e
  | UnOp op e _ => F.fprintf f "%s%a" (Unop.str op) pp_exp e
  | BinOp op (Const c) e2 when Config.smt_output => print_binop_stm_output (Const c) op e2
  | BinOp op e1 e2 => F.fprintf f "(%a %s %a)" pp_exp e1 (Binop.str pe op) pp_exp e2
  | Exn e => F.fprintf f "EXN %a" pp_exp e
  | Closure {name, captured_vars} =>
    let id_exps = List.map f::(fun (id_exp, _, _) => id_exp) captured_vars;
    F.fprintf f "(%a)" (Pp.comma_seq pp_exp) [Const (Cfun name), ...id_exps]
  | Lvar pv => Pvar.pp pe f pv
  | Lfield e fld _ => F.fprintf f "%a.%a" pp_exp e Fieldname.pp fld
  | Lindex e1 e2 => F.fprintf f "%a[%a]" pp_exp e1 pp_exp e2
  | Sizeof t l s =>
    let pp_len f l => Option.iter f::(F.fprintf f "[%a]" pp_exp) l;
    F.fprintf f "sizeof(%a%a%a)" pp_t t pp_len l Subtype.pp s
  }
};

let pp_printenv pe pp_typ f e => pp_ pe (pp_typ pe) f e;

let pp f e => pp_printenv Pp.text Typ.pp f e;

let to_string e => F.asprintf "%a" pp e;
