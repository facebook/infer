/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;


/** The Smallfoot Intermediate Language: Expressions */
let module L = Logging;

let module F = Format;

type closure = {name: Procname.t, captured_vars: list (t, Pvar.t, Typ.t)}
/** dynamically determined length of an array value, if any */
and dynamic_length = option t
/** Program expressions. */
and t =
  /** Pure variable: it is not an lvalue */
  | Var of Ident.t
  /** Unary operator with type of the result if known */
  | UnOp of Unop.t t (option Typ.t)
  /** Binary operator */
  | BinOp of Binop.t t t
  /** Exception */
  | Exn of t
  /** Anonymous function */
  | Closure of closure
  /** Constants */
  | Const of Const.t
  /** Type cast */
  | Cast of Typ.t t
  /** The address of a program variable */
  | Lvar of Pvar.t
  /** A field offset, the type is the surrounding struct type */
  | Lfield of t Ident.fieldname Typ.t
  /** An array index offset: [exp1\[exp2\]] */
  | Lindex of t t
  /** A sizeof expression. [Sizeof (Tarray elt (Some static_length)) (Some dynamic_length)]
      represents the size of an array value consisting of [dynamic_length] elements of type [elt].
      The [dynamic_length], tracked by symbolic execution, may differ from the [static_length]
      obtained from the type definition, e.g. when an array is over-allocated.  For struct types,
      the [dynamic_length] is that of the final extensible array, if any. */
  | Sizeof of Typ.t dynamic_length Subtype.t;


/** Compare expressions. Variables come before other expressions. */
let rec compare e1 e2 =>
  switch (e1, e2) {
  | (Var id1, Var id2) => Ident.compare id2 id1
  | (Var _, _) => (-1)
  | (_, Var _) => 1
  | (UnOp o1 e1 to1, UnOp o2 e2 to2) =>
    let n = Unop.compare o1 o2;
    if (n != 0) {
      n
    } else {
      let n = compare e1 e2;
      if (n != 0) {
        n
      } else {
        opt_compare Typ.compare to1 to2
      }
    }
  | (UnOp _, _) => (-1)
  | (_, UnOp _) => 1
  | (BinOp o1 e1 f1, BinOp o2 e2 f2) =>
    let n = Binop.compare o1 o2;
    if (n != 0) {
      n
    } else {
      let n = compare e1 e2;
      if (n != 0) {
        n
      } else {
        compare f1 f2
      }
    }
  | (BinOp _, _) => (-1)
  | (_, BinOp _) => 1
  | (Exn e1, Exn e2) => compare e1 e2
  | (Exn _, _) => (-1)
  | (_, Exn _) => 1
  | (Closure {name: n1, captured_vars: c1}, Closure {name: n2, captured_vars: c2}) =>
    let captured_var_compare acc (e1, pvar1, typ1) (e2, pvar2, typ2) =>
      if (acc != 0) {
        acc
      } else {
        let n = compare e1 e2;
        if (n != 0) {
          n
        } else {
          let n = Pvar.compare pvar1 pvar2;
          if (n != 0) {
            n
          } else {
            Typ.compare typ1 typ2
          }
        }
      };
    let n = Procname.compare n1 n2;
    if (n != 0) {
      n
    } else {
      IList.fold_left2 captured_var_compare 0 c1 c2
    }
  | (Closure _, _) => (-1)
  | (_, Closure _) => 1
  | (Const c1, Const c2) => Const.compare c1 c2
  | (Const _, _) => (-1)
  | (_, Const _) => 1
  | (Cast t1 e1, Cast t2 e2) =>
    let n = compare e1 e2;
    if (n != 0) {
      n
    } else {
      Typ.compare t1 t2
    }
  | (Cast _, _) => (-1)
  | (_, Cast _) => 1
  | (Lvar i1, Lvar i2) => Pvar.compare i1 i2
  | (Lvar _, _) => (-1)
  | (_, Lvar _) => 1
  | (Lfield e1 f1 t1, Lfield e2 f2 t2) =>
    let n = compare e1 e2;
    if (n != 0) {
      n
    } else {
      let n = Ident.fieldname_compare f1 f2;
      if (n != 0) {
        n
      } else {
        Typ.compare t1 t2
      }
    }
  | (Lfield _, _) => (-1)
  | (_, Lfield _) => 1
  | (Lindex e1 f1, Lindex e2 f2) =>
    let n = compare e1 e2;
    if (n != 0) {
      n
    } else {
      compare f1 f2
    }
  | (Lindex _, _) => (-1)
  | (_, Lindex _) => 1
  | (Sizeof t1 l1 s1, Sizeof t2 l2 s2) =>
    let n = Typ.compare t1 t2;
    if (n != 0) {
      n
    } else {
      let n = opt_compare compare l1 l2;
      if (n != 0) {
        n
      } else {
        Subtype.compare s1 s2
      }
    }
  };

let equal e1 e2 => compare e1 e2 == 0;

let hash = Hashtbl.hash;

let module Set = Set.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Map = Map.Make {
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
  Var (
    Ident.create_fresh (
      if footprint {
        Ident.kfootprint
      } else {
        Ident.kprimed
      }
    )
  );


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
let bool b =>
  if b {
    one
  } else {
    zero
  };


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
      IList.fold_left
        (fun vars_acc (captured_exp, _, _) => get_vars_ captured_exp vars_acc) vars captured_vars
    | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _ | Cptr_to_fld _) => vars
    /* TODO: Sizeof length expressions may contain variables, do not ignore them. */
    /* | Sizeof _ None _ => vars */
    /* | Sizeof _ (Some l) _ => get_vars_ l vars */
    | Sizeof _ _ _ => vars
    };
  get_vars_ exp ([], [])
};
