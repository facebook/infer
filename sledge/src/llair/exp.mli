(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Expressions

    Pure (heap-independent) expressions are complex arithmetic,
    bitwise-logical, etc. operations over literal values and registers.

    Expressions are represented in curried form, where the only recursive
    constructor is [App], which is an application of a function symbol to an
    argument. This is done to simplify the definition of 'subexpression' and
    make it explicit, which is a significant help for treating equality
    between expressions using congruence closure. The specific constructor
    functions indicate and check the expected arity and types of the
    function symbols. *)

type t = private
  | Var of {name: string; typ: Typ.t; loc: Loc.t}
      (** Local variable / virtual register *)
  | Global of {name: string; init: t option; typ: Typ.t; loc: Loc.t}
      (** Global variable, with initalizer *)
  | Nondet of {typ: Typ.t; loc: Loc.t; msg: string}
      (** Anonymous local variable with arbitrary value, representing
          non-deterministic approximation of value described by [msg]. *)
  | Label of {parent: string; name: string; loc: Loc.t}
      (** Address of named code block within parent function *)
  | Null of {typ: Typ.t}
      (** Pointer value that never refers to an object *)
  | App of {op: t; arg: t; loc: Loc.t}
      (** Application of function symbol to argument, curried *)
  | AppN of {op: t; args: t vector; loc: Loc.t}
      (** Application of function symbol to arguments. NOTE: may be cyclic
          when [op] is [Struct]. *)
  | PtrFld of {fld: int}  (** Pointer to a field of a struct *)
  | PtrIdx  (** Pointer to an index of an array *)
  | PrjFld of {fld: int}  (** Project a field from a constant struct *)
  | PrjIdx  (** Project an index from a constant array *)
  | UpdFld of {fld: int}  (** Constant struct with updated field *)
  | UpdIdx  (** Constant array with updated index *)
  | Integer of {data: Z.t; typ: Typ.t}  (** Integer constant *)
  | Float of {data: string; typ: Typ.t}  (** Floating-point constant *)
  | Array of {typ: Typ.t}  (** Array constant *)
  | Struct of {typ: Typ.t}  (** Struct constant *)
  | Cast of {typ: Typ.t}  (** Cast to specified type, invertible *)
  | Conv of {signed: bool; typ: Typ.t}
      (** Convert to specified type, possibly with loss of information *)
  | Select  (** Conditional *)
  (* binary: comparison *)
  | Eq  (** Equal test *)
  | Ne  (** Not-equal test *)
  | Gt  (** Greater-than test *)
  | Ge  (** Greater-than-or-equal test *)
  | Lt  (** Less-than test *)
  | Le  (** Less-than-or-equal test *)
  | Ugt  (** Unordered or greater-than test *)
  | Uge  (** Unordered or greater-than-or-equal test *)
  | Ult  (** Unordered or less-than test *)
  | Ule  (** Unordered or less-than-or-equal test *)
  | Ord  (** Ordered test (neither arg is nan) *)
  | Uno  (** Unordered test (some arg is nan) *)
  (* binary: boolean / bitwise *)
  | And  (** Conjunction *)
  | Or  (** Disjunction *)
  | Xor  (** Exclusive-or / Boolean disequality *)
  | Shl  (** Shift left *)
  | LShr  (** Logical shift right *)
  | AShr  (** Arithmetic shift right *)
  (* binary: arithmetic *)
  | Add  (** Addition *)
  | Sub  (** Subtraction *)
  | Mul  (** Multiplication *)
  | Div  (** Division *)
  | UDiv  (** Unsigned division *)
  | Rem  (** Remainder of division *)
  | URem  (** Remainder of unsigned division *)

val compare : t -> t -> int

val equal : t -> t -> bool

val t_of_sexp : Sexp.t -> t

val sexp_of_t : t -> Sexp.t

val fmt : t fmt

(** Re-exported modules *)

module Var : sig
  type nonrec t = private t

  include Comparator.S with type t := t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val fmt : t fmt

  val mk : ?loc:Loc.t -> string -> Typ.t -> t

  val name : t -> string

  val typ : t -> Typ.t

  val loc : t -> Loc.t
end

module Global : sig
  type init = t

  type nonrec t = private t

  include Comparator.S with type t := t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val hash : t -> int

  val fmt : t fmt

  val fmt_defn : t fmt

  val mk : ?init:init -> ?loc:Loc.t -> string -> Typ.t -> t

  val of_exp : init -> t option

  val name : t -> string

  val typ : t -> Typ.t

  val loc : t -> Loc.t
end

(** Constructors *)

val mkVar : Var.t -> t

val mkGlobal : Global.t -> t

val mkNondet : Typ.t -> string -> t

val mkLabel : parent:string -> name:string -> t

val mkNull : Typ.t -> t

val mkPtrFld : ptr:t -> fld:int -> t

val mkPtrIdx : ptr:t -> idx:t -> t

val mkPrjFld : agg:t -> fld:int -> t

val mkPrjIdx : arr:t -> idx:t -> t

val mkUpdFld : agg:t -> elt:t -> fld:int -> t

val mkUpdIdx : arr:t -> elt:t -> idx:t -> t

val mkBool : bool -> t

val mkInteger : Z.t -> Typ.t -> t

val mkFloat : string -> Typ.t -> t

val mkArray : t vector -> Typ.t -> t

val mkStruct : t vector -> Typ.t -> t

val mkStruct_rec :
  (module Hashtbl.Key_plain with type t = 'id)
  -> (id:'id -> t lazy_t vector -> Typ.t -> t) Staged.t
(** [mkStruct_rec Id id element_thunks typ] constructs a possibly-cyclic
    [Struct] value. Cycles are detected using [Id]. The caller of
    [mkStruct_rec Id] must ensure that a single unstaging of [mkStruct_rec
    Id] is used for each complete cyclic value. Also, the caller must ensure
    that recursive calls to [mkStruct_rec Id] provide [id] values that
    uniquely identify at least one point on each cycle. Failure to obey
    these requirements will lead to stack overflow. *)

val mkCast : t -> Typ.t -> t

val mkConv : t -> ?signed:bool -> Typ.t -> t

val mkSelect : cnd:t -> thn:t -> els:t -> t

val mkEq : t -> t -> t

val mkNe : t -> t -> t

val mkGt : t -> t -> t

val mkGe : t -> t -> t

val mkLt : t -> t -> t

val mkLe : t -> t -> t

val mkUgt : t -> t -> t

val mkUge : t -> t -> t

val mkUlt : t -> t -> t

val mkUle : t -> t -> t

val mkOrd : t -> t -> t

val mkUno : t -> t -> t

val mkAnd : t -> t -> t

val mkOr : t -> t -> t

val mkXor : t -> t -> t

val mkShl : t -> t -> t

val mkLShr : t -> t -> t

val mkAShr : t -> t -> t

val mkAdd : t -> t -> t

val mkSub : t -> t -> t

val mkMul : t -> t -> t

val mkDiv : t -> t -> t

val mkUDiv : t -> t -> t

val mkRem : t -> t -> t

val mkURem : t -> t -> t

val locate : Loc.t -> t -> t
(** Update the debug location *)

(** Queries *)

val typ : t -> Typ.t
