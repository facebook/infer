(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Location : sig
  type t

  val line : t -> int option

  val pp : Format.formatter -> t -> unit
end

module Error : sig
  type kind

  type t = Logging.error * Location.t * kind

  val pp_kind : Format.formatter -> kind -> unit
end

module NodeName : sig
  type t [@@deriving equal]

  module Map : Caml.Map.S with type key = t

  val pp : Format.formatter -> t -> unit
end

module SSA : sig
  type t

  val id : t -> int

  val pp : Format.formatter -> t -> unit

  module Hashtbl : Caml.Hashtbl.S with type key = t
end

module Ident : sig
  type t [@@deriving compare]

  val mk : string -> t

  val pp : Format.formatter -> t -> unit

  module Hashtbl : Caml.Hashtbl.S with type key = t

  module Special : sig
    val name : t

    val print : t
  end
end

module ScopedIdent : sig
  type scope = Global | Fast | Name

  type t = {scope: scope; ident: Ident.t}
end

module QualName : sig
  type t

  val pp : Format.formatter -> t -> unit

  module Map : Caml.Map.S with type key = t
end

module UnaryOp : sig
  type t = Positive | Negative | Not | Invert
end

module BinaryOp : sig
  type t =
    | Add
    | And
    | FloorDivide
    | LShift
    | MatrixMultiply
    | Modulo
    | Multiply
    | Or
    | Power
    | RShift
    | Subtract
    | TrueDivide
    | Xor
end

module CompareOp : sig
  type t = Lt | Le | Eq | Neq | Gt | Ge | In | NotIn | Is | IsNot | Exception | BAD
end

module FormatFunction : sig
  type t = Str | Repr | Ascii
end

module BuiltinCaller : sig
  type t =
    | BuildClass
    | BuildConstKeyMap
    | Format
    | FormatFn of FormatFunction.t
    | CallFunctionEx  (** [CALL_FUNCTION_EX] *)
    | Inplace of BinaryOp.t
    | Binary of BinaryOp.t
    | Unary of UnaryOp.t
    | Compare of CompareOp.t
    | GetAIter
    | GetIter
    | NextIter
    | HasNextIter
    | IterData
    | GetYieldFromIter
    | ListAppend
    | ListExtend
    | ListToTuple
    | SetAdd
    | SetUpdate
    | DictSetItem
    | DictUpdate
    | DictMerge
    | DeleteSubscr
    | YieldFrom
    | GetAwaitable
    | UnpackEx
    | GetPreviousException
end

module Const : sig
  type t =
    | Bool of bool
    | Int of Z.t
    | Float of float
    | Complex of {real: float; imag: float}
    | String of string
    | InvalidUnicode of int array
    | Bytes of bytes
    | None
end

module Exp : sig
  type collection = List | Set | Tuple | Map

  type t =
    | AssertionError
    | BuildFrozenSet of t list
    | BuildSlice of t list
    | BuildString of t list
    | Collection of {kind: collection; values: t list; unpack: bool}
    | Const of Const.t
    | Function of
        { qual_name: QualName.t
        ; short_name: Ident.t
        ; default_values: t
        ; default_values_kw: t
        ; annotations: t
        ; cells_for_closure: t }
    | GetAttr of {exp: t; attr: Ident.t}
    | ImportFrom of {name: Ident.t; exp: t}
    | ImportName of {name: Ident.t; fromlist: t; level: t}
    | LoadClassDeref of {name: Ident.t; slot: int}  (** [LOAD_CLASSDEREF] *)
    | LoadClosure of {name: Ident.t; slot: int}  (** [LOAD_CLOSURE] *)
    | LoadDeref of {name: Ident.t; slot: int}  (** [LOAD_DEREF] *)
    | MatchClass of {subject: t; type_: t; count: int; names: t}
    | BoolOfMatchClass of t
    | AttributesOfMatchClass of t
    | MatchSequence of t
    | GetLen of t
    | Subscript of {exp: t; index: t}
    | Temp of SSA.t
    | Var of ScopedIdent.t
    | Yield of t

  val pp : Format.formatter -> t -> unit
end

module Stmt : sig
  type gen_kind = Generator | Coroutine | AsyncGenerator

  type t =
    | Let of {lhs: SSA.t; rhs: Exp.t}
    | SetAttr of {lhs: Exp.t; attr: Ident.t; rhs: Exp.t}
    | Store of {lhs: ScopedIdent.t; rhs: Exp.t}
    | StoreSubscript of {lhs: Exp.t; index: Exp.t; rhs: Exp.t}
    | Call of {lhs: SSA.t; exp: Exp.t; args: Exp.t list; arg_names: Exp.t}
    | CallMethod of
        {lhs: SSA.t; name: Ident.t; self_if_needed: Exp.t; args: Exp.t list; arg_names: Exp.t}
    | BuiltinCall of {lhs: SSA.t; call: BuiltinCaller.t; args: Exp.t list; arg_names: Exp.t}
    | StoreDeref of {name: Ident.t; slot: int; rhs: Exp.t}  (** [STORE_DEREF] *)
    | Delete of ScopedIdent.t
    | DeleteDeref of {name: Ident.t; slot: int}  (** [DELETE_DEREF] *)
    | DeleteAttr of {exp: Exp.t; attr: Ident.t}
    | ImportStar of Exp.t
    | GenStart of {kind: gen_kind}
    | SetupAnnotations
end

module Terminator : sig
  type node_call = {label: NodeName.t; ssa_args: Exp.t list}

  type t =
    | Return of Exp.t
    | Jump of node_call
    | If of {exp: Exp.t; then_: node_call; else_: node_call}
    | Throw of Exp.t
end

module Node : sig
  type t =
    { name: NodeName.t
    ; first_loc: Location.t
    ; last_loc: Location.t
    ; ssa_parameters: SSA.t list
    ; stmts: (Location.t * Stmt.t) list
    ; last: Terminator.t }
end

module CodeInfo : sig
  type t =
    { co_name: Ident.t
    ; co_nlocals: int
    ; co_argcount: int
    ; co_posonlyargcount: int
    ; co_kwonlyargcount: int
    ; co_cellvars: Ident.t array
    ; co_freevars: Ident.t array
    ; co_names: Ident.t array
    ; co_varnames: Ident.t array
    ; has_star_arguments: bool
    ; has_star_keywords: bool
    ; is_generator: bool }
end

module CFG : sig
  type t = {entry: NodeName.t; nodes: Node.t NodeName.Map.t; code_info: CodeInfo.t}
end

module Module : sig
  type t = {name: Ident.t; toplevel: CFG.t; functions: CFG.t QualName.Map.t}
end

val mk : debug:bool -> FFI.Code.t -> (Module.t, Error.t) result

val test : ?filename:string -> ?debug:bool -> ?run:(Module.t -> unit) -> string -> unit
[@@warning "-unused-value-declaration"]
(* takes a Python source program as string argument, convert it into PyIR and print the result (or executes [run] on the result) *)

val test_files : ?debug:bool -> ?run:(Module.t list -> unit) -> (string * string) list -> unit
[@@warning "-unused-value-declaration"]
(* same as [test] but on a collection of module string representation. The input is a list of (filename, source) pairs *)

val test_cfg_skeleton : ?filename:string -> string -> unit [@@warning "-unused-value-declaration"]
