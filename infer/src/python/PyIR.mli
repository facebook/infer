(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Location : sig
  type t

  val pp : Format.formatter -> t -> unit
end

module Error : sig
  type kind

  type t = Logging.error * Location.t * kind

  val pp_kind : Format.formatter -> kind -> unit
end

module NodeName : sig
  type t

  module Map : Caml.Map.S with type key = t
end

module SSA : sig
  type t
end

module Ident : sig
  type t
end

module ScopedIdent : sig
  type scope = Global | Fast | Name

  type t = {scope: scope; ident: Ident.t}
end

module QualName : sig
  type t

  module Map : Caml.Map.S with type key = t
end

module Builtin = PyBuiltin
module Const = FFI.Constant

module BuiltinCaller : sig
  type format_function = Str | Repr | Ascii

  type t =
    | BuildClass
    | BuildConstKeyMap
    | Format
    | FormatFn of format_function
    | CallFunctionEx
    | Inplace of Builtin.binary_op
    | ImportName of string
    | ImportFrom of string
    | ImportStar
    | Binary of Builtin.binary_op
    | Unary of Builtin.unary_op
    | Compare of Builtin.Compare.t
    | LoadClosure of {name: Ident.t; slot: int}  (** [LOAD_CLOSURE] *)
    | LoadDeref of {name: Ident.t; slot: int}  (** [LOAD_DEREF] *)
    | LoadClassDeref of {name: Ident.t; slot: int}  (** [LOAD_CLASSDEREF] *)
    | StoreDeref of {name: Ident.t; slot: int}  (** [STORE_DEREF] *)
    | Function of {qual_name: QualName.t; short_name: Ident.t}
    | GetAIter
    | GetIter
    | NextIter
    | HasNextIter
    | IterData
    | GetYieldFromIter
    | ListAppend
    | SetAdd
    | DictSetItem
    | Delete of ScopedIdent.t
    | DeleteDeref of {name: Ident.t; slot: int}  (** [DELETE_DEREF] *)
    | DeleteAttr of string
    | DeleteSubscr
    | YieldFrom
    | GetAwaitable
    | UnpackEx
    | GetPreviousException
end

module Exp : sig
  type collection = List | Set | Tuple | Slice | Map | String

  type t =
    | Const of Const.t
    | Var of ScopedIdent.t
    | Temp of SSA.t
    | Subscript of {exp: t; index: t}
    | Collection of {kind: collection; values: t list; packed: bool}
    | GetAttr of (t * string)
    | LoadMethod of (t * string)
    | Not of t
    | BuiltinCaller of BuiltinCaller.t
    | CallFinallyReturn of {offset: int}
    | ContextManagerExit of t
    | Packed of {exp: t; is_map: bool}
    | Yield of t
end

module Stmt : sig
  type call_arg = Exp.t

  type t =
    | Let of {lhs: SSA.t; rhs: Exp.t}
    | SetAttr of {lhs: Exp.t; attr: string; rhs: Exp.t}
    | Store of {lhs: ScopedIdent.t; rhs: Exp.t}
    | StoreSubscript of {lhs: Exp.t; index: Exp.t; rhs: Exp.t}
    | Call of {lhs: SSA.t; exp: Exp.t; args: call_arg list; arg_names: Exp.t}
    | CallMethod of
        {lhs: SSA.t; name: string; self_if_needed: Exp.t; args: Exp.t list; arg_names: Exp.t}
    | BuiltinCall of {lhs: SSA.t; call: BuiltinCaller.t; args: call_arg list; arg_names: Exp.t}
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

module CFG : sig
  type t = {entry: NodeName.t; nodes: Node.t NodeName.Map.t}
end

module Module : sig
  type t = {name: Ident.t; toplevel: CFG.t; functions: CFG.t QualName.Map.t}
end

val mk : debug:bool -> FFI.Code.t -> (Module.t, Error.t) result

val test : ?filename:string -> ?debug:bool -> string -> unit [@@warning "-unused-value-declaration"]
(* takes a Python source program as string argument, convert it into PyIR and print the result *)

val test_cfg_skeleton : ?filename:string -> string -> unit [@@warning "-unused-value-declaration"]
