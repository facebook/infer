(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Location : sig
  type t = {line: int; col: int}
end

module type NAME = sig
  type t = {value: string; loc: Location.t}
end

module ProcBaseName : NAME (* procedure names *)

module VarName : NAME (* variables names *)

module FieldBaseName : NAME (* field names, without their enclosing types *)

module NodeName : NAME (* node names, also called labels *)

module TypeName : NAME (* structured value type name *)

module Typ : sig
  type t =
    | Tint  (** integer type *)
    | Tfloat  (** float type *)
    | Tnull
    | Tvoid  (** void type *)
    | Tptr of t  (** pointer type *)
    | Tstruct of TypeName.t  (** structured value type name *)
    | Tarray of t  (** array type *)
end

module Ident : sig
  type t

  val of_int : int -> t
end

module Const : sig
  type t =
    | Cint of Z.t  (** integer constants *)
    | Cnull
    | Cstr of string  (** string constants *)
    | Cfloat of float  (** float constants *)
end

module Procname : sig
  type proc_kind = Virtual | NonVirtual

  type kind =
    | Builtin  (** it models a specific langage feature not directly expressible in Sil *)
    | SilInstr  (** it will be turned into unop/binop SIL expression during translation *)
    | Proc of {pkind: proc_kind}  (** it has been defined by the programmer *)

  type t = {name: ProcBaseName.t; targs: Typ.t list; tres: Typ.t; kind: kind}
end

module Pvar : sig
  type kind = Global | Local of Procname.t

  type t = {name: VarName.t; kind: kind}
end

module Fieldname : sig
  type t = {name: FieldBaseName.t; typ: Typ.t; enclosing_type: TypeName.t}
  (* contrary to Sil, we put field types inside their name *)
end

module Exp : sig
  type t =
    | Var of Ident.t  (** pure variable: it is not an lvalue *)
    | Lvar of VarName.t  (** the address of a program variable *)
    | Lfield of {exp: t; tname: TypeName.t; fname: FieldBaseName.t}
        (** field offset, fname must be declared in type tname *)
    | Lindex of t * t  (** an array index offset: [exp1\[exp2\]] *)
    | EConst of Const.t
    | ECall of ProcBaseName.t * t list
    | ECast of Typ.t * t
end

module Instr : sig
  type t =
    | Load of {id: Ident.t; exp: Exp.t; typ: Typ.t; loc: Location.t}
        (** id <- *exp with *exp:typ *)
    | Store of {exp1: Exp.t; typ: Typ.t; exp2: Exp.t; loc: Location.t}
        (** *exp1 <- exp2 with exp2:typ *)
    | Prune of {exp: Exp.t; b: bool; loc: Location.t}  (** assume exp==b *)
    | Call of {ret: Ident.t; f: ProcBaseName.t; args: Exp.t list; loc: Location.t}
        (** ret <- f(args) *)

  (** Remark that because Sil operations (add, mult...) are calls, we let the Textual programmer put
      expression in local variables, while SIL forbid that. The to_sil transformation will have to
      inline these definitions. *)
end

module Terminator : sig
  type node_call = {label: NodeName.t; ssa_args: Ident.t list}

  type t = Ret of Exp.t | Jump of node_call list  (** non empty list *) | Throw of Exp.t
end

module Node : sig
  type t =
    { label: NodeName.t
    ; ssa_parameters: Ident.t list
    ; exn_succs: NodeName.t list  (** successor exception nodes *)
    ; last: Terminator.t
    ; instrs: Instr.t list
    ; last_loc: Location.t  (** location of last instruction in file *)
    ; label_loc: Location.t  (** location of label in file *) }
end

module Procdesc : sig
  type t = {procname: Procname.t; nodes: Node.t list; start: NodeName.t; params: VarName.t list}
end

module Struct : sig
  type t = {name: TypeName.t; fields: Fieldname.t list}
end

module Module : sig
  type decl = Global of Pvar.t | Struct of Struct.t | Procname of Procname.t | Proc of Procdesc.t

  type t = {decls: decl list; sourcefile: SourceFile.t}
end

module Verification : sig
  type error

  val pp_error : Format.formatter -> SourceFile.t -> error -> unit

  val run : Module.t -> error list
end
