(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Location : sig
  type t

  val known : line:int -> col:int -> t
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
    | Int  (** integer type *)
    | Float  (** float type *)
    | Null
    | Void  (** void type *)
    | Ptr of t  (** pointer type *)
    | Struct of TypeName.t  (** structured value type name *)
    | Array of t  (** array type *)
end

module Ident : sig
  type t

  val of_int : int -> t
end

module Const : sig
  type t =
    | Int of Z.t  (** integer constants *)
    | Null
    | Str of string  (** string constants *)
    | Float of float  (** float constants *)
end

module Lang : sig
  type t = Java | Hack [@@deriving equal]

  val of_string : string -> t option [@@warning "-32"]

  val to_string : t -> string [@@warning "-32"]
end

module SilProcname = Procname

module Procname : sig
  type kind = Virtual | NonVirtual

  type enclosing_class = TopLevel | Enclosing of TypeName.t

  type qualified_name = {enclosing_class: enclosing_class; name: ProcBaseName.t}

  type t =
    {qualified_name: qualified_name; formals_types: Typ.t list; result_type: Typ.t; kind: kind}

  val to_sil : Lang.t -> t -> SilProcname.t [@@warning "-32"]
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
    | Field of {exp: t; tname: TypeName.t; fname: FieldBaseName.t}
        (** field offset, fname must be declared in type tname *)
    | Index of t * t  (** an array index offset: [exp1\[exp2\]] *)
    | Const of Const.t
    | Call of {proc: Procname.qualified_name; args: t list}
    | Cast of Typ.t * t
end

module Instr : sig
  type t =
    | Load of {id: Ident.t; exp: Exp.t; typ: Typ.t; loc: Location.t}
        (** id <- *exp with *exp:typ *)
    | Store of {exp1: Exp.t; typ: Typ.t; exp2: Exp.t; loc: Location.t}
        (** *exp1 <- exp2 with exp2:typ *)
    | Prune of {exp: Exp.t; b: bool; loc: Location.t}  (** assume exp==b *)
    | Let of {id: Ident.t; exp: Exp.t; loc: Location.t}  (** id = exp *)
  (* Remark that because Sil operations (add, mult...) are calls, we let the Textual programmer put
     expression in local variables, while SIL forbid that. The to_sil transformation will have to
     inline these definitions. *)
end

module Terminator : sig
  type node_call = {label: NodeName.t; ssa_args: Exp.t list}

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
  type t =
    { procname: Procname.t
    ; nodes: Node.t list
    ; start: NodeName.t
    ; params: VarName.t list
    ; exit_loc: Location.t }
end

module Struct : sig
  type t = {name: TypeName.t; fields: Fieldname.t list; methods: Procname.t list}
end

module Attr : sig
  type t = {name: string; value: string; loc: Location.t}

  val name : t -> string [@@warning "-32"]

  val value : t -> string [@@warning "-32"]

  val pp : F.formatter -> t -> unit [@@warning "-32"]

  val pp_with_loc : F.formatter -> t -> unit [@@warning "-32"]
end

module Module : sig
  type decl = Global of Pvar.t | Struct of Struct.t | Procname of Procname.t | Proc of Procdesc.t

  type t = {attrs: Attr.t list; decls: decl list; sourcefile: SourceFile.t}

  val lang : t -> Lang.t option [@@warning "-32"]

  val pp : F.formatter -> t -> unit [@@warning "-32"]

  val from_java : filename:string -> Tenv.t -> Cfg.t -> unit
  (** generate a .sil file with name [filename] containing all the functions in the given cfg *)

  val to_sil : t -> Cfg.t * Tenv.t
end

module Verification : sig
  type error

  val pp_error : SourceFile.t -> Format.formatter -> error -> unit

  val run : Module.t -> error list
end

module Transformation : sig
  (* generates enough intermediate Let instructions to make the procdesc free
     of sub-expressions containing regular calls.
     Example:
       n2 = m(n0, g3(n1))
     -->
       n3 = g3(n1)
       n2 = m(n0, n3)
  *)
  val remove_internal_calls : Module.t -> Module.t
end

exception ToSilTransformationError of (Format.formatter -> unit -> unit)
