(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Lang : sig
  type t = Java | Hack [@@deriving equal]

  val of_string : string -> t option [@@warning "-32"]

  val to_string : t -> string [@@warning "-32"]
end

module Location : sig
  type t

  val known : line:int -> col:int -> t
end

module type NAME = sig
  type t = {value: string; loc: Location.t}
end

module ProcName : NAME (* procedure names, without their attachement type *)

module VarName : NAME (* variables names *)

module FieldName : NAME (* field names, without their enclosing types *)

module NodeName : NAME (* node names, also called labels *)

module TypeName : NAME (* structured value type name *)

type enclosing_class = TopLevel | Enclosing of TypeName.t

type qualified_procname = {enclosing_class: enclosing_class; name: ProcName.t}
(* procedure name [name] is attached to the name space [enclosing_class] *)

type qualified_fieldname = {enclosing_class: TypeName.t; name: FieldName.t}
(* field name [name] must be declared in type [enclosing_class] *)

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

module ProcDecl : sig
  type t = {qualified_name: qualified_procname; formals_types: Typ.t list; result_type: Typ.t}

  val to_sil : Lang.t -> t -> Procname.t [@@warning "-32"]
end

module Global : sig
  type t = {name: VarName.t}
end

module FieldDecl : sig
  type t = {qualified_name: qualified_fieldname; typ: Typ.t}
end

module Exp : sig
  type call_kind = Virtual | NonVirtual

  type t =
    | Var of Ident.t  (** pure variable: it is not an lvalue *)
    | Lvar of VarName.t  (** the address of a program variable *)
    | Field of {exp: t; field: qualified_fieldname}  (** field offset *)
    | Index of t * t  (** an array index offset: [exp1\[exp2\]] *)
    | Const of Const.t
    | Call of {proc: qualified_procname; args: t list; kind: call_kind}
    | Typ of Typ.t

  val call_virtual : qualified_procname -> t -> t list -> t

  (* logical not ! *)
  val not : t -> t
end

module Instr : sig
  type t =
    | Load of {id: Ident.t; exp: Exp.t; typ: Typ.t; loc: Location.t}
        (** id <- *exp with *exp:typ *)
    | Store of {exp1: Exp.t; typ: Typ.t; exp2: Exp.t; loc: Location.t}
        (** *exp1 <- exp2 with exp2:typ *)
    | Prune of {exp: Exp.t; loc: Location.t}  (** assume exp *)
    | Let of {id: Ident.t; exp: Exp.t; loc: Location.t}  (** id = exp *)
  (* Remark that because Sil operations (add, mult...) are calls, we let the Textual programmer put
     expression in local variables, while SIL forbid that. The to_sil transformation will have to
     inline these definitions. *)
end

module Terminator : sig
  type node_call = {label: NodeName.t; ssa_args: Exp.t list}

  type t =
    | Ret of Exp.t
    | Jump of node_call list  (** non empty list *)
    | Throw of Exp.t
    | Unreachable
end

module Node : sig
  type t =
    { label: NodeName.t
    ; ssa_parameters: (Ident.t * Typ.t) list
    ; exn_succs: NodeName.t list  (** successor exception nodes *)
    ; last: Terminator.t
    ; instrs: Instr.t list
    ; last_loc: Location.t  (** location of last instruction in file *)
    ; label_loc: Location.t  (** location of label in file *) }
end

module ProcDesc : sig
  type t =
    { procdecl: ProcDecl.t
    ; nodes: Node.t list
    ; start: NodeName.t
    ; params: VarName.t list
    ; exit_loc: Location.t }
end

module Struct : sig
  type t = {name: TypeName.t; supers: TypeName.t list; fields: FieldDecl.t list}
end

module Attr : sig
  type t = {name: string; value: string; loc: Location.t}

  val name : t -> string [@@warning "-32"]

  val value : t -> string [@@warning "-32"]

  val pp : F.formatter -> t -> unit [@@warning "-32"]

  val pp_with_loc : F.formatter -> t -> unit [@@warning "-32"]
end

module Module : sig
  type decl =
    | Global of Global.t
    | Struct of Struct.t
    | Procdecl of ProcDecl.t
    | Proc of ProcDesc.t

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

  val let_propagation : Module.t -> Module.t

  val out_of_ssa : Module.t -> Module.t
end

exception ToSilTransformationError of (Format.formatter -> unit -> unit)
