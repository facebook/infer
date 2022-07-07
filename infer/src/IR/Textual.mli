(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** textual name *)
type name

val name_of_string : string -> name

module TypName : sig
  type t

  val of_java_classname : name -> t
end

module Typ : sig
  type t =
    | Tint  (** integer type *)
    | Tfloat  (** float type *)
    | Tnull
    | Tvoid  (** void type *)
    | Tptr of t  (** pointer type *)
    | Tstruct of TypName.t  (** structured value type name *)
    | Tarray of t  (** array type *)
end

module Location : sig
  type t = {line: int; col: int}
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

  type t = {name: name; targs: Typ.t list; tres: Typ.t; kind: kind}
end

module Pvar : sig
  type kind = Global | Local of Procname.t

  type t = {name: name; kind: kind}
end

module Fieldname : sig
  type t = {name: name; typ: Typ.t; enclosing_type: TypName.t}
  (* contrary to Sil, we put field types inside their name *)
end

module Exp : sig
  type t =
    | Var of Ident.t  (** pure variable: it is not an lvalue *)
    | Lvar of name  (** the address of a program variable *)
    | Lfield of t * name  (** field offset *)
    | Lindex of t * t  (** an array index offset: [exp1\[exp2\]] *)
    | EConst of Const.t
    | ECall of name * t list
    | ECast of Typ.t * t
end

module Instr : sig
  type t =
    | Load of {id: Ident.t; e: Exp.t; typ: Typ.t; loc: Location.t}  (** id <- *e with *e:typ *)
    | Store of {e1: Exp.t; typ: Typ.t; e2: Exp.t; loc: Location.t}  (** *e1 <- e2 with e2:typ *)
    | Prune of {e: Exp.t; b: bool; loc: Location.t}  (** assume e==b *)
    | Call of {ret: Ident.t; f: name; args: Exp.t list; loc: Location.t}  (** ret <- f(args) *)

  (** Remark that because Sil operations (add, mult...) are calls, we let the Textual programmer put
      expression in local variables, while SIL forbid that. The to_sil transformation will have to
      inline these definitions. *)
end

type node_label = name

module Terminator : sig
  type node_call = {label: node_label; ssa_args: Ident.t list}

  type t = Ret of Exp.t | Jump of node_call list  (** non empty list *) | Throw of Exp.t
end

module Node : sig
  type t =
    { label: node_label
    ; ssa_parameters: Ident.t list
    ; exn_succs: node_label list  (** successor exception nodes *)
    ; last: Terminator.t
    ; instrs: Instr.t list
    ; last_loc: Location.t  (** location of last instruction in file *)
    ; label_loc: Location.t  (** location of label in file *) }
end

module Procdesc : sig
  type t =
    { name: Procname.t
    ; nodes: Node.t list
    ; start: node_label
    ; params: name list
    ; sourcefile: SourceFile.t }
end

module Struct : sig
  type t = {name: TypName.t; fields: Fieldname.t list; loc: Location.t}
end

module Module : sig
  type decl = Global of Pvar.t | Struct of Struct.t | Procname of Procname.t | Proc of Procdesc.t

  type t = decl list
end
