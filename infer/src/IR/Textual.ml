(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type name = string

let name_of_string s = s

let replace_dot_with_2colons s = String.substr_replace_all s ~pattern:"." ~with_:"::"

module TypName : sig
  type t

  val of_java_classname : string -> t
end = struct
  type t = JavaClass of string

  let of_java_classname s =
    let s_without_dot = replace_dot_with_2colons s in
    JavaClass s_without_dot
end

module Typ = struct
  type t = Tint | Tfloat | Tnull | Tvoid | Tptr of t | Tstruct of TypName.t | Tarray of t
end

module Location = struct
  type t = {line: int; col: int}
end

module Ident = struct
  type t = int

  let of_int x = x
end

module Const = struct
  type t = Cint of Z.t | Cnull | Cstr of string | Cfloat of float
end

module Procname = struct
  type proc_kind = Virtual | NonVirtual

  type kind = Builtin | SilInstr | Proc of {pkind: proc_kind}

  type t = {name: name; targs: Typ.t list; tres: Typ.t; kind: kind}
end

module Pvar = struct
  type kind = Global | Local of Procname.t

  type t = {name: name; kind: kind}
end

module Fieldname = struct
  type t = {name: name; typ: Typ.t; enclosing_type: TypName.t}
end

module Struct = struct
  type t = {name: TypName.t; fields: Fieldname.t list; loc: Location.t}
end

module Exp = struct
  type t =
    | Var of Ident.t
    | Lvar of name
    | Lfield of t * name
    | Lindex of t * t
    (*  | Sizeof of sizeof_data *)
    | EConst of Const.t
    | ECall of name * t list
    | ECast of Typ.t * t
end

module Instr = struct
  type t =
    | Load of {id: Ident.t; e: Exp.t; typ: Typ.t; loc: Location.t}
    | Store of {e1: Exp.t; typ: Typ.t; e2: Exp.t; loc: Location.t}
    | Prune of {e: Exp.t; b: bool; loc: Location.t}
    | Call of {ret: Ident.t; f: name; args: Exp.t list; loc: Location.t}
end

type node_label = name

module Terminator = struct
  type node_call = {label: node_label; ssa_args: Ident.t list}

  type t = Ret of Exp.t | Jump of node_call list | Throw of Exp.t
end

module Node = struct
  type t =
    { label: node_label
    ; ssa_parameters: Ident.t list
    ; exn_succs: node_label list
    ; last: Terminator.t
    ; instrs: Instr.t list
    ; last_loc: Location.t
    ; label_loc: Location.t }
end

module Procdesc = struct
  type t =
    { name: Procname.t
    ; nodes: Node.t list
    ; start: node_label
    ; params: name list
    ; sourcefile: SourceFile.t }
end

module Module = struct
  type decl = Global of Pvar.t | Struct of Struct.t | Procname of Procname.t | Proc of Procdesc.t

  type t = decl list
end
