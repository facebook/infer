(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual

let pyObject =
  let name = T.TypeName.{value= "PyObject"; loc= T.Location.Unknown} in
  T.Typ.(Ptr (Struct name))


let builtin_scope = T.Enclosing T.{TypeName.value= "$builtins"; loc= Unknown}

let builtin_name (value : string) : T.qualified_procname =
  let name = T.ProcName.{value; loc= T.Location.Unknown} in
  {enclosing_class= builtin_scope; name}


(* Helper to box Python's int/string into Textual *)
let python_int = builtin_name "python_int"

let python_string = builtin_name "python_string"

let python_tuple = builtin_name "python_tuple"

let mk_int (i : int64) =
  let proc = python_int in
  let z = Z.of_int64 i in
  let args = [Textual.(Exp.Const (Const.Int z))] in
  Textual.Exp.Call {proc; args; kind= NonVirtual}


let mk_string (s : string) =
  let proc = python_string in
  let args = [Textual.(Exp.Const (Const.Str s))] in
  Textual.Exp.Call {proc; args; kind= NonVirtual}


module Builtins = struct
  module Set = Caml.Set.Make (struct
    type t = string [@@deriving compare]
  end)

  let to_textual spotted =
    let annot typ = T.Typ.{typ; attributes= []} in
    let mk ?typ qualified_name =
      let formals_types = Option.to_list @@ Option.map ~f:annot typ in
      let result_type = annot pyObject in
      T.Module.Procdecl
        T.ProcDecl.
          { qualified_name
          ; formals_types
          ; are_formal_types_fully_declared= true
          ; result_type
          ; attributes= [] }
    in
    let python_int = mk python_int ~typ:T.Typ.Int in
    let python_string = mk python_string ~typ:pyObject in
    let python_tuple = mk python_tuple ~typ:pyObject in
    Set.fold
      (fun name acc ->
        let builtin_name = builtin_name name in
        mk builtin_name :: acc )
      spotted
      [python_int; python_string; python_tuple]


  let register name spotted = Set.add name spotted

  let is_builtin name builtins = Set.mem name builtins

  let mk () = register "print" Set.empty |> register "binary_add"
end
