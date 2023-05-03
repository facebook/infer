(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual

let type_name value = T.TypeName.{value; loc= T.Location.Unknown}

let mk_type name = T.Typ.(Ptr (Struct (type_name name)))

let string_ = T.Typ.(Ptr (Struct (type_name "String")))

let pyObject = mk_type "PyObject"

let pyInt = mk_type "PyInt"

let pyString = mk_type "PyString"

let pyBool = mk_type "PyBool"

let pyCode = mk_type "PyCode"

let is_pyCode = function T.Typ.Ptr (Struct {value}) -> String.equal value "PyCode" | _ -> false

let builtin_scope = T.Enclosing T.{TypeName.value= "$builtins"; loc= Unknown}

let builtin_name (value : string) : T.qualified_procname =
  let name = T.ProcName.{value; loc= T.Location.Unknown} in
  {enclosing_class= builtin_scope; name}


(* Helper to box Python's int/string into Textual.
   Until we support Python types, we can't use Textual types, like `int` for
   bool so we wrap them all. *)
let python_bool = builtin_name "python_bool"

let python_int = builtin_name "python_int"

let python_string = builtin_name "python_string"

let python_tuple = builtin_name "python_tuple"

let mk_int (i : int64) =
  let proc = python_int in
  let z = Z.of_int64 i in
  let args = [Textual.Exp.Const (Int z)] in
  Textual.Exp.Call {proc; args; kind= NonVirtual}


let mk_string (s : string) =
  let proc = python_string in
  let args = [Textual.Exp.Const (Str s)] in
  Textual.Exp.Call {proc; args; kind= NonVirtual}


let mk_bool (b : bool) =
  let proc = python_bool in
  let z = if b then Z.one else Z.zero in
  let args = [Textual.Exp.Const (Int z)] in
  Textual.Exp.Call {proc; args; kind= NonVirtual}


let mk_is_true exp =
  let proc = builtin_name "is_true" in
  let args = [exp] in
  Textual.Exp.Call {proc; args; kind= NonVirtual}


module Builtins = struct
  type elt = {formals_types: T.Typ.annotated list option; result_type: T.Typ.annotated}

  module Info = Caml.Map.Make (String)
  module Set = Caml.Set.Make (String)

  type t = Set.t

  let mk_builtin {formals_types; result_type} name =
    let qualified_name = builtin_name name in
    T.Module.Procdecl T.ProcDecl.{qualified_name; formals_types; result_type; attributes= []}


  let annot typ = T.Typ.{typ; attributes= []}

  let primitive_builtins =
    let builtins =
      [ ("python_int", {formals_types= Some [annot T.Typ.Int]; result_type= annot pyInt})
      ; ("python_bool", {formals_types= Some [annot T.Typ.Int]; result_type= annot pyBool})
      ; ("python_string", {formals_types= Some [annot string_]; result_type= annot pyString})
      ; ("python_tuple", {formals_types= None; result_type= annot pyObject}) ]
    in
    List.fold_left
      ~f:(fun acc (name, builtin) -> Info.add name builtin acc)
      ~init:Info.empty builtins


  let supported_builtins =
    let builtins =
      [ ("print", {formals_types= None; result_type= annot pyObject})
      ; ("is_true", {formals_types= Some [annot pyObject]; result_type= annot T.Typ.Int})
      ; ("range", {formals_types= None; result_type= annot pyObject})
      ; ( "binary_add"
        , {formals_types= Some [annot pyObject; annot pyObject]; result_type= annot pyObject} )
      ; ("python_code", {formals_types= Some [annot string_]; result_type= annot pyCode})
      ; ("python_call", {formals_types= None; result_type= annot pyObject})
        (* TODO: should we introduce a Textual type for iterators ? *)
      ; ("python_iter", {formals_types= Some [annot pyObject]; result_type= annot pyObject})
      ; ("python_iter_next", {formals_types= Some [annot pyObject]; result_type= annot T.Typ.Int})
      ; ("python_iter_item", {formals_types= Some [annot pyObject]; result_type= annot pyObject}) ]
    in
    List.fold_left
      ~f:(fun acc (name, builtin) -> Info.add name builtin acc)
      ~init:primitive_builtins builtins


  let to_textual spotted =
    let init = Info.fold (fun key elt l -> mk_builtin elt key :: l) primitive_builtins [] in
    Info.fold
      (fun key elt l -> if Set.mem key spotted then mk_builtin elt key :: l else l)
      supported_builtins init


  let register spotted name = Set.add name spotted

  let is_builtin name = Info.mem name supported_builtins

  let get_type name =
    let info = Info.find_opt name supported_builtins in
    Option.map info ~f:(fun b -> b.result_type.typ) |> Option.value ~default:pyObject


  let empty = Set.empty
end

let global name = sprintf "$globals::%s" name
