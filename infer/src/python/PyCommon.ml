(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual

let proc_name ?(loc = T.Location.Unknown) value = {T.ProcName.value; loc}

let type_name ?(loc = T.Location.Unknown) value = {T.TypeName.value; loc}

let var_name ?(loc = T.Location.Unknown) value = {T.VarName.value; loc}

let node_name ?(loc = T.Location.Unknown) value = {T.NodeName.value; loc}

let field_name ?(loc = T.Location.Unknown) value = {T.FieldName.value; loc}

(* TODO: only deal with toplevel functions for now *)
let qualified_procname ~enclosing_class name : T.qualified_procname =
  {enclosing_class= Enclosing enclosing_class; name}


let mk_type name = T.Typ.(Ptr (Struct (type_name name)))

let pyObject = mk_type "PyObject"

let pyInt = mk_type "PyInt"

let pyFloat = mk_type "PyFloat"

let pyString = mk_type "PyString"

let pyBytes = mk_type "PyBytes"

let pyBool = mk_type "PyBool"

let pyNone = mk_type "PyNone"

let pyCode = mk_type "PyCode"

let pyClass = mk_type "PyClass"

let py_iter_item = type_name "PyIterItem"

let pyIterItem = T.Typ.Ptr (Struct py_iter_item)

let py_iter_item_has_item =
  {T.enclosing_class= py_iter_item; name= {value= "has_item"; loc= Unknown}}


let py_iter_item_next_item =
  {T.enclosing_class= py_iter_item; name= {value= "next_item"; loc= Unknown}}


let pyIterItemStruct =
  let has_item =
    {T.FieldDecl.qualified_name= py_iter_item_has_item; typ= T.Typ.Int; attributes= []}
  in
  let item = {T.FieldDecl.qualified_name= py_iter_item_next_item; typ= pyObject; attributes= []} in
  let fields = [has_item; item] in
  {T.Struct.name= py_iter_item; supers= []; fields; attributes= []}


let py_method = type_name "PyMethod"

let pyMethod = T.Typ.Ptr (T.Typ.Struct py_method)

let py_method_code =
  {T.enclosing_class= py_method; name= {T.FieldName.value= "code"; loc= T.Location.Unknown}}


let py_method_self =
  {T.enclosing_class= py_method; name= {T.FieldName.value= "self"; loc= T.Location.Unknown}}


let pyMethodStruct =
  let code = {T.FieldDecl.qualified_name= py_method_code; typ= pyCode; attributes= []} in
  let self = {T.FieldDecl.qualified_name= py_method_self; typ= pyObject; attributes= []} in
  let fields = [code; self] in
  {T.Struct.name= py_method; supers= []; fields; attributes= []}


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

let python_float = builtin_name "python_float"

let python_bytes = builtin_name "python_bytes"

let python_tuple = builtin_name "python_tuple"

let mk_int (i : int64) =
  let proc = python_int in
  let z = Z.of_int64 i in
  let args = [T.Exp.Const (Int z)] in
  T.Exp.Call {proc; args; kind= NonVirtual}


let mk_float (f : float) =
  let proc = python_float in
  let args = [T.Exp.Const (Float f)] in
  T.Exp.Call {proc; args; kind= NonVirtual}


let read_int = function
  | T.Exp.Call {proc; args= [arg]; kind= NonVirtual} when T.equal_qualified_procname python_int proc
    -> (
    match arg with T.Exp.Const (Int z) -> Some z | _ -> None )
  | _ ->
      None


let mk_string (s : string) =
  let proc = python_string in
  let args = [T.Exp.Const (Str s)] in
  T.Exp.Call {proc; args; kind= NonVirtual}


let mk_bytes (s : bytes) =
  let proc = python_bytes in
  let s = Bytes.to_string s in
  let args = [T.Exp.Const (Str s)] in
  T.Exp.Call {proc; args; kind= NonVirtual}


let mk_bool (b : bool) =
  let proc = python_bool in
  let z = if b then Z.one else Z.zero in
  let args = [T.Exp.Const (Int z)] in
  T.Exp.Call {proc; args; kind= NonVirtual}


let unknown_global name = sprintf "$ambiguous::%s" name

type annotated_name = {name: string; annotation: string}

type method_info =
  { name: string
  ; raw_qualified_name: string
  ; code: FFI.Constant.t
  ; signature: annotated_name list
  ; flags: int }

let toplevel_function = "$toplevel"
