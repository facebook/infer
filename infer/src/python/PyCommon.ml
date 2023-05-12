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

let pyObject = mk_type "PyObject"

let pyInt = mk_type "PyInt"

let pyString = mk_type "PyString"

let pyBool = mk_type "PyBool"

let pyFloat = mk_type "PyFloat"

let pyNone = mk_type "PyNone"

let pyCode = mk_type "PyCode"

let pyClass = mk_type "PyClass"

let builtin_scope = T.Enclosing T.{TypeName.value= "$builtins"; loc= Unknown}

let builtin_name (value : string) : T.qualified_procname =
  let name = T.ProcName.{value; loc= T.Location.Unknown} in
  {enclosing_class= builtin_scope; name}


(* Helper to box Python's int/string into Textual.
   Until we support Python types, we can't use Textual types, like `int` for
   bool so we wrap them all. *)
let python_bool = builtin_name "$python_bool$"

let python_int = builtin_name "$python_int$"

let python_string = builtin_name "$python_string$"

let python_tuple = builtin_name "$python_tuple$"

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


let global name = sprintf "$module$::%s" name
