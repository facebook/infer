(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module State = Llair2TextualState

let to_qualified_proc_name ?loc method_class_index func_name =
  let proc_name = Textual.ProcName.of_string ?loc func_name in
  let enclosing_class =
    match Textual.ProcName.Hashtbl.find_opt method_class_index proc_name with
    | Some class_name ->
        Textual.QualifiedProcName.Enclosing class_name
    | None ->
        Textual.QualifiedProcName.TopLevel
  in
  Textual.QualifiedProcName.{enclosing_class; name= proc_name; metadata= None}


let builtin_qual_proc_name =
  let enclosing_class = Textual.(QualifiedProcName.Enclosing (TypeName.of_string "$builtins")) in
  fun name : Textual.QualifiedProcName.t ->
    {enclosing_class; name= Textual.ProcName.of_string name; metadata= None}
