(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual

module Builtin = struct
  type primitive = PythonInt | PythonFloat | PythonBool | PythonString | PythonBytes | PythonTuple
  [@@deriving compare]

  type textual =
    | IsTrue
    | BinaryAdd
    | PythonCall
    | PythonClass
    | PythonClassConstructor
    | PythonCode
    | PythonIter
    | PythonIterNext
  [@@deriving compare]

  type python = Print | Range [@@deriving compare]

  type t = Primitive of primitive | Textual of textual | Python of python [@@deriving compare]
end

include Builtin

type builtin = t = Primitive of primitive | Textual of textual | Python of python
[@@deriving compare]

let textual t = Textual t

let python_to_string = function Print -> "print" | Range -> "range"

let to_proc_name = function
  | Primitive primitive -> (
    match primitive with
    | PythonInt ->
        PyCommon.python_int
    | PythonFloat ->
        PyCommon.python_float
    | PythonBool ->
        PyCommon.python_bool
    | PythonString ->
        PyCommon.python_string
    | PythonBytes ->
        PyCommon.python_bytes
    | PythonTuple ->
        PyCommon.python_tuple )
  | Textual textual ->
      let str =
        match textual with
        | IsTrue ->
            "python_is_true"
        | BinaryAdd ->
            "binary_add"
        | PythonCall ->
            "python_call"
        | PythonClass ->
            "python_class"
        | PythonClassConstructor ->
            "python_class_constructor"
        | PythonCode ->
            "python_code"
        | PythonIter ->
            "python_iter"
        | PythonIterNext ->
            "python_iter_next"
      in
      PyCommon.builtin_name str
  | Python p ->
      let str = python_to_string p in
      PyCommon.builtin_name str


(** Lookup a [Python] builtin from its name *)
let of_string name =
  match name with "print" -> Some (Python Print) | "range" -> Some (Python Range) | _ -> None


let annot typ = T.Typ.{typ; attributes= []}

module Set = struct
  let string_ = T.Typ.(Ptr (Struct (PyCommon.type_name "String")))

  let bytes_ = T.Typ.(Ptr (Struct (PyCommon.type_name "Bytes")))

  type elt =
    { formals_types: T.Typ.annotated list option
    ; result_type: T.Typ.annotated
    ; used_struct_types: T.Struct.t list }

  module Info = Caml.Map.Make (Builtin)
  module Set_ = Caml.Set.Make (Builtin)

  let mk_builtin {formals_types; result_type; used_struct_types} builtin =
    let qualified_name = to_proc_name builtin in
    let procdecl =
      T.Module.Procdecl T.ProcDecl.{qualified_name; formals_types; result_type; attributes= []}
    in
    procdecl :: List.map ~f:(fun strct -> T.Module.Struct strct) used_struct_types


  type t = Set_.t

  let primitive_builtins =
    let builtins =
      [ ( Builtin.PythonInt
        , { formals_types= Some [annot T.Typ.Int]
          ; result_type= annot PyCommon.pyInt
          ; used_struct_types= [] } )
      ; ( Builtin.PythonFloat
        , { formals_types= Some [annot T.Typ.Float]
          ; result_type= annot PyCommon.pyFloat
          ; used_struct_types= [] } )
      ; ( Builtin.PythonBool
        , { formals_types= Some [annot T.Typ.Int]
          ; result_type= annot PyCommon.pyBool
          ; used_struct_types= [] } )
      ; ( Builtin.PythonString
        , { formals_types= Some [annot string_]
          ; result_type= annot PyCommon.pyString
          ; used_struct_types= [] } )
      ; ( Builtin.PythonBytes
        , { formals_types= Some [annot bytes_]
          ; result_type= annot PyCommon.pyBytes
          ; used_struct_types= [] } )
      ; ( Builtin.PythonTuple
        , {formals_types= None; result_type= annot PyCommon.pyObject; used_struct_types= []} ) ]
    in
    List.fold_left
      ~f:(fun acc (builtin, elt) -> Info.add (Builtin.Primitive builtin) elt acc)
      ~init:Info.empty builtins


  let textual_builtins =
    let builtins =
      [ ( Builtin.IsTrue
        , { formals_types= Some [annot PyCommon.pyObject]
          ; result_type= annot T.Typ.Int
          ; used_struct_types= [] } )
      ; ( Builtin.BinaryAdd
        , { formals_types= Some [annot PyCommon.pyObject; annot PyCommon.pyObject]
          ; result_type= annot PyCommon.pyObject
          ; used_struct_types= [] } )
      ; ( Builtin.PythonCall
        , {formals_types= None; result_type= annot PyCommon.pyObject; used_struct_types= []} )
      ; ( Builtin.PythonClass
        , { formals_types= Some [annot string_]
          ; result_type= annot PyCommon.pyClass
          ; used_struct_types= [] } )
      ; ( Builtin.PythonClassConstructor
          (* Class constructors can be implicitly inherited, so we are never sure of their
             arity. Also, we'll override their return type when we setup the call, to make it
             more precise. *)
        , {formals_types= None; result_type= annot PyCommon.pyObject; used_struct_types= []} )
      ; ( Builtin.PythonCode
        , { formals_types= Some [annot string_]
          ; result_type= annot PyCommon.pyCode
          ; used_struct_types= [] } )
        (* TODO: should we introduce a Textual type for iterators ? *)
      ; ( Builtin.PythonIter
        , { formals_types= Some [annot PyCommon.pyObject]
          ; result_type= annot PyCommon.pyObject
          ; used_struct_types= [] } )
      ; ( Builtin.PythonIterNext
        , { formals_types= Some [annot PyCommon.pyObject]
          ; result_type= annot PyCommon.pyIterItem
          ; used_struct_types= [PyCommon.pyIterItemStruct] } ) ]
    in
    List.fold_left
      ~f:(fun acc (builtin, elt) -> Info.add (Builtin.Textual builtin) elt acc)
      ~init:primitive_builtins builtins


  let python_builtins =
    [ ( Builtin.Print
      , {formals_types= None; result_type= annot PyCommon.pyObject; used_struct_types= []} )
    ; ( Builtin.Range
      , {formals_types= None; result_type= annot PyCommon.pyObject; used_struct_types= []} ) ]


  let supported_builtins =
    List.fold_left
      ~f:(fun acc (builtin, elt) -> Info.add (Builtin.Python builtin) elt acc)
      ~init:textual_builtins python_builtins


  (* [mk_builtin] always returns very small list, the nested iteration should be quite cheap *)
  let to_textual spotted =
    let init = Info.fold (fun key elt l -> mk_builtin elt key @ l) primitive_builtins [] in
    Info.fold
      (fun key elt l -> if Set_.mem key spotted then mk_builtin elt key @ l else l)
      supported_builtins init


  let register spotted name = Set_.add name spotted

  let get_type builtin =
    let info = Info.find_opt builtin supported_builtins in
    Option.value_map ~default:PyCommon.pyObject info ~f:(fun b -> b.result_type.typ)


  let supported_builtins () = List.map ~f:(fun (b, _elt) -> python_to_string b) python_builtins

  let empty = Set_.empty
end
