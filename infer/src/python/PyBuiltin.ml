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

  (* Some interesting source of information: https://docs.python.org/3/library/operator.html *)
  module Compare = struct
    type t = Lt | Le | Eq | Neq | Gt | Ge | In | NotIn | Is | IsNot | Exception | BAD
    [@@deriving compare, enumerate]

    let to_string = function
      | Lt ->
          "lt"
      | Le ->
          "le"
      | Eq ->
          "eq"
      | Neq ->
          "neq"
      | Gt ->
          "gt"
      | Ge ->
          "ge"
      | In ->
          "in"
      | NotIn ->
          "not_in"
      | Is ->
          "is"
      | IsNot ->
          "is_not"
      | Exception ->
          "exception"
      | BAD ->
          "bad"


    let pp fmt op = to_string op |> Format.pp_print_string fmt
  end

  type binary_op =
    | Add
    | And
    | FloorDivide
    | LShift
    | MatrixMultiply
    | Modulo
    | Multiply
    | Or
    | Power
    | RShift
    | Subtract
    | TrueDivide
    | Xor
  [@@deriving compare]

  let binary_op_to_string = function
    | Add ->
        "add"
    | And ->
        "and"
    | FloorDivide ->
        "floor_divide"
    | LShift ->
        "lshift"
    | MatrixMultiply ->
        "matrix_multiply"
    | Modulo ->
        "modulo"
    | Multiply ->
        "multiply"
    | Or ->
        "or"
    | Power ->
        "power"
    | RShift ->
        "rshift"
    | Subtract ->
        "subtract"
    | TrueDivide ->
        "true_divide"
    | Xor ->
        "xor"


  type unary_op = Positive | Negative | Not | Invert [@@deriving compare]

  let unary_op_to_string = function
    | Positive ->
        "positive"
    | Negative ->
        "negative"
    | Not ->
        "not"
    | Invert ->
        "invert"


  (* For slice, we could provide both Slice2 and Slice3 (see [0]) but having an
     unspecified number of argument will be enough for prototyping

     Also we could add a specific type for slices, but right now we don't need it

     [0] https://docs.python.org/3.8/library/functions.html#slice
  *)
  type builder = List | Set | Tuple | Map | String | Slice [@@deriving compare]

  let builder_to_string = function
    | List ->
        "list"
    | Set ->
        "set"
    | Tuple ->
        "tuple"
    | Map ->
        "map"
    | String ->
        "string"
    | Slice ->
        "slice"


  (* TODO: remove some of the redundant Python prefixes *)
  type textual =
    | IsTrue
    | Binary of binary_op
    (* BINARY_SUBSCR is more complex and is done in PyTrans *)
    | Inplace of binary_op
    | Unary of unary_op
    | PythonCall
    | PythonCallKW
    | PythonKWArg
    | PythonClass
    | PythonClassName
    | PythonCode
    | PythonIter
    | PythonIterNext
    | PythonBuild of builder
    | PythonIndex
    | PythonSubscriptGet
    | PythonSubscriptSet
    | PythonFormatRepr
    | PythonFormatStr
    | PythonFormatAscii
    | PythonFormat
    | CompareOp of Compare.t
  [@@deriving compare]

  type python = Print | Range | Open | Len | Type | Str [@@deriving compare]

  type t = Primitive of primitive | Textual of textual | Python of python [@@deriving compare]
end

include Builtin

type builtin = t = Primitive of primitive | Textual of textual | Python of python
[@@deriving compare]

let textual t = Textual t

let python_to_string = function
  | Print ->
      "print"
  | Range ->
      "range"
  | Open ->
      "open"
  | Len ->
      "len"
  | Type ->
      "type"
  | Str ->
      "str"


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
        | Binary op ->
            sprintf "binary_%s" (binary_op_to_string op)
        | Inplace op ->
            sprintf "inplace_%s" (binary_op_to_string op)
        | Unary op ->
            sprintf "unary_%s" (unary_op_to_string op)
        | PythonCall ->
            "python_call"
        | PythonCallKW ->
            "python_call_kw"
        | PythonKWArg ->
            "python_kw_arg"
        | PythonClass ->
            "python_class"
        | PythonClassName ->
            "python_class_name"
        | PythonCode ->
            "python_code"
        | PythonIter ->
            "python_iter"
        | PythonIterNext ->
            "python_iter_next"
        | PythonBuild builder ->
            sprintf "python_build_%s" (builder_to_string builder)
        | PythonIndex ->
            "python_index"
        | PythonSubscriptGet ->
            "python_subscript_get"
        | PythonSubscriptSet ->
            "python_subscript_set"
        | PythonFormatStr ->
            "python_format_str"
        | PythonFormatRepr ->
            "python_format_repr"
        | PythonFormatAscii ->
            "python_format_ascii"
        | PythonFormat ->
            "python_format"
        | CompareOp op ->
            sprintf "python_%s" (Compare.to_string op)
      in
      PyCommon.builtin_name str
  | Python p ->
      let str = python_to_string p in
      PyCommon.builtin_name str


(** Lookup a [Python] builtin from its name. *)
let of_string name =
  match name with
  | "print" ->
      Some (Python Print)
  | "range" ->
      Some (Python Range)
  | "open" ->
      Some (Python Open)
  | "len" ->
      Some (Python Len)
  | "type" ->
      Some (Python Type)
  | "str" ->
      Some (Python Str)
  | _ ->
      None


let annot typ = T.Typ.{typ; attributes= []}

let annotatedObject = annot PyCommon.pyObject

module Set = struct
  let string_ = PyCommon.mk_type "String"

  let bytes_ = PyCommon.mk_type "Bytes"

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
        , {formals_types= None; result_type= annotatedObject; used_struct_types= []} ) ]
    in
    List.fold_left
      ~f:(fun acc (builtin, elt) -> Info.add (Builtin.Primitive builtin) elt acc)
      ~init:Info.empty builtins


  let textual_builtins =
    let compare_op op =
      ( Builtin.CompareOp op
      , { formals_types= Some [annotatedObject; annotatedObject]
        ; result_type= annot PyCommon.pyBool
        ; used_struct_types= [] } )
    in
    let binary_op op =
      ( op
      , { formals_types= Some [annotatedObject; annotatedObject]
        ; result_type= annotatedObject
        ; used_struct_types= [] } )
    in
    let unary_op op =
      ( op
      , {formals_types= Some [annotatedObject]; result_type= annotatedObject; used_struct_types= []}
      )
    in
    let no_formal ?(result_type = annotatedObject) op =
      (op, {formals_types= None; result_type; used_struct_types= []})
    in
    let builtins =
      [ ( Builtin.IsTrue
        , { formals_types= Some [annotatedObject]
          ; result_type= annot T.Typ.Int
          ; used_struct_types= [] } )
      ; binary_op (Builtin.Binary Add)
      ; binary_op (Builtin.Binary And)
      ; binary_op (Builtin.Binary FloorDivide)
      ; binary_op (Builtin.Binary LShift)
      ; binary_op (Builtin.Binary MatrixMultiply)
      ; binary_op (Builtin.Binary Modulo)
      ; binary_op (Builtin.Binary Multiply)
      ; binary_op (Builtin.Binary Or)
      ; binary_op (Builtin.Binary Power)
      ; binary_op (Builtin.Binary RShift)
      ; binary_op (Builtin.Binary Subtract)
      ; binary_op (Builtin.Binary TrueDivide)
      ; binary_op (Builtin.Binary Xor)
      ; binary_op (Builtin.Inplace Add)
      ; binary_op (Builtin.Inplace And)
      ; binary_op (Builtin.Inplace FloorDivide)
      ; binary_op (Builtin.Inplace LShift)
      ; binary_op (Builtin.Inplace MatrixMultiply)
      ; binary_op (Builtin.Inplace Modulo)
      ; binary_op (Builtin.Inplace Multiply)
      ; binary_op (Builtin.Inplace Or)
      ; binary_op (Builtin.Inplace Power)
      ; binary_op (Builtin.Inplace RShift)
      ; binary_op (Builtin.Inplace Subtract)
      ; binary_op (Builtin.Inplace TrueDivide)
      ; binary_op (Builtin.Inplace Xor)
      ; unary_op (Builtin.Unary Positive)
      ; unary_op (Builtin.Unary Negative)
      ; unary_op (Builtin.Unary Not)
      ; unary_op (Builtin.Unary Invert)
      ; no_formal Builtin.PythonCall
      ; no_formal Builtin.PythonCallKW
      ; ( Builtin.PythonKWArg
        , { formals_types= Some [annot string_; annotatedObject]
          ; result_type= annotatedObject
          ; used_struct_types= [] } )
      ; ( Builtin.PythonClass
        , { formals_types= Some [annot string_]
          ; result_type= annot PyCommon.pyClass
          ; used_struct_types= [] } )
      ; ( Builtin.PythonClassName
        , {formals_types= None; result_type= annot PyCommon.pyString; used_struct_types= []} )
      ; ( Builtin.PythonCode
        , { formals_types= Some [annot string_]
          ; result_type= annot PyCommon.pyCode
          ; used_struct_types= [] } )
        (* TODO: should we introduce a Textual type for iterators ? *)
      ; ( Builtin.PythonIter
        , { formals_types= Some [annotatedObject]
          ; result_type= annotatedObject
          ; used_struct_types= [] } )
      ; ( Builtin.PythonIterNext
        , { formals_types= Some [annotatedObject]
          ; result_type= annot PyCommon.pyIterItem
          ; used_struct_types= [PyCommon.pyIterItemStruct] } )
      ; no_formal (Builtin.PythonBuild List) ~result_type:(annot PyCommon.pyList)
      ; no_formal (Builtin.PythonBuild Set) ~result_type:(annot PyCommon.pySet)
      ; no_formal (Builtin.PythonBuild Tuple) ~result_type:(annot PyCommon.pyTuple)
      ; no_formal (Builtin.PythonBuild Map) ~result_type:(annot PyCommon.pyMap)
      ; no_formal (Builtin.PythonBuild String) ~result_type:(annot string_)
      ; no_formal (Builtin.PythonBuild Slice)
      ; ( Builtin.PythonIndex
        , { formals_types= Some [annot PyCommon.pyObject; annot T.Typ.Int]
          ; result_type= annot PyCommon.pyObject
          ; used_struct_types= [] } )
      ; ( Builtin.PythonSubscriptGet
        , { formals_types= Some [annotatedObject; annotatedObject]
          ; result_type= annotatedObject
          ; used_struct_types= [] } )
      ; ( Builtin.PythonSubscriptSet
        , { formals_types= Some [annotatedObject; annotatedObject; annotatedObject]
          ; result_type= annot PyCommon.pyNone
          ; used_struct_types= [] } )
      ; ( Builtin.PythonFormatStr
        , { formals_types= Some [annotatedObject]
          ; result_type= annotatedObject (* usually a string, but not always *)
          ; used_struct_types= [] } )
      ; ( Builtin.PythonFormatAscii
        , { formals_types= Some [annotatedObject]
          ; result_type= annotatedObject (* usually a string, but not always *)
          ; used_struct_types= [] } )
      ; ( Builtin.PythonFormatRepr
        , { formals_types= Some [annotatedObject]
          ; result_type= annotatedObject (* usually a string, but not always *)
          ; used_struct_types= [] } )
      ; ( Builtin.PythonFormat
        , { formals_types= Some [annotatedObject; annotatedObject]
          ; result_type= annotatedObject (* usually a string, but might be None *)
          ; used_struct_types= [] } )
      ; compare_op Eq
      ; compare_op Neq
      ; compare_op Lt
      ; compare_op Le
      ; compare_op Gt
      ; compare_op Ge
      ; compare_op Is
      ; compare_op IsNot
      ; compare_op In
      ; compare_op NotIn (* TODO: add type signatures of other CompareOp when we support them *) ]
    in
    List.fold_left
      ~f:(fun acc (builtin, elt) -> Info.add (Builtin.Textual builtin) elt acc)
      ~init:primitive_builtins builtins


  let python_builtins =
    [ (Builtin.Print, {formals_types= None; result_type= annotatedObject; used_struct_types= []})
    ; (Builtin.Open, {formals_types= None; result_type= annotatedObject; used_struct_types= []})
    ; (Builtin.Range, {formals_types= None; result_type= annotatedObject; used_struct_types= []})
    ; ( Builtin.Len
      , { formals_types= Some [annotatedObject]
        ; result_type= annot PyCommon.pyInt
        ; used_struct_types= [] } )
    ; (Builtin.Str, {formals_types= None; result_type= annot string_; used_struct_types= []})
    ; ( Builtin.Type
      , {formals_types= Some [annotatedObject]; result_type= annotatedObject; used_struct_types= []}
      ) ]


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
