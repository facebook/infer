(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

let pp_array pp_item fmt arr =
  F.fprintf fmt "[|@[%a@]|]" (Pp.collection ~fold:Array.fold ~sep:"; " pp_item) arr


module Error = struct
  type kind =
    | MissingField of (string * Pytypes.pyobject)
    | InvalidField of (string * Pytypes.pyobject * string)
    | UnknownByteCodeConstant of Py.Type.t
    | InvalidCodeObject of Pytypes.pyobject
    | PymlExn of Pytypes.pyobject * Pytypes.pyobject
    | InvalidMagicNumber of bytes * bytes
    | ShortFile
    | SyntaxError of string

  type t = L.error * kind

  let pp_kind fmt = function
    | MissingField (f, obj) ->
        F.fprintf fmt "No field %s in object %s" f (Py.Object.to_string obj)
    | InvalidField (f, obj, kind) ->
        F.fprintf fmt "Field %s in object %s is not a valid %s" f (Py.Object.to_string obj) kind
    | UnknownByteCodeConstant ty ->
        F.fprintf fmt "Unknown bytecode constant: %s" (Py.Type.name ty)
    | InvalidCodeObject obj ->
        F.fprintf fmt "Failed to load code from object %s" (Py.Object.to_string obj)
    | PymlExn (errtype, errvalue) ->
        F.fprintf fmt "pyml exception: %s %s" (Py.Object.to_string errtype)
          (Py.Object.to_string errvalue)
    | InvalidMagicNumber (expected, actual) ->
        let expected = Base.Bytes.to_array expected in
        let actual = Base.Bytes.to_array actual in
        F.fprintf fmt "Invalid magic number for Python 3.8. Expected %a but got %a"
          (pp_array Char.pp) expected (pp_array Char.pp) actual
    | ShortFile ->
        F.pp_print_string fmt "Input file is too small"
    | SyntaxError msg ->
        F.pp_print_string fmt msg
end

module Z = struct
  include Z

  let pp fmt z = F.pp_print_string fmt (Z.to_string z)
end

type version = Python_3_10 | Python_3_12 [@@deriving equal, compare]

let pp_version fmt = function
  | Python_3_10 ->
      F.pp_print_string fmt "python3.10"
  | Python_3_12 ->
      F.pp_print_string fmt "python3.12"


let get_version () = match Py.version_pair () with 3, 12 -> Python_3_12 | _ -> Python_3_10

type pyConstant =
  | PYCBool of bool
  | PYCInt of Z.t
  | PYCFloat of float
  | PYCComplex of {real: float; imag: float}
  | PYCString of string
  | PYCInvalidUnicode
  | PYCBytes of bytes
  | PYCTuple of pyConstant array
  | PYCFrozenSet of pyConstant list
  | PYCCode of pyCode
  | PYCNone

and pyCode =
  { co_name: string
  ; co_firstlineno: int
  ; co_filename: string
  ; (* TODO Not in use at the moment, just keeping it around not to forget about it *)
    co_flags: int
  ; co_cellvars: string array
  ; co_freevars: string array
  ; co_names: string array
  ; co_varnames: string array
  ; co_nlocals: int
  ; co_argcount: int
  ; co_posonlyargcount: int
  ; co_stacksize: int
  ; co_kwonlyargcount: int
  ; co_lnotab: char array
  ; co_consts: pyConstant array
  ; (* Instead of keeping [co_code], they are translated into Python's [Instruction] *)
    instructions: pyInstruction list
  ; version: version }
[@@deriving equal]

and pyInstruction =
  { opname: string
  ; opcode: int
  ; arg: int
  ; argval: pyConstant
  ; (* TODO: python provides argval, not sure it is needed ? *)
    offset: int
  ; starts_line: int option
  ; is_jump_target: bool }
[@@deriving compare]

let rec pp_pyConstant fmt = function
  | PYCBool b ->
      F.pp_print_bool fmt b
  | PYCInt i ->
      Z.pp fmt i
  | PYCFloat f ->
      F.pp_print_float fmt f
  | PYCComplex {real; imag} ->
      F.fprintf fmt "Complex[real:%f; imag:%f ]" real imag
  | PYCString s ->
      F.fprintf fmt "\"%s\"" s
  | PYCInvalidUnicode ->
      F.pp_print_string fmt "InvalidUnicode"
  | PYCBytes bytes ->
      Bytes.pp fmt bytes
  | PYCTuple array ->
      F.fprintf fmt "(%a)" (Pp.comma_seq pp_pyConstant) (Array.to_list array)
  | PYCFrozenSet list ->
      F.fprintf fmt "{%a}" (Pp.comma_seq pp_pyConstant) list
  | PYCCode {co_name} ->
      F.fprintf fmt "<code object %s>" co_name
  | PYCNone ->
      F.pp_print_string fmt "None"


let pp_hint ~code:{co_consts; co_names; co_varnames; co_cellvars; co_freevars} fmt opname arg =
  let get_cell_name arg =
    let sz = Array.length co_cellvars in
    if arg < sz then co_cellvars.(arg) else co_freevars.(arg - sz)
  in
  match opname with
  | "LOAD_CONST" ->
      F.fprintf fmt "(%a)" pp_pyConstant co_consts.(arg)
  | "LOAD_NAME"
  | "LOAD_GLOBAL"
  | "LOAD_ATTR"
  | "STORE_NAME"
  | "STORE_GLOBAL"
  | "STORE_ATTR"
  | "LOAD_METHOD"
  | "IMPORT_NAME"
  | "IMPORT_FROM"
  | "DELETE_NAME"
  | "DELETE_GLOBAL"
  | "DELETE_ATTR" ->
      F.fprintf fmt "(%s)" co_names.(arg)
  | "LOAD_FAST" | "DELETE_FAST" | "STORE_FAST" ->
      F.fprintf fmt "(%s)" co_varnames.(arg)
  | "LOAD_DEREF" | "LOAD_CLASSDEREF" | "STORE_DEREF" | "LOAD_CLOSURE" | "DELETE_DEREF" ->
      F.fprintf fmt "(%s)" (get_cell_name arg)
  | "POP_JUMP_IF_TRUE"
  | "POP_JUMP_IF_FALSE"
  | "JUMP_ABSOLUTE"
  | "JUMP_IF_TRUE_OR_POP"
  | "JUMP_IF_FALSE_OR_POP" ->
      F.fprintf fmt "(to %d)" (2 * arg)
  | "JUMP_FORWARD" | "FOR_ITER" ->
      F.fprintf fmt "(to +%d)" (2 * arg)
  | _ ->
      F.fprintf fmt ""


let pp_pyInstruction ?code fmt {opname; arg; offset; starts_line; is_jump_target} =
  let pp_hint fmt = Option.iter code ~f:(fun code -> pp_hint ~code fmt opname arg) in
  F.fprintf fmt "%s %s %4d %-30s %4d %t"
    (match starts_line with None -> "    " | Some line -> F.asprintf "%4d" line)
    (if is_jump_target then ">>>" else "   ")
    offset opname arg pp_hint


let die_invalid_field ~kind f obj = Error (L.ExternalError, Error.InvalidField (f, obj, kind))

let read_field obj action f =
  match Py.Object.find_attr_string_opt obj f with
  | None ->
      Error (L.ExternalError, Error.MissingField (f, obj))
  | Some obj ->
      action f obj


let get_string f obj =
  if Py.String.check obj then Ok (Py.String.to_string obj)
  else die_invalid_field ~kind:"string" f obj


let read_string f obj = read_field obj get_string f

let get_int f obj =
  if Py.Int.check obj then Ok (Py.Int.to_int obj) else die_invalid_field ~kind:"int" f obj


let read_int f obj = read_field obj get_int f

let get_float f obj =
  if Py.Float.check obj then Ok (Py.Float.to_float obj) else die_invalid_field ~kind:"float" f obj


let read_float f obj = read_field obj get_float f

let read_bool f obj =
  let action f obj =
    if Py.Bool.check obj then Ok (Py.Bool.to_bool obj) else die_invalid_field ~kind:"bool" f obj
  in
  read_field obj action f


let array_all arr =
  let exception LocalFailure of Error.t in
  try
    let arr = Array.map ~f:(function Ok elt -> elt | Error err -> raise (LocalFailure err)) arr in
    Ok arr
  with LocalFailure err -> Error err


let read_symbol_array f obj =
  let action f obj =
    if Py.Tuple.check obj then
      let arr = Py.Tuple.to_array_map (get_string f) obj in
      array_all arr
    else die_invalid_field ~kind:"tuple" f obj
  in
  read_field obj action f


let read_char_array f obj =
  let open IResult.Let_syntax in
  let action f obj =
    try Ok (Py.Bytes.to_bytes obj) with Py.E _ -> die_invalid_field ~kind:"array of bytes" f obj
  in
  let* field = read_field obj action f in
  Ok (Core.Bytes.to_array field)


let rec new_py_constant obj =
  let open IResult.Let_syntax in
  let ty = Py.Type.get obj in
  match ty with
  | Py.Type.Bool ->
      Ok (PYCBool (Py.Bool.to_bool obj))
  | Int | Long ->
      (* Probably not the most efficient, but Python has integers with arbitrary size *)
      let s = Py.Int.to_string obj in
      let z = Z.of_string s in
      Ok (PYCInt z)
  | None | Null ->
      Ok PYCNone
  | Tuple ->
      let arr = Py.Tuple.to_array_map new_py_constant obj in
      let arr = array_all arr in
      Result.map ~f:(fun arr -> PYCTuple arr) arr
  | Unicode -> (
    try
      let s = Py.String.to_string obj in
      Ok (PYCString s)
    with Py.E _ -> Ok PYCInvalidUnicode )
  | Unknown ->
      let ty = Py.Object.get_type obj in
      let* class_name = read_string "__name__" ty in
      if String.equal "complex" class_name then
        let* real = read_float "real" obj in
        let* imag = read_float "imag" obj in
        Ok (PYCComplex {real; imag})
      else if String.equal "ellipsis" class_name then Ok PYCNone
      else if String.equal "frozenset" class_name then
        let lst = Py.Set.to_list_map new_py_constant obj in
        let* lst = Result.all lst in
        (* During testing the order sometimes changed, so let's make it determinist *)
        let lst = List.sort ~compare:compare_pyConstant lst in
        Ok (PYCFrozenSet lst)
      else Result.map ~f:(fun code -> PYCCode code) (new_py_code obj)
  | Bytes ->
      let s = Py.Bytes.to_bytes obj in
      Ok (PYCBytes s)
  | Float ->
      let f = Py.Float.to_float obj in
      Ok (PYCFloat f)
  | Callable | Capsule | Closure | Dict | List | Module | Type | Iter | Set ->
      Error (L.InternalError, Error.UnknownByteCodeConstant ty)


and new_py_code obj =
  let open IResult.Let_syntax in
  let* co_name = read_string "co_name" obj in
  let* co_filename = read_string "co_filename" obj in
  let* co_flags = read_int "co_flags" obj in
  let* co_cellvars = read_symbol_array "co_cellvars" obj in
  let* co_freevars = read_symbol_array "co_freevars" obj in
  let* co_names = read_symbol_array "co_names" obj in
  let* co_varnames = read_symbol_array "co_varnames" obj in
  let* co_nlocals = read_int "co_nlocals" obj in
  let* co_argcount = read_int "co_argcount" obj in
  let* co_firstlineno = read_int "co_firstlineno" obj in
  let* co_posonlyargcount = read_int "co_posonlyargcount" obj in
  let* co_stacksize = read_int "co_stacksize" obj in
  let* co_kwonlyargcount = read_int "co_kwonlyargcount" obj in
  let* co_lnotab = read_char_array "co_lnotab" obj in
  let* co_consts =
    let action f obj =
      if Py.Tuple.check obj then
        let arr = Py.Tuple.to_array_map new_py_constant obj in
        array_all arr
      else die_invalid_field ~kind:"array of constants" f obj
    in
    read_field obj action "co_consts"
  in
  (* TODO: this code is a simple function that takes the bytecode array
   * and turns it into a list of instructions. We might want to
   * replicate its job here, if speed gets in the way at some
   * point.
   * https://github.com/python/cpython/blob/main/Lib/dis.py#L337
   *
   * For now, I rely on calling Python to do the job for me:
   * The `pybc` module is an way to get/send data from/to python
   *)
  let code =
    {|
import dis, pybc
from pybc import input
l = list(dis.Bytecode(input))
pybc.output = l
|}
  in
  let m = Py.Import.add_module "pybc" in
  Py.Module.set m "input" obj ;
  let _none = Py.Run.eval ~start:Py.File code in
  let instructions = Py.Module.get m "output" in
  let* instructions =
    if Py.List.check instructions then
      let l = Py.List.to_list instructions in
      let l = List.map ~f:new_py_instruction l in
      Result.all l
    else die_invalid_field ~kind:"list of instructions" "co_code" obj
  in
  let version = get_version () in
  Ok
    { co_name
    ; co_filename
    ; co_flags
    ; co_cellvars
    ; co_freevars
    ; co_names
    ; co_varnames
    ; co_nlocals
    ; co_argcount
    ; co_firstlineno
    ; co_posonlyargcount
    ; co_stacksize
    ; co_kwonlyargcount
    ; co_lnotab
    ; co_consts
    ; instructions
    ; version }


and new_py_instruction obj =
  let open IResult.Let_syntax in
  let* opname = read_string "opname" obj in
  let* opcode = read_int "opcode" obj in
  let* opt = read_field obj (fun _ x -> Ok x) "arg" in
  let* arg = if Py.is_none opt then Ok 0 else get_int "arg" opt in
  (* [argval] is useful for debugging, but it is difficult to support it for
     FORMAT_VALUE at the moment, so for this opcode, I'll skip it. It just
     points to the function to use for formatting, so we don't need it. *)
  let* argval =
    if Py.is_none opt then Ok PYCNone
    else if String.equal opname "FORMAT_VALUE" then Ok PYCNone
    else
      let* opt = read_field obj (fun _ x -> Ok x) "argval" in
      new_py_constant opt
  in
  let* offset = read_int "offset" obj in
  let* starts_line =
    let* opt = read_field obj (fun _ x -> Ok x) "starts_line" in
    if Py.is_none opt then Ok None
    else
      let* i = get_int "starts_line" opt in
      Ok (Some i)
  in
  let* is_jump_target = read_bool "is_jump_target" obj in
  Ok {opname; opcode; arg; argval; offset; starts_line; is_jump_target}


module Code = struct
  type t = pyCode =
    { co_name: string
    ; co_firstlineno: int
    ; co_filename: string
    ; co_flags: int [@compare.ignore] [@equal.ignore]
    ; co_cellvars: string array [@compare.ignore] [@equal.ignore]
    ; co_freevars: string array [@compare.ignore] [@equal.ignore]
    ; co_names: string array [@compare.ignore] [@equal.ignore]
    ; co_varnames: string array [@compare.ignore] [@equal.ignore]
    ; co_nlocals: int [@compare.ignore] [@equal.ignore]
    ; co_argcount: int [@compare.ignore] [@equal.ignore]
    ; co_posonlyargcount: int [@compare.ignore] [@equal.ignore]
    ; co_stacksize: int [@compare.ignore] [@equal.ignore]
    ; co_kwonlyargcount: int [@compare.ignore] [@equal.ignore]
    ; co_lnotab: char array [@compare.ignore] [@equal.ignore]
    ; co_consts: pyConstant array [@compare.ignore] [@equal.ignore]
    ; instructions: pyInstruction list [@compare.ignore] [@equal.ignore]
    ; version: version [@compare.ignore] [@equal.ignore] }
  [@@deriving show, compare, equal]
end

module Constant = struct
  type t = pyConstant =
    | PYCBool of bool
    | PYCInt of Z.t
    | PYCFloat of float
    | PYCComplex of {real: float; imag: float}
    | PYCString of string
    | PYCInvalidUnicode
    | PYCBytes of bytes
    | PYCTuple of t array
    | PYCFrozenSet of t list
    | PYCCode of Code.t
    | PYCNone
  [@@deriving compare, equal]

  let pp = pp_pyConstant
end

module Instruction = struct
  type t = pyInstruction =
    { opname: string
    ; opcode: int
    ; arg: int
    ; argval: pyConstant
    ; offset: int
    ; starts_line: int option
    ; is_jump_target: bool }
  [@@deriving compare]

  let pp = pp_pyInstruction
end

let from_python_object obj =
  let open IResult.Let_syntax in
  try
    let* cst = new_py_constant obj in
    match cst with
    | PYCCode code ->
        Ok code
    | _ ->
        Error (L.InternalError, Error.InvalidCodeObject obj)
  with Py.E (errty, errvalue) -> Error (L.ExternalError, Error.PymlExn (errty, errvalue))


let from_string ~source ~filename =
  let pyobj = Py.Module.compile ~source ~filename Pytypes.File in
  from_python_object pyobj


let from_source filename =
  let source = Core.In_channel.read_all filename in
  from_string ~source ~filename


let from_bytecode filename =
  let open IResult.Let_syntax in
  (* see https://peps.python.org/pep-0552/ *)
  let fp = Core.In_channel.create ~binary:true filename in
  let size = Int64.to_int_exn @@ Core.In_channel.length fp in
  if size <= 4 then Error (L.UserError, Error.ShortFile)
  else
    let magic = Base.Bytes.create 4 in
    let read_magic = Core.In_channel.input fp ~buf:magic ~pos:0 ~len:4 in
    (* Check the magic word *)
    let mref = Base.Bytes.create 4 in
    Base.Bytes.set mref 0 (Char.of_int_exn 85) ;
    Base.Bytes.set mref 1 (Char.of_int_exn 13) ;
    Base.Bytes.set mref 2 (Char.of_int_exn 13) ;
    Base.Bytes.set mref 3 (Char.of_int_exn 10) ;
    let* () =
      if read_magic <> 4 || not (Base.Bytes.equal magic mref) then
        Error (L.UserError, Error.InvalidMagicNumber (mref, magic))
      else Ok ()
    in
    (* skipping 4 words = 16 bytes from the beginning, the rest is just marshalled data *)
    Core.In_channel.seek fp 16L ;
    (* Reading the full file here instead of passing the channel to pyml because there's a bug:
       In_channel read already too much, and pyml bindings don't account for that. So we'll do the
       reading explicitely and pass in a data buffer instead *)
    let data = Core.In_channel.input_all fp in
    Core.In_channel.close fp ;
    let pyobj = Py.Marshal.read_object_from_string data size in
    from_python_object pyobj


let from_file ~is_binary filename =
  let open IResult.Let_syntax in
  try
    let* code = if is_binary then from_bytecode filename else from_source filename in
    Ok code
  with Py.E (errtyp, errvalue) ->
    let msg = F.asprintf "%a, %a" Py.Object.format errtyp Py.Object.format errvalue in
    Error (L.ExternalError, Error.SyntaxError msg)
