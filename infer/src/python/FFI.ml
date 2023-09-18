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
  F.fprintf fmt "[|@[%a@]|]" (Pp.collection ~fold:Array.fold ~sep:"; " ~pp_item) arr


type pyConstant =
  | PYCBool of bool
  | PYCInt of int64
  | PYCFloat of float
  | PYCString of string
  | PYCBytes of bytes
  | PYCTuple of pyConstant array
  | PYCCode of pyCode
  | PYCNone

and pyCode =
  { co_name: string
  ; co_filename: string
  ; (* TODO Not in use at the moment, just keeping it around not to forget about it *)
    co_flags: int
  ; co_cellvars: string array
  ; co_freevars: string array
  ; co_names: string array
  ; co_varnames: string array
  ; co_nlocals: int
  ; co_argcount: int
  ; co_firstlineno: int
  ; co_posonlyargcount: int
  ; co_stacksize: int
  ; co_kwonlyargcount: int
  ; co_lnotab: char array
  ; co_consts: pyConstant array
  ; (* Instead of keeping [co_code], they are translated into Python's [Instruction] *)
    instructions: pyInstruction list }

and pyInstruction =
  { (* TODO: make opname static ? list all names somewhere *)
    opname: string
  ; opcode: int
  ; arg: int
  ; argval: pyConstant
  ; (* TODO: python provides argval, not sure t is needed ? *)
    offset: int
  ; starts_line: int option
  ; is_jump_target: bool }
[@@deriving show, compare]

let die_invalid_field ~kind f obj =
  L.external_error "Field %s in object %s is not a valid %s@\n" f (Py.Object.to_string obj) kind ;
  Error ()


let read_field obj action f =
  match Py.Object.find_attr_string_opt obj f with
  | None ->
      L.external_error "No field %s in object %s@\n" f (Py.Object.to_string obj) ;
      Error ()
  | Some obj ->
      action f obj


let get_string f obj =
  if Py.String.check obj then Ok (Py.String.to_string obj)
  else die_invalid_field ~kind:"string" f obj


let read_string f obj = read_field obj get_string f

let get_int f obj =
  if Py.Int.check obj then Ok (Py.Int.to_int obj) else die_invalid_field ~kind:"int" f obj


let read_int f obj = read_field obj get_int f

let read_bool f obj =
  let action f obj =
    if Py.Bool.check obj then Ok (Py.Bool.to_bool obj) else die_invalid_field ~kind:"bool" f obj
  in
  read_field obj action f


let array_all arr =
  let exception LocalFailure in
  try
    let arr = Array.map ~f:(function Ok elt -> elt | Error () -> raise LocalFailure) arr in
    Ok arr
  with LocalFailure -> Error ()


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
  let ty = Py.Type.get obj in
  match ty with
  | Py.Type.Bool ->
      Ok (PYCBool (Py.Bool.to_bool obj))
  | Int | Long ->
      (* TODO: deal with big ints since python has arbitrary precision *)
      Ok (PYCInt (Py.Int.to_int64 obj))
  | None | Null ->
      Ok PYCNone
  | Tuple ->
      let arr = Py.Tuple.to_array_map new_py_constant obj in
      let arr = array_all arr in
      Result.map ~f:(fun arr -> PYCTuple arr) arr
  | Unicode ->
      let s = Py.String.to_string obj in
      Ok (PYCString s)
  | Unknown ->
      Result.map ~f:(fun code -> PYCCode code) (new_py_code obj)
  | Bytes ->
      let s = Py.Bytes.to_bytes obj in
      Ok (PYCBytes s)
  | Float ->
      let f = Py.Float.to_float obj in
      Ok (PYCFloat f)
  | Callable | Capsule | Closure | Dict | List | Module | Type | Iter | Set ->
      L.internal_error "[new_py_constant] unknown bytecode constant: %s@\n" (Py.Type.name ty) ;
      Error ()


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
    ; instructions }


and new_py_instruction obj =
  let open IResult.Let_syntax in
  let* opname = read_string "opname" obj in
  let* opcode = read_int "opcode" obj in
  let* opt = read_field obj (fun _ x -> Ok x) "arg" in
  let* arg = if Py.is_none opt then Ok 0 else get_int "arg" opt in
  let* argval =
    if Py.is_none opt then Ok PYCNone
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
    ; co_filename: string
    ; co_flags: int
    ; co_cellvars: string array
    ; co_freevars: string array
    ; co_names: string array
    ; co_varnames: string array
    ; co_nlocals: int
    ; co_argcount: int
    ; co_firstlineno: int
    ; co_posonlyargcount: int
    ; co_stacksize: int
    ; co_kwonlyargcount: int
    ; co_lnotab: char array
    ; co_consts: pyConstant array
    ; instructions: pyInstruction list }
  [@@deriving show, compare]

  let full_show = show

  let full_pp = pp

  let show code = code.co_name

  let pp fmt code = F.pp_print_string fmt code.co_name

  let is_closure {co_freevars; co_cellvars} =
    Array.length co_freevars + Array.length co_cellvars <> 0


  let get_arguments {co_varnames; co_argcount} = Array.sub co_varnames ~pos:0 ~len:co_argcount

  let get_locals {co_varnames; co_argcount} =
    let nr_varnames = Array.length co_varnames in
    Array.sub co_varnames ~pos:co_argcount ~len:(nr_varnames - co_argcount)
end

module Constant = struct
  let full_show = show_pyConstant

  type t = pyConstant =
    | PYCBool of bool
    | PYCInt of int64
    | PYCFloat of float
    | PYCString of string
    | PYCBytes of bytes
    | PYCTuple of t array
    | PYCCode of Code.t
    | PYCNone
  [@@deriving show, compare]

  let show ?(full = false) c = if full then full_show c else show c

  let as_code = function
    | PYCCode c ->
        Some c
    | PYCBool _ | PYCInt _ | PYCString _ | PYCTuple _ | PYCNone | PYCFloat _ | PYCBytes _ ->
        None


  let as_name = function
    | PYCString name ->
        Some name
    (* TODO: not sure if we should do that. Experience will tell *)
    | PYCBytes bs ->
        Some (Bytes.to_string bs)
    | PYCBool _ | PYCInt _ | PYCCode _ | PYCTuple _ | PYCNone | PYCFloat _ ->
        None
end

module Instruction = struct
  type t = pyInstruction =
    { (* TODO: make opname static ? list all names somewhere ? *)
      opname: string
    ; opcode: int
    ; arg: int
    ; argval: pyConstant
    ; offset: int
    ; starts_line: int option
    ; is_jump_target: bool }
  [@@deriving show, compare]
end

let from_python_object obj =
  let open IResult.Let_syntax in
  try
    let* cst = new_py_constant obj in
    match cst with
    | PYCCode code ->
        Ok code
    | _ ->
        L.internal_error "[load_code] must always return a code object@\n" ;
        Error ()
  with Py.E _ as e ->
    L.external_error "[load_code] pyml exception: %s@\n" (Exn.to_string e) ;
    Error ()


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
  if size <= 4 then (
    L.user_error "[from_bytecode] Not enough data in file %s@\n" filename ;
    Error () )
  else
    let magic = Base.Bytes.create 4 in
    let read_magic = Core.In_channel.input fp ~buf:magic ~pos:0 ~len:4 in
    (* Check the magic word *)
    let mref = Base.Bytes.create 4 in
    Base.Bytes.set mref 0 (Char.of_int_exn 85) ;
    Base.Bytes.set mref 1 (Char.of_int_exn 13) ;
    Base.Bytes.set mref 2 (Char.of_int_exn 13) ;
    Base.Bytes.set mref 3 (Char.of_int_exn 10) ;
    let show_array bytes =
      let arr = Base.Bytes.to_array bytes in
      Format.asprintf "%a" (pp_array Char.pp) arr
    in
    let* () =
      if read_magic <> 4 || not (Base.Bytes.equal magic mref) then (
        L.user_error "Invalid magic number for Python 3.8. Expected %s but got %s@\n"
          (show_array mref) (show_array magic) ;
        Error () )
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


(* TODO: Revisit when we initialize/finalize the Python interpreter when we'll support
   more than one file at a time *)
let from_file ~is_binary filename =
  let open IResult.Let_syntax in
  let* code = if is_binary then from_bytecode filename else from_source filename in
  Ok code
