(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual
module L = Logging
module SMap = Caml.Map.Make (String)

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

let pyList = mk_type "PyList"

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


let builtins = "$builtins"

let builtin_scope = T.Enclosing T.{TypeName.value= builtins; loc= Unknown}

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


let static_companion name = sprintf "%s$static" name

(** Definitions related to Python Abstract Base Classes see
    https://docs.python.org/3/library/abc.html *)
module ABC = struct
  let abstract_method = "abstractmethod"

  let import_name = "abc"

  let base_class = "ABC"
end

module Ident = struct
  (** Python uses qualified identifiers such as [sys.exit]. Locally defined names don't have any
      prefix, but we still add some in textual to deal with ambiguity. The only identifiers without
      any prefix are local variables.

      Since we mostly append to these identifiers, we store them in reverse order, and only reverse
      them when generating textual. *)

  module IDENT = struct
    type t = {hd: string; tl: string list} [@@deriving compare]
  end

  include IDENT

  let from_string s =
    match List.rev (String.split ~on:'.' s) with
    | [] ->
        L.die InternalError "String '%s' is an invalid Ident" s
    | hd :: tl ->
        {hd; tl}


  let short {hd} = hd

  let pp_rev_list fmt l = Pp.seq ~sep:"::" Format.pp_print_string fmt (List.rev l)

  let pp fmt {hd; tl} =
    let l = hd :: tl in
    pp_rev_list fmt l


  let to_string ~sep {hd; tl} =
    if List.is_empty tl then hd else Format.asprintf "%a%s%s" pp_rev_list tl sep hd


  let to_enclosing_name l = Format.asprintf "%a" pp_rev_list l

  let to_qualified_procname ?(loc = T.Location.Unknown) {hd; tl} : T.qualified_procname =
    let enclosing_class =
      if List.is_empty tl then T.TopLevel
      else
        let value = to_enclosing_name tl in
        let type_name = type_name ~loc value in
        T.Enclosing type_name
    in
    let name = {T.ProcName.value= hd; loc} in
    {T.enclosing_class; name}


  let to_type_name ?(loc = T.Location.Unknown) ?(static = false) {hd; tl} : T.TypeName.t =
    let hd = if static then static_companion hd else hd in
    let value = to_enclosing_name (hd :: tl) in
    type_name ~loc value


  let to_proc_name ?(loc = T.Location.Unknown) {hd; tl} : T.ProcName.t =
    let value = to_enclosing_name tl in
    let value = sprintf "%s.%s" value hd in
    proc_name ~loc value


  let to_constructor ?(loc = T.Location.Unknown) {hd; tl} : T.ProcName.t =
    let value = to_enclosing_name (hd :: tl) in
    proc_name ~loc value


  let to_typ_struct loc id =
    let typ = to_type_name ~loc id in
    T.Typ.(Ptr (Struct typ))


  let primitive_types =
    let map = SMap.empty in
    let map = SMap.add "int" pyInt map in
    let map = SMap.add "str" pyString map in
    let map = SMap.add "bool" pyBool map in
    let map = SMap.add "float" pyFloat map in
    let map = SMap.add "None" pyNone map in
    let map = SMap.add "object" pyObject map in
    map


  let is_primitive_type {hd; tl} = match tl with [] -> SMap.mem hd primitive_types | _ -> false

  let to_typ ?(loc = T.Location.Unknown) id =
    let {hd; tl} = id in
    let default = to_typ_struct loc id in
    match tl with [] -> SMap.find_opt hd primitive_types |> Option.value ~default | _ -> default


  let to_var_name ?(loc = T.Location.Unknown) {hd; tl} : T.VarName.t =
    let value = String.concat ~sep:"::" @@ List.rev (hd :: tl) in
    var_name ~loc value


  let unknown_ident name = {hd= name; tl= ["$ambiguous"]}

  let mk ?prefix name =
    match prefix with Some {hd; tl} -> {hd= name; tl= hd :: tl} | None -> {hd= name; tl= []}


  let mk_builtin name = {hd= name; tl= [builtins]}

  let is_imported_ABC {hd; tl} =
    match tl with
    | [module_name] ->
        String.equal ABC.base_class hd && String.equal ABC.import_name module_name
    | _ ->
        false


  module Map = Caml.Map.Make (IDENT)
end

type annotated_name = {name: string; annotation: Ident.t}

let pp_annotated_name fmt {name; annotation} =
  Format.fprintf fmt "(%s: %a)" name Ident.pp annotation


type signature = annotated_name list

let pp_signature fmt signature = Pp.seq ~sep:" -> " pp_annotated_name fmt signature

let toplevel_function = "$toplevel"

let static_method = "staticmethod"

let init__ = "__init__"

let new__ = "__new__"

let return = "return"

let entry = "entry"

let self = "self"

(** Flags used by MAKE_FUNCTION *)
module MakeFunctionFlags = struct
  (* 0x01 a tuple of default values for positional-only and
   * positional-or-keyword parameters in positional order
   *
   * 0x02 a dictionary of keyword-only parameters’ default values
   *
   * 0x04 an annotation dictionary
   *
   * 0x08 a tuple containing cells for free variables, making a closure
   *)
  type flag = DefaultValues | DictDefaultValues | Annotations | Closure [@@deriving equal]

  let to_int = function
    | DefaultValues ->
        0x01
    | DictDefaultValues ->
        0x02
    | Annotations ->
        0x04
    | Closure ->
        0x08


  type t = int

  let mk flags = flags land 0xf

  let mem flags flag =
    let v = to_int flag in
    flags land v <> 0


  let pp fmt flags =
    let l = [] in
    let l = if mem flags DefaultValues then "default" :: l else l in
    let l = if mem flags DictDefaultValues then "dict-default" :: l else l in
    let l = if mem flags Annotations then "annotations" :: l else l in
    let l = if mem flags Closure then "closure" :: l else l in
    Format.fprintf fmt "[0x%x; %a]" flags (Pp.comma_seq Format.pp_print_string) l


  let set flags flag =
    let v = to_int flag in
    flags lor v


  let unset flags flag =
    let v = to_int flag in
    let not_v = lnot v land 0xf in
    flags land not_v
end

(* TODO: [raw_qualified_name] is not used at the moment. We might want to use it for some sanity
   checks. *)
type method_info =
  { name: string
  ; raw_qualified_name: string
  ; code: FFI.Constant.t
  ; signature: signature
  ; is_static: bool
  ; is_abstract: bool
  ; flags: MakeFunctionFlags.t }
