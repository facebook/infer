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
  (** Python uses qualified identifiers such as [sys.exit]. Top-level defined names don't have any
      prefix, but we still add some in textual to deal with ambiguity. The only identifiers without
      any prefix are local variables.

      We have to single out the "root" id, to remember if it was a local vs global name, and its
      location. We also single out the last element to ease the transformation to a procname (like
      [foo::bar.f]) vs a type name (like [foo::bar::T])

      Since we mostly append to these identifiers, we store them in reverse order, and only reverse
      them when generating textual. *)

  module IDENT = struct
    type root =
      { name: string
      ; loc: T.Location.t [@compare.ignore]
      ; global: bool [@compare.ignore] (* TODO: see if we can merge this with Symbol.key/Global *)
      }
    [@@deriving compare]

    type path = Empty | Path of {path: string list; last: string} [@@deriving compare]

    (* We store:
        A as {root= A; path= Empty}
        A.B as {root= A; path= Path{path= []; last= B}}
        A.B0...Bn.B as {root= A; path= Path{path= Bn..B0; last= B}}
    *)
    type t = {root: root; path: path} [@@deriving compare]
  end

  include IDENT

  let from_string ?(global = true) ?(loc = T.Location.Unknown) s =
    let items = String.split ~on:'.' s in
    match items with
    | [] ->
        L.die InternalError "String '%s' is an invalid Ident" s
    | name :: attrs -> (
      match List.rev attrs with
      | [] ->
          {root= {name; loc; global}; path= Empty}
      | last :: path ->
          {root= {name; loc; global}; path= Path {last; path}} )


  let last {root= {name}; path} = match path with Empty -> name | Path {last} -> last

  let as_list {root= {name}; path} =
    match path with Empty -> [name] | Path {path; last} -> name :: List.rev (last :: path)


  let pp_rev_list fmt l = Pp.seq ~sep:"::" Format.pp_print_string fmt (List.rev l)

  let pp fmt t =
    let l = as_list t in
    Pp.seq ~sep:"::" Format.pp_print_string fmt l


  let to_enclosing_name name path sep last =
    if List.is_empty path then sprintf "%s%s%s" name sep last
    else Format.asprintf "%s::%a%s%s" name pp_rev_list path sep last


  let to_string ~sep {root= {name}; path} =
    match path with Empty -> name | Path {path; last} -> to_enclosing_name name path sep last


  let to_qualified_procname {root= {name; loc}; path} : T.qualified_procname =
    match path with
    | Empty ->
        {T.enclosing_class= TopLevel; name= proc_name ~loc name}
    | Path {path; last} ->
        let enclosing_class =
          let value =
            if List.is_empty path then name else Format.asprintf "%s::%a" name pp_rev_list path
          in
          let type_name = type_name ~loc value in
          T.Enclosing type_name
        in
        let name = {T.ProcName.value= last; loc} in
        {T.enclosing_class; name}


  let to_type_name ?(static = false) {root= {name; loc}; path} : T.TypeName.t =
    match path with
    | Empty ->
        let name = if static then static_companion name else name in
        type_name ~loc name
    | Path {path; last} ->
        let last = if static then static_companion last else last in
        let value = to_enclosing_name name path "::" last in
        type_name ~loc value


  let to_proc_name_ sep {root= {name; loc}; path} : T.ProcName.t =
    match path with
    | Empty ->
        proc_name ~loc name
    | Path {path; last} ->
        let value = to_enclosing_name name path sep last in
        proc_name ~loc value


  let to_proc_name t = to_proc_name_ "." t

  let to_constructor t = to_proc_name_ "::" t

  let to_typ_struct t =
    let typ = to_type_name t in
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


  let is_primitive_type {root= {name}; path} =
    match path with Empty -> SMap.mem name primitive_types | Path _ -> false


  let to_typ ({root= {name}; path} as t) =
    let default = to_typ_struct t in
    match path with
    | Empty ->
        SMap.find_opt name primitive_types |> Option.value ~default
    | _ ->
        default


  let to_var_name {root= {name; loc}; path} : T.VarName.t =
    match path with
    | Empty ->
        var_name ~loc name
    | Path {path; last} ->
        let value = to_enclosing_name name path "::" last in
        var_name ~loc value


  let unknown_ident ?(loc = T.Location.Unknown) last =
    let root = {name= "$ambiguous"; loc; global= true} in
    let path = Path {last; path= []} in
    {root; path}


  let mk ?(global = true) ?(loc = T.Location.Unknown) name = {root= {name; global; loc}; path= Empty}

  let extend ~prefix name =
    let {root; path} = prefix in
    match path with
    | Empty ->
        {root; path= Path {last= name; path= []}}
    | Path {last; path} ->
        {root; path= Path {last= name; path= last :: path}}


  let mk_builtin last =
    let root = {name= builtins; loc= T.Location.Unknown; global= true} in
    let path = Path {last; path= []} in
    {root; path}


  let is_imported_ABC {root= {name}; path} =
    match path with
    | Path {path= []; last} ->
        String.equal ABC.base_class last && String.equal ABC.import_name name
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
