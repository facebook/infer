(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual
module SMap = Caml.Map.Make (String)
module SSet = Caml.Set.Make (String)
module IMap = Caml.Map.Make (Int)

let proc_name ?(loc = T.Location.Unknown) value = {T.ProcName.value; loc}

let type_name ?(loc = T.Location.Unknown) value = {T.TypeName.value; loc}

let var_name ?(loc = T.Location.Unknown) value = {T.VarName.value; loc}

let node_name ?(loc = T.Location.Unknown) value = {T.NodeName.value; loc}

let field_name ?(loc = T.Location.Unknown) value = {T.FieldName.value; loc}

(* TODO: only deal with toplevel functions for now *)
let qualified_procname ~enclosing_class name : T.QualifiedProcName.t =
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

let pyMap = mk_type "PyMap"

let pySet = mk_type "PySet"

let pyTuple = mk_type "PyTuple"

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

let builtin_scope = T.QualifiedProcName.Enclosing T.{TypeName.value= builtins; loc= Unknown}

let builtin_name (value : string) : T.QualifiedProcName.t =
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

let mk_int (z : Z.t) =
  let proc = python_int in
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


let get_string = function
  | T.Exp.Call {proc; args= [arg]; kind= NonVirtual}
    when T.QualifiedProcName.equal proc python_string -> (
    match arg with Const (Str s) -> Some s | _ -> None )
  | _ ->
      None


let get_tuple_as_list = function
  | T.Exp.Call {proc; args; kind= NonVirtual} when T.QualifiedProcName.equal proc python_tuple ->
      Some args
  | _ ->
      None


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
      location.

      Since we mostly append to these identifiers, we store them in reverse order, and only reverse
      them when generating textual. *)

  module IDENT = struct
    type root =
      { name: string
      ; loc: T.Location.t [@compare.ignore]
      ; global: bool [@compare.ignore] (* TODO: see if we can merge this with Symbol.key/Global *)
      }
    [@@deriving compare]

    (* We store:
        A as {root= A; path= Empty}
        A.B as {root= A; path= [B]}
        A.B0...Bn.B as {root= A; path= [B; Bn; ...; B0]}
    *)
    type t = {root: root; path: string list} [@@deriving compare]
  end

  include IDENT

  let root {root} = {root; path= []}

  let from_string ?(on = '/') ?(global = true) ?(loc = T.Location.Unknown) s =
    let items = String.split ~on s in
    match items with
    | [] ->
        None
    | name :: attrs ->
        let path = List.rev attrs in
        Some {root= {name; loc; global}; path}


  let as_list {root= {name}; path} = name :: List.rev path

  let pp_rev_list fmt l = Pp.seq ~sep:"::" Format.pp_print_string fmt (List.rev l)

  let pp fmt t =
    let l = as_list t in
    Pp.seq ~sep:"::" Format.pp_print_string fmt l


  let to_enclosing_name name path sep last =
    if List.is_empty path then sprintf "%s%s%s" name sep last
    else Format.asprintf "%s::%a%s%s" name pp_rev_list path sep last


  let fold ~f_root ~f_path ~init {root; path} =
    let {name; loc; global} = root in
    let acc = f_root init ~global ~loc name in
    List.fold_right path ~f:f_path ~init:acc


  let to_string ~sep {root= {name}; path} =
    match path with [] -> name | hd :: tl -> to_enclosing_name name tl sep hd


  let to_qualified_procname {root= {name; loc}; path} : T.QualifiedProcName.t =
    match path with
    | [] ->
        {enclosing_class= TopLevel; name= proc_name ~loc name}
    | hd :: tl ->
        let enclosing_class =
          let value =
            if List.is_empty tl then name else Format.asprintf "%s::%a" name pp_rev_list tl
          in
          let type_name = type_name ~loc value in
          T.QualifiedProcName.Enclosing type_name
        in
        let name = {T.ProcName.value= hd; loc} in
        {enclosing_class; name}


  let to_type_name ?(static = false) {root= {name; loc}; path} : T.TypeName.t =
    match path with
    | [] ->
        let name = if static then static_companion name else name in
        type_name ~loc name
    | hd :: tl ->
        let last = if static then static_companion hd else hd in
        let value = to_enclosing_name name tl "::" last in
        type_name ~loc value


  let to_proc_name_ sep {root= {name; loc}; path} : T.ProcName.t =
    match path with
    | [] ->
        proc_name ~loc name
    | hd :: tl ->
        let value = to_enclosing_name name tl sep hd in
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
    match path with
    | [] ->
        SMap.mem name primitive_types
    | [attrname] when String.equal builtins name ->
        SMap.mem attrname primitive_types
    | _ ->
        false


  let to_typ ({root= {name}; path} as t) =
    let default = to_typ_struct t in
    if List.is_empty path then SMap.find_opt name primitive_types |> Option.value ~default
    else default


  let to_var_name {root= {name; loc}; path} : T.VarName.t =
    match path with
    | [] ->
        var_name ~loc name
    | hd :: tl ->
        let value = to_enclosing_name name tl "::" hd in
        var_name ~loc value


  let ambiguous = "$ambiguous"

  let mk_unknown_ident ?(loc = T.Location.Unknown) last =
    let root = {name= ambiguous; loc; global= true} in
    let path = [last] in
    {root; path}


  let extend_unknown_ident {root= {name; loc}; path} =
    let root = {name= ambiguous; loc; global= true} in
    let path = path @ [name] in
    {root; path}


  let mk ?(global = true) ?(loc = T.Location.Unknown) name = {root= {name; global; loc}; path= []}

  let extend ~prefix name =
    let {root; path} = prefix in
    {root; path= name :: path}


  let pop {root; path} =
    match path with
    | [] ->
        let {name} = root in
        (name, None)
    | hd :: tl ->
        (hd, Some {root; path= tl})


  let mk_builtin last =
    let root = {name= builtins; loc= T.Location.Unknown; global= true} in
    let path = [last] in
    {root; path}


  let is_imported_ABC {root= {name}; path} =
    match path with
    | [last] ->
        String.equal ABC.base_class last && String.equal ABC.import_name name
    | _ ->
        false


  module Map = Caml.Map.Make (IDENT)
  module Set = Caml.Set.Make (IDENT)
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

let exit = "__exit__"

let enter = "__enter__"

let class__ = "__class__"

let classcell = "__classcell__"

let annotations = "__annotations__"

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
