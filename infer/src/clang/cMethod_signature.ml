(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Define the signature of a method consisting of its name, its arguments, *)
(** return type, location and whether its an instance method.  *)

type method_signature = {
  _name : Procname.t;
  _args : (string * string) list; (* (name, type) *)
  _ret_type : string;
  _loc : Clang_ast_t.source_range;
  _is_instance : bool
}

let ms_get_name ms =
  ms._name

let ms_get_args ms =
  ms._args

let ms_get_ret_type ms =
  ms._ret_type

let ms_get_loc ms =
  ms._loc

let ms_is_instance ms =
  ms._is_instance

type methodMap = method_signature Procname.Map.t

let methodMap = ref Procname.Map.empty

let make_ms procname args ret_type loc is_instance =
  let meth_signature = {
    _name = procname;
    _args = args;
    _ret_type = ret_type;
    _loc = loc;
    _is_instance = is_instance } in
  meth_signature

let replace_name_ms ms name =
  let meth_signature = {
    _name = name;
    _args = ms._args;
    _ret_type = ms._ret_type;
    _loc = ms._loc;
    _is_instance = ms._is_instance } in
  meth_signature

let ms_to_string ms =
  "Method "^(Procname.to_string ms._name)^" "^
  (Utils.list_to_string (fun (s1, s2) -> s1^", "^s2) ms._args)^"->"^ms._ret_type^" "^
  Clang_ast_j.string_of_source_range ms._loc

let find ms =
  Procname.Map.find ms !methodMap

let add ms =
  try ignore (find ms._name)
  with Not_found -> methodMap := Procname.Map.add ms._name ms !methodMap

let reset_map () =
  methodMap := Procname.Map.empty
