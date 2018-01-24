(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module Hashtbl = Caml.Hashtbl

(** Module for Type Environments. *)

(** Hash tables on strings. *)
module TypenameHash = Hashtbl.Make (struct
  type t = Typ.Name.t

  let equal tn1 tn2 = Typ.Name.equal tn1 tn2

  let hash = Hashtbl.hash
end)

(** Type for type environment. *)
type t = Typ.Struct.t TypenameHash.t

let iter f tenv = TypenameHash.iter f tenv

let pp fmt (tenv: t) =
  TypenameHash.iter
    (fun name typ ->
      Format.fprintf fmt "@[<6>NAME: %s@." (Typ.Name.to_string name) ;
      Format.fprintf fmt "@[<6>TYPE: %a@." (Typ.Struct.pp Pp.text name) typ )
    tenv


(** Create a new type environment. *)
let create () = TypenameHash.create 1000

(** Construct a struct type in a type environment *)
let mk_struct tenv ?default ?fields ?statics ?methods ?supers ?annots name =
  let struct_typ =
    Typ.Struct.internal_mk_struct ?default ?fields ?statics ?methods ?supers ?annots ()
  in
  TypenameHash.replace tenv name struct_typ ;
  struct_typ


(** Look up a name in the global type environment. *)
let lookup tenv name : Typ.Struct.t option =
  try Some (TypenameHash.find tenv name) with Not_found ->
    (* ToDo: remove the following additional lookups once C/C++ interop is resolved *)
    match (name : Typ.Name.t) with
    | CStruct m -> (
      try Some (TypenameHash.find tenv (CppClass (m, NoTemplate))) with Not_found -> None )
    | CppClass (m, NoTemplate) -> (
      try Some (TypenameHash.find tenv (CStruct m)) with Not_found -> None )
    | _ ->
        None


let compare_fields (name1, _, _) (name2, _, _) = Typ.Fieldname.compare name1 name2

let equal_fields f1 f2 = Int.equal (compare_fields f1 f2) 0

let sort_fields fields = List.sort ~cmp:compare_fields fields

let sort_fields_tenv tenv =
  let sort_fields_struct name ({Typ.Struct.fields} as st) =
    ignore (mk_struct tenv ~default:st ~fields:(sort_fields fields) name)
  in
  iter sort_fields_struct tenv


(** Add a field to a given struct in the global type environment. *)
let add_field tenv class_tn_name field =
  match lookup tenv class_tn_name with
  | Some ({fields} as struct_typ) ->
      if not (List.mem ~equal:equal_fields fields field) then
        let new_fields = List.merge [field] fields ~cmp:compare_fields in
        ignore (mk_struct tenv ~default:struct_typ ~fields:new_fields ~statics:[] class_tn_name)
  | _ ->
      ()


(** Serializer for type environments *)
let tenv_serializer : t Serialization.serializer =
  Serialization.create_serializer Serialization.Key.tenv


let global_tenv : t option ref = ref None

let tenv_filename_of_source_file source_file =
  DB.source_dir_get_internal_file (DB.source_dir_from_source_file source_file) ".tenv"


let load source_file : t option =
  tenv_filename_of_source_file source_file |> Serialization.read_from_file tenv_serializer


let global_tenv_path = Config.(captured_dir ^/ global_tenv_filename) |> DB.filename_from_string

let load_global () : t option =
  if is_none !global_tenv then
    global_tenv := Serialization.read_from_file tenv_serializer global_tenv_path ;
  !global_tenv


let store_to_filename tenv tenv_filename =
  Serialization.write_to_file tenv_serializer tenv_filename ~data:tenv ;
  if Config.debug_mode then
    let debug_filename = DB.filename_to_string (DB.filename_add_suffix tenv_filename ".debug") in
    let out_channel = Out_channel.create debug_filename in
    let fmt = Format.formatter_of_out_channel out_channel in
    Format.fprintf fmt "%a" pp tenv ; Out_channel.close out_channel


let store source_file tenv = tenv_filename_of_source_file source_file |> store_to_filename tenv

let store_global tenv =
  (* update in-memory global tenv for later uses by this process, e.g. in single-core mode the
     frontend and backend run in the same process *)
  global_tenv := Some tenv ;
  store_to_filename tenv global_tenv_path


exception Found of Typ.Name.t

let language_is tenv lang =
  match TypenameHash.iter (fun n -> raise (Found n)) tenv with
  | () ->
      false
  | exception Found JavaClass _ ->
      Language.equal lang Java
  | exception Found _ ->
      Language.equal lang Clang
