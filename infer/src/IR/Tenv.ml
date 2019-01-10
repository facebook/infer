(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging

(** Module for Type Environments. *)

(** Hash tables on strings. *)
module TypenameHash = Hashtbl.Make (struct
  type t = Typ.Name.t

  let equal tn1 tn2 = Typ.Name.equal tn1 tn2

  let hash = Hashtbl.hash
end)

(** Type for type environment. *)
type t = Typ.Struct.t TypenameHash.t

let pp fmt (tenv : t) =
  TypenameHash.iter
    (fun name typ ->
      Format.fprintf fmt "@[<6>NAME: %s@]@," (Typ.Name.to_string name) ;
      Format.fprintf fmt "@[<6>TYPE: %a@]@," (Typ.Struct.pp Pp.text name) typ )
    tenv


(** Create a new type environment. *)
let create () = TypenameHash.create 1000

(** Construct a struct type in a type environment *)
let mk_struct tenv ?default ?fields ?statics ?methods ?exported_objc_methods ?supers ?annots name =
  let struct_typ =
    Typ.Struct.internal_mk_struct ?default ?fields ?statics ?methods ?exported_objc_methods ?supers
      ?annots ()
  in
  TypenameHash.replace tenv name struct_typ ;
  struct_typ


(** Look up a name in the global type environment. *)
let lookup tenv name : Typ.Struct.t option =
  try Some (TypenameHash.find tenv name) with Caml.Not_found -> (
    (* ToDo: remove the following additional lookups once C/C++ interop is resolved *)
    match (name : Typ.Name.t) with
    | CStruct m -> (
      try Some (TypenameHash.find tenv (CppClass (m, NoTemplate))) with Caml.Not_found -> None )
    | CppClass (m, NoTemplate) -> (
      try Some (TypenameHash.find tenv (CStruct m)) with Caml.Not_found -> None )
    | _ ->
        None )


let compare_fields (name1, _, _) (name2, _, _) = Typ.Fieldname.compare name1 name2

let equal_fields f1 f2 = Int.equal (compare_fields f1 f2) 0

(** Add a field to a given struct in the global type environment. *)
let add_field tenv class_tn_name field =
  match lookup tenv class_tn_name with
  | Some ({fields} as struct_typ) ->
      if not (List.mem ~equal:equal_fields fields field) then
        let new_fields = List.merge [field] fields ~compare:compare_fields in
        ignore (mk_struct tenv ~default:struct_typ ~fields:new_fields ~statics:[] class_tn_name)
  | _ ->
      ()


type per_file = Global | FileLocal of t

let pp_per_file fmt = function
  | Global ->
      Format.fprintf fmt "Global"
  | FileLocal tenv ->
      Format.fprintf fmt "FileLocal @[<v>%a@]" pp tenv


module SQLite : SqliteUtils.Data with type t = per_file = struct
  type t = per_file

  let global_string = "global"

  let serialize = function
    | Global ->
        Sqlite3.Data.TEXT global_string
    | FileLocal tenv ->
        Sqlite3.Data.BLOB (Marshal.to_string tenv [])


  let deserialize = function[@warning "-8"]
    | Sqlite3.Data.TEXT g when String.equal g global_string ->
        Global
    | Sqlite3.Data.BLOB b ->
        FileLocal (Marshal.from_string b 0)
end

let merge ~src ~dst =
  match (src, dst) with
  | Global, Global ->
      Global
  | FileLocal src_tenv, FileLocal dst_tenv ->
      TypenameHash.iter (fun pname cfg -> TypenameHash.replace dst_tenv pname cfg) src_tenv ;
      FileLocal dst_tenv
  | Global, FileLocal _ | FileLocal _, Global ->
      L.die InternalError "Cannot merge Global tenv with FileLocal tenv"


let load_statement =
  ResultsDatabase.register_statement
    "SELECT type_environment FROM source_files WHERE source_file = :k"


(** Serializer for type environments *)
let tenv_serializer : t Serialization.serializer =
  Serialization.create_serializer Serialization.Key.tenv


let global_tenv : t option ref = ref None

let global_tenv_path = Config.(results_dir ^/ global_tenv_filename) |> DB.filename_from_string

let load_global () : t option =
  if is_none !global_tenv then
    global_tenv := Serialization.read_from_file tenv_serializer global_tenv_path ;
  !global_tenv


let load source =
  ResultsDatabase.with_registered_statement load_statement ~f:(fun db load_stmt ->
      SourceFile.SQLite.serialize source
      |> Sqlite3.bind load_stmt 1
      |> SqliteUtils.check_result_code db ~log:"load bind source file" ;
      SqliteUtils.result_single_column_option ~finalize:false ~log:"Tenv.load" db load_stmt
      |> Option.bind ~f:(fun x ->
             SQLite.deserialize x
             |> function Global -> load_global () | FileLocal tenv -> Some tenv ) )


let store_debug_file tenv tenv_filename =
  let debug_filename = DB.filename_to_string (DB.filename_add_suffix tenv_filename ".debug") in
  let out_channel = Out_channel.create debug_filename in
  let fmt = Format.formatter_of_out_channel out_channel in
  pp fmt tenv ; Out_channel.close out_channel


let store_debug_file_for_source source_file tenv =
  let tenv_filename_of_source_file =
    DB.source_dir_get_internal_file (DB.source_dir_from_source_file source_file) ".tenv"
  in
  store_debug_file tenv tenv_filename_of_source_file


let store_to_filename tenv tenv_filename =
  Serialization.write_to_file tenv_serializer tenv_filename ~data:tenv ;
  if Config.debug_mode then store_debug_file tenv tenv_filename


let store_global tenv =
  (* update in-memory global tenv for later uses by this process, e.g. in single-core mode the
     frontend and backend run in the same process *)
  global_tenv := Some tenv ;
  store_to_filename tenv global_tenv_path
