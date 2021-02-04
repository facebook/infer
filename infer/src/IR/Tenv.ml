(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

(** Module for Type Environments. *)

(** Hash tables on type names. *)
module TypenameHash = Caml.Hashtbl.Make (Typ.Name)

(** Type for type environment. *)
type t = Struct.t TypenameHash.t

let pp fmt (tenv : t) =
  TypenameHash.iter (fun name typ -> Format.fprintf fmt "%a@," (Struct.pp Pp.text name) typ) tenv


(** Create a new type environment. *)
let create () = TypenameHash.create 1000

(** Construct a struct type in a type environment *)
let mk_struct tenv ?default ?fields ?statics ?methods ?exported_objc_methods ?supers ?objc_protocols
    ?annots ?java_class_info ?dummy name =
  let struct_typ =
    Struct.internal_mk_struct ?default ?fields ?statics ?methods ?exported_objc_methods ?supers
      ?objc_protocols ?annots ?java_class_info ?dummy ()
  in
  TypenameHash.replace tenv name struct_typ ;
  struct_typ


(** Look up a name in the global type environment. *)
let lookup tenv name : Struct.t option =
  try Some (TypenameHash.find tenv name)
  with Caml.Not_found -> (
    (* ToDo: remove the following additional lookups once C/C++ interop is resolved *)
    match (name : Typ.Name.t) with
    | CStruct m ->
        TypenameHash.find_opt tenv
          (CppClass {name= m; template_spec_info= NoTemplate; is_union= false})
    | CppClass {name= m; template_spec_info= NoTemplate} ->
        TypenameHash.find_opt tenv (CStruct m)
    | _ ->
        None )


let compare_fields (name1, _, _) (name2, _, _) = Fieldname.compare name1 name2

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
  module Serializer = SqliteUtils.MarshalledDataNOTForComparison (struct
    type nonrec t = t
  end)

  type t = per_file

  let global_string = "global"

  let serialize = function
    | Global ->
        Sqlite3.Data.TEXT global_string
    | FileLocal tenv ->
        Serializer.serialize tenv


  let deserialize = function
    | Sqlite3.Data.TEXT g when String.equal g global_string ->
        Global
    | blob ->
        FileLocal (Serializer.deserialize blob)
end

let merge ~src ~dst =
  let merge_internal typename newer =
    match TypenameHash.find_opt dst typename with
    | None ->
        TypenameHash.add dst typename newer
    | Some current ->
        let merged_struct = Struct.merge typename ~newer ~current in
        TypenameHash.replace dst typename merged_struct
  in
  TypenameHash.iter merge_internal src


let merge_per_file ~src ~dst =
  match (src, dst) with
  | Global, Global ->
      Global
  | FileLocal src_tenv, FileLocal dst_tenv ->
      merge ~src:src_tenv ~dst:dst_tenv ;
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

let global_tenv_path = ResultsDir.get_path JavaGlobalTypeEnvironment |> DB.filename_from_string

let read path = Serialization.read_from_file tenv_serializer path

let load_global () : t option =
  if is_none !global_tenv then global_tenv := read global_tenv_path ;
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
  pp fmt tenv ;
  Out_channel.close out_channel


let store_debug_file_for_source source_file tenv =
  let tenv_filename_of_source_file =
    DB.source_dir_get_internal_file (DB.source_dir_from_source_file source_file) ".tenv"
  in
  store_debug_file tenv tenv_filename_of_source_file


let store_to_filename tenv tenv_filename =
  Serialization.write_to_file tenv_serializer tenv_filename ~data:tenv ;
  if Config.debug_mode then store_debug_file tenv tenv_filename


module Normalizer = struct
  let normalize tenv =
    let new_tenv = TypenameHash.create (TypenameHash.length tenv) in
    let normalize_mapping name tstruct =
      let name = Typ.Name.Normalizer.normalize name in
      let tstruct = Struct.Normalizer.normalize tstruct in
      TypenameHash.add new_tenv name tstruct
    in
    TypenameHash.iter normalize_mapping tenv ;
    new_tenv


  let reset () =
    Typ.Normalizer.reset () ;
    Typ.Name.Normalizer.reset () ;
    Struct.Normalizer.reset () ;
    Fieldname.Normalizer.reset () ;
    Procname.Normalizer.reset () ;
    SourceFile.Normalizer.reset () ;
    Location.Normalizer.reset () ;
    Annot.Item.Normalizer.reset () ;
    JavaClassName.Normalizer.reset () ;
    HashNormalizer.StringNormalizer.reset ()
end

let store_global tenv =
  (* update in-memory global tenv for later uses by this process, e.g. in single-core mode the
     frontend and backend run in the same process *)
  if Config.debug_level_capture > 0 then
    L.debug Capture Quiet "Tenv.store: global tenv has size %d bytes.@."
      (Obj.(reachable_words (repr tenv)) * (Sys.word_size / 8)) ;
  let tenv = Normalizer.normalize tenv in
  Normalizer.reset () ;
  if Config.debug_level_capture > 0 then
    L.debug Capture Quiet "Tenv.store: canonicalized tenv has size %d bytes.@."
      (Obj.(reachable_words (repr tenv)) * (Sys.word_size / 8)) ;
  global_tenv := Some tenv ;
  store_to_filename tenv global_tenv_path


let resolve_method ~method_exists tenv class_name proc_name =
  let visited = ref Typ.Name.Set.empty in
  let rec resolve_name_struct (class_name : Typ.Name.t) (class_struct : Struct.t) =
    if
      (not (Typ.Name.is_class class_name))
      || (not (Struct.is_not_java_interface class_struct))
      || Typ.Name.Set.mem class_name !visited
    then None
    else (
      visited := Typ.Name.Set.add class_name !visited ;
      let right_proc_name = Procname.replace_class proc_name class_name in
      if method_exists right_proc_name class_struct.methods then Some right_proc_name
      else
        let supers_to_search =
          match (class_name : Typ.Name.t) with
          | CStruct _ | CUnion _ | CppClass _ ->
              (* multiple inheritance possible, search all supers *)
              class_struct.supers
          | JavaClass _ ->
              (* multiple inheritance not possible, but cannot distinguish interfaces from typename so search all *)
              class_struct.supers
          | CSharpClass _ ->
              (* multiple inheritance not possible, but cannot distinguish interfaces from typename so search all *)
              class_struct.supers
          | ObjcClass _ ->
              (* multiple inheritance impossible, but recursive calls will throw away protocols *)
              class_struct.supers
          | ObjcProtocol _ ->
              []
        in
        List.find_map supers_to_search ~f:resolve_name )
  and resolve_name class_name =
    lookup tenv class_name |> Option.bind ~f:(resolve_name_struct class_name)
  in
  resolve_name class_name
