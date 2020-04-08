(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

(** Module for Type Environments. *)

module TypenameHash = Caml.Hashtbl.Make (Typ.Name)
(** Hash tables on type names. *)

module TypenameHashNormalizer = MaximumSharing.ForHashtbl (TypenameHash)

(** Type for type environment. *)
type t = Struct.t TypenameHash.t

let pp fmt (tenv : t) =
  TypenameHash.iter
    (fun name typ ->
      Format.fprintf fmt "@[<6>NAME: %s@]@," (Typ.Name.to_string name) ;
      Format.fprintf fmt "@[<6>TYPE: %a@]@," (Struct.pp Pp.text name) typ )
    tenv


(** Create a new type environment. *)
let create () = TypenameHash.create 1000

(** Construct a struct type in a type environment *)
let mk_struct tenv ?default ?fields ?statics ?methods ?exported_objc_methods ?supers ?annots
    ?java_class_info ?dummy name =
  let struct_typ =
    Struct.internal_mk_struct ?default ?fields ?statics ?methods ?exported_objc_methods ?supers
      ?annots ?java_class_info ?dummy ()
  in
  TypenameHash.replace tenv name struct_typ ;
  struct_typ


(** Look up a name in the global type environment. *)
let lookup tenv name : Struct.t option =
  try Some (TypenameHash.find tenv name)
  with Caml.Not_found -> (
    (* ToDo: remove the following additional lookups once C/C++ interop is resolved *)
    match (name : Typ.Name.t) with
    | CStruct m -> (
      try Some (TypenameHash.find tenv (CppClass (m, NoTemplate))) with Caml.Not_found -> None )
    | CppClass (m, NoTemplate) -> (
      try Some (TypenameHash.find tenv (CStruct m)) with Caml.Not_found -> None )
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
  TypenameHash.iter
    (fun pname cfg ->
      if (not (Struct.is_dummy cfg)) || not (TypenameHash.mem dst pname) then
        TypenameHash.replace dst pname cfg )
    src


let merge_per_file ~src ~dst =
  match (src, dst) with
  | Global, Global ->
      Global
  | FileLocal src_tenv, FileLocal dst_tenv ->
      merge ~src:src_tenv ~dst:dst_tenv ; FileLocal dst_tenv
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
  pp fmt tenv ; Out_channel.close out_channel


let store_debug_file_for_source source_file tenv =
  let tenv_filename_of_source_file =
    DB.source_dir_get_internal_file (DB.source_dir_from_source_file source_file) ".tenv"
  in
  store_debug_file tenv tenv_filename_of_source_file


let store_to_filename tenv tenv_filename =
  Serialization.write_to_file tenv_serializer tenv_filename ~data:tenv ;
  if Config.debug_mode then store_debug_file tenv tenv_filename


let init_inheritances tenv =
  let sub_to_supers =
    TypenameHash.fold (fun sub {Struct.supers} acc -> (sub, supers) :: acc) tenv []
  in
  List.iter sub_to_supers ~f:(fun (sub, supers) ->
      List.iter supers ~f:(fun super ->
          (* Ignore the super class of java.lang.Object since its sub-classes are too many, which
             harms the analysis precision. *)
          if not (Typ.Name.equal super Typ.Name.Java.java_lang_object) then
            Option.iter (lookup tenv super) ~f:(fun super_struct ->
                Struct.add_sub sub super_struct |> TypenameHash.replace tenv super ) ) )


let store_global tenv =
  (* update in-memory global tenv for later uses by this process, e.g. in single-core mode the
     frontend and backend run in the same process *)
  L.debug Capture Quiet "Tenv.store: global tenv has size %d bytes.@."
    (Obj.(reachable_words (repr tenv)) * (Sys.word_size / 8)) ;
  let tenv = TypenameHashNormalizer.normalize tenv in
  L.debug Capture Quiet "Tenv.store: canonicalized tenv has size %d bytes.@."
    (Obj.(reachable_words (repr tenv)) * (Sys.word_size / 8)) ;
  init_inheritances tenv ;
  global_tenv := Some tenv ;
  store_to_filename tenv global_tenv_path


let get_summary_formals tenv ~get_summary ~get_formals =
  let get pname =
    match (get_summary pname, get_formals pname) with
    | Some summary, Some formals ->
        `Found (summary, formals)
    | _, _ ->
        `NotFound
  in
  let found_from_subclass pname = function
    | `Found (summary, formals) ->
        `FoundFromSubclass (pname, summary, formals)
    | v ->
        v
  in
  let rec get_summary_formals_aux pname =
    match get pname with
    | `Found _ as v ->
        v
    | `NotFound -> (
      match Procname.get_class_type_name pname with
      | None ->
          `NotFound
      | Some class_name when String.is_prefix (Typ.Name.name class_name) ~prefix:"java." ->
          (* Note: We do not search sub-classes of `java.` because the super-classes are too
             general.  Selecting one arbitrary sub-class of them does not help in making preciser
             analysis results. *)
          `NotFound
      | Some class_name -> (
        match lookup tenv class_name with
        | Some {Struct.java_class_info= Some info; subs}
          when Struct.equal_java_class_kind info.kind Interface
               && Int.equal (Typ.Name.Set.cardinal subs) 1 ->
            let unique_sub = Typ.Name.Set.choose subs in
            Logging.d_printfln_escaped "Found a unique sub-class %a" Typ.Name.pp unique_sub ;
            let sub_pname = Procname.replace_class pname unique_sub in
            get_summary_formals_aux sub_pname |> found_from_subclass sub_pname
        | Some {Struct.java_class_info= Some info; subs}
          when Struct.equal_java_class_kind info.kind AbstractClass ->
            Option.value_map (Typ.Name.Set.min_elt_opt subs) ~default:`NotFound ~f:(fun sub ->
                Logging.d_printfln_escaped "Found an arbitrary sub-class %a" Typ.Name.pp sub ;
                let sub_pname = Procname.replace_class pname sub in
                get sub_pname |> found_from_subclass sub_pname )
        | _ ->
            `NotFound ) )
  in
  fun pname -> get_summary_formals_aux pname
