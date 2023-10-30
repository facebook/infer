(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
open Option.Monad_infix

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
    ?annots ?class_info ?dummy ?source_file name =
  let struct_typ =
    Struct.internal_mk_struct ?default ?fields ?statics ?methods ?exported_objc_methods ?supers
      ?objc_protocols ?annots ?class_info ?dummy ?source_file name
  in
  TypenameHash.replace tenv name struct_typ ;
  struct_typ


(** Look up a name in the given type environment. *)
let lookup tenv name : Struct.t option =
  let result =
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
  in
  (* Record Tenv lookups during analysis to facilitate conservative incremental invalidation *)
  Option.iter (result >>= Struct.get_source_file) ~f:Dependencies.record_srcfile_dep ;
  result


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


let fold_supers tenv name ~init ~f =
  let rec aux worklist visited result =
    match worklist with
    | [] ->
        (visited, result)
    | name :: worklist when Typ.Name.Set.mem name visited ->
        aux worklist visited result
    | name :: worklist -> (
        let visited = Typ.Name.Set.add name visited in
        let struct_opt = lookup tenv name in
        let result = f name struct_opt result in
        match struct_opt with
        | None ->
            aux worklist visited result
        | Some {supers} ->
            let visited, result = aux supers visited result in
            aux worklist visited result )
  in
  aux [name] Typ.Name.Set.empty init |> snd


let find_map_supers (type f_result) tenv name ~(f : Typ.Name.t -> Struct.t option -> f_result option)
    =
  let exception FOUND of f_result option in
  try
    fold_supers tenv name ~init:() ~f:(fun name struct_opt () ->
        match f name struct_opt with None -> () | Some _ as result -> raise (FOUND result) ) ;
    None
  with FOUND result -> result


let resolve_field_info tenv name fieldname =
  let field_string = Fieldname.get_field_name fieldname in
  find_map_supers tenv name ~f:(fun typ_name _ ->
      let fieldname = Fieldname.make typ_name field_string in
      let typ = Typ.mk_struct typ_name in
      Struct.get_field_info ~lookup:(lookup tenv) fieldname typ )


let resolve_fieldname tenv name fieldname_str =
  let is_fld (fieldname, _, _) = String.equal (Fieldname.get_field_name fieldname) fieldname_str in
  find_map_supers tenv name ~f:(fun name str_opt ->
      Option.bind str_opt ~f:(fun {Struct.fields} ->
          if List.exists fields ~f:is_fld then Some name else None ) )
  |> Option.map ~f:(fun name -> Fieldname.make name fieldname_str)


let mem_supers tenv name ~f =
  find_map_supers tenv name ~f:(fun name struct_opt -> if f name struct_opt then Some () else None)
  |> Option.is_some


let get_parent tenv name =
  (* We have to be careful since a class name is present in its [supers] list, and this list mixed
     traits and class *)
  let f parent struct_opt =
    if Typ.Name.equal name parent then None
    else Option.bind struct_opt ~f:(fun info -> Option.some_if (Struct.is_hack_class info) parent)
  in
  find_map_supers tenv ~f name


let implements_remodel_class tenv name =
  Option.exists Typ.Name.Objc.remodel_class ~f:(fun remodel_class ->
      mem_supers tenv name ~f:(fun name _ -> Typ.Name.equal name remodel_class) )


let get_fields_trans =
  let module Fields = Caml.Set.Make (struct
    type t = Struct.field

    let compare (x, _, _) (y, _, _) =
      String.compare (Fieldname.get_field_name x) (Fieldname.get_field_name y)
  end) in
  fun tenv name ->
    fold_supers tenv name ~init:Fields.empty ~f:(fun _ struct_opt acc ->
        Option.fold struct_opt ~init:acc ~f:(fun acc {Struct.fields} ->
            List.fold fields ~init:acc ~f:(fun acc (fieldname, typ, annot) ->
                let fieldname = Fieldname.make name (Fieldname.get_field_name fieldname) in
                Fields.add (fieldname, typ, annot) acc ) ) )
    |> Fields.elements


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
        if not (phys_equal merged_struct current) then
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


(** Serializer for type environments *)
let tenv_serializer : t Serialization.serializer =
  Serialization.create_serializer Serialization.Key.tenv


let global_tenv : t option ref = ref None

let global_tenv_path = ResultsDir.get_path GlobalTypeEnvironment |> DB.filename_from_string

let read path = Serialization.read_from_file tenv_serializer path

let load_global () : t option =
  if is_none !global_tenv then global_tenv := read global_tenv_path ;
  !global_tenv


let load =
  let load_statement =
    Database.register_statement CaptureDatabase
      "SELECT type_environment FROM source_files WHERE source_file = :k"
  in
  fun source ->
    let res_opt =
      Database.with_registered_statement load_statement ~f:(fun db load_stmt ->
          SourceFile.SQLite.serialize source
          |> Sqlite3.bind load_stmt 1
          |> SqliteUtils.check_result_code db ~log:"load bind source file" ;
          SqliteUtils.result_single_column_option ~finalize:false ~log:"Tenv.load" db load_stmt
          >>| SQLite.deserialize )
    in
    match res_opt with
    | None ->
        MissingDependencies.record_sourcefile source ;
        None
    | Some Global ->
        load_global ()
    | Some (FileLocal tenv) ->
        Some tenv


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


let write tenv tenv_filename =
  Serialization.write_to_file tenv_serializer tenv_filename ~data:tenv ;
  if Config.debug_mode then store_debug_file tenv tenv_filename ;
  let lstat = DB.filename_to_string tenv_filename |> Unix.lstat in
  let size = Int64.to_int lstat.st_size |> Option.value_exn in
  let value = size / 1024 / 1024 in
  let label = "global_tenv_size_mb" in
  L.debug Capture Quiet "Global tenv size %s: %d@\n" label value ;
  ScubaLogging.log_count ~label:"global_tenv_size_mb" ~value


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
end

let store_global ~normalize tenv =
  (* update in-memory global tenv for later uses by this process, e.g. in single-core mode the
     frontend and backend run in the same process *)
  if Config.debug_level_capture > 0 then
    L.debug Capture Quiet "Tenv.store: global tenv has size %d bytes.@."
      (Obj.(reachable_words (repr tenv)) * (Sys.word_size_in_bits / 8)) ;
  (* TODO(arr): normalization sometimes doesn't terminate for Hack. This needs to be investigated
     and fixed. For now we explicitly disable normalization in Hack capture. *)
  let tenv = if normalize then Normalizer.normalize tenv else tenv in
  HashNormalizer.reset_all_normalizers () ;
  if Config.debug_level_capture > 0 then
    L.debug Capture Quiet "Tenv.store: canonicalized tenv has size %d bytes.@."
      (Obj.(reachable_words (repr tenv)) * (Sys.word_size_in_bits / 8)) ;
  global_tenv := Some tenv ;
  write tenv global_tenv_path


let normalize = function
  | Global ->
      Global
  | FileLocal tenv ->
      let new_tenv = Normalizer.normalize tenv in
      HashNormalizer.reset_all_normalizers () ;
      FileLocal new_tenv


module MethodInfo = struct
  module Default = struct
    type t = {proc_name: Procname.t} [@@deriving show {with_path= false}]

    let mk_class proc_name = {proc_name}
  end

  module Hack = struct
    type kind = IsClass | IsTrait of {used: Typ.Name.t; is_direct: bool}
    [@@deriving show {with_path= false}]

    type t = {proc_name: Procname.t; kind: kind} [@@deriving show {with_path= false}]

    let mk_class ~kind proc_name =
      (* The Hack's init methods are addressed as normal class methods. *)
      let kind = if Procname.is_hack_init proc_name then IsClass else kind in
      {proc_name; kind}


    (* [hackc] introduces an extra method argument in traits, to account for [self].
       Because of this, the arity of the procname called might not match the arity of the procname in
       the trait declaration.

       This function is to compute the correct arity offset we should apply. *)
    let get_kind ~last_class_visited class_name (kind : Struct.hack_class_kind) =
      match kind with
      | Class ->
          IsClass
      | Trait -> (
        match last_class_visited with
        | Some used ->
            IsTrait {used; is_direct= false}
        | None ->
            IsTrait {used= class_name; is_direct= true} )
  end

  type t = HackInfo of Hack.t | DefaultInfo of Default.t [@@deriving show {with_path= false}]

  let return ~kind proc_name =
    match proc_name with
    | Procname.Hack _ ->
        HackInfo (Hack.mk_class ~kind proc_name)
    | Procname.Block _
    | Procname.C _
    | Procname.CSharp _
    | Procname.Erlang _
    | Procname.Java _
    | Procname.Linters_dummy_method
    | Procname.ObjC_Cpp _
    | Procname.Python _
    | Procname.WithFunctionParameters _ ->
        DefaultInfo (Default.mk_class proc_name)


  let mk_class proc_name = return ~kind:IsClass proc_name

  let get_kind_from_struct ~last_class_visited class_name {Struct.class_info} =
    match class_info with
    | HackClassInfo kind ->
        Hack.get_kind ~last_class_visited class_name kind
    | NoInfo | JavaClassInfo _ ->
        Hack.IsClass


  let get_procname = function HackInfo {proc_name} | DefaultInfo {proc_name} -> proc_name

  let get_hack_kind = function HackInfo {kind} -> Some kind | _ -> None
end

let resolve_method ~method_exists tenv class_name proc_name =
  let visited = ref Typ.Name.Set.empty in
  (* For Hack, we need to remember the last class we visited. Once we visit a trait, we are sure
     we will only visit traits from now on *)
  let last_class_visited = ref None in
  let rec resolve_name_struct (class_name : Typ.Name.t) (class_struct : Struct.t) =
    if
      (not (Typ.Name.is_class class_name))
      || (not (Struct.is_not_java_interface class_struct))
      || Typ.Name.Set.mem class_name !visited
    then None
    else (
      visited := Typ.Name.Set.add class_name !visited ;
      let kind =
        MethodInfo.get_kind_from_struct ~last_class_visited:!last_class_visited class_name
          class_struct
      in
      let arity_incr =
        match kind with
        | IsClass ->
            last_class_visited := Some class_name ;
            0
        | IsTrait {is_direct= false} ->
            1
        | IsTrait {is_direct= true} ->
            (* We do not need to increase the arity when the trait method is called directly, i.e.
               [T::foo], since the [proc_name] has the increased arity already. *)
            0
      in
      let right_proc_name = Procname.replace_class ~arity_incr proc_name class_name in
      if method_exists right_proc_name class_struct.methods then
        Some (MethodInfo.return ~kind right_proc_name)
      else
        let supers_to_search =
          match (class_name : Typ.Name.t) with
          | ErlangType _ ->
              L.die InternalError "attempting to call a method on an Erlang value"
          | CStruct _ | CUnion _ | CppClass _ ->
              (* multiple inheritance possible, search all supers *)
              class_struct.supers
          | HackClass _ ->
              (* super-classes, super-interfaces, and traits are modelled via multiple inheritance *)
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
          | PythonClass _ ->
              (* We currently only support single inheritance for Python so this is straightforward *)
              class_struct.supers
        in
        List.find_map supers_to_search ~f:resolve_name )
  and resolve_name class_name = lookup tenv class_name >>= resolve_name_struct class_name in
  resolve_name class_name


let find_cpp_destructor tenv class_name =
  let open IOption.Let_syntax in
  let* struct_ = lookup tenv class_name in
  List.find struct_.Struct.methods ~f:(function
    | Procname.ObjC_Cpp f ->
        Procname.ObjC_Cpp.(is_destructor f && not (is_inner_destructor f))
    | _ ->
        false )


let find_cpp_constructor tenv class_name =
  match lookup tenv class_name with
  | Some struct_ ->
      List.filter struct_.Struct.methods ~f:(function
        | Procname.ObjC_Cpp {kind= CPPConstructor _} ->
            true
        | _ ->
            false )
  | None ->
      []
