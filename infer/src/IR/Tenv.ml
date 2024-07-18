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


let length tenv = TypenameHash.length tenv

let fold tenv ~init ~f = TypenameHash.fold f tenv init

(** Create a new type environment. *)
let create () = TypenameHash.create 1000

(** Construct a struct type in a type environment *)
let mk_struct tenv ?default ?fields ?statics ?methods ?exported_objc_methods ?supers ?objc_protocols
    ?annots ?class_info ?dummy ?source_file name =
  match name with
  | Typ.ObjcBlock _ | Typ.CFunction _ ->
      L.die InternalError "%a is not allowed as a key in the tenv" Typ.Name.pp name
  | _ ->
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


let compare_fields {Struct.name= name1} {Struct.name= name2} = Fieldname.compare name1 name2

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


(** Reorder Hack's supers so that traits come first. *)
let reorder_hack_supers tenv ~ignore_require_extends ({Struct.supers} as str) =
  let supers = List.map supers ~f:(fun super -> (super, lookup tenv super)) in
  let supers =
    if ignore_require_extends && Struct.is_hack_trait str then
      List.filter supers ~f:(fun (_, str) ->
          Option.exists str ~f:(fun str ->
              Struct.is_hack_interface str || Struct.is_hack_trait str ) )
    else supers
  in
  let traits, others =
    List.partition_tf supers ~f:(fun (_, str) -> Option.exists str ~f:Struct.is_hack_trait)
  in
  List.map traits ~f:fst @ List.map others ~f:fst


let fold_supers ?(ignore_require_extends = false) tenv name ~init ~f =
  let is_hack = Typ.Name.is_hack_class name in
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
        | Some ({supers} as str) ->
            let supers =
              if is_hack then reorder_hack_supers tenv ~ignore_require_extends str else supers
            in
            let visited, result = aux supers visited result in
            aux worklist visited result )
  in
  aux [name] Typ.Name.Set.empty init |> snd


let find_map_supers (type f_result) ?ignore_require_extends tenv name
    ~(f : Typ.Name.t -> Struct.t option -> f_result option) =
  let exception FOUND of f_result option in
  try
    fold_supers ?ignore_require_extends tenv name ~init:() ~f:(fun name struct_opt () ->
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
  let is_fld {Struct.name} = String.equal (Fieldname.get_field_name name) fieldname_str in
  find_map_supers ~ignore_require_extends:true tenv name ~f:(fun name str_opt ->
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


let get_fields_trans =
  let module Fields = Caml.Set.Make (struct
    type t = Struct.field

    let compare {Struct.name= x} {Struct.name= y} =
      String.compare (Fieldname.get_field_name x) (Fieldname.get_field_name y)
  end) in
  fun tenv name ->
    fold_supers tenv name ~init:Fields.empty ~f:(fun _ struct_opt acc ->
        Option.fold struct_opt ~init:acc ~f:(fun acc {Struct.fields} ->
            List.fold fields ~init:acc ~f:(fun acc {Struct.name= fieldname; typ; annot} ->
                let fieldname = Fieldname.make name (Fieldname.get_field_name fieldname) in
                let field = Struct.mk_field fieldname typ ~annot in
                Fields.add field acc ) ) )
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

let read_global () = read global_tenv_path

let force_load_global () =
  global_tenv := read_global () ;
  !global_tenv


let load_global () : t option =
  if Option.is_some !global_tenv then !global_tenv else force_load_global ()


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
      let name = Typ.Name.hash_normalize name in
      let tstruct = Struct.hash_normalize tstruct in
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

    let mk_class ~kind proc_name = {proc_name; kind}

    (* [hackc] introduces an extra method argument in traits, to account for [self].
       Because of this, the arity of the procname called might not match the arity of the procname in
       the trait declaration.

       This function is to compute the correct arity offset we should apply. *)
    let get_kind ~last_class_visited class_name (kind : Struct.hack_class_kind) =
      match kind with
      | Class | AbstractClass | Interface | Alias ->
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
    | Procname.ObjC_Cpp _
    | Procname.Python _ ->
        DefaultInfo (Default.mk_class proc_name)


  let mk_class proc_name = return ~kind:IsClass proc_name

  let get_kind_from_struct ~last_class_visited class_name {Struct.class_info} =
    match class_info with
    | HackClassInfo kind ->
        Hack.get_kind ~last_class_visited class_name kind
    | NoInfo | CppClassInfo _ | JavaClassInfo _ ->
        Hack.IsClass


  let get_procname = function HackInfo {proc_name} | DefaultInfo {proc_name} -> proc_name

  let get_hack_kind = function HackInfo {kind} -> Some kind | _ -> None
end

let is_captured tenv type_name =
  lookup tenv type_name |> Option.exists ~f:(fun (s : Struct.t) -> not s.dummy)


let resolve_method ~method_exists tenv class_name proc_name =
  let visited = ref Typ.Name.Set.empty in
  (* For Hack, we need to remember the last class we visited. Once we visit a trait, we are sure
     we will only visit traits from now on *)
  let last_class_visited = ref None in
  let missed_capture_types = ref Typ.Name.Set.empty in
  let rec resolve_name (class_name : Typ.Name.t) =
    Option.bind (lookup tenv class_name) ~f:(fun ({Struct.methods; supers} as class_struct) ->
        if
          (not (Typ.Name.is_class class_name))
          || (not (Struct.is_not_java_interface class_struct))
          || Typ.Name.Set.mem class_name !visited
        then None
        else (
          visited := Typ.Name.Set.add class_name !visited ;
          if Language.curr_language_is Hack && not (is_captured tenv class_name) then
            (* we do not need to record class names for which [lookup tenv class_name == None] because
               we assume that all direct super classes of a captured class are declared in Tenv
               (even if not necessarily properly captured) *)
            missed_capture_types := Typ.Name.Set.add class_name !missed_capture_types ;
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
          if method_exists right_proc_name methods then
            Some (MethodInfo.return ~kind right_proc_name)
          else
            let supers_to_search =
              match (class_name : Typ.Name.t) with
              | ErlangType _ ->
                  L.die InternalError "attempting to call a method on an Erlang value"
              | CStruct _ | CUnion _ | CppClass _ ->
                  (* multiple inheritance possible, search all supers *)
                  supers
              | HackClass _ ->
                  (* super-classes, super-interfaces, and traits are modelled via multiple inheritance *)
                  reorder_hack_supers tenv ~ignore_require_extends:false class_struct
              | JavaClass _ ->
                  (* multiple inheritance not possible, but cannot distinguish interfaces from typename so search all *)
                  supers
              | CSharpClass _ ->
                  (* multiple inheritance not possible, but cannot distinguish interfaces from typename so search all *)
                  supers
              | ObjcClass _ ->
                  (* multiple inheritance impossible, but recursive calls will throw away protocols *)
                  supers
              | ObjcProtocol _ | ObjcBlock _ | CFunction _ ->
                  []
              | PythonClass _ ->
                  (* We currently only support single inheritance for Python so this is straightforward *)
                  supers
            in
            List.find_map supers_to_search ~f:resolve_name ) )
  in
  let opt_info = resolve_name class_name in
  (opt_info, !missed_capture_types)


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


let rec is_trivially_copyable tenv {Typ.desc} =
  match desc with
  | Tstruct name -> (
    match lookup tenv name with
    | Some {class_info= CppClassInfo {is_trivially_copyable}} ->
        is_trivially_copyable
    | _ ->
        false )
  | Tint _ | Tfloat _ | Tvoid | Tptr _ ->
      true
  | Tarray {elt} ->
      is_trivially_copyable tenv elt
  | Tfun | TVar _ ->
      false


let get_hack_direct_used_traits tenv class_name =
  Option.value_map (lookup tenv class_name) ~default:[] ~f:(fun {Struct.supers} ->
      List.fold supers ~init:[] ~f:(fun acc name ->
          match (name, lookup tenv name) with
          | Typ.HackClass name, Some str ->
              if Struct.is_hack_trait str then name :: acc else acc
          | _, _ ->
              acc ) )


let alias_expansion_limit = 100

(* recursively expand type alias, this returns None if not in Tenv at all, which may be wrong
   TODO: track when an alias is nullable, propagate that disjunctively as we unfold definitions
   and return that as part of the result of expansion
*)
let expand_hack_alias tenv tname =
  let rec _expand_hack_alias tname n =
    if Int.(n = 0) then (
      L.internal_error "exceeded alias expansion limit (cycle?), not expanding" ;
      None )
    else
      match lookup tenv tname with
      | None ->
          None
      | Some {class_info= HackClassInfo Alias; supers= [definition_name]} ->
          _expand_hack_alias definition_name (n - 1)
      | Some {class_info= HackClassInfo Alias; supers= ss} -> (
        match ss with
        | [] ->
            L.internal_error "empty type alias \"supers\", not expanding" ;
            None
        | x :: _xs ->
            L.internal_error "alias type defined as union, taking first element" ;
            Some x )
      | _ ->
          Some tname
  in
  _expand_hack_alias tname alias_expansion_limit


(* This one works on Typ.t rather than Typ.name and by default leaves input alone
   It also just preserves the quals 'cos I assume there's no reason to try to be
   more clever with them
*)
let expand_hack_alias_in_typ tenv typ =
  match typ with
  | {Typ.desc= Tstruct (HackClass hcn); quals} -> (
    match expand_hack_alias tenv (HackClass hcn) with
    | None ->
        typ (* leave it alone here ? *)
    | Some tname ->
        {Typ.desc= Tstruct tname; quals} )
  | _ ->
      typ
