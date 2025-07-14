(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open Option.Monad_infix

(** Module for Type Environments. *)

module TypenameHash = Concurrent.MakeHashtbl (Typ.Name.Hash)

type t = Struct.t TypenameHash.t

let pp fmt (tenv : t) =
  TypenameHash.iter (fun name typ -> F.fprintf fmt "%a@\n@," (Struct.pp Pp.text name) typ) tenv


let length tenv = TypenameHash.length tenv

let fold tenv ~init ~f = TypenameHash.fold f tenv init

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
    match TypenameHash.find_opt tenv name with
    | Some _ as some_s ->
        some_s
    | None -> (
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
  let is_hack = Typ.Name.Hack.is_class name in
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
  (* problem is that it gets dummy, missed capture *)
  let missed_capture_types = ref Typ.Name.Set.empty in
  let find ~f =
    find_map_supers ~ignore_require_extends:true tenv name ~f:(fun name str_opt ->
        match str_opt with
        | None | Some {dummy= true} ->
            if Language.curr_language_is Hack then
              missed_capture_types := Typ.Name.Set.add name !missed_capture_types ;
            None
        | Some {Struct.fields} ->
            if List.exists fields ~f then Some name else None )
  in
  let is_fld {Struct.name} = String.equal (Fieldname.get_field_name name) fieldname_str in
  let is_non_abstract_fld ({Struct.annot} as x) = is_fld x && not (Annot.Item.is_abstract annot) in
  let resolved_fld =
    (match find ~f:is_non_abstract_fld with Some _ as fld -> fld | None -> find ~f:is_fld)
    |> Option.map ~f:(fun name -> Fieldname.make name fieldname_str)
  in
  (resolved_fld, !missed_capture_types)


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
  let module Fields = Stdlib.Set.Make (struct
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
      F.fprintf fmt "Global"
  | FileLocal tenv ->
      F.fprintf fmt "FileLocal @[<v>%a@]" pp tenv


module SQLite : SqliteUtils.Data with type t = per_file = struct
  module Serializer = SqliteUtils.MarshalledDataNOTForComparison (struct
    type nonrec t = Struct.t Typ.Name.Hash.t
  end)

  type t = per_file

  let global_string = "global"

  let serialize = function
    | Global ->
        Sqlite3.Data.TEXT global_string
    | FileLocal tenv ->
        TypenameHash.with_hashtable Serializer.serialize tenv


  let deserialize = function
    | Sqlite3.Data.TEXT g when String.equal g global_string ->
        Global
    | blob ->
        FileLocal (Serializer.deserialize blob |> TypenameHash.wrap_hashtable)
end

let merge ~src ~dst =
  let merge_internal typename newer =
    match TypenameHash.find_opt dst typename with
    | None ->
        TypenameHash.replace dst typename newer
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


let store_debug_file tenv tenv_filename =
  let debug_filename = DB.filename_to_string (DB.filename_add_suffix tenv_filename ".debug") in
  let out_channel = Out_channel.create debug_filename in
  let fmt = F.formatter_of_out_channel out_channel in
  pp fmt tenv ;
  Out_channel.close out_channel


let store_debug_file_for_source source_file tenv =
  let tenv_filename_of_source_file =
    DB.source_dir_get_internal_file (DB.source_dir_from_source_file source_file) ".tenv"
  in
  store_debug_file tenv tenv_filename_of_source_file


let write tenv tenv_filename =
  let tenv_path = DB.filename_to_string tenv_filename in
  TypenameHash.with_hashtable
    (fun hashtable ->
      Utils.with_intermediate_temp_file_out ~retry:Sys.win32 tenv_path ~f:(fun outc ->
          Marshal.to_channel outc hashtable [] ) )
    tenv ;
  if Config.debug_mode then store_debug_file tenv tenv_filename ;
  let lstat = DB.filename_to_string tenv_filename |> Unix.lstat in
  let size = lstat.st_size in
  let value = size / 1024 / 1024 in
  let label = "global_tenv_size_mb" in
  L.debug Capture Quiet "Global tenv size %s: %d@\n" label value ;
  StatsLogging.log_count ~label:"global_tenv_size_mb" ~value


module Normalizer = struct
  let normalize tenv =
    let new_tenv = TypenameHash.create (TypenameHash.length tenv) in
    let normalize_mapping name tstruct =
      let name = Typ.Name.hash_normalize name in
      let tstruct = Struct.hash_normalize tstruct in
      TypenameHash.replace new_tenv name tstruct
    in
    TypenameHash.iter normalize_mapping tenv ;
    new_tenv
end

let read filename =
  try
    DB.filename_to_string filename
    |> Utils.with_file_in ~f:Marshal.from_channel
    |> TypenameHash.wrap_hashtable |> Option.some
  with Sys_error _ -> None


module Global : sig
  val read : unit -> t option

  val load : unit -> t option

  val force_load : unit -> t option

  val store : normalize:bool -> t -> unit
end = struct
  let global_tenv : t option Atomic.t = Atomic.make None

  let global_tenv_path = ResultsDir.get_path GlobalTypeEnvironment |> DB.filename_from_string

  let read () = read global_tenv_path

  let global_tenv_mutex = IMutex.create ()

  let set tenv =
    IMutex.critical_section global_tenv_mutex ~f:(fun () -> Atomic.set global_tenv tenv)


  let force_load () =
    let tenv = read () in
    set tenv ;
    tenv


  let load () : t option =
    let tenv = Atomic.get global_tenv in
    if Option.is_some tenv then tenv else force_load ()


  let store ~normalize tenv =
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
    set (Some tenv) ;
    write tenv global_tenv_path
end

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
        Global.load ()
    | Some (FileLocal tenv) ->
        Some tenv


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
    type kind = IsClass | IsTrait of {in_class: Typ.Name.t; is_direct: bool}
    [@@deriving show {with_path= false}]

    type t = {proc_name: (Procname.t[@show.printer Procname.pp_verbose]); kind: kind}
    [@@deriving show {with_path= false}]

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
        | Some in_class ->
            IsTrait {in_class; is_direct= false}
        | None ->
            IsTrait {in_class= class_name; is_direct= true} )
  end

  type t = HackInfo of Hack.t | DefaultInfo of Default.t [@@deriving show {with_path= false}]

  let return ~kind (proc_name : Procname.t) =
    match proc_name with
    | Hack _ ->
        HackInfo (Hack.mk_class ~kind proc_name)
    | Block _ | C _ | CSharp _ | Erlang _ | Java _ | ObjC_Cpp _ | Python _ | Swift _ ->
        DefaultInfo (Default.mk_class proc_name)


  let mk_class proc_name = return ~kind:IsClass proc_name

  let get_kind_from_struct ~last_class_visited class_name {Struct.class_info} =
    match class_info with
    | HackClassInfo kind ->
        Hack.get_kind ~last_class_visited class_name kind
    | NoInfo | CppClassInfo _ | JavaClassInfo _ ->
        Hack.IsClass


  let get_proc_name = function HackInfo {proc_name} | DefaultInfo {proc_name} -> proc_name

  let get_hack_kind = function HackInfo {kind} -> Some kind | _ -> None
end

type unresolved_reason =
  | ClassNameNotFound
  | CurryInfoNotFound
  | MaybeMissingDueToMissedCapture
  | MaybeMissingDueToIncompleteModel
[@@deriving show {with_path= false}]

type unresolved_data = {missed_captures: Typ.Name.Set.t; unresolved_reason: unresolved_reason option}

let mk_unresolved_data ?(missed_captures = Typ.Name.Set.empty) unresolved_reason =
  {missed_captures; unresolved_reason}


type resolution_result = (MethodInfo.t, unresolved_data) Result.t

let is_hack_model source_file =
  let source_file = SourceFile.to_abs_path source_file in
  String.is_suffix source_file ~suffix:Config.default_hack_builtin_models_rel
  || List.exists (Config.hack_builtin_models :: Config.hack_models) ~f:(fun hack_model ->
         String.equal source_file hack_model )


let resolve_method ?(is_virtual = false) ~method_exists tenv class_name proc_name =
  let visited = ref Typ.Name.Set.empty in
  (* For Hack, we need to remember the last class we visited. Once we visit a trait, we are sure
     we will only visit traits from now on *)
  let last_class_visited = ref None in
  let missed_capture_types = ref Typ.Name.Set.empty in
  let visited_hack_model = ref false in
  let rec resolve_name (class_name : Typ.Name.t) =
    if Typ.Name.Set.mem class_name !visited || not (Typ.Name.is_class class_name) then None
    else (
      visited := Typ.Name.Set.add class_name !visited ;
      let struct_opt = lookup tenv class_name in
      (* NOTE: We give an exception on [HH::classname].  The [visited_hack_model] value is to
         provide a hint "the resolved method may be incorrect".  However, [HH::classname] is just an
         alias to string, thus there is no method to be incorrect, even if it is defined in the
         Infer's models. *)
      if not (Typ.Name.Hack.is_HH_classname class_name) then
        Option.iter struct_opt ~f:(fun {Struct.source_file} ->
            Option.iter source_file ~f:(fun source_file ->
                if is_hack_model source_file then visited_hack_model := true ) ) ;
      match struct_opt with
      | None | Some {dummy= true} ->
          if Language.curr_language_is Hack then
            missed_capture_types := Typ.Name.Set.add class_name !missed_capture_types ;
          None
      | Some class_struct when not (Struct.is_not_java_interface class_struct) ->
          None
      | Some ({Struct.methods; supers} as class_struct) ->
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
                L.d_printfln
                  "method belongs to a Trait and is not called directly, adjusting arity +1" ;
                1
            | IsTrait {is_direct= true} ->
                if is_virtual then (
                  (* This happens, eg, in the wrapper methods we generate to deal with optional
                     arguments; in these cases the call to the "base" method with no optional
                     arguments is emitted by hackc as a virtual call *)
                  L.d_printfln
                    "method belongs to a Trait, is called directly but virtually, adjusting arity \
                     +1" ;
                  1 )
                else
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
              | SwiftClass _ ->
                  supers
            in
            List.find_map supers_to_search ~f:resolve_name )
  in
  match resolve_name class_name with
  | _ when not (Typ.Name.Set.is_empty !missed_capture_types) ->
      Error
        { missed_captures= !missed_capture_types
        ; unresolved_reason= Some MaybeMissingDueToMissedCapture }
  | None ->
      let unresolved_reason =
        if !visited_hack_model then Some MaybeMissingDueToIncompleteModel else None
      in
      Error {missed_captures= !missed_capture_types; unresolved_reason}
  | Some method_info ->
      Ok method_info


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
  | Tfun _ | TVar _ ->
      false


let get_hack_direct_used_traits_interfaces tenv class_name =
  Option.value_map (lookup tenv class_name) ~default:[] ~f:(fun {Struct.supers} ->
      List.fold supers ~init:[] ~f:(fun acc name ->
          match (name, lookup tenv name) with
          | Typ.HackClass name, Some str ->
              if Struct.is_hack_trait str then (`Trait, name) :: acc
              else if Struct.is_hack_interface str then (`Interface, name) :: acc
              else acc
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
      L.internal_error "exceeded alias expansion limit (cycle?), not expanding@\n" ;
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
            L.internal_error "empty type alias \"supers\", not expanding@\n" ;
            None
        | x :: _xs ->
            L.internal_error "alias type defined as union, taking first element@\n" ;
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
