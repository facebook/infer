(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging

(** Module for Type Environments. *)

(** Hash tables on type names. *)
module TypenameHash = Hashtbl.Make (Typ.Name)

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

let merge ~src ~dst = TypenameHash.iter (fun pname cfg -> TypenameHash.replace dst pname cfg) src

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


(** Use a prime for the initial hashtable size, since, typically, we expect a lot of types. 
    Prime sizes make hash functions happier across table resizes. *)
let medium_size_prime = 1003

module type HashconsS = sig
  module Hashtable : Caml.Hashtbl.S

  val reset : unit -> unit

  val hashcons : Hashtable.key -> Hashtable.key
end

module Hashcons (H : Caml.Hashtbl.S) : HashconsS with module Hashtable = H = struct
  module Hashtable = H

  let reset, hashcons =
    let tbl : H.key H.t = H.create medium_size_prime in
    ( (fun () -> H.reset tbl)
    , fun x -> match H.find_opt tbl x with Some x' -> x' | None -> H.add tbl x x ; x )
end

module HashconsList (T : Caml.Hashtbl.HashedType) (H : HashconsS with type Hashtable.key = T.t) :
  HashconsS with type Hashtable.key = T.t list = struct
  module Hashtable = Hashtbl.Make (struct
    type t = T.t list

    let hash = Hashtbl.hash

    let equal (x : t) (y : t) =
      let open Polymorphic_compare in
      x = y
  end)

  let reset, hashcons =
    let tbl : T.t list Hashtable.t = Hashtable.create medium_size_prime in
    ( (fun () -> Hashtable.reset tbl)
    , fun x ->
        let x = IList.map_changed x ~equal:phys_equal ~f:H.hashcons in
        match Hashtable.find_opt tbl x with Some x' -> x' | None -> Hashtable.add tbl x x ; x )
end

module PnameHC = Hashcons (Typ.Procname.Hash)
module PnameListHC = HashconsList (Typ.Procname) (PnameHC)
module TnameHC = Hashcons (TypenameHash)
module TnameListHC = HashconsList (Typ.Name) (TnameHC)
module StringHC = Hashcons (Hashtbl.Make (String))
module StringListHC = HashconsList (String) (StringHC)
module FieldnameHC = Hashcons (Hashtbl.Make (Typ.Fieldname))
module TypHC = Hashcons (Hashtbl.Make (Typ))

module AnnotHC = struct
  include Hashcons (Hashtbl.Make (Annot))

  let hashcons ({class_name; parameters} : Annot.t) : Annot.t =
    {class_name= StringHC.hashcons class_name; parameters= StringListHC.hashcons parameters}
end

module AnnotVis = struct
  type t = Annot.t * bool [@@deriving compare]

  let equal = [%compare.equal: t]

  let hash = Hashtbl.hash
end

module AnnotVisHC = struct
  include Hashcons (Hashtbl.Make (AnnotVis))

  let hashcons (annot, visibility) = hashcons (AnnotHC.hashcons annot, visibility)
end

module AnnotItemHC = HashconsList (AnnotVis) (AnnotVisHC)

module Field = struct
  type t = Typ.Fieldname.t * Typ.t * Annot.Item.t [@@deriving compare]

  let equal = [%compare.equal: t]

  let hash = Hashtbl.hash
end

module FieldHC = struct
  include Hashcons (Hashtbl.Make (Field))

  let hashcons (fieldname, typ, annot_item) =
    hashcons (FieldnameHC.hashcons fieldname, TypHC.hashcons typ, AnnotItemHC.hashcons annot_item)
end

module FieldListHC = HashconsList (Field) (FieldHC)

let reset_hashtables () =
  PnameHC.reset () ;
  PnameListHC.reset () ;
  TnameHC.reset () ;
  TnameListHC.reset () ;
  StringHC.reset () ;
  StringListHC.reset () ;
  FieldnameHC.reset () ;
  TypHC.reset () ;
  AnnotHC.reset () ;
  AnnotVisHC.reset () ;
  AnnotItemHC.reset () ;
  FieldHC.reset () ;
  FieldListHC.reset ()


(** Global tenv size is a problem in the genrule capture integration for java. 
    This function tries to improve sharing of values in the tenv, and assumes 
    Java data structures (it's still correct for Clangs, just not necessarily 
    as effective. *)
let canonicalize tenv =
  reset_hashtables () ;
  let result = create () in
  let canonicalize_one tname
      ({fields; statics; supers; methods; exported_objc_methods; annots} : Typ.Struct.t) =
    let tname = TnameHC.hashcons tname in
    let tstruct =
      Typ.Struct.internal_mk_struct ~supers:(TnameListHC.hashcons supers)
        ~fields:(FieldListHC.hashcons fields) ~statics:(FieldListHC.hashcons statics)
        ~methods:(PnameListHC.hashcons methods)
        ~exported_objc_methods:(PnameListHC.hashcons exported_objc_methods)
        ~annots:(AnnotItemHC.hashcons annots) ()
    in
    TypenameHash.add result tname tstruct
  in
  TypenameHash.iter canonicalize_one tenv ;
  reset_hashtables () ;
  result


let store_global tenv =
  (* update in-memory global tenv for later uses by this process, e.g. in single-core mode the
     frontend and backend run in the same process *)
  L.debug Capture Quiet "Tenv.store: global tenv has size %d bytes.@."
    (Obj.(reachable_words (repr tenv)) * (Sys.word_size / 8)) ;
  let tenv = canonicalize tenv in
  L.debug Capture Quiet "Tenv.store: canonicalized tenv has size %d bytes.@."
    (Obj.(reachable_words (repr tenv)) * (Sys.word_size / 8)) ;
  global_tenv := Some tenv ;
  store_to_filename tenv global_tenv_path
