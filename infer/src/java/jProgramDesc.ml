(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack
module L = Logging

let javalib_get_class classpath cn =
  let get_class = Utils.suppress_stderr2 Javalib.get_class in
  try Some (get_class classpath cn) with
  | JBasics.No_class_found _ ->
      (* TODO T28155039 Figure out when and what to log *)
      None
  | (JBasics.Class_structure_error _ | Invalid_argument _ | Failure _) as exn ->
      L.internal_error "ERROR: %s@." (Exn.to_string exn) ;
      None


type callee_status = Translated | Missing of JBasics.class_name * JBasics.method_signature

module Classmap = Caml.Hashtbl.Make (struct
  type t = JBasics.class_name

  let equal = JBasics.cn_equal

  let hash = JBasics.cn_hash
end)

type classmap = JCode.jcode Javalib.interface_or_class Classmap.t

module Sourcemap = Caml.Hashtbl.Make (String)

type sourcemap = JBasics.class_name list Sourcemap.t

(** We store for each classname the location of its declaration. This map is filled during
    JFrontend.compute_source_icfg and then it is used in JTransType.get_class_struct_typ before we
    lose access to program. At the end, the information seats in each Struct.t (stored in Tenv.t) *)
type java_location_map = Location.t JBasics.ClassMap.t

type t =
  { classpath_channel: Javalib.class_path
  ; classmap: classmap
  ; sourcemap: sourcemap
  ; mutable java_location_map: java_location_map
  ; callees: callee_status Procname.Hash.t }

let get_classmap program = program.classmap

let get_matching_class_names program source_file =
  try Sourcemap.find program.sourcemap source_file with Caml.Not_found -> []


let set_java_location program cn loc =
  program.java_location_map <- JBasics.ClassMap.add cn loc program.java_location_map


let get_java_location program cn =
  try Some (JBasics.ClassMap.find cn program.java_location_map) with Caml.Not_found -> None


let set_callee_translated program pname = Procname.Hash.replace program.callees pname Translated

let add_missing_callee program pname cn ms =
  if not (Procname.Hash.mem program.callees pname) then
    Procname.Hash.add program.callees pname (Missing (cn, ms))


let iter_missing_callees program ~f =
  let select proc_name = function Translated -> () | Missing (cn, ms) -> f proc_name cn ms in
  Procname.Hash.iter select program.callees


let lookup_node cn program =
  try Some (Classmap.find program.classmap cn)
  with Caml.Not_found -> (
    match javalib_get_class program.classpath_channel cn with
    | Some jclass ->
        Classmap.add program.classmap cn jclass ;
        Some jclass
    | None ->
        None )


let create_sourcemap classes program =
  let add_class cn jclass = Classmap.add program.classmap cn jclass in
  let add_source cn jclass =
    match Javalib.get_sourcefile jclass with
    | None ->
        ()
    | Some source_file ->
        let class_names =
          try Sourcemap.find program.sourcemap source_file with Caml.Not_found -> []
        in
        Sourcemap.replace program.sourcemap source_file (cn :: class_names)
  in
  let load cn =
    (* [prefix] must be a fresh class name *)
    let prefix = JBasics.cn_name cn ^ Config.java_lambda_marker_infix_generated_by_javalib in
    match javalib_get_class program.classpath_channel cn with
    | None ->
        ()
    | Some jclass -> (
      (* we rewrite each class to replace invokedynamic (closure construction)
         with equivalent old-style Java code that implements a suitable Java interface *)
      match Javalib.remove_invokedynamics jclass ~prefix with
      | _, new_classes when JBasics.ClassMap.is_empty new_classes ->
          (* no need to add the unmodified classes to the cache at this level since most of
             them will not be needed for the translation *)
          add_source cn jclass
      | rewritten_jclass, new_classes ->
          add_source cn rewritten_jclass ;
          JBasics.ClassMap.iter add_source new_classes ;
          (* the rewrite will generate new classes and we add them to the program *)
          add_class cn rewritten_jclass ;
          JBasics.ClassMap.iter add_class new_classes )
  in
  JBasics.ClassSet.iter load classes


let load JClasspath.{classpath_channel; classes} =
  L.(debug Capture Medium) "loading classes... %!" ;
  let program =
    { classpath_channel
    ; classmap= Classmap.create 128
    ; sourcemap= Sourcemap.create 32
    ; java_location_map= JBasics.ClassMap.empty
    ; callees= Procname.Hash.create 128 }
  in
  create_sourcemap classes program ;
  L.(debug Capture Medium) "done@." ;
  program
