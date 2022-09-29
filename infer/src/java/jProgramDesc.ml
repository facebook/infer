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

let javalib_get_class = Utils.suppress_stderr2 Javalib.get_class

type callee_status = Translated | Missing of JBasics.class_name * JBasics.method_signature

module Classmap = Caml.Hashtbl.Make (struct
  type t = JBasics.class_name

  let equal = JBasics.cn_equal

  let hash = JBasics.cn_hash
end)

type classmap = JCode.jcode Javalib.interface_or_class Classmap.t

(** We store for each classname the location of its declaration. This map is filled during
    JFrontend.compute_source_icfg and then it is used in JTransType.get_class_struct_typ before we
    lose access to program. At the end, the information seats in each Struct.t (stored in Tenv.t) *)
type java_location_map = Location.t JBasics.ClassMap.t

type t =
  { classpath_channel: Javalib.class_path
  ; classmap: classmap
  ; mutable java_location_map: java_location_map
  ; callees: callee_status Procname.Hash.t }

let get_classmap program = program.classmap

let set_java_location program cn loc =
  program.java_location_map <- JBasics.ClassMap.add cn loc program.java_location_map


let get_java_location program cn =
  try Some (JBasics.ClassMap.find cn program.java_location_map) with Caml.Not_found -> None


let add_class cn jclass program =
  let add = Classmap.replace program.classmap in
  (* [prefix] must be a fresh class name *)
  let prefix = JBasics.cn_name cn ^ Config.java_lambda_marker_infix in
  (* we rewrite each class to replace invokedynamic (closure construction)
     with equivalent old-style Java code that implements a suitable Java interface *)
  let rewritten_jclass, new_classes = Javalib.remove_invokedynamics jclass ~prefix in
  add cn rewritten_jclass ;
  (* the rewrite will generate new classes and we add them to the program *)
  JBasics.ClassMap.iter add new_classes ;
  rewritten_jclass


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
    try
      let jclass = javalib_get_class program.classpath_channel cn in
      Some (add_class cn jclass program)
    with
    | JBasics.No_class_found _ ->
        (* TODO T28155039 Figure out when and what to log *)
        None
    | (JBasics.Class_structure_error _ | Invalid_argument _ | Failure _) as exn ->
        L.internal_error "ERROR: %s@." (Exn.to_string exn) ;
        None )


let load JClasspath.{classpath_channel; classes} =
  L.(debug Capture Medium) "loading program ... %!" ;
  let program =
    { classpath_channel
    ; classmap= Classmap.create 128
    ; java_location_map= JBasics.ClassMap.empty
    ; callees= Procname.Hash.create 128 }
  in
  JBasics.ClassSet.iter (fun cn -> ignore (lookup_node cn program)) classes ;
  L.(debug Capture Medium) "done@." ;
  program
