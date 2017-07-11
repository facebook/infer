(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module to manage the table of attributes. *)

open! IStd

(** Save .attr file for the procedure into the attributes database. *)

val store_attributes : ProcAttributes.t -> unit

(** Load the attributes for the procedure from the attributes database.
    If cache is true, add the attribute to the global cache *)

val load_attributes : cache:bool -> Typ.Procname.t -> ProcAttributes.t option

(** Load attrubutes for the procedure but only if is_defined is true *)

val load_defined_attributes : cache_none:bool -> Typ.Procname.t -> ProcAttributes.t option

(** Given the name of an ObjC class, extract the type from the tenv where the class was defined. We
    do this by adding a method that is unique to each class, and then finding the tenv that
    corresponds to the class definition. *)

val get_correct_type_from_objc_class_name : Typ.Name.t -> Typ.t option

(* Find the file where the procedure was captured, if a cfg for that file exists.
   Return also a boolean indicating whether the procedure is defined in an
   include file.
   If cache is true, add the attribute to the global cache *)

val find_file_capturing_procedure :
  ?cache:bool -> Typ.Procname.t -> (SourceFile.t * [`Include | `Source]) option

type t

val stats : unit -> t

val to_json : t -> Yojson.Basic.json

val from_json : Yojson.Basic.json -> t

val aggregate : t list -> Yojson.Basic.json
