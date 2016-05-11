(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module to manage the table of attributes. *)


(** Save .attr file for the procedure into the attributes database. *)
val store_attributes : ProcAttributes.t -> unit

(** Load the attributes for the procedure from the attributes database. *)
val load_attributes : Procname.t -> ProcAttributes.t option

(** Given a procdesure name, find the file where it is defined and *)
(** its corresponding type environment *)
val find_tenv_from_class_of_proc : Procname.t -> Tenv.t option

(** Given an ObjC class c, extract the type from the tenv where the class was *)
(** defined. We do this by adding a method that is unique to each class, and then *)
(** finding the tenv that corresponds to the class definition.  *)
val get_correct_type_from_objc_class_name : Mangled.t -> Sil.typ option
