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

type attributes_kind

val deserialize_attributes_kind : Sqlite3.Data.t -> attributes_kind

val store : ProcAttributes.t -> unit
(** Save .attr file for the procedure into the attributes database. *)

val load : Typ.Procname.t -> ProcAttributes.t option
(** Load the attributes for the procedure from the attributes database. *)

val load_defined : Typ.Procname.t -> ProcAttributes.t option
(** Load attributes for the procedure but only if is_defined is true *)

val find_file_capturing_procedure : Typ.Procname.t -> (SourceFile.t * [`Include | `Source]) option
(** Find the file where the procedure was captured, if a cfg for that file exists.
   Return also a boolean indicating whether the procedure is defined in an
   include file. *)

val pp_attributes_kind : Format.formatter -> attributes_kind -> unit
