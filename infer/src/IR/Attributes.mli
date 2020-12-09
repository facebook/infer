(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Module to manage the table of attributes. *)

open! IStd

type attributes_kind

val deserialize_attributes_kind : Sqlite3.Data.t -> attributes_kind

val store : proc_desc:Procdesc.t option -> ProcAttributes.t -> unit
(** Save .attr file for the procedure into the attributes database. *)

val load : Procname.t -> ProcAttributes.t option
(** Load the attributes for the procedure from the attributes database. *)

val is_no_return : Procname.t -> bool

val pp_attributes_kind : Format.formatter -> attributes_kind -> unit
