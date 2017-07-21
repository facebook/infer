(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Printers for the analysis results *)

(** Module to read specific lines from files.
    The data from any file will stay in memory until the handle is collected by the gc *)
module LineReader : sig
  type t

  val create : unit -> t
  (** create a line reader *)

  val from_file_linenum_original : t -> SourceFile.t -> int -> string option
  (** get the line from a source file and line number *)

  val from_file_linenum : t -> SourceFile.t -> int -> string option
  (** get the line from a source file and line number looking for the copy of the file in the results dir *)

  val from_loc : t -> Location.t -> string option
  (** get the line from a location looking for the copy of the file in the results dir *)
end

val curr_html_formatter : Format.formatter ref
(** Current html formatter *)

val force_delayed_prints : unit -> unit
(** Execute the delayed print actions *)

val node_finish_session : Procdesc.Node.t -> unit
(** Finish a session, and perform delayed print actions if required *)

val node_is_visited : Procdesc.Node.t -> bool * bool
(** Return true if the node was visited during footprint and during re-execution *)

val node_start_session : Procdesc.Node.t -> int -> unit
(** Start a session, and create a new html fine for the node if it does not exist yet *)

val write_proc_html : Procdesc.t -> unit
(** Write html file for the procedure. *)

val write_all_html_files : Cluster.t -> unit
(** Create filename.ext.html for each file in the cluster. *)
