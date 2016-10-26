(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Printers for the analysis results *)

(** Module to read specific lines from files.
    The data from any file will stay in memory until the handle is collected by the gc *)
module LineReader : sig
  type t

  (** create a line reader *)
  val create : unit -> t

  (** get the line from a source file and line number *)
  val from_file_linenum_original : t -> DB.source_file -> int -> string option

  (** get the line from a source file and line number looking for the copy of the file in the results dir *)
  val from_file_linenum : t -> DB.source_file -> int -> string option

  (** get the line from a location looking for the copy of the file in the results dir *)
  val from_loc : t -> Location.t -> string option
end

(** Current html formatter *)
val curr_html_formatter : Format.formatter ref

(** Execute the delayed print actions *)
val force_delayed_prints : unit -> unit

(** Finish a session, and perform delayed print actions if required *)
val node_finish_session : Cfg.node -> DB.source_file -> unit

(** Return true if the node was visited during footprint and during re-execution *)
val node_is_visited : Cfg.Node.t -> bool * bool

(** Start a session, and create a new html fine for the node if it does not exist yet *)
val node_start_session : Cfg.node -> Location.t -> Procname.t -> int -> DB.source_file -> unit

(** Write html file for the procedure.
    The boolean indicates whether to print whole seconds only. *)
val write_proc_html : DB.source_file -> bool -> Cfg.Procdesc.t -> unit

val write_html_file : LineReader.t -> DB.source_file -> Cfg.Procdesc.t list -> unit

(** Create filename.ext.html for each file in the exe_env. *)
val write_all_html_files : Exe_env.t -> unit
