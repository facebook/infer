(*
* Copyright (c) 2009 - 2013 Monoidics ltd.
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(** Printers for the analysis results *)

(** return true if the node was visited during footprint and during re-execution*)
val is_visited_phase : Cfg.Node.t -> bool * bool

(** return true if the node was visited during analysis *)
val is_visited : Cfg.Node.t -> bool

(** Execute the delayed print actions *)
val force_delayed_prints : unit -> unit

(** Start a session, and create a new html fine for the node if it does not exist yet *)
val start_session : Cfg.node -> Sil.location -> Procname.t -> int -> unit

(** Finish a session, and perform delayed print actions if required *)
val finish_session : Cfg.node -> unit

(** Write log file for the proc, the boolean indicates whether to print whole seconds only *)
val proc_write_log : bool -> Cfg.cfg -> Procname.t -> unit

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
  val from_loc : t -> Sil.location -> string option
end

(** Create filename.c.html with line numbers and links to nodes for each file in the exe_env *)
val c_files_write_html : LineReader.t -> Exe_env.t -> unit
