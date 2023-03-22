(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Printers for the analysis results *)

val node_finish_session : Procdesc.Node.t -> unit
(** Finish a session, and perform delayed print actions if required *)

val node_start_session : pp_name:(Format.formatter -> unit) -> Procdesc.Node.t -> int -> unit
(** Start a session, and create a new html fine for the node if it does not exist yet *)

val write_proc_html : Procdesc.t -> unit
(** Write html file for the procedure. *)

val write_all_html_files : SourceFile.t -> unit
(** Group procedures used in the file by their corresponding source files, and create
    filename.ext.html for each such a file. *)
