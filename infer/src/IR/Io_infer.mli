(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to handle IO. Includes html and xml modules. *)

module Html : sig
  val close : Unix.File_descr.t * Format.formatter -> unit
  (** Close an Html file *)

  val create : SourceFile.t -> DB.Results_dir.path -> Unix.File_descr.t * Format.formatter
  (** Create a new html file *)

  val modified_during_analysis : SourceFile.t -> DB.Results_dir.path -> bool
  (** Return true if the html file was modified since the beginning of the analysis *)

  val node_filename : Procname.t -> int -> string
  (** File name for the node, given the procedure name and node id *)

  val open_out : SourceFile.t -> DB.Results_dir.path -> Unix.File_descr.t * Format.formatter
  (** Open an Html file to append data *)

  val pp_line_link :
       ?with_name:bool
    -> ?text:string option
    -> SourceFile.t
    -> DB.Results_dir.path
    -> Format.formatter
    -> int
    -> unit
  (** Print an html link to the given line number of the current source file *)

  val pp_hline : Format.formatter -> unit -> unit
  (** Print a horizontal line *)

  val pp_node_link :
       DB.Results_dir.path
    -> Procname.t
    -> description:string
    -> preds:int list
    -> succs:int list
    -> exn:int list
    -> isvisited:bool
    -> Format.formatter
    -> int
    -> unit
  (** Print an html link to the given node. Usage: [pp_node_link path_to_root ... fmt id].
      [path_to_root] is the path to the dir for the procedure in the spec db. [id] is the node
      identifier. *)

  val pp_proc_link : DB.Results_dir.path -> Procname.t -> Format.formatter -> string -> unit
  (** Print an html link to the given proc *)

  val pp_session_link :
       ?with_name:bool
    -> ?proc_name:Procname.t
    -> SourceFile.t
    -> string list
    -> Format.formatter
    -> int * int * int
    -> unit
  (** Print an html link given node id and session *)
end
