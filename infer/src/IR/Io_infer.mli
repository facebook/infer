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

(** Module to handle IO. Includes html and xml modules. *)

module Html : sig
  val close : Unix.File_descr.t * Format.formatter -> unit
  (** Close an Html file *)

  val create :
    DB.Results_dir.path_kind -> DB.Results_dir.path -> Unix.File_descr.t * Format.formatter
  (** Create a new html file *)

  val modified_during_analysis : SourceFile.t -> DB.Results_dir.path -> bool
  (** Return true if the html file was modified since the beginning of the analysis *)

  val node_filename : Typ.Procname.t -> int -> string
  (** File name for the node, given the procedure name and node id *)

  val open_out : SourceFile.t -> DB.Results_dir.path -> Unix.File_descr.t * Format.formatter
  (** Open an Html file to append data *)

  val pp_line_link :
    ?with_name:bool -> ?text:string option -> SourceFile.t -> DB.Results_dir.path
    -> Format.formatter -> int -> unit
  (** Print an html link to the given line number of the current source file *)

  val pp_hline : Format.formatter -> unit -> unit
  (** Print a horizontal line *)

  val pp_end_color : Format.formatter -> unit -> unit
  (** Print end color *)

  val pp_node_link :
    DB.Results_dir.path -> Typ.Procname.t -> description:string -> preds:int list -> succs:int list
    -> exn:int list -> isvisited:bool -> isproof:bool -> Format.formatter -> int -> unit
  (** Print an html link to the given node.
      Usage: [pp_node_link path_to_root ... fmt id].
      [path_to_root] is the path to the dir for the procedure in the spec db.
      [id] is the node identifier. *)

  val pp_proc_link : DB.Results_dir.path -> Typ.Procname.t -> Format.formatter -> string -> unit
  (** Print an html link to the given proc *)

  val pp_session_link :
    ?with_name:bool -> ?proc_name:Typ.Procname.t -> SourceFile.t -> string list -> Format.formatter
    -> int * int * int -> unit
  (** Print an html link given node id and session *)

  val pp_start_color : Format.formatter -> Pp.color -> unit
  (** Print start color *)
end

(** Create and print xml trees *)
module Xml : sig
  val tag_branch : string

  val tag_call_trace : string

  val tag_callee : string

  val tag_callee_id : string

  val tag_caller : string

  val tag_caller_id : string

  val tag_class : string

  val tag_code : string

  val tag_description : string

  val tag_err : string

  val tag_file : string

  val tag_flags : string

  val tag_hash : string

  val tag_in_calls : string

  val tag_key : string

  val tag_kind : string

  val tag_level : string

  val tag_line : string

  val tag_loc : string

  val tag_name : string

  val tag_name_id : string

  val tag_node : string

  val tag_out_calls : string

  val tag_precondition : string

  val tag_procedure : string

  val tag_procedure_id : string

  val tag_proof_coverage : string

  val tag_proof_trace : string

  val tag_qualifier : string

  val tag_qualifier_tags : string

  val tag_rank : string

  val tag_severity : string

  val tag_signature : string

  val tag_specs : string

  val tag_symop : string

  val tag_time : string

  val tag_to : string

  val tag_top : string

  val tag_trace : string

  val tag_type : string

  val tag_weight : string

  type tree = {name: string; attributes: (string * string) list; forest: node list}

  and node = Tree of tree | String of string  (** create a tree *)

  val create_tree : string -> (string * string) list -> node list -> node

  val pp_document : bool -> Format.formatter -> node -> unit
  (** print an xml document, if the first parameter is false on a single line without preamble *)

  val pp_open : Format.formatter -> string -> unit
  (** print the opening lines of an xml document consisting of a main tree with the given name *)

  val pp_close : Format.formatter -> string -> unit
  (** print the closing lines of an xml document consisting of a main tree with the given name *)

  val pp_inner_node : Format.formatter -> node -> unit
  (** print a node between a [pp_open] and a [pp_close] *)
end
