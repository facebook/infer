(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for error logs. *)

type node_tag =
  | Condition of bool
  | Exception of Typ.name
  | Procedure_start of Typ.Procname.t
  | Procedure_end of Typ.Procname.t

(** Element of a loc trace *)
type loc_trace_elem = private
  { lt_level: int  (** nesting level of procedure calls *)
  ; lt_loc: Location.t  (** source location at the current step in the trace *)
  ; lt_description: string  (** description of the current step in the trace *)
  ; lt_node_tags: node_tag list  (** tags describing the node at the current location *) }

val make_trace_element : int -> Location.t -> string -> node_tag list -> loc_trace_elem
(** build a loc_trace_elem from its constituents (unambiguously identified by their types). *)

(** Trace of locations *)
type loc_trace = loc_trace_elem list

val compute_local_exception_line : loc_trace -> int option
(** Look at all the trace steps and find those that are arising any exception,
    then bind them to the closest step at level 0.
    This extra information adds value to the report itself, and may avoid
    digging into the trace to understand the cause of the report. *)

type node_id_key = private {node_id: int; node_key: int}

type err_key = private
  { err_kind: Exceptions.err_kind
  ; in_footprint: bool
  ; err_name: IssueType.t
  ; err_desc: Localise.error_desc
  ; severity: string }
  [@@deriving compare]

(** Data associated to a specific error *)
type err_data = private
  { node_id_key: node_id_key
  ; session: int
  ; loc: Location.t
  ; loc_in_ml_source: Logging.ml_loc option
  ; loc_trace: loc_trace
  ; err_class: Exceptions.err_class
  ; visibility: Exceptions.visibility
  ; linters_def_file: string option
  ; doc_url: string option
  (* url to documentation of the issue type *) }

(** Type of the error log *)
type t [@@deriving compare]

val empty : unit -> t
(** Empty error log *)

(** type of the function to be passed to iter *)
type iter_fun = err_key -> err_data -> unit

val iter : iter_fun -> t -> unit
(** Apply f to nodes and error names *)

val pp_loc_trace_elem : Format.formatter -> loc_trace_elem -> unit

val pp_loc_trace : Format.formatter -> loc_trace -> unit

val pp_errors : Format.formatter -> t -> unit
(** Print errors from error log *)

val pp_warnings : Format.formatter -> t -> unit
(** Print warnings from error log *)

val pp_html : SourceFile.t -> DB.Results_dir.path -> Format.formatter -> t -> unit
(** Print an error log in html format *)

val size : (Exceptions.err_kind -> bool -> bool) -> t -> int
(** Return the number of elements in the error log which satisfy the filter.  *)

val update : t -> t -> unit
(** Update an old error log with a new one *)

val log_issue :
  Exceptions.err_kind -> t -> Location.t -> int * int -> int -> loc_trace
  -> ?linters_def_file:string -> ?doc_url:string -> exn -> unit

(** {2 Functions for manipulating per-file error tables} *)

(** Type for per-file error tables *)
type err_table

val create_err_table : unit -> err_table
(** Create an error table *)

val extend_table : err_table -> t -> unit
(** Add an error log to the global per-file table *)

val err_table_size_footprint : Exceptions.err_kind -> err_table -> int
(** Size of the global per-file error table for the footprint phase *)

val pp_err_table_stats : Exceptions.err_kind -> Format.formatter -> err_table -> unit
(** Print stats for the global per-file error table *)

val print_err_table_details : Format.formatter -> err_table -> unit
(** Print details of the global per-file error table *)
