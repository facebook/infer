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

(** Element of a loc trace *)
type loc_trace_elem = private {
  lt_level : int; (** nesting level of procedure calls *)
  lt_loc : Location.t; (** source location at the current step in the trace *)
  lt_description : string; (** description of the current step in the trace *)
  lt_node_tags : (string * string) list (** tags describing the node at the current location *)
}

(** build a loc_trace_elem from its constituents (unambiguously identified by their types). *)
val make_trace_element : int -> Location.t -> string -> (string * string) list -> loc_trace_elem

(** Trace of locations *)
type loc_trace = loc_trace_elem list

(** Type of the error log *)
type t [@@deriving compare]

(** Empty error log *)
val empty : unit -> t

(** type of the function to be passed to iter *)
type iter_fun =
  (int * int) ->
  Location.t ->
  Logging.ml_loc option ->
  Exceptions.err_kind ->
  bool ->
  Localise.t -> Localise.error_desc -> string ->
  loc_trace ->
  Exceptions.err_class ->
  Exceptions.visibility ->
  string option ->
  unit

(** Apply f to nodes and error names *)
val iter : iter_fun -> t -> unit

(** Print errors from error log *)
val pp_errors : Format.formatter -> t -> unit

(** Print warnings from error log *)
val pp_warnings : Format.formatter -> t -> unit

(** Print an error log in html format *)
val pp_html : SourceFile.t -> DB.Results_dir.path -> Format.formatter -> t -> unit

(** Return the number of elements in the error log which satisfy the filter.  *)
val size : (Exceptions.err_kind -> bool -> bool) -> t -> int

(** Update an old error log with a new one *)
val update : t -> t -> unit

val log_issue :
  Exceptions.err_kind -> t -> Location.t -> (int * int) -> int -> loc_trace ->
  ?linters_def_file:string -> exn -> unit

(** {2 Functions for manipulating per-file error tables} *)

(** Type for per-file error tables *)
type err_table

(** Create an error table *)
val create_err_table : unit -> err_table

(** Add an error log to the global per-file table *)
val extend_table : err_table -> t -> unit

(** Size of the global per-file error table for the footprint phase *)
val err_table_size_footprint : Exceptions.err_kind -> err_table -> int

(** Print stats for the global per-file error table *)
val pp_err_table_stats : Exceptions.err_kind -> Format.formatter -> err_table -> unit

(** Print details of the global per-file error table *)
val print_err_table_details : Format.formatter -> err_table -> unit
