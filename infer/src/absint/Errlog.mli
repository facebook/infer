(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for error logs. *)

type node_tag =
  | Condition of bool
  | Exception of Typ.name
  | Procedure_start of Procname.t
  | Procedure_end of Procname.t

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

val concat_traces : (string * loc_trace) list -> loc_trace

val compute_local_exception_line : loc_trace -> int option
(** Look at all the trace steps and find those that are arising any exception, then bind them to the
    closest step at level 0. This extra information adds value to the report itself, and may avoid
    digging into the trace to understand the cause of the report. *)

type node =
  | UnknownNode
  | FrontendNode of {node_key: Procdesc.NodeKey.t}
  | BackendNode of {node: Procdesc.Node.t}

type err_key = private
  {severity: IssueType.severity; issue_type: IssueType.t; err_desc: Localise.error_desc}
[@@deriving compare]

(** Data associated to a specific error *)
type err_data = private
  { node_id: int
  ; node_key: Procdesc.NodeKey.t option
  ; session: int
  ; loc: Location.t
  ; loc_in_ml_source: Logging.ocaml_pos option
  ; loc_trace: loc_trace
  ; visibility: IssueType.visibility
  ; access: string option
  ; extras: Jsonbug_t.extra option }
[@@deriving compare]

(** Type of the error log *)
type t

val empty : unit -> t
(** Empty error log *)

(** type of the function to be passed to iter *)
type iter_fun = err_key -> err_data -> unit

val iter : iter_fun -> t -> unit
(** Apply f to nodes and error names *)

val fold : (err_key -> err_data -> 'a -> 'a) -> t -> 'a -> 'a

val pp_loc_trace : Format.formatter -> loc_trace -> unit

val pp_errors : Format.formatter -> t -> unit
(** Print errors from error log *)

val pp_warnings : Format.formatter -> t -> unit
(** Print warnings from error log *)

val pp_html : SourceFile.t -> DB.Results_dir.path -> Format.formatter -> t -> unit
(** Print an error log in html format *)

val update : t -> t -> unit
(** Update an old error log with a new one *)

val log_issue :
     ?severity_override:IssueType.severity
  -> t
  -> loc:Location.t
  -> node:node
  -> session:int
  -> ltr:loc_trace
  -> access:string option
  -> extras:Jsonbug_t.extra option
  -> Checker.t
  -> IssueToReport.t
  -> unit

val merge : into:t -> t -> [`Modified | `Intact]

val loc_trace_length : loc_trace -> int

val loc_trace_max_depth : loc_trace -> int

val issue_type_override : err_data -> string option

val category_override : err_data -> string option
