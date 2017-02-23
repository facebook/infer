(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

module L = Logging
module F = Format

(** Element of a loc trace *)
type loc_trace_elem = {
  lt_level : int; (** nesting level of procedure calls *)
  lt_loc : Location.t; (** source location at the current step in the trace *)
  lt_description : string; (** description of the current step in the trace *)
  lt_node_tags : (string * string) list (** tags describing the node at the current location *)
}

let make_trace_element lt_level lt_loc lt_description lt_node_tags =
  { lt_level; lt_loc; lt_description; lt_node_tags }

(** Trace of locations *)
type loc_trace = loc_trace_elem list

(** Data associated to a specific error *)
type err_data =
  (int * int) * int * Location.t * L.ml_loc option * loc_trace *
  Exceptions.err_class * Exceptions.visibility

let compare_err_data
    (_, _, loc1, _, _, _, _)
    (_, _, loc2, _, _, _, _) =
  Location.compare loc1 loc2

module ErrDataSet = (* set err_data with no repeated loc *)
  Caml.Set.Make(struct
    type t = err_data
    let compare = compare_err_data
  end)

(** Hash table to implement error logs *)
module ErrLogHash = struct
  module Key = struct

    type t = Exceptions.err_kind * bool * Localise.t * Localise.error_desc * string
    [@@deriving compare]

    (* NOTE: changing the hash function can change the order in which issues are reported. *)
    let hash (ekind, in_footprint, err_name, desc, _) =
      Hashtbl.hash (ekind, in_footprint, err_name, Localise.error_desc_hash desc)

    let equal
        (ekind1, in_footprint1, err_name1, desc1, _)
        (ekind2, in_footprint2, err_name2, desc2, _) =
      [%compare.equal : Exceptions.err_kind * bool * Localise.t]
        (ekind1, in_footprint1, err_name1)
        (ekind2, in_footprint2, err_name2) &&
      Localise.error_desc_equal desc1 desc2

  end
  include Hashtbl.Make (Key)
end

(** Type of the error log, to be reset once per function.
    Map err_kind, fotprint / re - execution flag, error name,
    error description, severity, to set of err_data. *)
type t = ErrDataSet.t ErrLogHash.t

let compare x y =
  let bindings x = ErrLogHash.fold (fun k d l -> (k, d) :: l) x [] in
  [%compare: (ErrLogHash.Key.t * ErrDataSet.t) list] (bindings x) (bindings y)

(** Empty error log *)
let empty () = ErrLogHash.create 13

(** type of the function to be passed to iter *)
type iter_fun =
  (int * int) ->
  Location.t ->
  L.ml_loc option ->
  Exceptions.err_kind ->
  bool ->
  Localise.t -> Localise.error_desc -> string ->
  loc_trace ->
  Exceptions.err_class ->
  Exceptions.visibility ->
  unit

(** Apply f to nodes and error names *)
let iter (f: iter_fun) (err_log: t) =
  ErrLogHash.iter (fun (ekind, in_footprint, err_name, desc, severity) set ->
      ErrDataSet.iter
        (fun (node_id_key, _, loc, ml_loc_opt, ltr, eclass, visibility) ->
           f
             node_id_key loc ml_loc_opt ekind in_footprint err_name
             desc severity ltr eclass visibility)
        set)
    err_log

(** Return the number of elements in the error log which satisfy [filter] *)
let size filter (err_log: t) =
  let count = ref 0 in
  ErrLogHash.iter (fun (ekind, in_footprint, _, _, _) eds ->
      if filter ekind in_footprint then count := !count + (ErrDataSet.cardinal eds)) err_log;
  !count

(** Print errors from error log *)
let pp_errors fmt (errlog : t) =
  let f (ekind, _, ename, _, _) _ =
    if Exceptions.equal_err_kind ekind Exceptions.Kerror then
      F.fprintf fmt "%a@ " Localise.pp ename in
  ErrLogHash.iter f errlog

(** Print warnings from error log *)
let pp_warnings fmt (errlog : t) =
  let f (ekind, _, ename, desc, _) _ =
    if Exceptions.equal_err_kind ekind Exceptions.Kwarning then
      F.fprintf fmt "%a %a@ " Localise.pp ename Localise.pp_error_desc desc in
  ErrLogHash.iter f errlog

(** Print an error log in html format *)
let pp_html source path_to_root fmt (errlog: t) =
  let pp_eds fmt eds =
    let pp_nodeid_session_loc
        fmt ((nodeid, _), session, loc, _, _, _, _) =
      Io_infer.Html.pp_session_link source path_to_root fmt (nodeid, session, loc.Location.line) in
    ErrDataSet.iter (pp_nodeid_session_loc fmt) eds in
  let f do_fp ek (ekind, infp, err_name, desc, _) eds =
    if Exceptions.equal_err_kind ekind ek && Bool.equal do_fp infp
    then
      F.fprintf fmt "<br>%a %a %a"
        Localise.pp err_name
        Localise.pp_error_desc desc
        pp_eds eds in
  F.fprintf fmt "%aERRORS DURING FOOTPRINT@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (f true Exceptions.Kerror) errlog;
  F.fprintf fmt "%aERRORS DURING RE-EXECUTION@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (f false Exceptions.Kerror) errlog;
  F.fprintf fmt "%aWARNINGS DURING FOOTPRINT@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (f true Exceptions.Kwarning) errlog;
  F.fprintf fmt "%aWARNINGS DURING RE-EXECUTION@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (f false Exceptions.Kwarning) errlog;
  F.fprintf fmt "%aINFOS DURING FOOTPRINT@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (f true Exceptions.Kinfo) errlog;
  F.fprintf fmt "%aINFOS DURING RE-EXECUTION@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (f false Exceptions.Kinfo) errlog


(* I use string in case we want to display a different name to the user*)
let severity_to_str severity = match severity with
  | Exceptions.High -> "HIGH"
  | Exceptions.Medium -> "MEDIUM"
  | Exceptions.Low -> "LOW"

(** Add an error description to the error log unless there is
    one already at the same node + session; return true if added *)
let add_issue tbl (ekind, in_footprint, err_name, desc, severity) (eds: ErrDataSet.t) : bool =
  try
    let current_eds = ErrLogHash.find tbl (ekind, in_footprint, err_name, desc, severity) in
    if ErrDataSet.subset eds current_eds then false
    else
      begin
        ErrLogHash.replace tbl
          (ekind, in_footprint, err_name, desc, severity)
          (ErrDataSet.union eds current_eds);
        true
      end
  with Not_found ->
    begin
      ErrLogHash.add tbl (ekind, in_footprint, err_name, desc, severity) eds;
      true
    end

(** Update an old error log with a new one *)
let update errlog_old errlog_new =
  ErrLogHash.iter
    (fun (ekind, infp, s, desc, severity) l ->
       ignore (add_issue errlog_old (ekind, infp, s, desc, severity) l)) errlog_new

let log_issue _ekind err_log loc node_id_key session ltr exn =
  let err_name, desc, ml_loc_opt, visibility, severity, force_kind, eclass =
    Exceptions.recognize_exception exn in
  let ekind = match force_kind with
    | Some ekind -> ekind
    | _ -> _ekind in
  let hide_java_loc_zero = (* hide java errors at location zero unless in -developer_mode *)
    not Config.developer_mode &&
    Config.curr_language_is Config.Java &&
    Int.equal loc.Location.line 0 in
  let hide_memory_error =
    match Localise.error_desc_get_bucket desc with
    | Some bucket when String.equal bucket Mleak_buckets.ml_bucket_unknown_origin ->
        not Mleak_buckets.should_raise_leak_unknown_origin
    | _ -> false in
  let log_it =
    Exceptions.equal_visibility visibility Exceptions.Exn_user ||
    (Config.developer_mode &&
     Exceptions.equal_visibility visibility Exceptions.Exn_developer) in
  if log_it && not hide_java_loc_zero && not hide_memory_error then begin
    let added =
      add_issue err_log
        (ekind, !Config.footprint, err_name, desc, severity_to_str severity)
        (ErrDataSet.singleton
           (node_id_key, session, loc, ml_loc_opt, ltr, eclass, visibility)) in
    let should_print_now =
      match exn with
      | Exceptions.Internal_error _ -> true
      | _ -> added in
    let print_now () =
      let ex_name, desc, ml_loc_opt, _, _, _, _ = Exceptions.recognize_exception exn in
      L.err "@\n%a@\n@?" (Exceptions.pp_err node_id_key loc ekind ex_name desc ml_loc_opt) ();
      if _ekind <> Exceptions.Kerror then begin
        let warn_str =
          let pp fmt =
            Format.fprintf fmt "%s %a"
              (Localise.to_string err_name)
              Localise.pp_error_desc desc in
          F.asprintf "%t" pp in
        let d = match ekind with
          | Exceptions.Kerror -> L.d_error
          | Exceptions.Kwarning -> L.d_warning
          | Exceptions.Kinfo | Exceptions.Kadvice -> L.d_info in
        d warn_str; L.d_ln();
      end in
    if should_print_now then print_now ()
  end

type err_log = t

(** Global per-file error table *)
module Err_table = struct
  type t = err_log
  let create = empty

  let count_err err_table err_name locs =
    ignore (add_issue err_table err_name locs)

  let table_size filter (err_table: t) =
    size filter err_table

  let pp_stats_footprint ekind fmt (err_table: err_log) =
    let err_name_map = ref String.Map.empty in (* map error name to count *)
    let count_err (err_name: Localise.t) n =
      let err_string = Localise.to_string err_name in
      let count = try String.Map.find_exn !err_name_map err_string with Not_found -> 0 in
      err_name_map := String.Map.add ~key:err_string ~data:(count + n) !err_name_map in
    let count (ekind', in_footprint, err_name, _, _) eds =
      if Exceptions.equal_err_kind ekind ekind' && in_footprint
      then count_err err_name (ErrDataSet.cardinal eds) in
    ErrLogHash.iter count err_table;
    let pp ~key:err_string ~data:count = F.fprintf fmt " %s:%d" err_string count in
    String.Map.iteri ~f:pp !err_name_map

  module LocMap =
    Caml.Map.Make(struct
      type t = ErrDataSet.elt
      let compare = compare_err_data
    end)

  let print_err_table_details fmt err_table =
    let map_err_fp = ref LocMap.empty in
    let map_err_re = ref LocMap.empty in
    let map_warn_fp = ref LocMap.empty in
    let map_warn_re = ref LocMap.empty in
    let map_info = ref LocMap.empty in
    let map_advice = ref LocMap.empty in
    let add_err nslm (ekind , in_fp, err_name, desc, _) =
      let map = match in_fp, ekind with
        | true, Exceptions.Kerror -> map_err_fp
        | false, Exceptions.Kerror -> map_err_re
        | true, Exceptions.Kwarning -> map_warn_fp
        | false, Exceptions.Kwarning -> map_warn_re
        | _, Exceptions.Kinfo -> map_info
        | _, Exceptions.Kadvice -> map_advice in
      try
        let err_list = LocMap.find nslm !map in
        map := LocMap.add nslm ((err_name, desc) :: err_list) !map
      with Not_found ->
        map := LocMap.add nslm [(err_name, desc)] !map in
    let f err_name eds =
      ErrDataSet.iter (fun loc -> add_err loc err_name) eds in
    ErrLogHash.iter f err_table;

    let pp ekind (nodeidkey, _, loc, ml_loc_opt, _, _, _) fmt err_names =
      List.iter ~f:(fun (err_name, desc) ->
          Exceptions.pp_err nodeidkey loc ekind err_name desc ml_loc_opt fmt ()) err_names in
    F.fprintf fmt "@.Detailed errors during footprint phase:@.";
    LocMap.iter (fun nslm err_names ->
        F.fprintf fmt "%a" (pp Exceptions.Kerror nslm) err_names) !map_err_fp;
    F.fprintf fmt "@.Detailed errors during re-execution phase:@.";
    LocMap.iter (fun nslm err_names ->
        F.fprintf fmt "%a" (pp Exceptions.Kerror nslm) err_names) !map_err_re;
    F.fprintf fmt "@.Detailed warnings during footprint phase:@.";
    LocMap.iter (fun nslm err_names ->
        F.fprintf fmt "%a" (pp Exceptions.Kwarning nslm) err_names) !map_warn_fp;
    F.fprintf fmt "@.Detailed warnings during re-execution phase:@.";
    LocMap.iter (fun nslm err_names ->
        F.fprintf fmt "%a" (pp Exceptions.Kwarning nslm) err_names) !map_warn_re
end

type err_table = Err_table.t

(** Create an error table *)
let create_err_table = Err_table.create

(** Print an error log and add it to the global per-file table *)
let extend_table err_table err_log =
  ErrLogHash.iter (Err_table.count_err err_table) err_log

(** Size of the global per-file error table for the footprint phase *)
let err_table_size_footprint ekind =
  let filter ekind' in_footprint = Exceptions.equal_err_kind ekind ekind' && in_footprint in
  Err_table.table_size filter

(** Print stats for the global per-file error table *)
let pp_err_table_stats ekind = Err_table.pp_stats_footprint ekind

(** Print details of the global per-file error table *)
let print_err_table_details =
  Err_table.print_err_table_details
