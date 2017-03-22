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

type node_id_key = {
  node_id : int;
  node_key : int
}

type err_key = {
  err_kind : Exceptions.err_kind;
  in_footprint : bool;
  err_name : Localise.t;
  err_desc : Localise.error_desc;
  severity : string
}[@@deriving compare]

(** Data associated to a specific error *)
type err_data = {
  node_id_key : node_id_key;
  session : int;
  loc : Location.t;
  loc_in_ml_source : L.ml_loc option;
  loc_trace : loc_trace;
  err_class : Exceptions.err_class;
  visibility : Exceptions.visibility;
  linters_def_file : string option
}

let compare_err_data err_data1 err_data2 =
  Location.compare err_data1.loc err_data2.loc

module ErrDataSet = (* set err_data with no repeated loc *)
  Caml.Set.Make(struct
    type t = err_data
    let compare = compare_err_data
  end)

(** Hash table to implement error logs *)
module ErrLogHash = struct
  module Key = struct
    type t = err_key[@@deriving compare]

    (* NOTE: changing the hash function can change the order in which issues are reported. *)
    let hash key =
      Hashtbl.hash
        (key.err_kind, key.in_footprint, key.err_name, Localise.error_desc_hash key.err_desc)

    let equal key1 key2 =
      [%compare.equal : Exceptions.err_kind * bool * Localise.t]
        (key1.err_kind, key1.in_footprint, key1.err_name)
        (key2.err_kind, key2.in_footprint, key2.err_name) &&
      Localise.error_desc_equal key1.err_desc key2.err_desc

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
type iter_fun = err_key -> err_data -> unit

(** Apply f to nodes and error names *)
let iter (f: iter_fun) (err_log: t) =
  ErrLogHash.iter (fun err_key set ->
      ErrDataSet.iter (fun err_data -> f err_key err_data) set)
    err_log

let iter_filter (f: iter_fun) (err_log: t) =
  ErrLogHash.iter (fun err_key set ->
      ErrDataSet.iter (fun err_data -> f err_key err_data) set)
    err_log

(** Return the number of elements in the error log which satisfy [filter] *)
let size filter (err_log: t) =
  let count = ref 0 in
  ErrLogHash.iter (fun key err_datas ->
      if filter key.err_kind key.in_footprint
      then count := !count + (ErrDataSet.cardinal err_datas)) err_log;
  !count

(** Print errors from error log *)
let pp_errors fmt (errlog : t) =
  let f key _ =
    if Exceptions.equal_err_kind key.err_kind Exceptions.Kerror then
      F.fprintf fmt "%a@ " Localise.pp key.err_name in
  ErrLogHash.iter f errlog

(** Print warnings from error log *)
let pp_warnings fmt (errlog : t) =
  let f key _ =
    if Exceptions.equal_err_kind key.err_kind Exceptions.Kwarning then
      F.fprintf fmt "%a %a@ " Localise.pp key.err_name Localise.pp_error_desc key.err_desc in
  ErrLogHash.iter f errlog

(** Print an error log in html format *)
let pp_html source path_to_root fmt (errlog: t) =
  let pp_eds fmt err_datas =
    let pp_nodeid_session_loc
        fmt err_data =
      Io_infer.Html.pp_session_link
        source path_to_root fmt
        (err_data.node_id_key.node_id, err_data.session, err_data.loc.Location.line) in
    ErrDataSet.iter (pp_nodeid_session_loc fmt) err_datas in
  let pp_err_log do_fp ek key err_datas =
    if Exceptions.equal_err_kind key.err_kind ek && Bool.equal do_fp key.in_footprint
    then
      F.fprintf fmt "<br>%a %a %a"
        Localise.pp key.err_name
        Localise.pp_error_desc key.err_desc
        pp_eds err_datas in
  F.fprintf fmt "%aERRORS DURING FOOTPRINT@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (pp_err_log true Exceptions.Kerror) errlog;
  F.fprintf fmt "%aERRORS DURING RE-EXECUTION@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (pp_err_log false Exceptions.Kerror) errlog;
  F.fprintf fmt "%aWARNINGS DURING FOOTPRINT@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (pp_err_log true Exceptions.Kwarning) errlog;
  F.fprintf fmt "%aWARNINGS DURING RE-EXECUTION@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (pp_err_log false Exceptions.Kwarning) errlog;
  F.fprintf fmt "%aINFOS DURING FOOTPRINT@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (pp_err_log true Exceptions.Kinfo) errlog;
  F.fprintf fmt "%aINFOS DURING RE-EXECUTION@\n" Io_infer.Html.pp_hline ();
  ErrLogHash.iter (pp_err_log false Exceptions.Kinfo) errlog


(* I use string in case we want to display a different name to the user*)
let severity_to_str severity = match severity with
  | Exceptions.High -> "HIGH"
  | Exceptions.Medium -> "MEDIUM"
  | Exceptions.Low -> "LOW"

(** Add an error description to the error log unless there is
    one already at the same node + session; return true if added *)
let add_issue tbl err_key (err_datas: ErrDataSet.t) : bool =
  try
    let current_eds = ErrLogHash.find tbl err_key in
    if ErrDataSet.subset err_datas current_eds then false
    else
      begin
        ErrLogHash.replace tbl err_key (ErrDataSet.union err_datas current_eds);
        true
      end
  with Not_found ->
    begin
      ErrLogHash.add tbl err_key err_datas;
      true
    end

(** Update an old error log with a new one *)
let update errlog_old errlog_new =
  ErrLogHash.iter
    (fun err_key l ->
       ignore (add_issue errlog_old err_key l)) errlog_new

let log_issue err_kind err_log loc (node_id, node_key) session ltr ?linters_def_file exn =
  let err_name, err_desc, ml_loc_opt, visibility, severity, force_kind, eclass =
    Exceptions.recognize_exception exn in
  let err_kind = match force_kind with
    | Some err_kind -> err_kind
    | _ -> err_kind in
  let hide_java_loc_zero = (* hide java errors at location zero unless in -developer_mode *)
    not Config.developer_mode &&
    Config.curr_language_is Config.Java &&
    Int.equal loc.Location.line 0 in
  let hide_memory_error =
    match Localise.error_desc_get_bucket err_desc with
    | Some bucket when String.equal bucket Mleak_buckets.ml_bucket_unknown_origin ->
        not Mleak_buckets.should_raise_leak_unknown_origin
    | _ -> false in
  let log_it =
    Exceptions.equal_visibility visibility Exceptions.Exn_user ||
    (Config.developer_mode &&
     Exceptions.equal_visibility visibility Exceptions.Exn_developer) in
  if log_it && not hide_java_loc_zero && not hide_memory_error then begin
    let added =
      let node_id_key = {node_id; node_key} in
      let err_data = {
        node_id_key;
        session;
        loc;
        loc_in_ml_source = ml_loc_opt;
        loc_trace = ltr;
        err_class = eclass;
        visibility;
        linters_def_file;
      } in
      let err_key = {
        err_kind;
        in_footprint = !Config.footprint;
        err_name;
        err_desc;
        severity = severity_to_str severity
      } in
      add_issue err_log err_key (ErrDataSet.singleton err_data) in
    let should_print_now =
      match exn with
      | Exceptions.Internal_error _ -> true
      | _ -> added in
    let print_now () =
      let ex_name, desc, ml_loc_opt, _, _, _, _ = Exceptions.recognize_exception exn in
      L.err "@\n%a@\n@?"
        (Exceptions.pp_err ~node_key loc err_kind ex_name desc ml_loc_opt) ();
      if err_kind <> Exceptions.Kerror then begin
        let warn_str =
          let pp fmt =
            Format.fprintf fmt "%s %a"
              (Localise.to_issue_id err_name)
              Localise.pp_error_desc desc in
          F.asprintf "%t" pp in
        let d = match err_kind with
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
      let err_string = Localise.to_issue_id err_name in
      let count = try String.Map.find_exn !err_name_map err_string with Not_found -> 0 in
      err_name_map := String.Map.add ~key:err_string ~data:(count + n) !err_name_map in
    let count key err_datas =
      if Exceptions.equal_err_kind ekind key.err_kind && key.in_footprint
      then count_err key.err_name (ErrDataSet.cardinal err_datas) in
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
    let add_err nslm key =
      let map = match key.in_footprint, key.err_kind with
        | true, Exceptions.Kerror -> map_err_fp
        | false, Exceptions.Kerror -> map_err_re
        | true, Exceptions.Kwarning -> map_warn_fp
        | false, Exceptions.Kwarning -> map_warn_re
        | _, Exceptions.Kinfo -> map_info
        | _, Exceptions.Kadvice -> map_advice in
      try
        let err_list = LocMap.find nslm !map in
        map := LocMap.add nslm ((key.err_name, key.err_desc) :: err_list) !map
      with Not_found ->
        map := LocMap.add nslm [(key.err_name, key.err_desc)] !map in
    let f err_name eds =
      ErrDataSet.iter (fun loc -> add_err loc err_name) eds in
    ErrLogHash.iter f err_table;

    let pp ekind err_data fmt err_names =
      List.iter ~f:(fun (err_name, desc) ->
          Exceptions.pp_err
            ~node_key:err_data.node_id_key.node_key err_data.loc ekind err_name desc
            err_data.loc_in_ml_source fmt ()) err_names in
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
