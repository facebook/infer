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

(** Specifications and spec table *)

module L = Logging
module F = Format

(* =============== START of support for spec tables =============== *)

(** Module for joined props *)
module Jprop = struct

  (** Remember when a prop is obtained as the join of two other props; the first parameter is an id *)
  type 'a t =
    | Prop of int * 'a Prop.t
    | Joined of int * 'a Prop.t * 'a t * 'a t

  let to_prop = function
    | Prop (_, p) -> p
    | Joined (_, p, _, _) -> p

  let to_number = function
    | Prop (n, _) -> n
    | Joined (n, _, _, _) -> n

  let rec fav_add_dfs fav = function
    | Prop (_, p) -> Prop.prop_fav_add_dfs fav p
    | Joined (_, p, jp1, jp2) ->
        Prop.prop_fav_add_dfs fav p;
        fav_add_dfs fav jp1;
        fav_add_dfs fav jp2

  let rec normalize = function
    | Prop (n, p) -> Prop (n, Prop.normalize p)
    | Joined (n, p, jp1, jp2) -> Joined (n, Prop.normalize p, normalize jp1, normalize jp2)

  (** Return a compact representation of the jprop *)
  let rec compact sh = function
    | Prop (n, p) ->
        Prop (n, Prop.prop_compact sh p)
    | Joined(n, p, jp1, jp2) ->
        Joined(n, Prop.prop_compact sh p, compact sh jp1, compact sh jp2)

  (** Print the toplevel prop *)
  let pp_short pe f jp =
    Prop.pp_prop pe f (to_prop jp)

  (** Dump the toplevel prop *)
  let d_shallow (jp: Prop.normal t) = L.add_print_action (L.PTjprop_short, Obj.repr jp)

  (** Get identifies of the jprop *)
  let get_id = function
    | Prop (n, _) -> n
    | Joined (n, _, _, _) -> n

  (** Print a list of joined props, the boolean indicates whether to print subcomponents of joined props *)
  let pp_list pe shallow f jplist =
    let rec pp_seq_newline f = function
      | [] -> ()
      | [Prop (n, p)] -> F.fprintf f "PROP %d:@\n%a" n (Prop.pp_prop pe) p
      | [Joined (n, p, p1, p2)] ->
          if not shallow then F.fprintf f "%a@\n" pp_seq_newline [p1];
          if not shallow then F.fprintf f "%a@\n" pp_seq_newline [p2];
          F.fprintf f "PROP %d (join of %d,%d):@\n%a" n (get_id p1) (get_id p2) (Prop.pp_prop pe) p
      | jp:: l ->
          F.fprintf f "%a@\n" pp_seq_newline [jp];
          pp_seq_newline f l in
    pp_seq_newline f jplist

  (** dump a joined prop list, the boolean indicates whether to print toplevel props only *)
  let d_list (shallow: bool) (jplist: Prop.normal t list) = L.add_print_action (L.PTjprop_list, Obj.repr (shallow, jplist))

  (** Comparison for joined_prop *)
  let rec compare jp1 jp2 = match jp1, jp2 with
    | Prop (_, p1), Prop (_, p2) ->
        Prop.prop_compare p1 p2
    | Prop _, _ -> - 1
    | _, Prop _ -> 1
    | Joined (_, p1, jp1, jq1), Joined (_, p2, jp2, jq2) ->
        let n = Prop.prop_compare p1 p2 in
        if n <> 0 then n
        else
          let n = compare jp1 jp2 in
          if n <> 0 then n else compare jq1 jq2

  (** Return true if the two join_prop's are equal *)
  let equal jp1 jp2 =
    compare jp1 jp2 == 0

  let rec fav_add fav = function
    | Prop (_, p) -> Prop.prop_fav_add fav p
    | Joined (_, p, jp1, jp2) ->
        Prop.prop_fav_add fav p;
        fav_add fav jp1;
        fav_add fav jp2

  let rec jprop_sub sub = function
    | Prop (n, p) -> Prop (n, Prop.prop_sub sub p)
    | Joined (n, p, jp1, jp2) ->
        let p' = Prop.prop_sub sub p in
        let jp1' = jprop_sub sub jp1 in
        let jp2' = jprop_sub sub jp2 in
        Joined (n, p', jp1', jp2')

  let filter (f: 'a t -> 'b option) jpl =
    let rec do_filter acc = function
      | [] -> acc
      | (Prop _ as jp) :: jpl ->
          (match f jp with
           | Some x ->
               do_filter (x:: acc) jpl
           | None -> do_filter acc jpl)
      | (Joined (_, _, jp1, jp2) as jp) :: jpl ->
          (match f jp with
           | Some x ->
               do_filter (x:: acc) jpl
           | None ->
               do_filter acc (jpl @ [jp1; jp2])) in
    do_filter [] jpl

  let rec map (f : 'a Prop.t -> 'b Prop.t) = function
    | Prop (n, p) -> Prop (n, f p)
    | Joined (n, p, jp1, jp2) -> Joined (n, f p, map f jp1, map f jp2)

(*
  let rec jprop_sub sub = function
    | Prop (n, p) -> Prop (n, Prop.prop_sub sub p)
    | Joined (n, p, jp1, jp2) ->
        Joined (n, Prop.prop_sub sub p, jprop_sub sub jp1, jprop_sub sub jp2)
*)
end
(***** End of module Jprop *****)

module Visitedset =
  Set.Make (struct
    type t = Cfg.Node.id * int list
    let compare (node_id1, _) (node_id2, _) = Cfg.Node.id_compare node_id1 node_id2
  end)

let visited_str vis =
  let s = ref "" in
  let lines = ref IntSet.empty in
  let do_one (_, ns) =
    (* if IList.length ns > 1 then
       begin
       let ss = ref "" in
       IList.iter (fun n -> ss := !ss ^ " " ^ string_of_int n) ns;
       L.err "Node %d has lines %s@." node !ss
       end; *)
    IList.iter (fun n -> lines := IntSet.add n !lines) ns in
  Visitedset.iter do_one vis;
  IntSet.iter (fun n -> s := !s ^ " " ^ string_of_int n) !lines;
  !s

(** A spec consists of:
    pre: a joined prop
    post: a list of props with path
    visited: a list of pairs (node_id, line) for the visited nodes *)
type 'a spec = { pre: 'a Jprop.t; posts: ('a Prop.t * Paths.Path.t) list; visited : Visitedset.t }

module NormSpec : sig (* encapsulate type for normalized specs *)
  type t
  val normalize : Prop.normal spec -> t
  val tospecs : t list -> Prop.normal spec list
  val compact : Sil.sharing_env -> t -> t (** Return a compact representation of the spec *)
  val erase_join_info_pre : t -> t (** Erase join info from pre of spec *)
end = struct
  type t = Prop.normal spec

  let tospecs specs = specs

  let spec_fav (spec: Prop.normal spec) : Sil.fav =
    let fav = Sil.fav_new () in
    Jprop.fav_add_dfs fav spec.pre;
    IList.iter (fun (p, _) -> Prop.prop_fav_add_dfs fav p) spec.posts;
    fav

  let spec_sub sub spec =
    { pre = Jprop.normalize (Jprop.jprop_sub sub spec.pre);
      posts = IList.map (fun (p, path) -> (Prop.normalize (Prop.prop_sub sub p), path)) spec.posts;
      visited = spec.visited }

  (** Convert spec into normal form w.r.t. variable renaming *)
  let normalize (spec: Prop.normal spec) : Prop.normal spec =
    let fav = spec_fav spec in
    let idlist = Sil.fav_to_list fav in
    let count = ref 0 in
    let sub = Sil.sub_of_list (IList.map (fun id -> incr count; (id, Sil.Var (Ident.create_normal Ident.name_spec !count))) idlist) in
    spec_sub sub spec

  (** Return a compact representation of the spec *)
  let compact sh spec =
    let pre = Jprop.compact sh spec.pre in
    let posts = IList.map (fun (p, path) -> (Prop.prop_compact sh p, path)) spec.posts in
    { pre = pre; posts = posts; visited = spec.visited }

  (** Erase join info from pre of spec *)
  let erase_join_info_pre spec =
    let spec' = { spec with pre = Jprop.Prop (1, Jprop.to_prop spec.pre) } in
    normalize spec'
end

(** Convert spec into normal form w.r.t. variable renaming *)
let spec_normalize =
  NormSpec.normalize

(** Cast a list of normalized specs to a list of specs *)
let normalized_specs_to_specs =
  NormSpec.tospecs

module CallStats = struct (** module for tracing stats of function calls *)
  module PnameLocHash = Hashtbl.Make (struct
      type t = Procname.t * Location.t
      let hash (pname, loc) = Hashtbl.hash (Procname.hash_pname pname, loc.Location.line)
      let equal (pname1, loc1) (pname2, loc2) =
        Location.equal loc1 loc2 && Procname.equal pname1 pname2
    end)

  type call_result = (** kind of result of a procedure call *)
    | CR_success (** successful call *)
    | CR_not_met (** precondition not met *)
    | CR_not_found (** the callee has no specs *)
    | CR_skip (** the callee was skipped *)

  type trace = (call_result * bool) list

  type t = trace PnameLocHash.t

  let trace_add tr (res : call_result) in_footprint = (res, in_footprint) :: tr

  let empty_trace : trace = []

  let init calls =
    let hash = PnameLocHash.create 1 in
    let do_call pn_loc = PnameLocHash.add hash pn_loc empty_trace in
    IList.iter do_call calls;
    hash

  let trace t proc_name loc res in_footprint =
    let tr_old = try PnameLocHash.find t (proc_name, loc) with
      | Not_found ->
          PnameLocHash.add t (proc_name, loc) empty_trace;
          empty_trace in
    let tr_new = trace_add tr_old res in_footprint in
    PnameLocHash.replace t (proc_name, loc) tr_new

  let tr_elem_str (cr, in_footprint) =
    let s1 = match cr with
      | CR_success -> "OK"
      | CR_not_met -> "NotMet"
      | CR_not_found -> "NotFound"
      | CR_skip -> "Skip" in
    let s2 = if in_footprint then "FP" else "RE" in
    s1 ^ ":" ^ s2

  let pp_trace fmt tr =
    pp_seq
      (fun fmt x -> F.fprintf fmt "%s" (tr_elem_str x))
      fmt (IList.rev tr)

  let iter f t =
    let elems = ref [] in
    PnameLocHash.iter (fun x tr -> elems := (x, tr) :: !elems) t;
    let sorted_elems =
      let compare ((pname1, loc1), _) ((pname2, loc2), _) =
        let n = Procname.compare pname1 pname2 in
        if n <> 0 then n else Location.compare loc1 loc2 in
      IList.sort compare !elems in
    IList.iter (fun (x, tr) -> f x tr) sorted_elems

(*
  let pp fmt t =
    let do_call (pname, loc) tr =
      F.fprintf fmt "%a %a: %a@\n" Procname.pp pname Location.pp loc pp_trace tr in
    iter do_call t
*)
end

(** stats of the calls performed during the analysis *)
type call_stats = CallStats.t

(** Execution statistics *)
type stats =
  { stats_time: float; (** Analysis time for the procedure *)
    stats_failure:
      SymOp.failure_kind option; (** what type of failure stopped the analysis (if any) *)
    stats_calls: Cg.in_out_calls; (** num of procs calling, and called *)
    symops: int; (** Number of SymOp's throughout the whole analysis of the function *)
    mutable nodes_visited_fp : IntSet.t; (** Nodes visited during the footprint phase *)
    mutable nodes_visited_re : IntSet.t; (** Nodes visited during the re-execution phase *)
    call_stats : call_stats;
  }

type status = ACTIVE | INACTIVE | STALE

type phase = FOOTPRINT | RE_EXECUTION

type dependency_map_t = int Procname.Map.t

type call = Procname.t * Location.t

type call_summary = {
  expensive_calls: call list;
  allocations: call list
}

(** Payload: results of some analysis *)
type payload =
  {
    preposts : NormSpec.t list option; (** list of specs *)
    typestate : unit TypeState.t option; (** final typestate *)
    calls: call_summary option;
  }

type summary =
  { dependency_map: dependency_map_t;  (** maps children procs to timestamp as last seen at the start of an analysys phase for this proc *)
    nodes: Cfg.Node.id list; (** ids of cfg nodes of the procedure *)
    phase: phase; (** in FOOTPRINT phase or in RE_EXECUTION PHASE *)
    payload: payload;  (** payload containing the result of some analysis *)
    sessions: int ref; (** Session number: how many nodes went trough symbolic execution *)
    stats: stats;  (** statistics: execution time and list of errors *)
    status: status; (** ACTIVE when the proc is being analyzed *)
    timestamp: int; (** Timestamp of the specs, >= 0, increased every time the specs change *)
    attributes : ProcAttributes.t; (** Attributes of the procedure *)
  }

(** origin of a summary: current results dir, a spec library, or models *)
type origin =
  | Res_dir
  | Spec_lib
  | Models

type spec_tbl = (summary * origin) Procname.Hash.t

let spec_tbl: spec_tbl = Procname.Hash.create 128

let clear_spec_tbl () = Procname.Hash.clear spec_tbl

(** pretty print analysis time; if [whole_seconds] is true, only print time in seconds *)
let pp_time whole_seconds fmt t =
  if whole_seconds then F.fprintf fmt "%3.0f s" t
  else F.fprintf fmt "%f s" t

let pp_failure_kind_opt fmt failure_kind_opt = match failure_kind_opt with
  | Some failure_kind -> SymOp.pp_failure_kind fmt failure_kind
  | None -> F.fprintf fmt "NONE"

let pp_stats err_log whole_seconds fmt stats =
  F.fprintf fmt "TIME:%a FAILURE:%a SYMOPS:%d CALLS:%d,%d@\n" (pp_time whole_seconds)
    stats.stats_time pp_failure_kind_opt stats.stats_failure stats.symops
    stats.stats_calls.Cg.in_calls stats.stats_calls.Cg.out_calls;
  F.fprintf fmt "ERRORS: @[<h>%a@]@." Errlog.pp_errors err_log;
  F.fprintf fmt "WARNINGS: @[<h>%a@]" Errlog.pp_warnings err_log

(** Print the spec *)
let pp_spec pe num_opt fmt spec =
  let num_str = match num_opt with
    | None -> "----------"
    | Some (n, tot) -> Format.sprintf "%d of %d [nvisited:%s]" n tot (visited_str spec.visited) in
  let pre = Jprop.to_prop spec.pre in
  let pe_post = Prop.prop_update_obj_sub pe pre in
  let post_list = IList.map fst spec.posts in
  match pe.pe_kind with
  | PP_TEXT ->
      F.fprintf fmt "--------------------------- %s ---------------------------@\n" num_str;
      F.fprintf fmt "PRE:@\n%a@\n" (Prop.pp_prop pe_text) pre;
      F.fprintf fmt "%a@\n" (Propgraph.pp_proplist pe_post "POST" (pre, true)) post_list;
      F.fprintf fmt "----------------------------------------------------------------"
  | PP_HTML ->
      F.fprintf fmt "--------------------------- %s ---------------------------@\n" num_str;
      F.fprintf fmt "PRE:@\n%a%a%a@\n" Io_infer.Html.pp_start_color Blue (Prop.pp_prop (pe_html Blue)) pre Io_infer.Html.pp_end_color ();
      F.fprintf fmt "%a" (Propgraph.pp_proplist pe_post "POST" (Jprop.to_prop spec.pre, true)) post_list;
      F.fprintf fmt "----------------------------------------------------------------"
  | PP_LATEX ->
      F.fprintf fmt "\\textbf{\\large Requires}\\\\@\n@[%a%a%a@]\\\\@\n" Latex.pp_color Blue (Prop.pp_prop (pe_latex Blue)) pre Latex.pp_color pe.pe_color;
      F.fprintf fmt "\\textbf{\\large Ensures}\\\\@\n@[%a@]" (Propgraph.pp_proplist pe_post "POST" (pre, true)) post_list

(** Dump a spec *)
let d_spec (spec: 'a spec) = L.add_print_action (L.PTspec, Obj.repr spec)

let pp_specs pe fmt specs =
  let total = IList.length specs in
  let cnt = ref 0 in
  match pe.pe_kind with
  | PP_TEXT ->
      IList.iter (fun spec -> incr cnt; F.fprintf fmt "%a@\n" (pp_spec pe (Some (!cnt, total))) spec) specs
  | PP_HTML ->
      IList.iter (fun spec -> incr cnt; F.fprintf fmt "%a<br>@\n" (pp_spec pe (Some (!cnt, total))) spec) specs
  | PP_LATEX ->
      IList.iter (fun spec -> incr cnt; F.fprintf fmt "\\subsection*{Spec %d of %d}@\n\\(%a\\)@\n" !cnt total (pp_spec pe None) spec) specs

(** Print the decpendency map *)
let pp_dependency_map fmt dependency_map =
  let pp_entry fmt proc_name n = F.fprintf fmt "%a=%d " Procname.pp proc_name n in
  Procname.Map.iter (pp_entry fmt) dependency_map

let describe_timestamp summary =
  ("Timestamp", Printf.sprintf "%d" summary.timestamp)

let describe_status summary =
  ("Status", if summary.status == ACTIVE then "ACTIVE" else "INACTIVE")

let describe_phase summary =
  ("Phase", if summary.phase == FOOTPRINT then "FOOTPRINT" else "RE_EXECUTION")

(** Return the signature of a procedure declaration as a string *)
let get_signature summary =
  let s = ref "" in
  IList.iter
    (fun (p, typ) ->
       let pp_name f () = F.fprintf f "%a" Mangled.pp p in
       let pp f () = Sil.pp_type_decl pe_text pp_name Sil.pp_exp f typ in
       let decl = pp_to_string pp () in
       s := if !s = "" then decl else !s ^ ", " ^ decl)
    summary.attributes.ProcAttributes.formals;
  let pp_procname f () = F.fprintf f "%a"
      Procname.pp summary.attributes.ProcAttributes.proc_name in
  let pp f () =
    Sil.pp_type_decl pe_text pp_procname Sil.pp_exp f summary.attributes.ProcAttributes.ret_type in
  let decl = pp_to_string pp () in
  decl ^ "(" ^ !s ^ ")"

let pp_summary_no_stats_specs fmt summary =
  let pp_pair fmt (x, y) = F.fprintf fmt "%s: %s" x y in
  F.fprintf fmt "%s@\n" (get_signature summary);
  F.fprintf fmt "%a@\n" pp_pair (describe_timestamp summary);
  F.fprintf fmt "%a@\n" pp_pair (describe_status summary);
  F.fprintf fmt "%a@\n" pp_pair (describe_phase summary);
  F.fprintf fmt "Dependency_map: @[%a@]@\n" pp_dependency_map summary.dependency_map

let pp_stats_html err_log fmt =
  Errlog.pp_html [] fmt err_log

let get_specs_from_payload summary =
  match summary.payload.preposts with
  | Some specs -> NormSpec.tospecs specs
  | None -> []

(** Print the summary *)
let pp_summary pe whole_seconds fmt summary =
  let err_log = summary.attributes.ProcAttributes.err_log in
  match pe.pe_kind with
  | PP_TEXT ->
      pp_summary_no_stats_specs fmt summary;
      F.fprintf fmt "%a@\n" (pp_stats err_log whole_seconds) summary.stats;
      F.fprintf fmt "%a" (pp_specs pe) (get_specs_from_payload summary)
  | PP_HTML ->
      Io_infer.Html.pp_start_color fmt Black;
      F.fprintf fmt "@\n%a" pp_summary_no_stats_specs summary;
      Io_infer.Html.pp_end_color fmt ();
      pp_stats_html err_log fmt;
      Io_infer.Html.pp_hline fmt ();
      F.fprintf fmt "<LISTING>@\n";
      pp_specs pe fmt (get_specs_from_payload summary);
      F.fprintf fmt "</LISTING>@\n"
  | PP_LATEX ->
      F.fprintf fmt "\\begin{verbatim}@\n";
      pp_summary_no_stats_specs fmt summary;
      F.fprintf fmt "%a@\n" (pp_stats err_log whole_seconds) summary.stats;
      F.fprintf fmt "\\end{verbatim}@\n";
      F.fprintf fmt "%a@\n" (pp_specs pe) (get_specs_from_payload summary)

(** Print the spec table *)
let pp_spec_table pe whole_seconds fmt () =
  Procname.Hash.iter (fun proc_name (summ, _) ->
      F.fprintf fmt "PROC %a@\n%a@\n" Procname.pp proc_name (pp_summary pe whole_seconds) summ
    ) spec_tbl

let empty_stats calls in_out_calls_opt =
  { stats_time = 0.0;
    stats_failure = None;
    stats_calls =
      (match in_out_calls_opt with
       | Some in_out_calls -> in_out_calls
       | None -> { Cg.in_calls = 0; Cg.out_calls = 0 });
    symops = 0;
    nodes_visited_fp = IntSet.empty;
    nodes_visited_re = IntSet.empty;
    call_stats = CallStats.init calls;
  }

let payload_compact sh payload =
  match payload.preposts with
  | Some specs ->
      { payload with
        preposts = Some (IList.map (NormSpec.compact sh) specs);
      }
  | None ->
      payload

(** Return a compact representation of the summary *)
let summary_compact sh summary =
  { summary with payload = payload_compact sh summary.payload }

let set_summary_origin proc_name summary origin =
  Procname.Hash.replace spec_tbl proc_name (summary, origin)

let add_summary_origin (proc_name : Procname.t) (summary: summary) (origin: origin) : unit =
  L.out "Adding summary for %a@\n@[<v 2>  %a@]@." Procname.pp proc_name (pp_summary pe_text false) summary;
  set_summary_origin proc_name summary origin

(** Add the summary to the table for the given function *)
let add_summary (proc_name : Procname.t) (summary: summary) : unit =
  add_summary_origin proc_name summary Res_dir

let specs_filename pname =
  let pname_file = Procname.to_filename pname in
  pname_file ^ ".specs"

(** path to the .specs file for the given procedure in the current results directory *)
let res_dir_specs_filename pname =
  DB.Results_dir.path_to_filename DB.Results_dir.Abs_root [Config.specs_dir_name; specs_filename pname]

(** paths to the .specs file for the given procedure in the current spec libraries *)
let specs_library_filenames pname =
  IList.map
    (fun specs_dir -> DB.filename_from_string (Filename.concat specs_dir (specs_filename pname)))
    !Config.specs_library

(** paths to the .specs file for the given procedure in the models folder *)
let specs_models_filename pname =
  DB.filename_from_string (Filename.concat Config.models_dir (specs_filename pname))

let summary_exists_in_models pname =
  Sys.file_exists (DB.filename_to_string (specs_models_filename pname))

let summary_serializer : summary Serialization.serializer =
  Serialization.create_serializer Serialization.summary_key

(** Save summary for the procedure into the spec database *)
let store_summary pname (summ: summary) =
  let process_payload payload = match payload.preposts with
    | Some specs ->
        { payload with
          preposts = Some (IList.map NormSpec.erase_join_info_pre specs);
        }
    | None -> payload in
  let summ1 = { summ with payload = process_payload summ.payload } in
  let summ2 = if !Config.save_compact_summaries
    then summary_compact (Sil.create_sharing_env ()) summ1
    else summ1 in
  let summ3 = if !Config.save_time_in_summaries
    then summ2
    else
      { summ2 with
        stats = { summ1.stats with stats_time = 0.0} } in
  Serialization.to_file summary_serializer (res_dir_specs_filename pname) summ3

(** Load procedure summary from the given file *)
let load_summary specs_file =
  Serialization.from_file summary_serializer specs_file

(** Load procedure summary from the given zip file *)
(* TODO: instead of always going through the same list for zip files for every proc_name, *)
(* create beforehand a map from specs filenames to zip filenames, so that looking up the specs for a given procedure is fast *)
let load_summary_from_zip zip_specs_path zip_channel =
  let found_summary =
    try
      let entry = Zip.find_entry zip_channel zip_specs_path in
      begin
        match Serialization.from_string summary_serializer (Zip.read_entry zip_channel entry) with
        | Some summ -> Some summ
        | None ->
            L.err "Could not load specs datastructure from %s@." zip_specs_path;
            None
      end
    with Not_found -> None in
  found_summary

(** Load procedure summary for the given procedure name and update spec table *)
let load_summary_to_spec_table proc_name =
  let add summ origin =
    add_summary_origin proc_name summ origin;
    true in
  let load_summary_models models_dir =
    match load_summary models_dir with
    | None -> false
    | Some summ -> add summ Models in
  let rec load_summary_libs = function (* try to load the summary from a list of libs *)
    | [] -> false
    | spec_path :: spec_paths ->
        (match load_summary spec_path with
         | None -> load_summary_libs spec_paths
         | Some summ ->
             add summ Spec_lib) in
  let rec load_summary_ziplibs zip_libraries = (* try to load the summary from a list of zip libraries *)
    let zip_specs_filename = specs_filename proc_name in
    let zip_specs_path =
      let root = Filename.concat Config.default_in_zip_results_dir Config.specs_dir_name in
      Filename.concat root zip_specs_filename in
    match zip_libraries with
    | [] -> false
    | zip_library:: zip_libraries ->
        begin
          match load_summary_from_zip zip_specs_path (Config.zip_channel zip_library) with
          | None -> load_summary_ziplibs zip_libraries
          | Some summ ->
              let origin = if zip_library.Config.models then Models else Spec_lib in
              add summ origin
        end in
  let default_spec_dir = res_dir_specs_filename proc_name in
  match load_summary default_spec_dir with
  | None ->
      (* search on models, libzips, and libs *)
      if load_summary_models (specs_models_filename proc_name) then true
      else if load_summary_ziplibs !Config.zip_libraries then true
      else load_summary_libs (specs_library_filenames proc_name)

  | Some summ ->
      add summ Res_dir

let rec get_summary_origin proc_name =
  try
    Some (Procname.Hash.find spec_tbl proc_name)
  with Not_found ->
    if load_summary_to_spec_table proc_name then
      get_summary_origin proc_name
    else None

let get_summary proc_name =
  match get_summary_origin proc_name with
  | Some (summary, _) -> Some summary
  | None -> None

let get_summary_unsafe s proc_name =
  match get_summary proc_name with
  | None ->
      raise (Failure (
          "[" ^ s ^ "] Specs.get_summary_unsafe: " ^ (Procname.to_string proc_name) ^ "Not_found"))
  | Some summary -> summary

(** Check if the procedure is from a library:
    It's not defined, and there is no spec file for it. *)
let proc_is_library proc_attributes =
  if not proc_attributes.ProcAttributes.is_defined then
    match get_summary proc_attributes.ProcAttributes.proc_name with
    | None -> true
    | Some _ -> false
  else false

(** Try to find the attributes for a defined proc.
    First look at specs (to get attributes computed by analysis)
    then look at the attributes table.
    If no attributes can be found, return None.
*)
let proc_resolve_attributes proc_name =
  let from_attributes_table () =
    AttributesTable.load_attributes proc_name in
  let from_specs () = match get_summary proc_name with
    | Some summary ->
        Some summary.attributes
    | None -> None in
  match from_specs () with
  | Some attributes ->
      if attributes.ProcAttributes.is_defined
      then Some attributes
      else begin
        match from_attributes_table () with
        | Some attributes' ->
            Some attributes'
        | None ->
            Some attributes
      end
  | None ->
      from_attributes_table ()

(** Like proc_resolve_attributes but start from a proc_desc. *)
let pdesc_resolve_attributes proc_desc =
  let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
  match proc_resolve_attributes proc_name with
  | Some proc_attributes ->
      proc_attributes
  | None ->
      (* this should not happen *)
      assert false

let get_origin proc_name =
  match get_summary_origin proc_name with
  | Some (_, origin) -> origin
  | None -> Res_dir

let summary_exists proc_name =
  match get_summary proc_name with
  | Some _ -> true
  | None -> false

let get_status summary =
  summary.status

let is_active proc_name =
  get_status (get_summary_unsafe "is_active" proc_name) = ACTIVE

let is_inactive proc_name =
  get_status (get_summary_unsafe "is_active" proc_name) = INACTIVE

let get_timestamp summary =
  summary.timestamp

let get_proc_name summary =
  summary.attributes.ProcAttributes.proc_name

let get_ret_type summary =
  summary.attributes.ProcAttributes.ret_type

let get_formals summary =
  summary.attributes.ProcAttributes.formals

let get_attributes summary =
  summary.attributes

(** Get the flag with the given key for the procedure, if any *)
(* TODO get_flag should get a summary as parameter *)
let get_flag proc_name key =
  match get_summary proc_name with
  | None -> None
  | Some summary ->
      let proc_flags = summary.attributes.ProcAttributes.proc_flags in
      try
        Some (Hashtbl.find proc_flags key)
      with Not_found -> None

(** Return the specs and parameters for the proc in the spec table *)
let get_specs_formals proc_name =
  match get_summary proc_name with
  | None ->
      raise (Failure ("Specs.get_specs_formals: " ^ (Procname.to_string proc_name) ^ "Not_found"))
  | Some summary ->
      let specs = get_specs_from_payload summary in
      let formals = get_formals summary in
      (specs, formals)

(** Return the specs for the proc in the spec table *)
let get_specs proc_name =
  fst (get_specs_formals proc_name)

(** Return the current phase for the proc *)
let get_phase proc_name =
  match get_summary_origin proc_name with
  | None -> raise (Failure ("Specs.get_phase: " ^ (Procname.to_string proc_name) ^ " Not_found"))
  | Some (summary, _) -> summary.phase

(** Set the current status for the proc *)
let set_status proc_name status =
  match get_summary_origin proc_name with
  | None -> raise (Failure ("Specs.set_status: " ^ (Procname.to_string proc_name) ^ " Not_found"))
  | Some (summary, origin) -> set_summary_origin proc_name { summary with status = status } origin

(** Create the initial dependency map with the given list of dependencies *)
let mk_initial_dependency_map proc_list : dependency_map_t =
  IList.fold_left (fun map pname -> Procname.Map.add pname (- 1) map) Procname.Map.empty proc_list

(** Re-initialize a dependency map *)
let re_initialize_dependency_map dependency_map =
  Procname.Map.map (fun _ -> - 1) dependency_map

(** Update the dependency map of [proc_name] with the current
    timestamps of the dependents *)
let update_dependency_map proc_name =
  match get_summary_origin proc_name with
  | None ->
      raise
        (Failure ("Specs.update_dependency_map: " ^ (Procname.to_string proc_name) ^ " Not_found"))
  | Some (summary, origin) ->
      let current_dependency_map =
        Procname.Map.mapi
          (fun _ _ -> get_timestamp summary)
          summary.dependency_map in
      set_summary_origin proc_name { summary with dependency_map = current_dependency_map } origin

let empty_payload =
  {
    preposts = None;
    typestate = None;
    calls = None;
  }

(** [init_summary (depend_list, nodes,
    proc_flags, calls, in_out_calls_opt, proc_attributes)]
    initializes the summary for [proc_name] given dependent procs in list [depend_list]. *)
let init_summary
    (depend_list, nodes,
     proc_flags, calls, in_out_calls_opt,
     proc_attributes) =
  let dependency_map = mk_initial_dependency_map depend_list in
  let summary =
    {
      dependency_map = dependency_map;
      nodes = nodes;
      phase = FOOTPRINT;
      sessions = ref 0;
      payload = empty_payload;
      stats = empty_stats calls in_out_calls_opt;
      status = INACTIVE;
      timestamp = 0;
      attributes =
        { proc_attributes with
          ProcAttributes.proc_flags = proc_flags; };
    } in
  Procname.Hash.replace spec_tbl proc_attributes.ProcAttributes.proc_name (summary, Res_dir)

(** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)
let reset_summary call_graph proc_name attributes_opt =
  let dependents = Cg.get_defined_children call_graph proc_name in
  let proc_attributes = match attributes_opt with
    | Some attributes ->
        attributes
    | None ->
        ProcAttributes.default proc_name !Config.curr_language in
  init_summary (
    Procname.Set.elements dependents,
    [],
    proc_flags_empty (),
    [],
    Some (Cg.get_calls call_graph proc_name),
    proc_attributes
  )

(* =============== END of support for spec tables =============== *)

(*
let rec post_equal pl1 pl2 = match pl1, pl2 with
  | [],[] -> true
  | [], _:: _ -> false
  | _:: _,[] -> false
  | p1:: pl1', p2:: pl2' ->
      if Prop.prop_equal p1 p2 then post_equal pl1' pl2'
      else false
*)
