(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module F = Format
open Utils

(** Module to process clusters of procedures. *)

(** if true, print tracing information for functions that manipulate clusters *)
let trace_clusters = ref false

(** cluster element: the file name, the number of procedures defined in it,
    and the list of active procedures.
    A procedure is active if it is defined only in this file,
    or if it is defined in several files and this
    is the representative file for it (see Exe_env.add_cg) *)
type elem =
  {
    ce_active_procs : Procname.t list; (** list of active procedures *)
    ce_file : DB.source_file;
    ce_naprocs : int; (** number of active procedures defined in the file *)
    ce_ondemand : DB.source_dir option; (** if present, the other fields are unused *)
  }

(** cluster of files *)
type t = elem list

(** type stored in .cluster file: (n,m,cl) indicates cl is cluster n out of m *)
type serializer_t = int * int * t

(** Serializer for clusters *)
let serializer : serializer_t Serialization.serializer =
  Serialization.create_serializer Serialization.cluster_key

(** Load a cluster from a file *)
let load_from_file (filename : DB.filename) : serializer_t option =
  Serialization.from_file serializer filename

(** Save a cluster into a file *)
let store_to_file (filename : DB.filename) (serializer_t: serializer_t) =
  Serialization.to_file serializer filename serializer_t

let get_ondemand_info ce =
  ce.ce_ondemand

let one_cluster_per_procedure =  true

let create_ondemand source_dir =
  let defined_procs_opt =
    if Ondemand.one_cluster_per_procedure () then
      let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
      match Cg.load_from_file cg_fname with
      | None -> None
      | Some cg ->
          Some (Cg.get_defined_nodes cg)
    else
      None in
  let ce =
    {
      ce_active_procs = [];
      ce_file = DB.source_file_from_string "";
      ce_naprocs = 0;
      ce_ondemand = Some source_dir;
    } in
  let mk_cluster pname =
    [{ce with ce_active_procs = [pname]}] in
  let clusters = match  defined_procs_opt with
    | None ->
        [[ce]]
    | Some defined_procs ->
        IList.map mk_cluster defined_procs in
  clusters

let create_bottomup source_file naprocs active_procs =
  {
    ce_active_procs = active_procs;
    ce_file = source_file;
    ce_naprocs = naprocs;
    ce_ondemand = None;
  }

let cluster_nfiles cluster = IList.length cluster

let cluster_naprocs cluster =
  IList.fold_left (fun n ce -> ce.ce_naprocs + n) 0 cluster

let clusters_nfiles clusters =
  IList.fold_left (fun n cluster -> cluster_nfiles cluster + n) 0 clusters

let clusters_naprocs clusters =
  IList.fold_left (fun n cluster -> cluster_naprocs cluster + n) 0 clusters

let print_clusters_stats clusters =
  let pp_cluster num cluster =
    L.err "cluster #%d files: %d active procedures: %d@."
      num
      (cluster_nfiles cluster)
      (cluster_naprocs cluster) in
  let i = ref 0 in
  IList.iter
    (fun cluster ->
       incr i;
       pp_cluster !i cluster)
    clusters

let cluster_split_prefix (cluster : t) size =
  let rec split (cluster_seen : t) (cluster_todo : t) n =
    if n <= 0 then (IList.rev cluster_seen, cluster_todo)
    else match cluster_todo with
      | [] -> raise Not_found
      | ce :: todo' -> split (ce :: cluster_seen) todo' (n - ce.ce_naprocs) in
  split [] cluster size

let combine_split_clusters (clusters : t list) max_size desired_size =
  if !trace_clusters then L.err "[combine_split_clusters]@.";
  let old_clusters = ref clusters in
  let old_size = clusters_naprocs !old_clusters in
  let new_clusters = ref [] in
  let current = ref [] in
  let current_size = ref 0 in
  while !old_clusters != [] do
    if old_size !=
       clusters_naprocs !old_clusters + clusters_naprocs !new_clusters + !current_size
    then begin
      L.err "mismatch in invariant for cluster size@.";
      assert (cluster_naprocs !current = !current_size);
      L.err "old size: %d@." old_size;
      L.err "old clusters size: %d@." (clusters_naprocs !old_clusters);
      L.err "new clusters size: %d@." (clusters_naprocs !new_clusters);
      L.err "current size: %d@." !current_size;
      assert false
    end;
    let next_cluster = IList.hd !old_clusters in
    let next_size = cluster_naprocs next_cluster in
    let new_size = !current_size + next_size in
    if (new_size > max_size || new_size > desired_size) && !current_size > 0 then
      begin
        new_clusters := !new_clusters @ [!current];
        current := [];
        current_size := 0
      end
    else if new_size > max_size then
      begin
        let next_cluster', next_cluster'' = cluster_split_prefix next_cluster max_size in
        current := [];
        current_size := 0;
        new_clusters := !new_clusters @ [next_cluster'];
        old_clusters := next_cluster'' :: (IList.tl !old_clusters)
      end
    else
      begin
        current := !current @ next_cluster;
        current_size := !current_size + next_size;
        old_clusters := IList.tl !old_clusters
      end
  done;
  if !current_size > 0 then new_clusters := !new_clusters @ [!current];
  !new_clusters

(** return the set of active procedures in a cluster *)
let get_active_procs cluster =
  match !Config.ondemand_enabled, cluster with
  | true, [{ce_active_procs = []}] ->
      None
  | _ ->
      let procset = ref Procname.Set.empty in
      let do_cluster_elem cluster_elem =
        let add proc =
          if not (Procname.Set.mem proc !procset) then
            procset := Procname.Set.add proc !procset in
        IList.iter add cluster_elem.ce_active_procs in
      IList.iter do_cluster_elem cluster;
      Some !procset

let cl_name n = "cl" ^ string_of_int n
let cl_file n = "x" ^ (cl_name n) ^ ".cluster"
let pp_cl fmt n = Format.fprintf fmt "%s" (cl_name n)

let pp_cluster_dependency nr tot_nr cluster print_files fmt dependents =
  let fname = cl_file nr in
  let pp_cl fmt n = Format.fprintf fmt "%s" (cl_name n) in
  store_to_file (DB.filename_from_string fname) (nr, tot_nr, cluster);
  let pp_active_procs fmt cluster =
    let procnames = match get_active_procs cluster with
      | None ->
          []
      | Some procset ->
          Procname.Set.elements procset in
    let pp_pname fmt pname = Format.fprintf fmt "%s" (Procname.to_string pname) in
    F.fprintf fmt "procedures: %a" (pp_seq pp_pname) procnames in
  let pp_file fmt ce = F.fprintf fmt "%s" (DB.source_file_to_string ce.ce_file) in
  let pp_files fmt cluster = F.fprintf fmt "files: %a" (pp_seq pp_file) cluster in
  F.fprintf fmt "%a : %a@\n" pp_cl nr (pp_seq pp_cl) dependents;
  F.fprintf fmt "\t$(INFERANALYZE) -cluster %s >%a@\n" fname pp_cl nr;
  if print_files then F.fprintf fmt "# %a %a" pp_files cluster pp_active_procs cluster;
  F.fprintf fmt "@\n"
