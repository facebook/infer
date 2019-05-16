(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

module type NodeSig = sig
  type t = private {id: int; pname: Typ.Procname.t; successors: int list; mutable flag: bool}

  val make : int -> Typ.Procname.t -> int list -> t

  val set_flag : t -> unit

  val unset_flag : t -> unit

  val pp_dot : F.formatter -> t -> unit
end

module Node : NodeSig = struct
  type t = {id: int; pname: Typ.Procname.t; successors: int list; mutable flag: bool}

  let make id pname successors = {id; pname; successors; flag= false}

  let set_flag n = n.flag <- true

  let unset_flag n = n.flag <- false

  let pp_dot fmt {id; pname; successors} =
    let pp_id fmt id = F.fprintf fmt "N%d" id in
    let pp_edge fmt src dst = F.fprintf fmt "  %a -> %a ;@\n" pp_id src pp_id dst in
    F.fprintf fmt "  %a [ label = %S ];@\n" pp_id id (F.asprintf "%a" Typ.Procname.pp pname) ;
    List.iter successors ~f:(pp_edge fmt id) ;
    F.pp_print_newline fmt ()
end

module IdMap = Typ.Procname.Hash
module NodeMap = Caml.Hashtbl.Make (Int)

(** [node_map] is a map from ids (unique ints) to nodes corresponding to defined procedures. 
    [id_map] is a map from all encountered (not necessarily defined) procnames to their ids, 
    and thus its image is a superset of the domain of [node_map], and usually a strict superset.
    [trim_id_map] makes the image equal to the domain of [node_map]. *)
type t = {id_map: int IdMap.t; node_map: Node.t NodeMap.t}

let create initial_capacity =
  {id_map= IdMap.create initial_capacity; node_map= NodeMap.create initial_capacity}


let id_of_procname {id_map} pname = IdMap.find_opt id_map pname

let node_of_id {node_map} id = NodeMap.find_opt node_map id

let mem {node_map} id = NodeMap.mem node_map id

(** [id_map] may contain undefined procedures, so use [node_map] for actual size *)
let length {node_map} = NodeMap.length node_map

let node_of_procname g pname = id_of_procname g pname |> Option.bind ~f:(node_of_id g)

let remove (g : t) pname id = IdMap.remove g.id_map pname ; NodeMap.remove g.node_map id

let add ({id_map; node_map} as graph) pname successor_pnames =
  let get_or_set_id procname =
    match id_of_procname graph procname with
    | None ->
        let id = IdMap.length id_map in
        IdMap.replace id_map procname id ; id
    | Some id ->
        id
  in
  let id = get_or_set_id pname in
  let successors = List.map successor_pnames ~f:get_or_set_id in
  let node = Node.make id pname successors in
  NodeMap.replace node_map id node


let remove_reachable g start_pname =
  let add_live_successors_and_remove_self init (n : Node.t) =
    remove g n.pname n.id ;
    List.fold n.successors ~init ~f:(fun init succ_id ->
        node_of_id g succ_id |> Option.fold ~init ~f:(fun acc s -> s :: acc) )
  in
  let rec remove_list frontier =
    if not (List.is_empty frontier) then
      remove_list (List.fold frontier ~init:[] ~f:add_live_successors_and_remove_self)
  in
  node_of_procname g start_pname |> Option.iter ~f:(fun start_node -> remove_list [start_node])


let flag_reachable g start_pname =
  let process_node init (n : Node.t) =
    if n.flag then init
    else (
      Node.set_flag n ;
      List.fold n.successors ~init ~f:(fun acc id ->
          match node_of_id g id with Some n' when not n'.flag -> n' :: acc | _ -> acc ) )
  in
  let rec flag_list frontier =
    if not (List.is_empty frontier) then flag_list (List.fold frontier ~init:[] ~f:process_node)
  in
  node_of_procname g start_pname |> Option.iter ~f:(fun start_node -> flag_list [start_node])


let remove_unflagged_and_unflag_all {id_map; node_map} =
  NodeMap.filter_map_inplace
    (fun _id (n : Node.t) ->
      if n.flag then ( Node.unset_flag n ; Some n ) else ( IdMap.remove id_map n.pname ; None ) )
    node_map


(** remove pnames for all undefined procedures *)
let trim_id_map (g : t) =
  IdMap.filter_map_inplace (fun _pname id -> Option.some_if (mem g id) id) g.id_map


let pp_dot fmt {node_map} =
  F.fprintf fmt "@\ndigraph callgraph {@\n" ;
  NodeMap.iter (fun _id n -> Node.pp_dot fmt n) node_map ;
  F.fprintf fmt "}@."


let to_dotty g =
  let outc = Filename.concat Config.results_dir "callgraph.dot" |> Out_channel.create in
  let fmt = F.formatter_of_out_channel outc in
  pp_dot fmt g ; Out_channel.close outc


let build_from_captured_procs g =
  let hashcons_pname =
    let pname_tbl : Typ.Procname.t IdMap.t = IdMap.create 1001 in
    fun pname ->
      match IdMap.find_opt pname_tbl pname with
      | Some pname' ->
          pname'
      | None ->
          IdMap.add pname_tbl pname pname ; pname
  in
  let db = ResultsDatabase.get_database () in
  let stmt = Sqlite3.prepare db "SELECT proc_name, callees FROM procedures" in
  SqliteUtils.result_fold_rows db ~log:"creating call graph" stmt ~init:() ~f:(fun () stmt ->
      let proc_name = Sqlite3.column stmt 0 |> Typ.Procname.SQLite.deserialize |> hashcons_pname in
      let callees =
        Sqlite3.column stmt 1 |> Typ.Procname.SQLiteList.deserialize |> List.map ~f:hashcons_pname
      in
      add g proc_name callees )


let build_from_sources g sources =
  let time0 = Mtime_clock.counter () in
  L.progress "Building call graph...@\n%!" ;
  build_from_captured_procs g ;
  let captured_length = length g in
  List.iter sources ~f:(fun sf ->
      SourceFiles.proc_names_of_source sf |> List.iter ~f:(flag_reachable g) ) ;
  remove_unflagged_and_unflag_all g ;
  trim_id_map g ;
  if Config.debug_level_analysis > 0 then to_dotty g ;
  L.progress "Building call graph took %a@\n" Mtime.Span.pp (Mtime_clock.count time0) ;
  L.progress
    "Constructed call graph from %d total procs, %d reachable defined procs, and takes %d bytes@."
    captured_length (length g)
    (Obj.(reachable_words (repr g)) * (Sys.word_size / 8))


let get_unflagged_leaves g =
  NodeMap.fold
    (fun _id (n : Node.t) acc ->
      if n.flag || List.exists n.successors ~f:(mem g) then acc else n :: acc )
    g.node_map []
