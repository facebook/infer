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

(** Execution Paths *)

module L = Logging
module F = Format

(* =============== START of the Path module ===============*)

module Path : sig
  (** type for paths *)
  type t

  type session = int

  (** add a call with its sub-path, the boolean indicates whether the subtrace for the procedure should be included *)
  val add_call : bool -> t -> Procname.t -> t -> t

  (** check whether a path contains another path *)
  val contains : t -> t -> bool

  (** check wether the path contains the given position *)
  val contains_position : t -> PredSymb.path_pos -> bool

  (** Create the location trace of the path, up to the path position if specified *)
  val create_loc_trace : t -> PredSymb.path_pos option -> Errlog.loc_trace

  (** return the current node of the path *)
  val curr_node : t -> Cfg.node option

  (** dump a path *)
  val d : t -> unit

  (** dump statistics of the path *)
  val d_stats : t -> unit

  (** extend a path with a new node reached from the given session, with an optional string for exceptions *)
  val extend : Cfg.node -> Typename.t option -> session -> t -> t

  (** extend a path with a new node reached from the given session, with an optional string for exceptions *)
  val add_description : t -> string -> t

  (** iterate over each node in the path, excluding calls, once *)
  val iter_all_nodes_nocalls : (Cfg.node -> unit) -> t -> unit

  (** iterate over the longest sequence belonging to the path, restricting to those containing the given position if given.
      Do not iterate past the given position.
      [f level path session exn_opt] is passed the current nesting [level] and [path] and previous [session] *)
  val iter_longest_sequence :
    (int -> t -> int -> Typename.t option -> unit) -> PredSymb.path_pos option -> t -> unit

  (** join two paths *)
  val join : t -> t -> t

  (** pretty print a path *)
  val pp : Format.formatter -> t -> unit

  (** pretty print statistics of the path *)
  val pp_stats : Format.formatter -> t -> unit

  (** create a new path with given start node *)
  val start : Cfg.node -> t

(*
  (** equality for paths *)
  val equal : t -> t -> bool

  val get_description : t -> string option
*)
end = struct
  type session = int
  type stats =
    { mutable max_length : int; (* length of the longest linear sequence *)
      mutable linear_num : float; (* number of linear sequences described by the path *) }

  type path =
    (* INVARIANT: stats are always set to dummy_stats unless we are in the middle of a traversal *)
    (* in particular: a new traversal cannot be initiated during an existing traversal *)
    | Pstart of Cfg.node * stats (** start node *)
    | Pnode of Cfg.node * Typename.t option * session * path * stats * string option
    (** we got to [node] from last [session] perhaps propagating exception [exn_opt],
        and continue with [path].  *)
    | Pjoin of path * path * stats (** join of two paths *)
    | Pcall of path * Procname.t * path * stats (** add a sub-path originating from a call *)

  type t = path

  let get_dummy_stats () =
    { max_length = - 1;
      linear_num = - 1.0 }

  let get_description path =
    match path with
    | Pnode (_, _, _, _, _, descr_opt) ->
        descr_opt
    | _ -> None

  let add_description path description =
    let add_descr descr_option description =
      match descr_option with
      | Some descr -> descr^" "^description
      | None -> description in
    match path with
    | Pnode (node, exn_opt, session, path, stats, descr_opt) ->
        let description = add_descr descr_opt description in
        Pnode (node, exn_opt, session, path, stats, Some description)
    | _ -> path

  let set_dummy_stats stats =
    stats.max_length <- - 1;
    stats.linear_num <- - 1.0

  let rec curr_node = function
    | Pstart (node, _) -> Some node
    | Pnode (node, _, _, _, _, _) -> Some node
    | Pcall(p1, _, _, _) -> curr_node p1
    | Pjoin _ ->
        None

  let exname_opt_compare eo1 eo2 = match eo1, eo2 with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some n1, Some n2 -> Typename.compare n1 n2

  let rec compare p1 p2 : int =
    if p1 == p2 then 0 else match p1, p2 with
      | Pstart (n1, _), Pstart (n2, _) ->
          Cfg.Node.compare n1 n2
      | Pstart _, _ -> - 1
      | _, Pstart _ -> 1
      | Pnode (n1, eo1, s1, p1, _, _), Pnode (n2, eo2, s2, p2, _, _) ->
          let n = Cfg.Node.compare n1 n2 in
          if n <> 0 then n else let n = exname_opt_compare eo1 eo2 in
            if n <> 0 then n else let n = int_compare s1 s2 in
              if n <> 0 then n else compare p1 p2
      | Pnode _, _ -> - 1
      | _, Pnode _ -> 1
      | Pjoin (p1, q1, _), Pjoin (p2, q2, _) ->
          let n = compare p1 p2 in
          if n <> 0 then n else compare q1 q2
      | Pjoin _, _ -> -1
      | _, Pjoin _ -> 1
      | Pcall(p1, _, sub1, _), Pcall(p2, _, sub2, _) ->
          let n = compare p1 p2 in
          if n <> 0 then n else compare sub1 sub2

  let start node = Pstart (node, get_dummy_stats ())

  let extend (node: Cfg.node) exn_opt session path =
    Pnode (node, exn_opt, session, path, get_dummy_stats (), None)

  let join p1 p2 =
    Pjoin (p1, p2, get_dummy_stats ())

  let add_call include_subtrace p pname p_sub =
    if include_subtrace then Pcall(p, pname, p_sub, get_dummy_stats ())
    else p

  (** functions in this module either do not assume, or do not re-establish, the invariant on dummy
      stats *)
  module Invariant = struct
    (** check whether a stats is the dummy stats *)
    let stats_is_dummy stats =
      stats.max_length == - 1

    (** return the stats of the path, assumes that the stats are computed *)
    let get_stats = function
      | Pstart (_, stats) -> stats
      | Pnode (_, _, _, _, stats, _) -> stats
      | Pjoin (_, _, stats) -> stats
      | Pcall (_, _, _, stats) -> stats

    (** restore the invariant that all the stats are dummy, so the path is ready for another
        traversal assumes that the stats are computed beforehand, and ensures that the invariant
        holds afterwards *)
    let rec reset_stats = function
      | Pstart (_, stats) ->
          if not (stats_is_dummy stats) then set_dummy_stats stats
      | Pnode (_, _, _, path, stats, _) ->
          if not (stats_is_dummy stats) then
            begin
              reset_stats path;
              set_dummy_stats stats
            end
      | Pjoin (path1, path2, stats) ->
          if not (stats_is_dummy stats) then
            begin
              reset_stats path1;
              reset_stats path2;
              set_dummy_stats stats
            end
      | Pcall (path1, _, path2, stats) ->
          if not (stats_is_dummy stats) then
            begin
              reset_stats path1;
              reset_stats path2;
              set_dummy_stats stats
            end

    (** Iterate [f] over the path and compute the stats, assuming the invariant: all the stats are
        dummy.  Function [f] (typically with side-effects) is applied once to every node, and
        max_length in the stats is the length of a longest sequence of nodes in the path where [f]
        returned [true] on at least one node.  max_length is 0 if the path was visited but no node
        satisfying [f] was found.  Assumes that the invariant holds beforehand, and ensures that all
        the stats are computed afterwards.  Since this breaks the invariant, it must be followed by
        reset_stats. *)
    let rec compute_stats do_calls (f : Cfg.Node.t -> bool) =
      let nodes_found stats = stats.max_length > 0 in
      function
      | Pstart (node, stats) ->
          if stats_is_dummy stats then
            begin
              let found = f node in
              stats.max_length <- if found then 1 else 0;
              stats.linear_num <- 1.0;
            end
      | Pnode (node, _, _, path, stats, _) ->
          if stats_is_dummy stats then
            begin
              compute_stats do_calls f path;
              let stats1 = get_stats path in
              let found = f node || nodes_found stats1 (* the order is important as f has side-effects *) in
              stats.max_length <- if found then 1 + stats1.max_length else 0;
              stats.linear_num <- stats1.linear_num;
            end
      | Pjoin (path1, path2, stats) ->
          if stats_is_dummy stats then
            begin
              compute_stats do_calls f path1;
              compute_stats do_calls f path2;
              let stats1, stats2 = get_stats path1, get_stats path2 in
              stats.max_length <- max stats1.max_length stats2.max_length;
              stats.linear_num <- stats1.linear_num +. stats2.linear_num
            end
      | Pcall (path1, _, path2, stats) ->
          if stats_is_dummy stats then
            begin
              let stats2 = match do_calls with
                | true ->
                    compute_stats do_calls f path2;
                    get_stats path2
                | false ->
                    { max_length = 0;
                      linear_num = 0.0 } in
              let stats1 =
                let f' =
                  if nodes_found stats2
                  then fun _ -> true (* already found in call, no need to search before the call *)
                  else f in
                compute_stats do_calls f' path1;
                get_stats path1 in
              stats.max_length <- stats1.max_length + stats2.max_length;
              stats.linear_num <- stats1.linear_num;
            end
  end (* End of module Invariant *)

  (** iterate over each node in the path, excluding calls, once *)
  let iter_all_nodes_nocalls f path =
    Invariant.compute_stats false (fun node -> f node; true) path;
    Invariant.reset_stats path

  let get_path_pos node =
    let pn = Cfg.Procdesc.get_proc_name (Cfg.Node.get_proc_desc node) in
    let n_id = Cfg.Node.get_id node in
    (pn, (n_id :> int))

  let contains_position path pos =
    let found = ref false in
    let f node =
      if PredSymb.path_pos_equal (get_path_pos node) pos then found := true;
      true in
    Invariant.compute_stats true f path;
    Invariant.reset_stats path;
    !found

  (** iterate over the longest sequence belonging to the path, restricting to those where [filter] holds of some element.
      if a node is reached via an exception, pass the exception information to [f] on the previous node *)
  let iter_longest_sequence_filter
      (f : int -> t -> int -> Typename.t option -> unit)
      (filter: Cfg.Node.t -> bool) (path: t) : unit =
    let rec doit level session path prev_exn_opt = match path with
      | Pstart _ -> f level path session prev_exn_opt
      | Pnode (_, exn_opt, session', p, _, _) ->
          (* no two consecutive exceptions *)
          let next_exn_opt = if prev_exn_opt <> None then None else exn_opt in
          doit level (session' :> int) p next_exn_opt;
          f level path session prev_exn_opt
      | Pjoin (p1, p2, _) ->
          if (Invariant.get_stats p1).max_length >= (Invariant.get_stats p2).max_length then
            doit level session p1 prev_exn_opt
          else
            doit level session p2 prev_exn_opt
      | Pcall (p1, _, p2, _) ->
          let next_exn_opt = None in (* exn must already be inside the call *)
          doit level session p1 next_exn_opt;
          doit (level +1) session p2 next_exn_opt in
    Invariant.compute_stats true filter path;
    doit 0 0 path None;
    Invariant.reset_stats path

  (** iterate over the longest sequence belonging to the path, restricting to those containing the given position if given.
      Do not iterate past the last occurrence of the given position.
      [f level path session exn_opt] is passed the current nesting [level] and [path] and previous [session] and possible exception [exn_opt] *)
  let iter_longest_sequence
      (f : int -> t -> int -> Typename.t option -> unit)
      (pos_opt : PredSymb.path_pos option) (path: t) : unit =
    let filter node = match pos_opt with
      | None -> true
      | Some pos -> PredSymb.path_pos_equal (get_path_pos node) pos in
    let path_pos_at_path p =
      try
        match curr_node p with
        | Some node ->
            pos_opt <> None && filter node
        | None -> false
      with exn when SymOp.exn_not_failure exn -> false in
    let position_seen = ref false in
    let inverse_sequence =
      let log = ref [] in
      let g level p session exn_opt =
        if path_pos_at_path p then position_seen := true;
        log := (level, p, session, exn_opt) :: !log in
      iter_longest_sequence_filter g filter path;
      !log in
    let sequence_up_to_last_seen =
      if !position_seen then
        let rec remove_until_seen = function
          | ((_, p, _, _) as x):: l ->
              if path_pos_at_path p then IList.rev (x :: l)
              else remove_until_seen l
          | [] -> [] in
        remove_until_seen inverse_sequence
      else IList.rev inverse_sequence in
    IList.iter (fun (level, p, session, exn_opt) -> f level p session exn_opt) sequence_up_to_last_seen

  module NodeMap = Map.Make (Cfg.Node)

  (** return the node visited most, and number of visits, in the longest linear sequence *)
  let repetitions path =
    let map = ref NodeMap.empty in
    let add_node = function
      | Some node ->
          begin
            try
              let n = NodeMap.find node !map in
              map := NodeMap.add node (n + 1) !map
            with Not_found ->
              map := NodeMap.add node 1 !map
          end
      | None ->
          () in
    iter_longest_sequence (fun _ p _ _ -> add_node (curr_node p)) None path;
    let max_rep_node = ref (Cfg.Node.dummy ()) in
    let max_rep_num = ref 0 in
    NodeMap.iter (fun node num -> if num > !max_rep_num then (max_rep_node := node; max_rep_num := num)) !map;
    (!max_rep_node, !max_rep_num)

  let stats_string path =
    Invariant.compute_stats true (fun _ -> true) path;
    let node, repetitions = repetitions path in
    let str =
      "linear paths: " ^ string_of_float (Invariant.get_stats path).linear_num ^
      " max length: " ^ string_of_int (Invariant.get_stats path).max_length ^
      " has repetitions: " ^ string_of_int repetitions ^
      " of node " ^ (string_of_int (Cfg.Node.get_id node :> int)) in
    Invariant.reset_stats path;
    str

  let pp_stats fmt path =
    F.fprintf fmt "%s" (stats_string path)

  let d_stats path =
    L.d_str (stats_string path)

  module PathMap = Map.Make (struct
      type t = path
      let compare = compare
    end)

  let pp fmt path =
    let delayed_num = ref 0 in
    let delayed = ref PathMap.empty in
    let add_path p =
      try ignore (PathMap.find p !delayed) with Not_found ->
        incr delayed_num;
        delayed := PathMap.add p !delayed_num !delayed in
    let path_seen p = (* path seen before *)
      PathMap.mem p !delayed in
    let rec add_delayed path =
      if not (path_seen path) (* avoid exponential blowup *)
      then match path with (* build a map from delayed paths to a unique number *)
        | Pstart _ -> ()
        | Pnode (_, _, _, p, _, _) -> add_delayed p
        | Pjoin (p1, p2, _) | Pcall(p1, _, p2, _) -> (* delay paths occurring in a join *)
            add_delayed p1;
            add_delayed p2;
            add_path p1;
            add_path p2 in
    let rec doit n fmt path =
      try
        if n > 0 then raise Not_found;
        let num = PathMap.find path !delayed in
        F.fprintf fmt "P%d" num
      with Not_found ->
      match path with
      | Pstart (node, _) ->
          F.fprintf fmt "n%a" Cfg.Node.pp node
      | Pnode (node, _, session, path, _, _) ->
          F.fprintf fmt "%a(s%d).n%a" (doit (n - 1)) path (session :> int) Cfg.Node.pp node
      | Pjoin (path1, path2, _) ->
          F.fprintf fmt "(%a + %a)" (doit (n - 1)) path1 (doit (n - 1)) path2
      | Pcall (path1, _, path2, _) ->
          F.fprintf fmt "(%a{%a})" (doit (n - 1)) path1 (doit (n - 1)) path2 in
    let print_delayed () =
      if not (PathMap.is_empty !delayed) then begin
        let f path num = F.fprintf fmt "P%d = %a@\n" num (doit 1) path in
        F.fprintf fmt "where@\n";
        PathMap.iter f !delayed
      end in
    add_delayed path;
    doit 0 fmt path;
    print_delayed ()

  let d p =
    L.add_print_action (L.PTpath, Obj.repr p)

  let rec contains p1 p2 = match p2 with
    | Pjoin (p2', p2'', _) ->
        contains p1 p2' || contains p1 p2''
    | _ -> p1 == p2

  let create_loc_trace path pos_opt : Errlog.loc_trace =
    let trace = ref [] in
    let mk_trace_elem level loc descr node_tags =
      { Errlog.lt_level = level;
        Errlog.lt_loc = loc;
        Errlog.lt_description = descr;
        Errlog.lt_node_tags = node_tags } in
    let g level path _ exn_opt =
      match curr_node path with
      | Some curr_node ->
          begin
            let curr_loc = Cfg.Node.get_loc curr_node in
            match Cfg.Node.get_kind curr_node with
            | Cfg.Node.Join_node -> () (* omit join nodes from error traces *)
            | Cfg.Node.Start_node pname ->
                let name = Procname.to_string pname in
                let name_id = Procname.to_filename pname in
                let descr = "start of procedure " ^ (Procname.to_simplified_string pname) in
                let node_tags =
                  [(Io_infer.Xml.tag_kind,"procedure_start");
                   (Io_infer.Xml.tag_name, name);
                   (Io_infer.Xml.tag_name_id, name_id)] in
                trace := mk_trace_elem level curr_loc descr node_tags :: !trace
            | Cfg.Node.Prune_node (is_true_branch, if_kind, _) ->
                let descr = match is_true_branch, if_kind with
                  | true, Sil.Ik_if -> "Taking true branch"
                  | false, Sil.Ik_if -> "Taking false branch"
                  | true, (Sil.Ik_for | Sil.Ik_while | Sil.Ik_dowhile) ->
                      "Loop condition is true. Entering loop body"
                  | false, (Sil.Ik_for | Sil.Ik_while | Sil.Ik_dowhile) ->
                      "Loop condition is false. Leaving loop"
                  | true, Sil.Ik_switch -> "Switch condition is true. Entering switch case"
                  | false, Sil.Ik_switch -> "Switch condition is false. Skipping switch case"
                  | true, (Sil.Ik_bexp | Sil.Ik_land_lor) -> "Condition is true"
                  | false, (Sil.Ik_bexp | Sil.Ik_land_lor) -> "Condition is false" in
                let node_tags =
                  [(Io_infer.Xml.tag_kind,"condition");
                   (Io_infer.Xml.tag_branch, if is_true_branch then "true" else "false")] in
                trace := mk_trace_elem level curr_loc descr node_tags :: !trace
            | Cfg.Node.Exit_node pname ->
                let descr = "return from a call to " ^ (Procname.to_string pname) in
                let name = Procname.to_string pname in
                let name_id = Procname.to_filename pname in
                let node_tags =
                  [(Io_infer.Xml.tag_kind,"procedure_end");
                   (Io_infer.Xml.tag_name, name);
                   (Io_infer.Xml.tag_name_id, name_id)] in
                trace := mk_trace_elem level curr_loc descr node_tags :: !trace
            | _ ->
                let descr, node_tags =
                  match exn_opt with
                  | None -> "", []
                  | Some exn_name ->
                      let exn_str = Typename.name exn_name in
                      if exn_str = ""
                      then "exception", [(Io_infer.Xml.tag_kind,"exception")]
                      else
                        "exception " ^ exn_str,
                        [(Io_infer.Xml.tag_kind,"exception");
                         (Io_infer.Xml.tag_name, exn_str)] in
                let descr =
                  match get_description path with
                  | Some path_descr ->
                      if String.length descr > 0 then descr^" "^path_descr else path_descr
                  | None -> descr in
                trace := mk_trace_elem level curr_loc descr node_tags :: !trace
          end
      | None ->
          () in
    iter_longest_sequence g pos_opt path;
    let compare lt1 lt2 =
      let n = int_compare lt1.Errlog.lt_level lt2.Errlog.lt_level in
      if n <> 0 then n else Location.compare lt1.Errlog.lt_loc lt2.Errlog.lt_loc in
    let relevant lt = lt.Errlog.lt_node_tags <> [] in
    IList.remove_irrelevant_duplicates compare relevant (IList.rev !trace)
    (* IList.remove_duplicates compare (IList.sort compare !trace) *)

(*
  let equal p1 p2 =
    compare p1 p2 = 0
*)
end
(* =============== END of the Path module ===============*)

module PropMap = Map.Make (struct
    type t = Prop.normal Prop.t
    let compare = Prop.prop_compare
  end)

(* =============== START of the PathSet module ===============*)
module PathSet : sig
  type t

  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the prop *)
  val add_renamed_prop : Prop.normal Prop.t -> Path.t -> t -> t

  (** dump the pathset *)
  val d : t -> unit

  (** difference between two pathsets *)
  val diff : t -> t -> t

  (** empty pathset *)
  val empty : t

  (** list of elements in a pathset *)
  val elements : t -> (Prop.normal Prop.t * Path.t) list

  (** equality for pathsets *)
  val equal : t -> t -> bool

  (** filter a pathset on the prop component *)
  val filter : (Prop.normal Prop.t -> bool) -> t -> t

  (** find the list of props whose associated path contains the given path *)
  val filter_path : Path.t -> t -> Prop.normal Prop.t list

  (** fold over a pathset *)
  val fold : (Prop.normal Prop.t -> Path.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the list *)
  val from_renamed_list: (Prop.normal Prop.t * Path.t) list -> t

  (** check whether the pathset is empty *)
  val is_empty : t -> bool

  (** iterate over a pathset *)
  val iter : (Prop.normal Prop.t -> Path.t -> unit) -> t -> unit

  (** map over the prop component of a pathset *)
  val map : (Prop.normal Prop.t -> Prop.normal Prop.t) -> t -> t

  (** map over the prop component of a pathset using a partial function; elements mapped to None are discarded *)
  val map_option : (Prop.normal Prop.t -> Prop.normal Prop.t option) -> t -> t

  (** partition a pathset on the prop component *)
  val partition : (Prop.normal Prop.t -> bool) -> t -> t * t

  (** pretty print the pathset *)
  val pp : printenv -> Format.formatter -> t -> unit

  (** number of elements in the pathset *)
  val size : t -> int

  (** convert to a list of props *)
  val to_proplist : t -> Prop.normal Prop.t list

  (** convert to a set of props *)
  val to_propset : Tenv.t -> t -> Propset.t

  (** union of two pathsets *)
  val union : t -> t -> t
end = struct
  type t = Path.t PropMap.t

  let equal = PropMap.equal (fun _ _ -> true) (* only discriminate props, and ignore paths *)

  let empty : t = PropMap.empty

  let elements ps =
    let plist = ref [] in
    let f prop path = plist := (prop, path) :: !plist in
    PropMap.iter f ps;
    !plist

  let to_proplist ps =
    IList.map fst (elements ps)

  let to_propset tenv ps =
    Propset.from_proplist tenv (to_proplist ps)

  let filter f ps =
    let elements = ref [] in
    PropMap.iter (fun p _ -> elements := p :: !elements) ps;
    elements := IList.filter (fun p -> not (f p)) !elements;
    let filtered_map = ref ps in
    IList.iter (fun p -> filtered_map := PropMap.remove p !filtered_map) !elements;
    !filtered_map

  let partition f ps =
    let elements = ref [] in
    PropMap.iter (fun p _ -> elements := p :: !elements) ps;
    let el1, el2 = ref ps, ref ps in
    IList.iter (fun p -> if f p then el2 := PropMap.remove p !el2 else el1 := PropMap.remove p !el1) !elements;
    !el1, !el2

  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the prop *)
  let add_renamed_prop (p: Prop.normal Prop.t) (path: Path.t) (ps: t) : t =
    let path_new =
      try
        let path_old = PropMap.find p ps in
        Path.join path_old path
      with Not_found -> path in
    PropMap.add p path_new ps

  let union (ps1: t) (ps2: t) : t =
    PropMap.fold add_renamed_prop ps1 ps2

  (** check if the nodes in path p1 are a subset of those in p2 (not trace subset) *)
  let path_nodes_subset p1 p2 =
    let get_nodes p =
      let s = ref Cfg.NodeSet.empty in
      Path.iter_all_nodes_nocalls (fun n -> s := Cfg.NodeSet.add n !s) p;
      !s in
    Cfg.NodeSet.subset (get_nodes p1) (get_nodes p2)

  (** difference between pathsets for the differential fixpoint *)
  let diff (ps1: t) (ps2: t) : t =
    let res = ref ps1 in
    let rem p path =
      try
        let path_old = PropMap.find p !res in
        if path_nodes_subset path path_old (* do not propagate new path if it has no new nodes *)
        then res := PropMap.remove p !res
      with Not_found ->
        res := PropMap.remove p !res in
    PropMap.iter rem ps2;
    !res

  let is_empty = PropMap.is_empty

  let iter = PropMap.iter

  let fold = PropMap.fold

  let map_option f ps =
    let res = ref empty in
    let do_elem prop path = match f prop with
      | None -> ()
      | Some prop' -> res := add_renamed_prop prop' path !res in
    iter do_elem ps;
    !res

  let map f ps =
    map_option (fun p -> Some (f p)) ps

  let size ps =
    let res = ref 0 in
    let add _ _ = incr res in
    let () = PropMap.iter add ps
    in !res

  let pp pe fmt ps =
    let count = ref 0 in
    let pp_path fmt path =
      F.fprintf fmt "[path: %a@\n%a]" Path.pp_stats path Path.pp path in
    let f prop path =
      incr count;
      F.fprintf fmt "PROP %d:%a@\n%a@\n" !count pp_path path (Prop.pp_prop pe) prop in
    iter f ps

  let d (ps: t) = L.add_print_action (L.PTpathset, Obj.repr ps)

  let filter_path path ps =
    let plist = ref [] in
    let f prop path' =
      if Path.contains path path'
      then plist := prop :: !plist in
    iter f ps;
    !plist

  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the list *)
  let from_renamed_list (pl : ('a Prop.t * Path.t) list) : t =
    IList.fold_left (fun ps (p, pa) -> add_renamed_prop p pa ps) empty pl
end
(* =============== END of the PathSet module ===============*)
