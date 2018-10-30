(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Execution Paths *)

module F = Format

(* =============== START of the Path module ===============*)

module Path : sig
  (** type for paths *)
  type t

  type session = int

  val add_call : bool -> t -> Typ.Procname.t -> t -> t
  (** add a call with its sub-path, the boolean indicates whether the subtrace for the procedure should be included *)

  val add_skipped_call : t -> Typ.Procname.t -> string -> Location.t option -> t
  (** add a call to a procname that's had to be skipped, along with the reason and the location of the procname when known *)

  val contains_position : t -> PredSymb.path_pos -> bool
  (** check wether the path contains the given position *)

  val create_loc_trace : t -> PredSymb.path_pos option -> Errlog.loc_trace
  (** Create the location trace of the path, up to the path position if specified *)

  val curr_node : t -> Procdesc.Node.t option
  (** return the current node of the path *)

  val extend : Procdesc.Node.t -> Typ.Name.t option -> session -> t -> t
  (** extend a path with a new node reached from the given session, with an optional string for exceptions *)

  val add_description : t -> string -> t
  (** extend a path with a new node reached from the given session, with an optional string for exceptions *)

  val fold_all_nodes_nocalls : (t, Procdesc.Node.t, 'accum) Container.fold
  (** fold over each node in the path, excluding calls, once *)

  val iter_shortest_sequence :
    (int -> t -> int -> Typ.Name.t option -> unit) -> PredSymb.path_pos option -> t -> unit

  val join : t -> t -> t
  (** join two paths *)

  val pp : Format.formatter -> t -> unit
  (** pretty print a path *)

  val pp_stats : Format.formatter -> t -> unit
  (** pretty print statistics of the path *)

  val start : Procdesc.Node.t -> t
  (** create a new path with given start node *)
  (*
  (** equality for paths *)
  val equal : t -> t -> bool

  val get_description : t -> string option
*)
end = struct
  type session = int [@@deriving compare]

  type stats =
    { mutable max_length: int
    ; (* length of the longest linear sequence *)
      mutable linear_num: float
    (* number of linear sequences described by the path *) }

  (* type aliases for components of t values that compare should ignore *)
  type stats_ = stats

  let compare_stats_ _ _ = 0

  type procname_ = Typ.Procname.t

  let compare_procname_ _ _ = 0

  type string_option_ = string option

  let compare_string_option_ _ _ = 0

  type path_exec_ =
    | ExecSkipped of string * Location.t option  (** call was skipped with a reason *)
    | ExecCompleted of t  (** call was completed *)

  and t =
    (* INVARIANT: stats are always set to dummy_stats unless we are in the middle of a traversal *)
    (* in particular: a new traversal cannot be initiated during an existing traversal *)
    | Pstart of Procdesc.Node.t * stats_  (** start node *)
    | Pnode of Procdesc.Node.t * Typ.Name.t option * session * t * stats_ * string_option_
        (** we got to [node] from last [session] perhaps propagating exception [exn_opt],
        and continue with [path].  *)
    | Pjoin of t * t * stats_  (** join of two paths *)
    | Pcall of t * procname_ * path_exec_ * stats_  (** add a sub-path originating from a call *)
  [@@deriving compare]

  let get_dummy_stats () = {max_length= -1; linear_num= -1.0}

  let get_description path =
    match path with Pnode (_, _, _, _, _, descr_opt) -> descr_opt | _ -> None


  let add_description path description =
    let add_descr descr_option description =
      match descr_option with Some descr -> descr ^ " " ^ description | None -> description
    in
    match path with
    | Pnode (node, exn_opt, session, path, stats, descr_opt) ->
        let description = add_descr descr_opt description in
        Pnode (node, exn_opt, session, path, stats, Some description)
    | _ ->
        path


  let set_dummy_stats stats =
    stats.max_length <- -1 ;
    stats.linear_num <- -1.0


  let rec curr_node = function
    | Pstart (node, _) ->
        Some node
    | Pnode (node, _, _, _, _, _) ->
        Some node
    | Pcall (path, _, _, _) ->
        curr_node path
    | Pjoin _ ->
        None


  let start node = Pstart (node, get_dummy_stats ())

  let extend (node : Procdesc.Node.t) exn_opt session path =
    Pnode (node, exn_opt, session, path, get_dummy_stats (), None)


  let join p1 p2 = Pjoin (p1, p2, get_dummy_stats ())

  let add_call include_subtrace p pname p_sub =
    if include_subtrace then Pcall (p, pname, ExecCompleted p_sub, get_dummy_stats ()) else p


  let add_skipped_call p pname reason loc_opt =
    Pcall (p, pname, ExecSkipped (reason, loc_opt), get_dummy_stats ())


  (** functions in this module either do not assume, or do not re-establish, the invariant on dummy
      stats *)
  module Invariant = struct
    (** check whether a stats is the dummy stats *)
    let stats_is_dummy stats = Int.equal stats.max_length (-1)

    (** return the stats of the path, assumes that the stats are computed *)
    let get_stats = function
      | Pstart (_, stats) ->
          stats
      | Pnode (_, _, _, _, stats, _) ->
          stats
      | Pjoin (_, _, stats) ->
          stats
      | Pcall (_, _, _, stats) ->
          stats


    (** restore the invariant that all the stats are dummy, so the path is ready for another
        traversal assumes that the stats are computed beforehand, and ensures that the invariant
        holds afterwards *)
    let rec reset_stats = function
      | Pstart (_, stats) ->
          if not (stats_is_dummy stats) then set_dummy_stats stats
      | Pnode (_, _, _, path, stats, _) | Pcall (path, _, ExecSkipped _, stats) ->
          if not (stats_is_dummy stats) then ( reset_stats path ; set_dummy_stats stats )
      | Pjoin (path1, path2, stats) ->
          if not (stats_is_dummy stats) then (
            reset_stats path1 ; reset_stats path2 ; set_dummy_stats stats )
      | Pcall (path1, _, ExecCompleted path2, stats) ->
          if not (stats_is_dummy stats) then (
            reset_stats path1 ; reset_stats path2 ; set_dummy_stats stats )


    (** Iterate [f] over the path and compute the stats, assuming the invariant: all the stats are
        dummy.  Function [f] (typically with side-effects) is applied once to every node, and
        max_length in the stats is the length of a longest sequence of nodes in the path where [f]
        returned [true] on at least one node.  max_length is 0 if the path was visited but no node
        satisfying [f] was found.  Assumes that the invariant holds beforehand, and ensures that all
        the stats are computed afterwards.  Since this breaks the invariant, it must be followed by
        reset_stats. *)
    let rec compute_stats do_calls (f : Procdesc.Node.t -> bool) =
      let nodes_found stats = stats.max_length > 0 in
      function
      | Pstart (node, stats) ->
          if stats_is_dummy stats then (
            let found = f node in
            stats.max_length <- (if found then 1 else 0) ;
            stats.linear_num <- 1.0 )
      | Pnode (node, _, _, path, stats, _) ->
          if stats_is_dummy stats then (
            compute_stats do_calls f path ;
            let stats1 = get_stats path in
            let found =
              f node || nodes_found stats1
              (* the order is important as f has side-effects *)
            in
            stats.max_length <- (if found then 1 + stats1.max_length else 0) ;
            stats.linear_num <- stats1.linear_num )
      | Pjoin (path1, path2, stats) ->
          if stats_is_dummy stats then (
            compute_stats do_calls f path1 ;
            compute_stats do_calls f path2 ;
            let stats1, stats2 = (get_stats path1, get_stats path2) in
            stats.max_length <- max stats1.max_length stats2.max_length ;
            stats.linear_num <- stats1.linear_num +. stats2.linear_num )
      | Pcall (path1, _, ExecCompleted path2, stats) ->
          if stats_is_dummy stats then (
            let stats2 =
              match do_calls with
              | true ->
                  compute_stats do_calls f path2 ; get_stats path2
              | false ->
                  {max_length= 0; linear_num= 0.0}
            in
            let stats1 =
              let f' =
                if nodes_found stats2 then fun _ -> true
                  (* already found in call, no need to search before the call *)
                else f
              in
              compute_stats do_calls f' path1 ; get_stats path1
            in
            stats.max_length <- stats1.max_length + stats2.max_length ;
            stats.linear_num <- stats1.linear_num )
      | Pcall (path, _, ExecSkipped _, stats) ->
          if stats_is_dummy stats then (
            let stats1 = compute_stats do_calls f path ; get_stats path in
            stats.max_length <- stats1.max_length ;
            stats.linear_num <- stats1.linear_num )
  end

  (* End of module Invariant *)

  (** fold over each node in the path, excluding calls, once *)
  let fold_all_nodes_nocalls path ~init ~f =
    let acc = ref init in
    Invariant.compute_stats false
      (fun node ->
        acc := f !acc node ;
        true )
      path ;
    Invariant.reset_stats path ;
    !acc


  let get_path_pos node =
    let pn = Procdesc.Node.get_proc_name node in
    let n_id = Procdesc.Node.get_id node in
    (pn, (n_id :> int))


  let contains_position path pos =
    let found = ref false in
    let f node =
      if PredSymb.equal_path_pos (get_path_pos node) pos then found := true ;
      true
    in
    Invariant.compute_stats true f path ;
    Invariant.reset_stats path ;
    !found


  (** iterate over the longest sequence belonging to the path,
      restricting to those where [filter] holds of some element.
      If a node is reached via an exception,
      pass the exception information to [f] on the previous node *)
  let iter_shortest_sequence_filter (f : int -> t -> int -> Typ.Name.t option -> unit)
      (filter : Procdesc.Node.t -> bool) (path : t) : unit =
    let rec doit level session path prev_exn_opt =
      match path with
      | Pstart _ ->
          f level path session prev_exn_opt
      | Pnode (_, exn_opt, session', p, _, _) ->
          (* no two consecutive exceptions *)
          let next_exn_opt = if prev_exn_opt <> None then None else exn_opt in
          doit level (session' :> int) p next_exn_opt ;
          f level path session prev_exn_opt
      | Pjoin (p1, p2, _) ->
          if (Invariant.get_stats p1).max_length <= (Invariant.get_stats p2).max_length then
            doit level session p1 prev_exn_opt
          else doit level session p2 prev_exn_opt
      | Pcall (p1, _, ExecCompleted p2, _) ->
          let next_exn_opt = None in
          (* exn must already be inside the call *)
          doit level session p1 next_exn_opt ;
          doit (level + 1) session p2 next_exn_opt
      | Pcall (p, _, ExecSkipped _, _) ->
          let next_exn_opt = None in
          doit level session p next_exn_opt ; f level path session prev_exn_opt
    in
    Invariant.compute_stats true filter path ;
    doit 0 0 path None ;
    Invariant.reset_stats path


  (** iterate over the shortest sequence belonging to the path,
      restricting to those containing the given position if given.
      Do not iterate past the last occurrence of the given position.
      [f level path session exn_opt] is passed the current nesting [level] and [path]
      and previous [session] and possible exception [exn_opt] *)
  let iter_shortest_sequence (f : int -> t -> int -> Typ.Name.t option -> unit)
      (pos_opt : PredSymb.path_pos option) (path : t) : unit =
    let filter node =
      match pos_opt with
      | None ->
          true
      | Some pos ->
          PredSymb.equal_path_pos (get_path_pos node) pos
    in
    let path_pos_at_path p =
      try match curr_node p with Some node -> pos_opt <> None && filter node | None -> false
      with exn when SymOp.exn_not_failure exn -> false
    in
    let position_seen = ref false in
    let inverse_sequence =
      let log = ref [] in
      let g level p session exn_opt =
        if path_pos_at_path p then position_seen := true ;
        log := (level, p, session, exn_opt) :: !log
      in
      iter_shortest_sequence_filter g filter path ;
      !log
    in
    let sequence_up_to_last_seen =
      if !position_seen then
        let rec remove_until_seen = function
          | ((_, p, _, _) as x) :: l ->
              if path_pos_at_path p then List.rev (x :: l) else remove_until_seen l
          | [] ->
              []
        in
        remove_until_seen inverse_sequence
      else List.rev inverse_sequence
    in
    List.iter
      ~f:(fun (level, p, session, exn_opt) -> f level p session exn_opt)
      sequence_up_to_last_seen


  (** return the node visited most, and number of visits, in the shortest linear sequence *)
  let repetitions path =
    let map = ref Procdesc.NodeMap.empty in
    let add_node = function
      | Some node -> (
        try
          let n = Procdesc.NodeMap.find node !map in
          map := Procdesc.NodeMap.add node (n + 1) !map
        with Caml.Not_found -> map := Procdesc.NodeMap.add node 1 !map )
      | None ->
          ()
    in
    iter_shortest_sequence (fun _ p _ _ -> add_node (curr_node p)) None path ;
    let max_rep_opt =
      Procdesc.NodeMap.fold
        (fun node num max_rep_opt ->
          if num > Option.value_map max_rep_opt ~default:0 ~f:fst then Some (num, node)
          else max_rep_opt )
        !map None
    in
    Option.value_exn max_rep_opt


  let pp_stats f path =
    Invariant.compute_stats true (fun _ -> true) path ;
    let repetitions, node = repetitions path in
    F.fprintf f "linear paths: %f max length: %d has repetitions: %d of node %a"
      (Invariant.get_stats path).linear_num (Invariant.get_stats path).max_length repetitions
      Procdesc.Node.pp node ;
    Invariant.reset_stats path


  module PathMap = Caml.Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  let pp fmt path =
    let delayed_num = ref 0 in
    let delayed = ref PathMap.empty in
    let add_path p =
      try ignore (PathMap.find p !delayed) with Caml.Not_found ->
        incr delayed_num ;
        delayed := PathMap.add p !delayed_num !delayed
    in
    let path_seen p =
      (* path seen before *)
      PathMap.mem p !delayed
    in
    let rec add_delayed path =
      if not (path_seen path) (* avoid exponential blowup *) then
        match path with
        (* build a map from delayed paths to a unique number *)
        | Pstart _ ->
            ()
        | Pnode (_, _, _, p, _, _) | Pcall (p, _, ExecSkipped _, _) ->
            add_delayed p
        | Pjoin (p1, p2, _) | Pcall (p1, _, ExecCompleted p2, _) ->
            (* delay paths occurring in a join *)
            add_delayed p1 ; add_delayed p2 ; add_path p1 ; add_path p2
    in
    let rec doit n fmt path =
      try
        if n > 0 then raise Caml.Not_found ;
        let num = PathMap.find path !delayed in
        F.fprintf fmt "P%d" num
      with Caml.Not_found -> (
        match path with
        | Pstart (node, _) ->
            F.fprintf fmt "n%a" Procdesc.Node.pp node
        | Pnode (node, _, session, path, _, _) ->
            F.fprintf fmt "%a(s%d).n%a" (doit (n - 1)) path (session :> int) Procdesc.Node.pp node
        | Pjoin (path1, path2, _) ->
            F.fprintf fmt "(%a + %a)" (doit (n - 1)) path1 (doit (n - 1)) path2
        | Pcall (path1, _, ExecCompleted path2, _) ->
            F.fprintf fmt "(%a{%a})" (doit (n - 1)) path1 (doit (n - 1)) path2
        | Pcall (path, _, ExecSkipped (reason, _), _) ->
            F.fprintf fmt "(%a: %s)" (doit (n - 1)) path reason )
    in
    let print_delayed () =
      if not (PathMap.is_empty !delayed) then (
        let f path num = F.fprintf fmt "P%d = %a@\n" num (doit 1) path in
        F.fprintf fmt "where@\n" ; PathMap.iter f !delayed )
    in
    add_delayed path ; doit 0 fmt path ; print_delayed ()


  let create_loc_trace path pos_opt : Errlog.loc_trace =
    let trace = ref [] in
    let g level path _ exn_opt =
      match (path, curr_node path) with
      | Pcall (_, pname, ExecSkipped (reason, loc_opt), _), Some curr_node ->
          let curr_loc = Procdesc.Node.get_loc curr_node in
          let descr =
            Format.sprintf "Skipping %s: %s" (Typ.Procname.to_simplified_string pname) reason
          in
          let node_tags = [] in
          trace := Errlog.make_trace_element level curr_loc descr node_tags :: !trace ;
          Option.iter
            ~f:(fun loc ->
              if Typ.Procname.is_java pname && not (SourceFile.is_invalid loc.Location.file) then
                let definition_descr =
                  Format.sprintf "Definition of %s" (Typ.Procname.to_simplified_string pname)
                in
                trace := Errlog.make_trace_element (level + 1) loc definition_descr [] :: !trace )
            loc_opt
      | _, Some curr_node -> (
          let curr_loc = Procdesc.Node.get_loc curr_node in
          match Procdesc.Node.get_kind curr_node with
          | Procdesc.Node.Join_node ->
              () (* omit join nodes from error traces *)
          | Procdesc.Node.Start_node ->
              let pname = Procdesc.Node.get_proc_name curr_node in
              let descr = "start of procedure " ^ Typ.Procname.to_simplified_string pname in
              let node_tags = [Errlog.Procedure_start pname] in
              trace := Errlog.make_trace_element level curr_loc descr node_tags :: !trace
          | Procdesc.Node.Prune_node (is_true_branch, if_kind, _) ->
              let descr =
                match (is_true_branch, if_kind) with
                | true, Sil.Ik_if ->
                    "Taking true branch"
                | false, Sil.Ik_if ->
                    "Taking false branch"
                | true, (Sil.Ik_for | Sil.Ik_while | Sil.Ik_dowhile) ->
                    "Loop condition is true. Entering loop body"
                | false, (Sil.Ik_for | Sil.Ik_while | Sil.Ik_dowhile) ->
                    "Loop condition is false. Leaving loop"
                | true, Sil.Ik_switch ->
                    "Switch condition is true. Entering switch case"
                | false, Sil.Ik_switch ->
                    "Switch condition is false. Skipping switch case"
                | true, (Sil.Ik_bexp | Sil.Ik_land_lor) ->
                    "Condition is true"
                | false, (Sil.Ik_bexp | Sil.Ik_land_lor) ->
                    "Condition is false"
              in
              let node_tags = [Errlog.Condition is_true_branch] in
              trace := Errlog.make_trace_element level curr_loc descr node_tags :: !trace
          | Procdesc.Node.Exit_node ->
              let pname = Procdesc.Node.get_proc_name curr_node in
              let descr = "return from a call to " ^ Typ.Procname.to_string pname in
              let node_tags = [Errlog.Procedure_end pname] in
              trace := Errlog.make_trace_element level curr_loc descr node_tags :: !trace
          | _ ->
              let descr, node_tags =
                match exn_opt with
                | None ->
                    ("", [])
                | Some exn_name ->
                    let exn_str = Typ.Name.name exn_name in
                    let desc =
                      if String.is_empty exn_str then "exception" else "exception " ^ exn_str
                    in
                    (desc, [Errlog.Exception exn_name])
              in
              let descr =
                match get_description path with
                | Some path_descr ->
                    if String.length descr > 0 then descr ^ " " ^ path_descr else path_descr
                | None ->
                    descr
              in
              trace := Errlog.make_trace_element level curr_loc descr node_tags :: !trace )
      | _, None ->
          ()
    in
    iter_shortest_sequence g pos_opt path ;
    let equal lt1 lt2 =
      [%compare.equal: int * Location.t]
        (lt1.Errlog.lt_level, lt1.Errlog.lt_loc)
        (lt2.Errlog.lt_level, lt2.Errlog.lt_loc)
    in
    let relevant lt = lt.Errlog.lt_node_tags <> [] in
    IList.remove_irrelevant_duplicates ~equal ~f:relevant (List.rev !trace)
end

(* =============== END of the Path module ===============*)

module PropMap = Caml.Map.Make (struct
  type t = Prop.normal Prop.t

  let compare = Prop.compare_prop
end)

(* =============== START of the PathSet module ===============*)
module PathSet : sig
  type t

  val add_renamed_prop : Prop.normal Prop.t -> Path.t -> t -> t
  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the prop *)

  val diff : t -> t -> t
  (** difference between two pathsets *)

  val empty : t
  (** empty pathset *)

  val elements : t -> (Prop.normal Prop.t * Path.t) list
  (** list of elements in a pathset *)

  val equal : t -> t -> bool
  (** equality for pathsets *)

  val fold : (Prop.normal Prop.t -> Path.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** fold over a pathset *)

  val from_renamed_list : (Prop.normal Prop.t * Path.t) list -> t
  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the list *)

  val is_empty : t -> bool
  (** check whether the pathset is empty *)

  val iter : (Prop.normal Prop.t -> Path.t -> unit) -> t -> unit
  (** iterate over a pathset *)

  val map : (Prop.normal Prop.t -> Prop.normal Prop.t) -> t -> t
  (** map over the prop component of a pathset *)

  val map_option : (Prop.normal Prop.t -> Prop.normal Prop.t option) -> t -> t
  (** map over the prop component of a pathset using a partial function; elements mapped to None are discarded *)

  val partition : (Prop.normal Prop.t -> bool) -> t -> t * t
  (** partition a pathset on the prop component *)

  val size : t -> int
  (** number of elements in the pathset *)

  val to_proplist : t -> Prop.normal Prop.t list
  (** convert to a list of props *)

  val to_propset : Tenv.t -> t -> Propset.t
  (** convert to a set of props *)

  val union : t -> t -> t
  (** union of two pathsets *)
end = struct
  type t = Path.t PropMap.t

  let equal = PropMap.equal (fun _ _ -> true)

  (* only discriminate props, and ignore paths *)

  let empty : t = PropMap.empty

  let elements ps =
    let plist = ref [] in
    let f prop path = plist := (prop, path) :: !plist in
    PropMap.iter f ps ; !plist


  let to_proplist ps = List.map ~f:fst (elements ps)

  let to_propset tenv ps = Propset.from_proplist tenv (to_proplist ps)

  let partition f ps =
    let elements = ref [] in
    PropMap.iter (fun p _ -> elements := p :: !elements) ps ;
    let el1, el2 = (ref ps, ref ps) in
    List.iter
      ~f:(fun p -> if f p then el2 := PropMap.remove p !el2 else el1 := PropMap.remove p !el1)
      !elements ;
    (!el1, !el2)


  (** It's the caller's responsibility to ensure that [Prop.prop_rename_primed_footprint_vars] was
     called on the prop *)
  let add_renamed_prop (p : Prop.normal Prop.t) (path : Path.t) (ps : t) : t =
    let path_new =
      try
        let path_old = PropMap.find p ps in
        Path.join path_old path
      with Caml.Not_found -> path
    in
    PropMap.add p path_new ps


  let union (ps1 : t) (ps2 : t) : t = PropMap.fold add_renamed_prop ps1 ps2

  (** check if the nodes in path p1 are a subset of those in p2 (not trace subset) *)
  let path_nodes_subset p1 p2 =
    let get_nodes p =
      Path.fold_all_nodes_nocalls p ~init:Procdesc.NodeSet.empty ~f:(fun s n ->
          Procdesc.NodeSet.add n s )
    in
    Procdesc.NodeSet.subset (get_nodes p1) (get_nodes p2)


  (** difference between pathsets for the differential fixpoint *)
  let diff (ps1 : t) (ps2 : t) : t =
    let res = ref ps1 in
    let rem p path =
      try
        let path_old = PropMap.find p !res in
        if path_nodes_subset path path_old (* do not propagate new path if it has no new nodes *)
        then res := PropMap.remove p !res
      with Caml.Not_found -> res := PropMap.remove p !res
    in
    PropMap.iter rem ps2 ; !res


  let is_empty = PropMap.is_empty

  let iter = PropMap.iter

  let fold = PropMap.fold

  let map_option f ps =
    let res = ref empty in
    let do_elem prop path =
      match f prop with None -> () | Some prop' -> res := add_renamed_prop prop' path !res
    in
    iter do_elem ps ; !res


  let map f ps = map_option (fun p -> Some (f p)) ps

  let size ps =
    let res = ref 0 in
    let add _ _ = incr res in
    let () = PropMap.iter add ps in
    !res


  let[@warning "-32"] pp pe fmt ps =
    let count = ref 0 in
    let pp_path fmt path = F.fprintf fmt "[path: %a@\n%a]" Path.pp_stats path Path.pp path in
    let f prop path =
      incr count ;
      F.fprintf fmt "PROP %d:%a@\n%a@\n" !count pp_path path (Prop.pp_prop pe) prop
    in
    iter f ps


  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the list *)
  let from_renamed_list (pl : ('a Prop.t * Path.t) list) : t =
    List.fold ~f:(fun ps (p, pa) -> add_renamed_prop p pa ps) ~init:empty pl
end

(* =============== END of the PathSet module ===============*)
