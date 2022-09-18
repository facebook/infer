(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Llair

(** An automaton state in the sequence of calls and returns that the
    symbolic executor will attempt to follow. Represented as an integer and
    interpreted as an amount of progress through the shared ref [trace],
    analogously to the [cursor] of [Goal.Sparse_trace] but with a
    lighter-weight representation and different operations/structures
    defined over it. *)
module Automaton_state = struct
  type checkpoint = [`Call of FuncName.t | `Retn of FuncName.t]

  let trace : checkpoint iarray ref = ref IArray.empty
  let set_sparse_trace st = trace := st

  type t = int [@@deriving equal]

  module Map = Int.Map

  (** Because there are always exactly [IArray.length !trace + 1] automaton
      states, we can represent a "table" keyed on automaton states using an
      array of that length. *)
  module Tbl = struct
    type 'a t = 'a option array

    let create () = Array.init (IArray.length !trace + 1) ~f:(fun _ -> None)
    let find = Array.get
    let set tbl key data = Array.set tbl key (Some data)

    let find_or_add tbl key ~default =
      match find tbl key with
      | Some data -> data
      | None ->
          let data = default () in
          Array.set tbl key (Some data) ;
          data
  end

  let join = Int.max
  let is_accepting state = Int.equal state (IArray.length !trace)

  let call_is_next f state =
    state < IArray.length !trace
    &&
    match IArray.get !trace state with
    | `Call f' -> FuncName.equal f f'
    | _ -> false

  let retn_is_next f state =
    state < IArray.length !trace
    &&
    match IArray.get !trace state with
    | `Retn f' -> FuncName.equal f f'
    | _ -> false

  let pp ppf state =
    if is_accepting state then Format.fprintf ppf "accept state"
    else
      match IArray.get !trace state with
      | `Call f -> Format.fprintf ppf "call(%a)" FuncName.pp f
      | `Retn f -> Format.fprintf ppf "retn(%a)" FuncName.pp f
end

(** The shortest distance to the current location from each block that can
    reach it. *)
module Distance_map = struct
  module Map = Block.Map

  (** A distance map [{max; offsets}] conceptually maps each key [k] of
      [offsets] to distance [max - offsets(k)], but is more efficient to
      manipulate than a direct representation. Note that this representation
      is non-canonical: two different values can represent the same distance
      map, if the difference in their [max] field is equal to the pointwise
      difference between each of their offsets. This requires [equal] below
      to do some extra work, but no additional allocation is required. *)
  type t = {max: int; offsets: int Map.t}

  let equal dm dm' =
    dm == dm'
    ||
    if dm.max = dm'.max then Map.equal Int.equal dm.offsets dm'.offsets
    else
      let diff_max = dm.max - dm'.max in
      Map.equal (fun x y -> x - y = diff_max) dm.offsets dm'.offsets

  let pp ppf {max; offsets} =
    Format.fprintf ppf "@[<v 2>" ;
    Map.iteri offsets ~f:(fun ~key ~data:offset ->
        Format.fprintf ppf "@ %a -> %i" Block.pp_ident key (max - offset) ) ;
    Format.fprintf ppf "@]"

  let init = {max= 0; offsets= Map.empty}

  (** Join two distance maps, choosing the shorter distance to each
      location. *)
  let join {max; offsets} {max= max'; offsets= offsets'} =
    (* When [max = max'], the offsets can be joined directly with pointwise
       `Int.min`. When they are unequal, we take the larger one and shift
       the offsets of the other distance map accordingly. *)
    if max = max' then
      { max
      ; offsets=
          Map.union offsets offsets' ~f:(fun _ d d' -> Some (Int.max d d'))
      }
    else if max > max' then
      let diff = max - max' in
      { max
      ; offsets=
          Map.merge offsets offsets' ~f:(fun _ -> function
            | `Left d -> Some d
            | `Right d -> Some (d + diff)
            | `Both (d, d') -> Some (Int.max d (d' + diff)) ) }
    else
      let diff = max' - max in
      { max= max'
      ; offsets=
          Map.merge offsets offsets' ~f:(fun _ -> function
            | `Left d -> Some (d + diff)
            | `Right d -> Some d
            | `Both (d, d') -> Some (Int.max (d + diff) d') ) }

  (** Take one step to [block], incrementing the [max] distance and adding
      [block] with distance [0] (i.e. with offset equal to the new [max]). *)
  let visit block {max; offsets} =
    let max = max + 1 in
    {max; offsets= Map.add ~key:block ~data:max offsets}

  let add_pointwise ~n dm = {dm with max= dm.max + n}

  let iteri ~f {max; offsets} =
    Map.iteri offsets ~f:(fun ~key ~data -> f ~key ~data:(max - data))
end

(** The shortest distance to each visited trace checkpoint on any path to
    some location. *)
module Checkpoint_dists = struct
  open Automaton_state.Map

  type t = int Automaton_state.Map.t [@@deriving equal]

  let pp = pp Int.pp Int.pp
  let init = empty
  let join = union ~f:(fun _ d d' -> Some (min d d'))

  let hit_checkpoint auto_state shortest_path =
    add ~key:auto_state ~data:shortest_path

  let add_pointwise ~n = map ~f:(Int.add n)
end

(** A [summary] is composed of

    (1) the length of the shortest path from procedure entry to the current
    location,

    (2) the current automaton state,

    (3) a distance map upper-bounding the shortest path from blocks to the
    current location, and

    (4) the lengths of the shortest paths to each trace checkpoint that has
    already been reached from the program entry. *)
module Summary = struct
  (* Summaries and their basic operations: join, transfer, apply, etc. *)

  type summary =
    { shortest_path: int
    ; auto_state: Automaton_state.t
    ; dists: Distance_map.t
    ; checkpoint_dists: Checkpoint_dists.t }
  [@@deriving equal]

  type t = Summary of summary | Bottom [@@deriving equal]

  let pp ppf {shortest_path; auto_state; dists; checkpoint_dists} =
    Format.fprintf ppf
      "@[<v 4>Distance Computation Summary@ shortest path: %i@ next trace \
       checkpoint: %a@ distances: %a@ checkpoint distances: %a@]"
      shortest_path Automaton_state.pp auto_state Distance_map.pp dists
      Checkpoint_dists.pp checkpoint_dists

  let init auto_state =
    { shortest_path= 0
    ; auto_state
    ; dists= Distance_map.init
    ; checkpoint_dists= Checkpoint_dists.init }

  let join
      ({shortest_path= n; auto_state= a; dists= d; checkpoint_dists= c} as s)
      ( {shortest_path= n'; auto_state= a'; dists= d'; checkpoint_dists= c'}
      as s' ) =
    if s == s' then s
    else if a > a' then s
    else if a < a' then s'
    else
      { shortest_path= Int.min n n'
      ; auto_state= Automaton_state.join a a'
      ; dists= Distance_map.join d d'
      ; checkpoint_dists= Checkpoint_dists.join c c' }

  let intraproc_transfer ~block s =
    if Automaton_state.is_accepting s.auto_state then s
    else
      let dists = Distance_map.visit block s.dists in
      let shortest_path = s.shortest_path + 1 in
      {s with dists; shortest_path}

  let apply ~summary absstate =
    if Automaton_state.is_accepting absstate.auto_state then absstate
    else
      let dists =
        let open Distance_map in
        join summary.dists
          (add_pointwise ~n:summary.shortest_path absstate.dists)
      in
      let checkpoint_dists =
        let open Checkpoint_dists in
        join absstate.checkpoint_dists
          (add_pointwise ~n:absstate.shortest_path summary.checkpoint_dists)
      in
      { shortest_path= absstate.shortest_path + summary.shortest_path
      ; dists
      ; auto_state= summary.auto_state
      ; checkpoint_dists }

  let hit_checkpoint absstate =
    let checkpoint_dists =
      Checkpoint_dists.hit_checkpoint absstate.auto_state
        absstate.shortest_path absstate.checkpoint_dists
    in
    {absstate with checkpoint_dists; auto_state= absstate.auto_state + 1}

  (* Machinery to deal with pausing/restarting _intraprocedural_ analyses
     during tabulation without performing redundant work. We associate a
     (mutable) invariant map and worklist with each intraprocedural analysis
     problem (indexed by function name and initial automaton state). *)
  type intraproc_work =
    {state: summary; loc: Block.t; check_convergence: bool}

  let intraproc_work ?(check_convergence = true) state loc =
    {state; loc; check_convergence}

  type intraproc_state =
    {worklist: intraproc_work Stack.t; invariant_map: summary Block.Tbl.t}

  let intraproc_analyses :
      intraproc_state Automaton_state.Tbl.t FuncName.Tbl.t =
    FuncName.Tbl.create ()

  let intraproc_analysis f a =
    let analyses_over_f =
      FuncName.Tbl.find_or_add intraproc_analyses f
        ~default:Automaton_state.Tbl.create
    in
    Automaton_state.Tbl.find_or_add analyses_over_f a ~default:(fun () ->
        {worklist= Stack.create (); invariant_map= Block.Tbl.create ()} )

  (* Interprocedural analysis: summary queries and tabulation *)

  (* The tabulation algorithm's summary table, supporting memoization and
     lookup. *)

  let summary_table : t Automaton_state.Tbl.t FuncName.Tbl.t =
    FuncName.Tbl.create ()

  let memoize f a summary =
    let summaries_of_f =
      FuncName.Tbl.find_or_add summary_table f
        ~default:Automaton_state.Tbl.create
    in
    Automaton_state.Tbl.set summaries_of_f a summary

  let lookup_summary f a =
    let summaries_of_f =
      FuncName.Tbl.find_or_add summary_table f
        ~default:Automaton_state.Tbl.create
    in
    Automaton_state.Tbl.find summaries_of_f a

  exception Summary_query of (FuncName.t * Automaton_state.t)

  let lookup_summary_exn f a =
    match lookup_summary f a with
    | Some s -> s
    | None -> raise_notrace (Summary_query (f, a))

  (** Interprocedural worklist, consisting of two types of work: "Summarize"
      work elements require computing a summary of some function in some
      initial automaton state, while "Fix" elements require computing the
      fixed point of some summary of [caller_name] with a recursive call to
      [callee_name]. *)
  module Worklist = struct
    type work_elt =
      | Summarize of FuncName.t * Automaton_state.t
      | Fix of
          { caller_name: FuncName.t
          ; caller_state: Automaton_state.t
          ; callee_name: FuncName.t
          ; callee_state: Automaton_state.t
          ; prev_summary: t }

    let init f a = Stack.create () $> Stack.push (Summarize (f, a))

    let contains callee init_state worklist =
      Iter.(exists (of_stack worklist)) ~f:(function
        | Fix _ -> false
        | Summarize (f, a) ->
            Automaton_state.equal init_state a && FuncName.equal callee f )

    let push f a callee_f callee_a wl =
      Stack.push (Summarize (f, a)) wl ;
      Stack.push (Summarize (callee_f, callee_a)) wl

    let pop = Stack.pop
  end

  let summarize_call callee absstate =
    let prestate =
      if Automaton_state.call_is_next callee absstate.auto_state then
        hit_checkpoint absstate
      else absstate
    in
    match lookup_summary callee prestate.auto_state with
    | Some (Summary summary) ->
        let poststate = apply ~summary prestate in
        let res =
          if Automaton_state.retn_is_next callee poststate.auto_state then
            hit_checkpoint poststate
          else poststate
        in
        Ok (Summary res)
    | Some Bottom -> Ok Bottom
    | None -> Error (callee, prestate.auto_state)

  (** Compute and return a summary of the given [func] in the given
      [init_state]. Raises a [Summary_query] if a callee summary is required
      that has not yet been computed. *)
  let summarize func init_state worklist =
    if
      Automaton_state.is_accepting init_state.auto_state
      || Func.is_undefined func
    then Summary init_state
    else
      let analysis = intraproc_analysis func.name init_state.auto_state in
      let push absstate block =
        Stack.push (intraproc_work absstate block) analysis.worklist
      in
      push init_state func.entry ;
      while not (Stack.is_empty analysis.worklist) do
        let {state= worklist_absstate; loc= block; check_convergence} =
          Stack.pop analysis.worklist
        in
        let state, converged =
          let new_state = intraproc_transfer ~block worklist_absstate in
          match Block.Tbl.find analysis.invariant_map block with
          | None -> (new_state, false)
          | Some old_state ->
              let joined_absstate = join old_state new_state in
              (joined_absstate, equal_summary joined_absstate old_state)
        in
        if (not check_convergence) || not converged then (
          ( match block.term with
          | Switch {tbl; els; _} ->
              IArray.iter tbl ~f:(fun (_, jmp) -> push state jmp.dst) ;
              push state els.dst
          | Iswitch {tbl; _} ->
              IArray.iter tbl ~f:(fun jmp -> push state jmp.dst)
          | Return _ | Throw _ | Abort _ | Unreachable -> ()
          | Call {callee; return; _} -> (
              (* Summarize a procedure call using existing summaries if
                 possible, raising a [Summary_query] if one or more is
                 missing. Push a [Fix] element onto the interprocedural
                 worklist if [callee] is recursive. *)
              let call {func= callee; _} =
                let is_recursive () =
                  let directly =
                    FuncName.equal func.name callee.name
                    && Automaton_state.equal init_state.auto_state
                         state.auto_state
                  in
                  let mutually () =
                    Worklist.contains callee.name state.auto_state worklist
                  in
                  directly || mutually ()
                in
                let interproc_fixed_point prev_summary =
                  let fix_on_call =
                    Worklist.Fix
                      { caller_name= func.name
                      ; caller_state= init_state.auto_state
                      ; callee_name= callee.name
                      ; callee_state= state.auto_state
                      ; prev_summary }
                  in
                  Stack.push fix_on_call worklist
                in
                match summarize_call callee.name state with
                | Ok Bottom -> ()
                | Ok (Summary s as summ) ->
                    if is_recursive () then interproc_fixed_point summ ;
                    push s return.dst
                | Error sq ->
                    if is_recursive () then interproc_fixed_point Bottom
                    else (
                      (* re-add the current worklist element so as to
                         continue from the same point once the requisite
                         summary is computed *)
                      push worklist_absstate block ;
                      raise_notrace (Summary_query sq) )
              in
              match callee with
              | Direct f -> call f
              | Indirect {candidates; _} -> IArray.iter candidates ~f:call
              | Intrinsic _ -> push state return.dst ) ) ;
          Block.Tbl.set analysis.invariant_map ~key:block ~data:state )
      done ;
      Func.fold_cfg func Bottom ~f:(fun block acc_summary ->
          match block.term with
          | Return _ -> (
              let return_state =
                Block.Tbl.find analysis.invariant_map block
                |> Option.map_or ~default:Bottom ~f:(fun s -> Summary s)
              in
              match (acc_summary, return_state) with
              | Summary acc, Summary curr -> Summary (join acc curr)
              | (Summary _ as s), _ | _, (Summary _ as s) -> s
              | _, _ -> Bottom )
          | _ -> acc_summary )

  let top_down pgm ~entry =
    let worklist = Worklist.init entry 0 in
    while not (Stack.is_empty worklist) do
      match Worklist.pop worklist with
      | Fix
          { caller_name= caller_f
          ; caller_state= caller_a
          ; callee_name= callee_f
          ; callee_state= callee_a
          ; prev_summary } ->
          let curr_summary = lookup_summary callee_f callee_a in
          (* If there has been no change in the summary for which a fixed
             point is requested, then analysis has converged and there's
             nothing to do.

             `Fix` work elements are always pushed immediately beneath a
             corresponding `Summarize`, so we compare the result summary of
             that `Summarize` to the original (possibly unsound) summary
             that was applied to compute it.

             Otherwise, propagate any new dataflow to each recursive return
             site in the intraprocedural analysis for this summary and
             continue. *)
          if Option.exists curr_summary ~f:(equal prev_summary) then ()
          else
            let caller = FuncName.Map.find_exn caller_f pgm.functions in
            let intraproc_state = intraproc_analysis caller_f caller_a in
            Func.fold_cfg caller () ~f:(fun blk () ->
                let resummarize_call () =
                  Block.Tbl.get intraproc_state.invariant_map blk
                  |> Option.iter ~f:(fun absstate ->
                         let work =
                           intraproc_work ~check_convergence:false absstate
                             blk
                         in
                         Stack.push work intraproc_state.worklist )
                in
                let is_recursive_callee {func; _} =
                  FuncName.equal callee_f func.name
                in
                match blk.term with
                | Call {callee= Direct tgt; _} ->
                    if is_recursive_callee tgt then resummarize_call ()
                | Call {callee= Indirect {candidates; _}; _} ->
                    if IArray.exists candidates ~f:is_recursive_callee then
                      resummarize_call ()
                | _ -> () )
      | Summarize (f, a) -> (
        match lookup_summary f a with
        | Some _ ->
            ()
            (* this summary has already been computed on some other path *)
        | None -> (
          try
            summarize
              (FuncName.Map.find_exn f pgm.functions)
              (init a) worklist
            |> memoize f a
          with Summary_query (callee_f, callee_a) ->
            Worklist.push f a callee_f callee_a worklist ) )
    done ;
    match lookup_summary_exn entry 0 with
    | Summary main_summary ->
        [%Dbg.info "main summary: %a" pp main_summary] ;
        Ok main_summary
    | _ -> Error (Format.dprintf "No path found through entrypoint func")
end

let top_down pgm ~entry sparse_trace =
  Automaton_state.set_sparse_trace sparse_trace ;
  let ( let* ) = Result.bind in
  let* summ = Summary.top_down pgm ~entry in
  if Automaton_state.is_accepting summ.auto_state then
    Ok
      (Distance_map.iteri summ.dists ~f:(fun ~key:block ~data:dist ->
           Block.set_goal_distance dist block ) )
  else
    Error
      (Format.dprintf "Unable to find a path to trace checkpoint %a"
         Automaton_state.pp summ.auto_state )
