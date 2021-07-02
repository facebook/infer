(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The analysis' semantics of control flow. *)

open Domain_intf
open Control_intf

(** An element of work to be scheduled. The scheduling strategies have very
    few dependencies on elements, mainly some just need to test if two
    elements have the same destination. An example element instance is a
    pair of a control-flow edge with a symbolic state that has reached the
    source of the edge and has yet to be propagated to the destination. *)
module type Elt = sig
  type t [@@deriving compare, equal, sexp_of]

  val pp : t pp
  val equal_destination : t -> t -> bool
  val dnf : t -> t list
end

(** Interface of analysis control scheduler "queues". *)
module type QueueS = sig
  type elt

  (** a "queue" of elements, which need not be FIFO *)
  type t

  val pp : t pp

  val create : unit -> t
  (** create an empty queue *)

  val add : elt -> t -> t
  (** add an element *)

  val pop : t -> (elt * elt list * t) option
  (** [pop q] is [None] if [q] is empty and otherwise is [Some (e, es, q')]
      where [e] is the selected element in [q], any elements [es] have the
      same destination as [e], and [q'] is [q] without [e] and [es]. *)
end

(** Type of a queue implementation, which is parameterized over elements. *)
module type Queue = functor (Elt : Elt) -> QueueS with type elt = Elt.t

(** A strategy that implements an iterative breadth-first exploration that
    joins scheduled elements whenever possible, thereby DAGifying the
    execution tree. *)
module PriorityQueue (Elt : Elt) : QueueS with type elt = Elt.t = struct
  type elt = Elt.t

  module Elts = Set.Make (Elt)

  type t = {queue: Elt.t FHeap.t; removed: Elts.t}

  let pp ppf {queue; removed} =
    let rev_elts =
      FHeap.fold queue ~init:[] ~f:(fun rev_elts elt ->
          if Elts.mem elt removed then rev_elts else elt :: rev_elts )
    in
    Format.fprintf ppf "@[%a@]" (List.pp " ::@ " Elt.pp) (List.rev rev_elts)

  let create () = {queue= FHeap.create ~cmp:Elt.compare; removed= Elts.empty}

  let add elt {queue; removed} =
    let removed' = Elts.remove elt removed in
    if removed' == removed then {queue= FHeap.add queue elt; removed}
    else {queue; removed= removed'}

  let rec pop {queue; removed} =
    let* top, queue = FHeap.pop queue in
    let removed' = Elts.remove top removed in
    if removed' != removed then pop {queue; removed= removed'}
    else
      let elts, removed =
        FHeap.fold queue ~init:([], removed') ~f:(fun (elts, removed) elt ->
            if Elt.equal_destination top elt && not (Elts.mem elt removed)
            then (elt :: elts, Elts.add elt removed)
            else (elts, removed) )
      in
      Some (top, elts, {queue; removed})
end

module RandomQueue (Elt : Elt) : QueueS with type elt = Elt.t = struct
  type elt = Elt.t

  module M = Int.Map

  (** The analyzer, after calling [create], performs a sequence of [add] and
      [pop] operations. Implicitly, each [add] is for an element that is a
      successor of the element that was returned by the last [pop]. This
      module assumes this implicit protocol in order to infer the structure
      of the execution tree, and uses it to assign weights aiming to
      implement fair random sampling of paths.

      Each edge of an execution tree conceptually has a "weight" [1/w]
      (represented by just the denominator [w]) indicating that there is a
      [1] in [w] chance of making the sequence of branching choices leading
      to the edge. An execution tree starts with a single edge to the
      initial control point with weight [1]. For an edge with weight [1/w]
      that has [n] successors, the edge to each of the successors has weight
      [1 / (w * n)].

      The scheduling "frontier" is a set of edges with weights (represented
      as a map from weights to lists of edges) that have been reached but
      not followed.

      Edges are selected from the frontier randomly using the weights to
      simulate fair sampling of paths as follows. Let [{eᵢ}] be a sequence
      of the edges of the frontier in decreasing weight order. The weight of
      edge [eᵢ] is written [wᵢ]. The sum of the weights of the frontier
      is [s = Σᵢ wᵢ]. Now choose a random number [0 ≤ n ≤ s]. This
      determines an edge [eᵣ] where [r] is least such that
      [Σᵢ₌₀ʳ wᵢ ≥ n].

      The inferred structure of the execution tree is also used to schedule
      the analysis to proceed depth-first where a random successor is chosen
      at each point until no further progress is possible, at which point a
      new path is sampled. Successors are added by the analyzer prior to
      knowing whether they are feasible. For example executing a conditional
      branch results in two [add] operations where the next instruction on
      each is to assume the condition and the negation of the condition. To
      avoid depth-first execution from being thwarted by choosing infeasible
      branches in such cases, a [recent] list is maintained that contains
      the successors of the last popped element. When an element is popped
      from the recent list, it is not known whether or not it is immediately
      infeasible. If it is, the next operation will be another [pop], and
      this is also taken from the [recent] list. If the element was not
      immediately infeasible, the next operation is an [add] (of a
      successor), at which point the recent list is flushed to the
      "frontier". In this way, each [pop] that requests the next branch to
      explore is chosen from the successors of the last control point,
      effecting depth-first exploration. Only when the recent list is empty,
      is an element chosen from the "frontier" of untaken branches. *)

  type t =
    { recent: Elt.t RAL.t  (** elements added since last pop; add *)
    ; recent_weight: int  (** combined weight of recent *)
    ; frontier: Elt.t RAL.t M.t  (** weight-keyed elements to be explored *)
    ; frontier_weight: float  (** combined weight of frontier *)
    ; last: last_operation  (** single step of execution history *) }

  and last_operation =
    | Add_or_pop_frontier
        (** last operation was either [add] or [pop] where [recent] was
            empty *)
    | Pop_recent of int
        (** last operation was [pop] where [recent] was not empty, and the
            returned element had given weight *)

  let pp ppf {recent; frontier} =
    Format.fprintf ppf "@[%a @@@ %a@]" (List.pp " ::@ " Elt.pp)
      (RAL.to_list recent)
      (M.pp Int.pp (RAL.pp " ::@ " Elt.pp))
      frontier

  let create () =
    { recent= RAL.empty
    ; recent_weight= 1
    ; frontier= M.empty
    ; frontier_weight= 0.
    ; last= Add_or_pop_frontier }

  let add elt q =
    let add_elt l = List.fold ~f:RAL.cons (Elt.dnf elt) l in
    match q.last with
    | Add_or_pop_frontier ->
        (* elt is a sibling of the elements of recent, so extend recent *)
        {q with recent= add_elt q.recent}
    | Pop_recent elt_weight ->
        (* elt is a successor of the last popped element (which is itself a
           sibling of the elements of recent), so flush recent to frontier
           and reset recent to the singleton elt with a combined weight
           equal to that of the previously popped element *)
        { recent= add_elt RAL.empty
        ; recent_weight= elt_weight
        ; frontier=
            ( if RAL.is_empty q.recent then q.frontier
            else
              M.update elt_weight q.frontier ~f:(function
                | Some data -> Some (RAL.append q.recent data)
                | None -> Some q.recent ) )
        ; frontier_weight=
            q.frontier_weight
            +. Float.of_int (RAL.length q.recent)
               /. Float.of_int elt_weight
        ; last= Add_or_pop_frontier }

  let pop q =
    let num_recent = RAL.length q.recent in
    if num_recent > 0 then
      let elt, recent =
        RAL.get_and_remove_exn q.recent (Random.int num_recent)
      in
      match q.last with
      | Pop_recent _ ->
          (* elt is sibling to last popped element, with same elt_weight *)
          Some (elt, [], {q with recent})
      | Add_or_pop_frontier ->
          (* recent is now complete, and weight of each element can be
             computed from combined weight and length *)
          let elt_weight = q.recent_weight * num_recent in
          Some (elt, [], {q with recent; last= Pop_recent elt_weight})
    else
      let random_weight = Random.float q.frontier_weight in
      M.fold_until q.frontier 0.
        ~f:(fun ~key ~data prefix_weight ->
          let len = RAL.length data in
          let w = Float.of_int len /. Float.of_int key in
          let prefix_weight = prefix_weight +. w in
          if Float.(prefix_weight < random_weight) then
            `Continue prefix_weight
          else
            let elt, data = RAL.get_and_remove_exn data (Random.int len) in
            `Stop
              (Some
                 ( elt
                 , []
                 , { recent= RAL.empty
                   ; recent_weight= key
                   ; frontier=
                       ( if RAL.is_empty data then M.remove key q.frontier
                       else M.add ~key ~data q.frontier )
                   ; frontier_weight= q.frontier_weight -. w
                   ; last= Add_or_pop_frontier } )) )
        ~finish:(fun _ ->
          assert (M.is_empty q.frontier) ;
          None )
end

module Make (Config : Config) (D : Domain) (Queue : Queue) = struct
  module Stack : sig
    type t

    val pp : t pp
    val empty : t
    val push_call : Llair.func Llair.call -> D.from_call -> t -> t
    val pop_return : t -> (D.from_call * Llair.jump * t) option

    val pop_throw :
         t
      -> 'a
      -> unwind:
           (   Llair.Reg.t iarray
            -> Llair.Reg.Set.t
            -> D.from_call
            -> 'a
            -> 'a)
      -> (D.from_call * Llair.jump * t * 'a) option

    type as_inlined_location = t [@@deriving compare, equal, sexp_of]
  end = struct
    type t =
      | Return of
          { dst: Llair.Jump.t
          ; formals: Llair.Reg.t iarray
          ; locals: Llair.Reg.Set.t
          ; from_call: D.from_call
          ; stk: t }
      | Throw of Llair.Jump.t * t
      | Empty
    [@@deriving sexp_of]

    let rec pp ppf stk =
      let pp ppf = function
        | Empty -> ()
        | stk -> Format.fprintf ppf "; %a" pp stk
      in
      match stk with
      | Return {dst; stk= s} ->
          Format.fprintf ppf "R#%i%a" dst.dst.sort_index pp s
      | Throw (dst, s) ->
          Format.fprintf ppf "T#%i%a" dst.dst.sort_index pp s
      | Empty -> ()

    let invariant s =
      let@ () = Invariant.invariant [%here] s [%sexp_of: t] in
      match s with
      | Return _ | Throw (_, Return _) | Empty -> ()
      | Throw _ -> fail "malformed stack: %a" pp s ()

    let empty = Empty |> check invariant

    let push_return call from_call stk =
      let Llair.{callee= {formals; locals}; return; _} = call in
      Return {dst= return; formals; locals; from_call; stk}
      |> check invariant

    let push_throw call stk =
      ( match call.Llair.throw with
      | None -> stk
      | Some jmp -> Throw (jmp, stk) )
      |> check invariant

    let push_call call from_call stk =
      push_throw call (push_return call from_call stk)

    let rec pop_return = function
      | Throw (_, stk) -> pop_return stk
      | Return {from_call; dst; stk} -> Some (from_call, dst, stk)
      | Empty -> None

    let pop_throw stk state ~unwind =
      let rec pop_throw_ state = function
        | Return {formals; locals; from_call; stk} ->
            pop_throw_ (unwind formals locals from_call state) stk
        | Throw (dst, Return {from_call; stk}) ->
            Some (from_call, dst, stk, state)
        | Empty -> None
        | Throw _ as stk -> violates invariant stk
      in
      pop_throw_ state stk

    type as_inlined_location = t [@@deriving sexp_of]

    (* Treat a stack as a code location in a hypothetical expansion of the
       program where functions have been inlined. In particular, only the
       dst and stk of Return frames is considered. *)
    let rec compare_as_inlined_location x y =
      if x == y then 0
      else
        match (x, y) with
        | Return {dst= j; stk= x}, Return {dst= k; stk= y} -> (
          match Llair.Jump.compare j k with
          | 0 -> compare_as_inlined_location x y
          | n -> n )
        | Return _, _ -> -1
        | _, Return _ -> 1
        | Throw (j, x), Throw (k, y) -> (
          match Llair.Jump.compare j k with
          | 0 -> compare_as_inlined_location x y
          | n -> n )
        | Throw _, _ -> -1
        | _, Throw _ -> 1
        | Empty, Empty -> 0

    let equal_as_inlined_location = [%compare.equal: as_inlined_location]
  end

  (** Instruction Pointer. Functions are treated as-if-inlined by including
      a call stack in each instruction pointer, effectively copying the
      control-flow graph for each calling context. *)
  type ip = {ip: Llair.IP.t; stk: Stack.t}

  (** Instruction Pointer *)
  module IP : sig
    type t = ip [@@deriving compare, equal, sexp_of]

    val pp : t pp
  end = struct
    type t = ip = {ip: Llair.IP.t; stk: Stack.as_inlined_location}
    [@@deriving compare, equal, sexp_of]

    let pp ppf {ip} = Llair.IP.pp ppf ip
  end

  (** A control-flow transition. An edge from block [src] to
      [dst = {ip; stk}] represents a transition with call stack [stk] from
      (the terminator of) block [src] to the instruction pointer [ip]. *)
  type edge = {dst: IP.t; src: Llair.Block.t} [@@deriving sexp_of]

  module Edge = struct
    type t = edge [@@deriving sexp_of]

    let pp fs {dst; src= {sort_index; lbl}} =
      Format.fprintf fs "%a <-- #%i %%%s" IP.pp dst sort_index lbl

    (** Each retreating edge has a depth for each calling context, except
        for recursive calls. Recursive call edges are instead compared
        without considering their stacks. Bounding the depth of edges
        therefore has the effect of bounding the number of recursive calls
        in any calling context. *)
    let compare x y =
      let open Ord.Infix in
      if x == y then 0
      else
        let is_rec_call = function
          | {Llair.term= Call {recursive= true}} -> true
          | _ -> false
        in
        let compare_stk stk1 stk2 =
          if is_rec_call x.src then 0
          else Stack.compare_as_inlined_location stk1 stk2
        in
        Llair.IP.compare x.dst.ip y.dst.ip
        <?> (Llair.Block.compare, x.src, y.src)
        <?> (compare_stk, x.dst.stk, y.dst.stk)

    let equal = [%compare.equal: t]
  end

  module Depths = struct
    module M = Map.Make (Edge)

    type t = int M.t [@@deriving compare, equal, sexp_of]

    let empty = M.empty
    let find = M.find
    let add = M.add

    let join x y =
      M.merge x y ~f:(fun _ -> function
        | `Left d | `Right d -> Some d
        | `Both (d1, d2) -> Some (Int.max d1 d2) )
  end

  (** Abstract memory, control, and history state, with a slot used for the
      current "control position", such as an instruction pointer. Consists
      of a symbolic [state], plus a coarse abstraction of the preceding
      execution history in the form of [depths] representing the number of
      times retreating edges have been crossed. *)
  type 'a memory_control_history =
    { ctrl: 'a  (** current control position *)
    ; state: D.t  (** symbolic memory and register state *)
    ; depths: Depths.t  (** count of retreating edge crossings *) }
  [@@deriving sexp_of]

  (** An abstract machine state consists of the instruction pointer plus the
      memory, control, and history state. *)
  type ams = IP.t memory_control_history [@@deriving sexp_of]

  (** A unit of analysis work is an abstract machine state from which
      execution should continue, with additional control-flow [edge] info
      used by the analysis scheduler. *)
  type work = edge memory_control_history

  (** An element of the frontier of execution is a control-flow [edge] that
      has been executed, yielding a memory, control, and history state. *)
  type elt = elt_ctrl memory_control_history [@@deriving sexp_of]

  and elt_ctrl =
    { edge: Edge.t
    ; depth: int
          (** pre-computed depth of [edge], for use by e.g. [Elt.compare] *)
    }

  module Work : sig
    type t

    val init : D.t -> Llair.block -> t
    val add : retreating:bool -> work -> t -> t
    val run : f:(ams -> t -> t) -> t -> unit
  end = struct
    (** Element of the frontier of execution, ordered for scheduler's
        priority queue *)
    module Elt = struct
      type t = elt [@@deriving sexp_of]

      let pp ppf {ctrl= {edge; depth}} =
        Format.fprintf ppf "%i: %a" depth Edge.pp edge

      let compare x y =
        let open Ord.Infix in
        if x == y then 0
        else
          ( (Int.compare >|= fun x -> x.ctrl.depth)
          @? (Edge.compare >|= fun x -> x.ctrl.edge)
          @? (Depths.compare >|= fun x -> x.depths)
          @? (D.compare >|= fun x -> x.state) )
            x y

      let equal = [%compare.equal: t]
      let equal_destination x y = IP.equal x.ctrl.edge.dst y.ctrl.edge.dst
      let dnf x = List.map ~f:(fun state -> {x with state}) (D.dnf x.state)
    end

    module Queue = Queue (Elt)

    (** State and history projection of abstract machine states.
        [StateHistory] represents the subset of [ams] fields that can be
        joined across several executions. *)
    module StateHistory = struct
      module T = struct
        type t = D.t * Depths.t [@@deriving compare, equal, sexp_of]
      end

      include T
      module Set = Set.Make (T)

      let join s =
        let states, depths =
          Set.fold s ([], Depths.empty) ~f:(fun (q, d) (qs, ds) ->
              let qqs =
                match qs with
                | q0 :: _ when D.equal q q0 -> qs
                | _ -> q :: qs
              in
              (qqs, Depths.join d ds) )
        in
        (D.joinN states, depths)
    end

    (** Analysis exploration state *)
    type t = Queue.t

    let prune depth {ctrl= edge} wl =
      [%Trace.info " %i: %a" depth Edge.pp edge] ;
      Report.hit_bound Config.bound ;
      wl

    let enqueue depth ({ctrl= edge; depths} as elt) queue =
      [%Trace.info " %i: %a@ | %a" depth Edge.pp edge Queue.pp queue] ;
      let depths = Depths.add ~key:edge ~data:depth depths in
      let queue = Queue.add {elt with ctrl= {edge; depth}; depths} queue in
      queue

    let init state curr =
      let depth = 0 in
      let ip = Llair.IP.mk curr in
      let stk = Stack.empty in
      let prev = curr in
      let edge = {dst= {ip; stk}; src= prev} in
      let depths = Depths.empty in
      let queue = Queue.create () in
      enqueue depth {ctrl= edge; state; depths} queue

    let add ~retreating ({ctrl= edge; depths} as elt) wl =
      let depth = Option.value (Depths.find edge depths) ~default:0 in
      let depth = if retreating then depth + 1 else depth in
      if depth > Config.bound && Config.bound >= 0 then prune depth elt wl
      else enqueue depth elt wl

    let dequeue queue =
      let+ ({ctrl= {edge= {dst}}; state; depths} as top), elts, queue =
        Queue.pop queue
      in
      [%Trace.info
        " %i: %a [%a]@ | %a" top.ctrl.depth Edge.pp top.ctrl.edge Stack.pp
          dst.stk Queue.pp queue] ;
      let state, depths =
        StateHistory.join
          (List.fold
             ~f:(fun {state; depths} -> StateHistory.Set.add (state, depths))
             elts
             (StateHistory.Set.of_ (state, depths)))
      in
      ({ctrl= dst; state; depths}, queue)

    let rec run ~f wl =
      match dequeue wl with
      | Some (ams, wl) -> run ~f (f ams wl)
      | None -> ()
  end

  let summary_table = Llair.Function.Tbl.create ()

  let pp_st () =
    [%Trace.printf
      "@[<v>%t@]" (fun fs ->
          Llair.Function.Tbl.iteri summary_table ~f:(fun ~key ~data ->
              Format.fprintf fs "@[<v>%a:@ @[%a@]@]@ " Llair.Function.pp key
                (List.pp "@," D.pp_summary)
                data ) )]

  let exec_jump jump ({ctrl= {ip; stk}} as ams) wl =
    let src = Llair.IP.block ip in
    let {Llair.dst; retreating} = jump in
    let ip = Llair.IP.mk dst in
    let edge = {dst= {ip; stk}; src} in
    Work.add ~retreating {ams with ctrl= edge} wl

  let exec_skip_func areturn return ({ctrl= {ip}; state} as ams) wl =
    Report.unknown_call (Llair.IP.block ip).term ;
    let state = Option.fold ~f:D.exec_kill areturn state in
    exec_jump return {ams with state} wl

  let exec_call globals call ({ctrl= {stk}; state} as ams) wl =
    let Llair.{callee; actuals; areturn; return; recursive} = call in
    let Llair.{name; formals; freturn; locals; entry} = callee in
    [%Trace.call fun {pf} ->
      pf " @[<2>@ %a from %a with state@]@;<1 2>%a" Llair.Func.pp_call call
        Llair.Function.pp return.dst.parent.name D.pp state]
    ;
    let dnf_states =
      if Config.function_summaries then D.dnf state else [state]
    in
    let domain_call =
      D.call ~globals ~actuals ~areturn ~formals ~freturn ~locals
    in
    List.fold dnf_states wl ~f:(fun state wl ->
        match
          if not Config.function_summaries then None
          else
            let state = fst (domain_call ~summaries:false state) in
            let* summary = Llair.Function.Tbl.find summary_table name in
            List.find_map ~f:(D.apply_summary state) summary
        with
        | None ->
            let state, from_call =
              domain_call ~summaries:Config.function_summaries state
            in
            let ip = Llair.IP.mk entry in
            let stk = Stack.push_call call from_call stk in
            let src = Llair.IP.block ams.ctrl.ip in
            let edge = {dst= {ip; stk}; src} in
            Work.add ~retreating:recursive {ams with ctrl= edge; state} wl
        | Some post -> exec_jump return {ams with state= post} wl )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_call call ams wl =
    let Llair.{callee= {name} as callee; areturn; return; _} = call in
    if Llair.Func.is_undefined callee then
      exec_skip_func areturn return ams wl
    else
      let globals = Domain_used_globals.by_function Config.globals name in
      exec_call globals call ams wl

  let exec_return exp ({ctrl= {ip; stk}; state} as ams) wl =
    let block = Llair.IP.block ip in
    let func = block.parent in
    let Llair.{name; formals; freturn; locals} = func in
    [%Trace.call fun {pf} -> pf " @ from: %a" Llair.Function.pp name]
    ;
    let summarize post_state =
      if not Config.function_summaries then post_state
      else
        let function_summary, post_state =
          D.create_summary ~locals ~formals post_state
        in
        Llair.Function.Tbl.add_multi ~key:name ~data:function_summary
          summary_table ;
        pp_st () ;
        post_state
    in
    let pre_state = state in
    let exit_state =
      match (freturn, exp) with
      | Some freturn, Some return_val ->
          D.exec_move (IArray.of_ (freturn, return_val)) pre_state
      | None, None -> pre_state
      | _ -> violates Llair.Func.invariant func
    in
    ( match Stack.pop_return stk with
    | Some (from_call, retn_site, stk) ->
        let post_state = summarize (D.post locals from_call exit_state) in
        let retn_state = D.retn formals freturn from_call post_state in
        exec_jump retn_site
          {ams with ctrl= {ams.ctrl with stk}; state= retn_state}
          wl
    | None ->
        if Config.function_summaries then
          summarize exit_state |> (ignore : D.t -> unit) ;
        wl )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_throw exc ({ctrl= {ip; stk}; state} as ams) wl =
    let func = (Llair.IP.block ip).parent in
    let Llair.{name; formals; freturn; fthrow; locals} = func in
    [%Trace.call fun {pf} -> pf "@ from %a" Llair.Function.pp name]
    ;
    let unwind formals scope from_call state =
      D.retn formals (Some fthrow) from_call (D.post scope from_call state)
    in
    let pre_state = state in
    ( match Stack.pop_throw stk ~unwind pre_state with
    | Some (from_call, retn_site, stk, unwind_state) ->
        let exit_state =
          D.exec_move (IArray.of_ (fthrow, exc)) unwind_state
        in
        let post_state = D.post locals from_call exit_state in
        let retn_state = D.retn formals freturn from_call post_state in
        exec_jump retn_site
          {ams with ctrl= {ams.ctrl with stk}; state= retn_state}
          wl
    | None -> wl )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_assume cond jump ({state} as ams) wl =
    match D.exec_assume state cond with
    | Some state -> exec_jump jump {ams with state} wl
    | None ->
        [%Trace.info " infeasible %a@\n@[%a@]" Llair.Exp.pp cond D.pp state] ;
        wl

  let resolve_callee (pgm : Llair.program) callee state =
    let lookup name = Llair.Func.find name pgm.functions in
    D.resolve_callee lookup callee state

  let exec_term pgm ({ctrl= {ip}; state} as ams) wl =
    let block = Llair.IP.block ip in
    let term = block.term in
    [%Trace.info " @\n@[%a@]@\n%a" D.pp state Llair.Term.pp block.term] ;
    Report.step_term block ;
    match (term : Llair.term) with
    | Switch {key; tbl; els} ->
        let wl =
          exec_assume
            (IArray.fold tbl Llair.Exp.true_ ~f:(fun (case, _) b ->
                 Llair.Exp.and_ (Llair.Exp.dq key case) b ))
            els ams wl
        in
        IArray.fold tbl wl ~f:(fun (case, jump) wl ->
            exec_assume (Llair.Exp.eq key case) jump ams wl )
    | Iswitch {ptr; tbl} ->
        IArray.fold tbl wl ~f:(fun jump wl ->
            exec_assume
              (Llair.Exp.eq ptr
                 (Llair.Exp.label
                    ~parent:(Llair.Function.name jump.dst.parent.name)
                    ~name:jump.dst.lbl))
              jump ams wl )
    | Call call -> exec_call call ams wl
    | ICall ({callee; areturn; return} as call) -> (
      match resolve_callee pgm callee state with
      | [] -> exec_skip_func areturn return ams wl
      | callees ->
          List.fold callees wl ~f:(fun callee wl ->
              exec_call {call with callee} ams wl ) )
    | Return {exp} -> exec_return exp ams wl
    | Throw {exc} -> exec_throw exc ams wl
    | Unreachable -> wl

  let rec exec_ip pgm ({ctrl= {ip}; state} as ams) wl =
    match Llair.IP.inst ip with
    | Some inst -> (
        [%Trace.info " @\n@[%a@]@\n%a" D.pp state Llair.Inst.pp inst] ;
        Report.step_inst ip ;
        match D.exec_inst inst state with
        | Ok state ->
            let ip = Llair.IP.succ ip in
            exec_ip pgm {ams with ctrl= {ams.ctrl with ip}; state} wl
        | Error alarm ->
            Report.alarm alarm ;
            wl )
    | None -> exec_term pgm ams wl

  let call_entry_point pgm =
    let+ {name; formals; freturn; locals; entry} =
      List.find_map Config.entry_points ~f:(fun entry_point ->
          let* func = Llair.Func.find entry_point pgm.Llair.functions in
          if IArray.is_empty func.formals then Some func else None )
    in
    let summaries = Config.function_summaries in
    let globals = Domain_used_globals.by_function Config.globals name in
    let actuals = IArray.empty in
    let areturn = None in
    let state, _ =
      D.call ~summaries ~globals ~actuals ~areturn ~formals ~freturn ~locals
        (D.init pgm.globals)
    in
    Work.init state entry

  let exec_pgm pgm =
    match call_entry_point pgm with
    | Some wl -> Work.run ~f:(exec_ip pgm) wl
    | None -> fail "no entry point found" ()

  let compute_summaries pgm =
    assert Config.function_summaries ;
    exec_pgm pgm ;
    Llair.Function.Tbl.fold summary_table Llair.Function.Map.empty
      ~f:(fun ~key ~data map ->
        match data with
        | [] -> map
        | _ -> Llair.Function.Map.add ~key ~data map )
end
[@@inlined]
