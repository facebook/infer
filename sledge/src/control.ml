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

  module Priority : sig
    type t [@@deriving compare]
  end

  val pp : t pp
  val prio : t -> Priority.t
  val dnf : t -> t iter
end

(** Interface of analysis control scheduler "queues". *)
module type QueueS = sig
  (** a single element of work *)
  type elt

  (** a "queue" of elements, which need not be FIFO *)
  type t

  val pp : t pp

  val create : unit -> t
  (** create an empty queue *)

  val add : elt -> t -> t
  (** add an element *)

  val top : t -> (elt * elt iter * t) option
  (** [top q] is [None] if [q] is empty and otherwise is [Some (e, es, q')]
      where [e] is the selected element in [q] and any elements [es] have
      the same destination as [e]. [q'] is equivalent to [q] but possibly
      more compactly represented. *)

  val remove_top : t -> t
  (** [remove_top q] is [q'] where [q'] is [q] with all elements returned by
      [top] removed *)
end

(** Type of a queue implementation, which is parameterized over elements. *)
module type Queue = functor (Elt : Elt) -> QueueS with type elt = Elt.t

(** A strategy that implements an iterative breadth-first exploration that
    joins scheduled elements whenever possible, thereby DAGifying the
    execution tree. *)
module PriorityQueue (Elt : Elt) : QueueS with type elt = Elt.t = struct
  type elt = Elt.t

  module Pq = Psq.Make (Elt) (Elt.Priority)

  type t = Pq.t

  let pp =
    let sep f () = Format.fprintf f " ::@ " in
    Pq.pp ~sep (fun f (elt, _) -> Elt.pp f elt)

  let create () = Pq.empty
  let add elt = Pq.add elt (Elt.prio elt)

  let top q =
    let+ top_elt, top_prio = Pq.min q in
    let elts =
      Iter.from_iter (fun f -> Pq.iter_at_most top_prio (fun x _ -> f x) q)
    in
    (top_elt, elts, q)

  let remove_top q =
    match Pq.min q with
    | None -> q
    | Some (_, top_prio) ->
        Pq.fold_at_most top_prio (fun e _ q -> Pq.remove e q) q q
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
      edge [eᵢ] is written [wᵢ]. The sum of the weights of the frontier is
      [s = Σᵢ wᵢ]. Now choose a random number [0 ≤ n ≤ s]. This determines
      an edge [eᵣ] where [r] is least such that [Σᵢ₌₀ʳ wᵢ ≥ n].

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
    let add_elt l = Iter.fold ~f:RAL.cons (Elt.dnf elt) l in
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

  let _pop q =
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
                   ; last= Add_or_pop_frontier } ) ) )
        ~finish:(fun _ ->
          assert (M.is_empty q.frontier) ;
          None )

  let top _ = todo "concurrent sampling analysis" ()
  let remove_top _ = todo "concurrent sampling analysis" ()
end

module MakeDirected
    (Config : Config)
    (D : Domain)
    (Queue : Queue)
    (Goal : Goal.S) =
struct
  module Stack : sig
    type t

    val empty : t
    val push_call : Llair.call_target Llair.call -> D.from_call -> t -> t
    val pop_return : t -> (D.from_call * Llair.jump * t) option

    val pop_throw :
         t
      -> 'a
      -> unwind:
           (Llair.Reg.t iarray -> Llair.Reg.Set.t -> D.from_call -> 'a -> 'a)
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
      let Llair.{callee= {func= {formals; locals}; _}; return; _} = call in
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

  (** Instruction Pointer, of a single thread so includes thread id.
      Functions are treated as-if-inlined by including a call stack in each
      instruction pointer, effectively copying the control-flow graph for
      each calling context. *)
  type ip = {ip: Llair.IP.t; stk: Stack.t; tid: ThreadID.t}

  (** Instruction Pointer of a single thread *)
  module IP : sig
    type t = ip [@@deriving compare, equal, sexp_of]

    val pp : t pp
  end = struct
    type t = ip =
      {ip: Llair.IP.t; stk: Stack.as_inlined_location; tid: ThreadID.t}
    [@@deriving compare, equal, sexp_of]

    let pp ppf {ip} = Llair.IP.pp ppf ip
  end

  (** Representation of a single thread, including identity and scheduling
      state *)
  module Thread : sig
    type t =
      | Runnable of IP.t
      | Suspended of IP.t
      | Terminated of D.term_code * ThreadID.t
    [@@deriving equal, sexp_of]

    val compare : t Ord.t
    val compare_without_tid : t Ord.t
    val pp : t pp
    val id : t -> ThreadID.t
    val ip : t -> Llair.IP.t option
  end = struct
    (** Because [ip] needs to include [tid], this is represented as a sum of
        products, but it may be more natural to think in terms of the
        isomorphic representation using a product of a sum such as
        [(Runnable of ... | ...) * ThreadID.t]. *)
    type t =
      | Runnable of IP.t
      | Suspended of IP.t
      | Terminated of D.term_code * ThreadID.t
    [@@deriving sexp_of]

    let pp ppf = function
      | Runnable ip -> IP.pp ppf ip
      | Suspended ip -> Format.fprintf ppf "S%a" IP.pp ip
      | Terminated (_, tid) -> Format.fprintf ppf "T%i" tid

    let id = function
      | Runnable {tid} | Suspended {tid} -> tid
      | Terminated (_, tid) -> tid

    let ip = function Runnable ip | Suspended ip -> Some ip.ip | _ -> None

    (* Note: Threads.inactive relies on comparing tid last *)
    let compare_aux compare_tid x y =
      let open Ord.Infix in
      if x == y then 0
      else
        match (x, y) with
        | Runnable x, Runnable y | Suspended x, Suspended y ->
            Llair.IP.compare x.ip y.ip
            <?> (Stack.compare_as_inlined_location, x.stk, y.stk)
            <?> (compare_tid, x.tid, y.tid)
        | Runnable _, _ -> -1
        | _, Runnable _ -> 1
        | Suspended _, _ -> -1
        | _, Suspended _ -> 1
        | Terminated (x_tc, x_tid), Terminated (y_tc, y_tid) ->
            D.compare_term_code x_tc y_tc <?> (compare_tid, x_tid, y_tid)

    let compare = compare_aux ThreadID.compare
    let equal = [%compare.equal: t]
    let compare_without_tid = compare_aux (fun _ _ -> 0)
  end

  (** Set of threads *)
  module Threads : sig
    type t [@@deriving compare, equal, sexp_of]

    val pp : t pp

    type inactive [@@deriving sexp_of]

    val compare_inactive : inactive Ord.t
    val compare_inactive_tids : inactive Ord.t
    val init : t
    val create : Llair.block -> t -> ThreadID.t * t
    val after_step : Thread.t -> t -> t * inactive
    val resume : ThreadID.t -> t -> t option
    val join : ThreadID.t -> t -> (D.term_code * t) option
    val fold : t -> 's -> f:(Thread.t -> 's -> 's) -> 's
  end = struct
    module M = Map.Make (ThreadID)

    type t = Thread.t M.t [@@deriving compare, equal, sexp_of]

    let pp ppf threads =
      Format.fprintf ppf "@[[%a]@]" (List.pp "" Thread.pp)
        (Iter.to_list (M.values threads))

    type inactive = Thread.t array [@@deriving sexp_of]

    let compare_inactive = Ord.array Thread.compare_without_tid
    let compare_inactive_tids = Ord.(array (ThreadID.compare >|= Thread.id))

    let inactive active_id threads =
      let a = Iter.to_array (M.values (M.remove active_id threads)) in
      Array.sort ~cmp:Thread.compare a ;
      a

    let init = M.empty

    let fold threads s ~f =
      M.fold threads s ~f:(fun ~key:_ ~data s -> f data s)

    let create entry threads =
      let ip = Llair.IP.mk entry in
      let max_tid =
        match M.max_binding threads with
        | Some (tid, _) -> tid
        | None -> ThreadID.init
      in
      let tid = max_tid + 1 in
      let thread = Thread.Suspended {ip; stk= Stack.empty; tid} in
      (tid, M.add ~key:tid ~data:thread threads)

    let after_step active threads =
      let tid = Thread.id active in
      let inactive = inactive tid threads in
      let threads = M.add ~key:tid ~data:active threads in
      (threads, inactive)

    let resume tid threads =
      match M.find tid threads with
      | Some (Thread.Suspended ip) ->
          Some (M.add ~key:tid ~data:(Thread.Runnable ip) threads)
      | _ ->
          [%Dbg.info " prune resume of non-suspended thread: %i" tid] ;
          None

    let join tid threads =
      match M.find tid threads with
      | Some (Thread.Terminated (tc, _)) -> Some (tc, M.remove tid threads)
      | _ ->
          [%Dbg.info " prune join of non-terminated thread: %i" tid] ;
          None
  end

  (** A control-flow transition of a single thread. In the common case an
      edge from block [src] to [dst = Runnable {ip; stk; tid}] represents a
      transition by thread [tid] with call stack [stk] from (usually the
      terminator of) block [src] to the instruction pointer [ip]. If a
      scheduling point is encountered within a block, the represented
      transition need not originate from the terminator of [src]. Edges can
      also represent transitions that produce threads in non-[Runnable]
      scheduling states, determined by the form of [dst]. *)
  type edge = {dst: Thread.t; src: Llair.Block.t; retreating: bool}
  [@@deriving sexp_of]

  module Edge = struct
    type t = edge [@@deriving sexp_of]

    let pp fs {dst; src= {sort_index}} =
      Format.fprintf fs "%a <-t%i- #%i" Thread.pp dst (Thread.id dst)
        sort_index

    (** Each retreating edge has a depth for each calling context, except
        for recursive calls. Recursive call edges are instead compared
        without considering their stacks. Bounding the depth of edges
        therefore has the effect of bounding the number of recursive calls
        in any calling context. *)
    let compare_aux compare_tid x y =
      let open Ord.Infix in
      if x == y then 0
      else
        match (x, y) with
        | {dst= Runnable x_t}, {dst= Runnable y_t}
         |{dst= Suspended x_t}, {dst= Suspended y_t} ->
            let compare_stk stk1 stk2 =
              if x.retreating then 0
              else Stack.compare_as_inlined_location stk1 stk2
            in
            Llair.IP.compare x_t.ip y_t.ip
            <?> (Llair.Block.compare, x.src, y.src)
            <?> (compare_stk, x_t.stk, y_t.stk)
            <?> (compare_tid, x_t.tid, y_t.tid)
        | {dst= Runnable _}, _ -> -1
        | _, {dst= Runnable _} -> 1
        | {dst= Suspended _}, _ -> -1
        | _, {dst= Suspended _} -> 1
        | {dst= Terminated (x_tc, x_tid)}, {dst= Terminated (y_tc, y_tid)}
          ->
            Llair.Block.compare x.src y.src
            <?> (D.compare_term_code, x_tc, y_tc)
            <?> (compare_tid, x_tid, y_tid)

    let compare = compare_aux ThreadID.compare
    let equal = [%compare.equal: t]
    let compare_without_tid = compare_aux (fun _ _ -> 0)
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

  module Hist = struct
    (** a history is a current instruction pointer and some list of
        predecessors. [preds] are empty iff this is an entrypoint. *)
    type t = {curr: Llair.IP.t; preds: t iarray} [@@deriving sexp_of]

    let init ip = {curr= ip; preds= IArray.empty}
    let extend curr preds = {curr; preds= IArray.of_list preds}

    let dump h fs =
      (* todo: output nicely-formatted DAG; just printing a single
         arbitrarily-chosen witness path from the root for now. *)
      let path =
        let rec path_impl h =
          let tail =
            if IArray.is_empty h.preds then []
            else path_impl (IArray.get h.preds 0)
          in
          if Llair.IP.index h.curr = 0 || IArray.length h.preds > 1 then
            h.curr :: tail
          else tail
        in
        path_impl >> List.rev
      in
      let pp_ip fs ip =
        let open Llair in
        Format.fprintf fs "%a%a%a" FuncName.pp (IP.block ip).parent.name
          IP.pp ip Loc.pp (IP.loc ip)
      in
      Format.fprintf fs "@[<v 2>Witness Trace:@ %a@]" (List.pp "@ " pp_ip)
        (path h)
  end

  type switches = int [@@deriving compare, equal, sexp_of]

  (** Abstract memory, control, and history state, with a slot used for the
      current "control position", such as an instruction pointer. Consists
      of a symbolic [state] and a scheduling state of the [threads], plus a
      coarse abstraction of the preceding execution history in the form of
      the number of context [switches] and [depths] representing the number
      of times retreating edges have been crossed. *)
  type 'a memory_control_history =
    { ctrl: 'a  (** current control position *)
    ; state: D.t  (** symbolic memory and register state *)
    ; threads: Threads.t  (** scheduling state of the threads *)
    ; switches: switches  (** count of preceding context switches *)
    ; depths: Depths.t  (** count of retreating edge crossings *)
    ; goal: Goal.t  (** goal for symbolic execution exploration *)
    ; history: Hist.t  (** DAG history of executions to this point *) }
  [@@deriving sexp_of]

  (** An abstract machine state consists of the instruction pointer of the
      active thread plus the memory, control, and history state. *)
  type ams = IP.t memory_control_history [@@deriving sexp_of]

  (** A unit of analysis work is an abstract machine state from which
      execution should continue, with additional control-flow [edge] info
      used by the analysis scheduler. *)
  type work = edge memory_control_history

  (** An element of the frontier of execution is a control-flow [edge] that
      has been executed by the thread [Thread.id edge.dst], yielding a
      memory, control, and history state. The [threads] indicates the
      scheduling state of the point after the transition indicated by the
      edge. *)
  type elt = elt_ctrl memory_control_history [@@deriving sexp_of]

  and elt_ctrl =
    { edge: Edge.t
    ; depth: int
          (** pre-computed depth of [edge], for use by e.g. [Elt.compare] *)
    ; inactive: Threads.inactive
          (** pre-computed summary of inactive thread scheduling states, for
              use by e.g. [Elt.compare] *) }

  let pp_state ppf state = [%Dbg.fprintf ppf "@[%a@]@\n" D.pp state]

  module Work : sig
    type t

    val init : D.t -> Llair.block -> Goal.t -> t
    val add : work -> t -> t
    val run : f:(ams -> t -> t) -> t -> unit
  end = struct
    (** Element of the frontier of execution, ordered for scheduler's
        priority queue *)
    module Elt = struct
      type t = elt [@@deriving sexp_of]

      module Priority = struct
        type t = {goal: Goal.t; dst: Llair.block option; threads: Threads.t}

        let status {goal; dst; threads= _} = Goal.status goal dst
        let threads {goal= _; dst= _; threads} = threads

        let compare =
          let open Ord.Infix in
          (Goal.compare_status >|= status) @? (Threads.compare >|= threads)
      end

      let prio x : Priority.t =
        let dst = Thread.ip x.ctrl.edge.dst |>= Llair.IP.block in
        {goal= x.goal; dst; threads= x.threads}

      let pp ppf {ctrl= {edge; depth}; threads; switches; goal} =
        Format.fprintf ppf "%i,%i: %a %a %a" switches depth Edge.pp edge
          Threads.pp threads Goal.pp goal

      let goal_status x =
        Goal.status x.goal (Thread.ip x.ctrl.edge.dst |>= Llair.IP.block)

      let compare x y =
        let open Ord.Infix in
        if x == y then 0
        else
          ( (Goal.compare_status >|= goal_status)
          @? (Int.compare >|= fun x -> x.switches)
          @? (Int.compare >|= fun x -> x.ctrl.depth)
          @? (Edge.compare_without_tid >|= fun x -> x.ctrl.edge)
          @? (Threads.compare_inactive >|= fun x -> x.ctrl.inactive)
          @? (ThreadID.compare >|= fun x -> Thread.id x.ctrl.edge.dst)
          @? (Threads.compare_inactive_tids >|= fun x -> x.ctrl.inactive)
          @? (Depths.compare >|= fun x -> x.depths)
          @? (D.compare >|= fun x -> x.state) )
            x y

      let equal = [%compare.equal: t]
      let dnf x = Iter.map ~f:(fun state -> {x with state}) (D.dnf x.state)
    end

    module Queue = Queue (Elt)

    (** Projection of abstract machine states to components that can not be
        joined across multiple executions. Abstract machine states with the
        same [switches], [ip], [threads], and [goal] fields have, as far as
        the scheduler is concerned, the same execution history and can be
        joined. *)
    module Partition = struct
      module T = struct
        type t = switches * IP.t * Threads.t * Goal.t
        [@@deriving compare, equal, sexp_of]
      end

      include T
      module Map = Map.Make (T)
    end

    (** Projection of abstract machine states to components that can be
        joined across multiple executions. Complementary to [Partition],
        [Joinable] represents the subset of [ams] fields that can be joined
        across several executions that share the same execution history. *)
    module Joinable = struct
      module Elt = struct
        type t = {state: D.t; depths: Depths.t; history: Hist.t [@ignore]}
        [@@deriving compare, equal, sexp_of]
      end

      module M = Map.Make (Elt)

      type t = Edge.t list M.t

      let empty = M.empty
      let is_empty = M.is_empty
      let add = M.add_multi

      let diff =
        M.merge ~f:(fun _ -> function
          | `Left v -> Some v | `Right _ | `Both _ -> None )

      let union = M.union ~f:(fun _ v1 v2 -> Some (List.append v1 v2))

      let join m =
        let states, depths, hists, edges =
          M.fold m (D.Set.empty, Depths.empty, [], [])
            ~f:(fun ~key ~data:e (qs, ds, hs, es) ->
              ( D.Set.add key.state qs
              , Depths.join key.depths ds
              , key.history :: hs
              , List.append e es ) )
        in
        (D.joinN states, depths, hists, edges)
    end

    (** Sequential states indexed by concurrent states. When sequential
        states and histories are joined across executions that reach the
        same abstract concurrent state and history, there are multiple
        successor executions corresponding to which thread is selected to
        execute. Executing some such successors can lead to additional
        executions that reach the original abstract concurrent state and
        history. These new executions also need to be joined with the old
        ones. To handle this, the successors of a join are enumerated
        lazily, returning them one-by-one from the scheduler and adding them
        to the analysis worklist. The "cursor" that maintains the current
        progress of this enumeration is a set of sequential states that is
        indexed by concurrent states. *)
    module Cursor = struct
      type t = Joinable.t Partition.Map.t

      let empty = Partition.Map.empty
      let add = Partition.Map.add
      let find = Partition.Map.find
    end

    (** Analysis exploration state *)
    type t = Queue.t * Cursor.t

    let prune switches depth edge =
      [%Dbg.info " %i,%i: %a" switches depth Edge.pp edge]

    let pp_queue ppf queue = [%Dbg.fprintf ppf "@ | %a" Queue.pp queue]

    let enqueue depth ({ctrl= {dst} as edge; state; threads; depths} as elt)
        (queue, cursor) =
      [%Dbg.info
        " %i,%i: %a%a@\n@[%a@]" elt.switches depth Edge.pp edge pp_queue
          queue pp_state state] ;
      let threads, inactive = Threads.after_step dst threads in
      let queue =
        Queue.add
          {elt with ctrl= {edge; depth; inactive}; threads; depths}
          queue
      in
      (queue, cursor)

    let init state curr goal =
      let depth = 0 in
      let ip = Llair.IP.mk curr in
      let stk = Stack.empty in
      let prev = curr in
      let tid = ThreadID.init in
      let edge =
        {dst= Runnable {ip; stk; tid}; src= prev; retreating= false}
      in
      let threads = Threads.init in
      let switches = 0 in
      let depths = Depths.empty in
      let queue = Queue.create () in
      let cursor = Cursor.empty in
      let history = Hist.init ip in
      enqueue depth
        {ctrl= edge; state; threads; switches; depths; goal; history}
        (queue, cursor)

    let add ({ctrl= edge; depths} as elt) wl =
      if not edge.retreating then enqueue 0 elt wl
      else
        let depth = 1 + Option.value (Depths.find edge depths) ~default:0 in
        if depth <= Config.loop_bound then
          let depths = Depths.add ~key:edge ~data:depth depths in
          enqueue depth {elt with depths} wl
        else (
          prune elt.switches depth elt.ctrl ;
          Report.hit_loop_bound Config.loop_bound ;
          wl )

    module Succs = struct
      module M = Partition.Map

      let empty = M.empty

      let add ~key ~data:(elt, edge) m =
        M.update key m ~f:(fun data ->
            let joinable = Option.value data ~default:Joinable.empty in
            Some (Joinable.add ~key:elt ~data:edge joinable) )

      let find_first m ~f =
        let exception Stop in
        let found = ref None in
        let hit_end = ref true in
        ( try
            M.iteri m ~f:(fun ~key ~data ->
                match !found with
                | None -> (
                  match f ~key ~data with
                  | None -> ()
                  | Some r -> found := Some r )
                | Some _ ->
                    hit_end := false ;
                    raise_notrace Stop )
          with Stop -> () ) ;
        (!found, !hit_end)
    end

    let rec dequeue (queue, cursor) =
      let* ({threads} as top), elts, queue = Queue.top queue in
      let succs =
        Iter.fold elts Succs.empty ~f:(fun incoming succs ->
            let {ctrl= {edge}; state; switches; depths; goal; history} =
              incoming
            in
            let incoming_tid = Thread.id edge.dst in
            Threads.fold threads succs ~f:(fun active succs ->
                match active with
                | Terminated _ | Suspended _ -> succs
                | Runnable ({tid} as ip) ->
                    let switches =
                      if tid = incoming_tid then switches else switches + 1
                    in
                    Succs.add
                      ~key:(switches, ip, threads, goal)
                      ~data:({state; depths; history}, edge)
                      succs ) )
      in
      let found, hit_end =
        Succs.find_first succs
          ~f:(fun ~key:(switches, ip, threads, goal) ~data:incoming ->
            let next = (switches, ip, threads, goal) in
            let+ done_states, next_states =
              match Cursor.find next cursor with
              | Some done_states ->
                  let next_states = Joinable.diff incoming done_states in
                  if Joinable.is_empty next_states then None
                  else Some (done_states, next_states)
              | None -> Some (Joinable.empty, incoming)
            in
            let cursor =
              Cursor.add ~key:next
                ~data:(Joinable.union done_states next_states)
                cursor
            in
            (next, next_states, cursor) )
      in
      let queue = if hit_end then Queue.remove_top queue else queue in
      match found with
      | Some ((switches, _, _, _), _, cursor)
        when switches > Config.switch_bound ->
          prune switches top.ctrl.depth top.ctrl.edge ;
          Report.hit_switch_bound Config.switch_bound ;
          dequeue (queue, cursor)
      | Some ((switches, ip, threads, goal), next_states, cursor) ->
          let state, depths, histories, edges = Joinable.join next_states in
          let history = Hist.extend ip.ip histories in
          [%Dbg.info
            " %i,%i: %a <-t%i- {@[%a@]}%a" switches top.ctrl.depth IP.pp ip
              ip.tid
              (List.pp " ∨@ " Edge.pp)
              edges pp_queue queue] ;
          Some
            ( {ctrl= ip; state; threads; switches; depths; goal; history}
            , (queue, cursor) )
      | None -> dequeue (queue, cursor)

    let rec run ~f wl =
      match dequeue wl with
      | Some (ams, wl) -> run ~f (f ams wl)
      | None -> ()
  end

  let summary_table = Llair.FuncName.Tbl.create ()

  let pp_st () =
    [%Dbg.printf
      "@[<v>%t@]" (fun fs ->
          Llair.FuncName.Tbl.iteri summary_table ~f:(fun ~key ~data ->
              Format.fprintf fs "@[<v>%a:@ @[%a@]@]@ " Llair.FuncName.pp key
                (List.pp "@," D.pp_summary)
                data ) )]

  let exec_jump jump ({ctrl= {ip; stk; tid}} as ams) wl =
    let src = Llair.IP.block ip in
    let {Llair.dst; retreating} = jump in
    let ip = Llair.IP.mk dst in
    let edge = {dst= Runnable {ip; stk; tid}; src; retreating} in
    Work.add {ams with ctrl= edge} wl

  let exec_skip_func areturn return ({ctrl= {ip; tid}; state} as ams) wl =
    Report.unknown_call (Llair.IP.block ip).term ;
    let state = Option.fold ~f:(D.exec_kill tid) areturn state in
    exec_jump return {ams with state} wl

  let exec_call globals call ({ctrl= {stk; tid}; state; history} as ams) wl
      =
    let Llair.{callee; actuals; areturn; return} = call in
    let Llair.{func; recursive} = callee in
    let Llair.{name; formals; freturn; locals; entry} = func in
    [%Dbg.call fun {pf} ->
      pf " t%i@[<2>@ %a from %a with state@]@;<1 2>%a" tid
        Llair.Func.pp_call {call with callee= func} Llair.FuncName.pp
        return.dst.parent.name D.pp state]
    ;
    let ip = Llair.IP.mk entry in
    let goal = Goal.update_after_call name ams.goal in
    if goal != ams.goal && Goal.reached goal then
      Report.reached_goal
        ~dp_goal:(fun fs -> Goal.pp fs goal)
        ~dp_witness:(Hist.dump (Hist.extend ip [history])) ;
    let dnf_states =
      if Config.function_summaries then D.dnf state
      else Iter.singleton state
    in
    let domain_call =
      D.call tid ~globals ~actuals ~areturn ~formals ~freturn ~locals
    in
    Iter.fold dnf_states wl ~f:(fun state wl ->
        match
          if not Config.function_summaries then None
          else
            let state = fst (domain_call ~summaries:false state) in
            let* summary = Llair.FuncName.Tbl.find summary_table name in
            List.find_map ~f:(D.apply_summary state) summary
        with
        | None ->
            let state, from_call =
              domain_call ~summaries:Config.function_summaries state
            in
            let stk = Stack.push_call call from_call stk in
            let src = Llair.IP.block ams.ctrl.ip in
            let edge =
              {dst= Runnable {ip; stk; tid}; src; retreating= recursive}
            in
            Work.add {ams with ctrl= edge; state; goal} wl
        | Some post -> exec_jump return {ams with state= post; goal} wl )
    |>
    [%Dbg.retn fun {pf} _ -> pf ""]

  let exec_call call ams wl =
    let Llair.{callee= {func}; areturn; return; _} = call in
    if Llair.Func.is_undefined func then
      exec_skip_func areturn return ams wl
    else
      let globals =
        Domain_used_globals.by_function Config.globals func.name
      in
      exec_call globals call ams wl

  let exec_return exp ({ctrl= {ip; stk; tid}; state; history} as ams) wl =
    let block = Llair.IP.block ip in
    let func = block.parent in
    let Llair.{name; formals; freturn; locals} = func in
    [%Dbg.call fun {pf} -> pf " t%i@ from: %a" tid Llair.FuncName.pp name]
    ;
    let goal = Goal.update_after_retn name ams.goal in
    if goal != ams.goal && Goal.reached goal then
      Report.reached_goal
        ~dp_goal:(fun fs -> Goal.pp fs goal)
        ~dp_witness:(Hist.dump (Hist.extend ip [history])) ;
    let summarize post_state =
      if not Config.function_summaries then post_state
      else
        let function_summary, post_state =
          D.create_summary tid ~locals ~formals post_state
        in
        Llair.FuncName.Tbl.add_multi ~key:name ~data:function_summary
          summary_table ;
        pp_st () ;
        post_state
    in
    let pre_state = state in
    let exit_state =
      match (freturn, exp) with
      | Some freturn, Some return_val ->
          D.exec_move tid (IArray.of_ (freturn, return_val)) pre_state
      | None, None -> pre_state
      | _ -> violates Llair.Func.invariant func
    in
    ( match Stack.pop_return stk with
    | Some (from_call, retn_site, stk) ->
        let post_state =
          summarize (D.post tid locals from_call exit_state)
        in
        let retn_state = D.retn tid formals freturn from_call post_state in
        exec_jump retn_site
          {ams with ctrl= {ams.ctrl with stk}; state= retn_state; goal}
          wl
    | None ->
        summarize exit_state |> ignore ;
        let tc = D.term tid formals freturn exit_state in
        Work.add
          { ams with
            ctrl= {dst= Terminated (tc, tid); src= block; retreating= false}
          ; goal }
          wl )
    |>
    [%Dbg.retn fun {pf} _ -> pf ""]

  let exec_throw exc ({ctrl= {ip; stk; tid}; state} as ams) wl =
    let func = (Llair.IP.block ip).parent in
    let Llair.{name; formals; freturn; fthrow; locals} = func in
    [%Dbg.call fun {pf} -> pf "@ from %a" Llair.FuncName.pp name]
    ;
    let unwind formals scope from_call state =
      D.retn tid formals (Some fthrow) from_call
        (D.post tid scope from_call state)
    in
    let pre_state = state in
    ( match Stack.pop_throw stk ~unwind pre_state with
    | Some (from_call, retn_site, stk, unwind_state) ->
        let exit_state =
          D.exec_move tid (IArray.of_ (fthrow, exc)) unwind_state
        in
        let post_state = D.post tid locals from_call exit_state in
        let retn_state = D.retn tid formals freturn from_call post_state in
        exec_jump retn_site
          {ams with ctrl= {ams.ctrl with stk}; state= retn_state}
          wl
    | None -> wl )
    |>
    [%Dbg.retn fun {pf} _ -> pf ""]

  let exec_assume cond jump ({ctrl= {tid}; state} as ams) wl =
    match D.exec_assume tid state cond with
    | Some state -> exec_jump jump {ams with state} wl
    | None ->
        [%Dbg.info " infeasible %a@\n@[%a@]" Llair.Exp.pp cond D.pp state] ;
        wl

  let exec_thread_create areturn
      {Llair.name; formals; freturn; entry; locals} actual return
      ({ctrl= {tid}; state; threads} as ams) wl =
    let child, threads = Threads.create entry threads in
    let state =
      match areturn with
      | None -> state
      | Some reg ->
          let child =
            Llair.Exp.integer (Llair.Reg.typ reg) (Z.of_int child)
          in
          D.exec_move tid (IArray.of_ (reg, child)) state
    in
    let state, _ =
      let globals = Domain_used_globals.by_function Config.globals name in
      let actuals = IArray.of_ actual in
      D.call ~summaries:false tid ~child ~globals ~actuals ~areturn:None
        ~formals ~freturn ~locals state
    in
    exec_jump return {ams with state; threads} wl

  let exec_thread_resume thread return
      ({ctrl= {tid}; state; threads} as ams) wl =
    List.fold (D.resolve_int tid state thread) wl ~f:(fun resume_tid wl ->
        match Threads.resume resume_tid threads with
        | None -> wl
        | Some threads -> exec_jump return {ams with threads} wl )

  let exec_thread_join thread areturn return
      ({ctrl= {tid}; state; threads} as ams) wl =
    List.fold (D.resolve_int tid state thread) wl ~f:(fun join_tid wl ->
        match Threads.join join_tid threads with
        | None -> wl
        | Some (term_code, threads) ->
            let state =
              match areturn with
              | None -> state
              | Some reg -> D.move_term_code tid reg term_code state
            in
            exec_jump return {ams with state; threads} wl )

  let resolve_callee (pgm : Llair.program) tid callee state =
    let lookup name = Llair.Func.find name pgm.functions in
    D.resolve_callee lookup tid callee state

  let exec_term pgm ({ctrl= {ip; tid}; state} as ams) wl =
    let block = Llair.IP.block ip in
    let term = block.term in
    Report.step_term block ;
    match term with
    | Switch {key; tbl; els} ->
        let wl =
          exec_assume
            (IArray.fold tbl Llair.Exp.true_ ~f:(fun (case, _) b ->
                 Llair.Exp.and_ (Llair.Exp.dq key case) b ) )
            els ams wl
        in
        IArray.fold tbl wl ~f:(fun (case, jump) wl ->
            exec_assume (Llair.Exp.eq key case) jump ams wl )
    | Iswitch {ptr; tbl} ->
        IArray.fold tbl wl ~f:(fun jump wl ->
            exec_assume
              (Llair.Exp.eq ptr
                 (Llair.Exp.label
                    ~parent:(Llair.FuncName.name jump.dst.parent.name)
                    ~name:jump.dst.lbl ) )
              jump ams wl )
    | Call ({callee= Direct callee} as call) ->
        exec_call {call with callee} ams wl
    | Call
        ( {callee= Indirect {ptr= callee; candidates}; areturn; return} as
        call ) -> (
      match resolve_callee pgm tid callee state with
      | [] -> exec_skip_func areturn return ams wl
      | callees ->
          List.fold callees wl ~f:(fun callee_func wl ->
              let callee =
                match
                  IArray.find candidates ~f:(fun {Llair.func; _} ->
                      Llair.Func.equal func callee_func )
                with
                | Some callee -> callee
                | None ->
                    warn "unexpected call target %a at indirect callsite %a"
                      Llair.Func.pp callee_func Llair.Term.pp term () ;
                    (* Conservatively assume this call may be recursive *)
                    {Llair.func= callee_func; recursive= true}
              in
              exec_call {call with callee} ams wl ) )
    | Call {callee= Intrinsic callee; actuals; areturn; return} -> (
      match (callee, IArray.to_array actuals) with
      | `sledge_thread_create, [|callee; arg|] -> (
        match resolve_callee pgm tid callee state with
        | [] -> exec_skip_func areturn return ams wl
        | callees ->
            List.fold callees wl ~f:(fun callee wl ->
                exec_thread_create areturn callee arg return ams wl ) )
      | `sledge_thread_resume, [|thread|] ->
          exec_thread_resume thread return ams wl
      | `sledge_thread_join, [|thread|] ->
          exec_thread_join thread areturn return ams wl
      | ( ( `sledge_thread_create | `sledge_thread_resume
          | `sledge_thread_join )
        , _ ) ->
          violates Llair.Term.invariant term )
    | Return {exp} -> exec_return exp ams wl
    | Throw {exc} -> exec_throw exc ams wl
    | Abort {loc} ->
        if not (D.is_unsat state) then
          Report.alarm
            (Alarm.v Abort loc Llair.Term.pp term D.pp state)
            ~dp_witness:(Hist.dump ams.history) ;
        wl
    | Unreachable -> wl

  let rec exec_ip pgm ({ctrl= {ip; stk; tid}; state} as ams) wl =
    match Llair.IP.inst ip with
    | Some inst -> (
        [%Dbg.info
          " t%i %a@\n@[%a@]%a" tid Llair.IP.pp ip pp_state state
            Llair.Inst.pp inst] ;
        Report.step_inst ip ;
        match D.exec_inst tid inst state with
        | Ok state ->
            let ip = Llair.IP.succ ip in
            if Llair.IP.is_schedule_point ip then
              let src = Llair.IP.block ip in
              let edge =
                {dst= Runnable {ip; stk; tid}; src; retreating= false}
              in
              Work.add {ams with ctrl= edge; state} wl
            else exec_ip pgm {ams with ctrl= {ams.ctrl with ip}; state} wl
        | Error alarm ->
            Report.alarm alarm ~dp_witness:(Hist.dump ams.history) ;
            wl )
    | None ->
        [%Dbg.info
          " t%i %a@\n@[%a@]%a" tid Llair.IP.pp ip pp_state state
            Llair.Term.pp (Llair.IP.block ip).term] ;
        exec_term pgm ams wl

  let call_entry_point pgm goal =
    let+ {name; formals; freturn; locals; entry} =
      List.find_map Config.entry_points ~f:(fun entry_point ->
          Llair.Func.find entry_point pgm.Llair.functions )
    in
    let summaries = Config.function_summaries in
    let globals = Domain_used_globals.by_function Config.globals name in
    let actuals = IArray.map ~f:Llair.Exp.reg formals in
    let areturn = None in
    let state, _ =
      D.call ThreadID.init ~summaries ~globals ~actuals ~areturn ~formals
        ~freturn ~locals (D.init pgm.globals)
    in
    Goal.initialize ~pgm ~entry:name goal ;
    Work.init state entry goal

  let exec_pgm pgm goal =
    match call_entry_point pgm goal with
    | Some wl -> Work.run ~f:(exec_ip pgm) wl
    | None -> fail "no entry point found" ()

  let compute_summaries pgm goal =
    assert Config.function_summaries ;
    exec_pgm pgm goal ;
    Llair.FuncName.Tbl.fold summary_table Llair.FuncName.Map.empty
      ~f:(fun ~key ~data map ->
        match data with
        | [] -> map
        | _ -> Llair.FuncName.Map.add ~key ~data map )
end
[@@inlined]

module Make (C : Config) (D : Domain) (Q : Queue) = struct
  module Ctrl = MakeDirected (C) (D) (Q) (Goal.Undirected)

  let exec_pgm pgm = Ctrl.exec_pgm pgm ()
  let compute_summaries pgm = Ctrl.compute_summaries pgm ()
end
[@@inlined]
