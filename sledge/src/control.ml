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

  val top : t -> (elt * elt iter * t) option
  (** [top q] is [None] if [q] is empty and otherwise is [Some (e, es, q')]
      where [e] is the selected element in [q] and any elements [es] have
      the same destination as [e]. [q'] is equivalent to [q] but possibly
      more compactly represented. *)

  val remove : elt -> elt iter -> t -> t
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

  let elts {queue; removed} =
    Iter.unfoldr FHeap.pop queue
    |> Iter.filter ~f:(fun elt -> not (Elts.mem elt removed))

  let pp ppf q =
    Format.fprintf ppf "@[%a@]" (List.pp " ::@ " Elt.pp)
      (Iter.to_list (elts q))

  let create () = {queue= FHeap.create ~cmp:Elt.compare; removed= Elts.empty}

  let add elt {queue; removed} =
    let removed' = Elts.remove elt removed in
    if removed' == removed then {queue= FHeap.add queue elt; removed}
    else {queue; removed= removed'}

  let rec top {queue; removed} =
    let* next = FHeap.top queue in
    let removed' = Elts.remove next removed in
    if removed' != removed then
      let queue' = FHeap.remove_top_exn queue in
      top {queue= queue'; removed= removed'}
    else
      let elts =
        Iter.filter ~f:(Elt.equal_destination next) (elts {queue; removed})
      in
      Some (next, elts, {queue; removed})

  let remove top elts {queue; removed} =
    assert (Elt.equal top (FHeap.top_exn queue)) ;
    let queue = FHeap.remove_top_exn queue in
    let removed = Elts.union (Elts.of_iter elts) removed in
    {queue; removed}
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
  let remove _ = todo "concurrent sampling analysis" ()
end

module Make (Config : Config) (D : Domain) (Queue : Queue) = struct
  module Stack : sig
    type t

    val empty : t
    val push_call : Llair.func Llair.call -> D.from_call -> t -> t
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
    type t = Runnable of IP.t | Terminated of ThreadID.t
    [@@deriving equal, sexp_of]

    val compare : t Ord.t
    val compare_without_tid : t Ord.t
    val pp : t pp
    val id : t -> ThreadID.t
  end = struct
    (** Because [ip] needs to include [tid], this is represented as a sum of
        products, but it may be more natural to think in terms of the
        isomorphic representation using a product of a sum such as
        [(Runnable of ... | Terminated ...) * ThreadID.t]. *)
    type t = Runnable of IP.t | Terminated of ThreadID.t
    [@@deriving sexp_of]

    let pp ppf = function
      | Runnable ip -> IP.pp ppf ip
      | Terminated tid -> Format.fprintf ppf "T%i" tid

    let id = function Runnable {tid} -> tid | Terminated tid -> tid

    (* Note: Threads.inactive relies on comparing tid last *)
    let compare_aux compare_tid x y =
      let open Ord.Infix in
      if x == y then 0
      else
        match (x, y) with
        | Runnable x, Runnable y ->
            Llair.IP.compare x.ip y.ip
            <?> (Stack.compare_as_inlined_location, x.stk, y.stk)
            <?> (compare_tid, x.tid, y.tid)
        | Runnable _, _ -> -1
        | _, Runnable _ -> 1
        | Terminated x_tid, Terminated y_tid -> compare_tid x_tid y_tid

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
    val join : ThreadID.t -> t -> t option
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
      let thread = Thread.Runnable {ip; stk= Stack.empty; tid} in
      (tid, M.add ~key:tid ~data:thread threads)

    let after_step active threads =
      let tid = Thread.id active in
      let inactive = inactive tid threads in
      let threads = M.add ~key:tid ~data:active threads in
      (threads, inactive)

    let join tid threads =
      match M.find tid threads with
      | Some (Thread.Terminated _) -> Some (M.remove tid threads)
      | _ ->
          [%Trace.info " prune join of non-terminated thread: %i" tid] ;
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
  type edge = {dst: Thread.t; src: Llair.Block.t} [@@deriving sexp_of]

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
        | {dst= Runnable x_t}, {dst= Runnable y_t} ->
            let is_rec_call = function
              | {Llair.term= Call {recursive= true}} -> true
              | _ -> false
            in
            let compare_stk stk1 stk2 =
              if is_rec_call x.src then 0
              else Stack.compare_as_inlined_location stk1 stk2
            in
            Llair.IP.compare x_t.ip y_t.ip
            <?> (Llair.Block.compare, x.src, y.src)
            <?> (compare_stk, x_t.stk, y_t.stk)
            <?> (compare_tid, x_t.tid, y_t.tid)
        | {dst= Runnable _}, _ -> -1
        | _, {dst= Runnable _} -> 1
        | {dst= Terminated x_tid}, {dst= Terminated y_tid} ->
            Llair.Block.compare x.src y.src <?> (compare_tid, x_tid, y_tid)

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
    ; depths: Depths.t  (** count of retreating edge crossings *) }
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

  let pp_state ppf state = [%Trace.fprintf ppf "@[%a@]@\n" D.pp state]

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

      let pp ppf {ctrl= {edge; depth}; threads; switches} =
        Format.fprintf ppf "%i,%i: %a %a" switches depth Edge.pp edge
          Threads.pp threads

      let compare x y =
        let open Ord.Infix in
        if x == y then 0
        else
          ( (Int.compare >|= fun x -> x.switches)
          @? (Int.compare >|= fun x -> x.ctrl.depth)
          @? (Edge.compare_without_tid >|= fun x -> x.ctrl.edge)
          @? (Threads.compare_inactive >|= fun x -> x.ctrl.inactive)
          @? (ThreadID.compare >|= fun x -> Thread.id x.ctrl.edge.dst)
          @? (Threads.compare_inactive_tids >|= fun x -> x.ctrl.inactive)
          @? (Depths.compare >|= fun x -> x.depths)
          @? (D.compare >|= fun x -> x.state) )
            x y

      let equal = [%compare.equal: t]
      let equal_destination x y = Threads.equal x.threads y.threads

      let dnf x =
        List.map
          ~f:(fun state -> {x with state})
          (D.Set.to_list (D.dnf x.state))
    end

    module Queue = Queue (Elt)

    (** Concurrent state and history projection of abstract machine states.
        Abstract machine states with the same [switches], [ip], and
        [threads] fields have, as far as the scheduler is concerned, the
        same concurrent state and history and can be joined. *)
    module ConcSH = struct
      module T = struct
        type t = switches * IP.t * Threads.t
        [@@deriving compare, equal, sexp_of]
      end

      include T
      module Map = Map.Make (T)
    end

    (** Sequential state and history projection of abstract machine states.
        Complementary to [ConcSH], [SeqSH] represents the subset of [ams]
        fields that can be joined across several executions that share the
        same abstract concurrent state and history. *)
    module SeqSH = struct
      module T = struct
        type t = D.t * Depths.t [@@deriving compare, equal, sexp_of]
      end

      module M = Map.Make (T)

      type t = Edge.t list M.t

      let empty = M.empty
      let is_empty = M.is_empty

      let diff =
        M.merge ~f:(fun _ -> function
          | `Left v -> Some v | `Right _ | `Both _ -> None )

      let union = M.union ~f:(fun _ v1 v2 -> Some (List.append v1 v2))

      let of_list l =
        List.fold l M.empty ~f:(fun (key, data) m ->
            M.add_multi ~key ~data m )

      let join m =
        let states, depths, edges =
          M.fold m (D.Set.empty, Depths.empty, [])
            ~f:(fun ~key:(q, d) ~data:e (qs, ds, es) ->
              (D.Set.add q qs, Depths.join d ds, List.append e es) )
        in
        (D.joinN states, depths, edges)
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
      type t = SeqSH.t ConcSH.Map.t

      let empty = ConcSH.Map.empty
      let add = ConcSH.Map.add
      let find = ConcSH.Map.find
    end

    (** Analysis exploration state *)
    type t = Queue.t * Cursor.t

    let prune switches depth edge =
      [%Trace.info " %i,%i: %a" switches depth Edge.pp edge]

    let pp_queue ppf queue = [%Trace.fprintf ppf "@ | %a" Queue.pp queue]

    let enqueue depth ({ctrl= {dst} as edge; state; threads; depths} as elt)
        (queue, cursor) =
      [%Trace.info
        " %i,%i: %a%a@\n@[%a@]" elt.switches depth Edge.pp edge pp_queue
          queue pp_state state] ;
      let depths = Depths.add ~key:edge ~data:depth depths in
      let threads, inactive = Threads.after_step dst threads in
      let queue =
        Queue.add
          {elt with ctrl= {edge; depth; inactive}; threads; depths}
          queue
      in
      (queue, cursor)

    let init state curr =
      let depth = 0 in
      let ip = Llair.IP.mk curr in
      let stk = Stack.empty in
      let prev = curr in
      let tid = ThreadID.init in
      let edge = {dst= Runnable {ip; stk; tid}; src= prev} in
      let threads = Threads.init in
      let switches = 0 in
      let depths = Depths.empty in
      let queue = Queue.create () in
      let cursor = Cursor.empty in
      enqueue depth
        {ctrl= edge; state; threads; switches; depths}
        (queue, cursor)

    let add ~retreating ({ctrl= edge; depths} as elt) wl =
      let depth = Option.value (Depths.find edge depths) ~default:0 in
      let depth = if retreating then depth + 1 else depth in
      if depth > Config.loop_bound then (
        prune elt.switches depth elt.ctrl ;
        Report.hit_loop_bound Config.loop_bound ;
        wl )
      else enqueue depth elt wl

    module Succs = struct
      module M = ConcSH.Map

      let empty = M.empty
      let add = M.add_multi

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
            let {ctrl= {edge}; state; switches; depths} = incoming in
            let incoming_tid = Thread.id edge.dst in
            Threads.fold threads succs ~f:(fun active succs ->
                match active with
                | Terminated _ -> succs
                | Runnable ({tid} as ip) ->
                    let switches =
                      if tid = incoming_tid then switches else switches + 1
                    in
                    Succs.add ~key:(switches, ip, threads)
                      ~data:((state, depths), edge)
                      succs ) )
      in
      let found, hit_end =
        Succs.find_first succs
          ~f:(fun ~key:(switches, ip, threads) ~data:incoming ->
            let next = (switches, ip, threads) in
            let curr = SeqSH.of_list incoming in
            let+ done_states, next_states =
              match Cursor.find next cursor with
              | Some done_states ->
                  let next_states = SeqSH.diff curr done_states in
                  if SeqSH.is_empty next_states then None
                  else Some (done_states, next_states)
              | None -> Some (SeqSH.empty, curr)
            in
            let cursor =
              Cursor.add ~key:next
                ~data:(SeqSH.union done_states next_states)
                cursor
            in
            (next, next_states, cursor) )
      in
      let queue = if hit_end then Queue.remove top elts queue else queue in
      match found with
      | Some ((switches, _, _), _, cursor)
        when switches > Config.switch_bound ->
          prune switches top.ctrl.depth top.ctrl.edge ;
          Report.hit_switch_bound Config.switch_bound ;
          dequeue (queue, cursor)
      | Some ((switches, ip, threads), next_states, cursor) ->
          let state, depths, edges = SeqSH.join next_states in
          [%Trace.info
            " %i,%i: %a <-t%i- {@[%a@]}%a" switches top.ctrl.depth IP.pp ip
              ip.tid
              (List.pp " ∨@ " Edge.pp)
              edges pp_queue queue] ;
          Some
            ({ctrl= ip; state; threads; switches; depths}, (queue, cursor))
      | None -> dequeue (queue, cursor)

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

  let exec_jump jump ({ctrl= {ip; stk; tid}} as ams) wl =
    let src = Llair.IP.block ip in
    let {Llair.dst; retreating} = jump in
    let ip = Llair.IP.mk dst in
    let edge = {dst= Runnable {ip; stk; tid}; src} in
    Work.add ~retreating {ams with ctrl= edge} wl

  let exec_skip_func areturn return ({ctrl= {ip; tid}; state} as ams) wl =
    Report.unknown_call (Llair.IP.block ip).term ;
    let state = Option.fold ~f:(D.exec_kill tid) areturn state in
    exec_jump return {ams with state} wl

  let exec_call globals call ({ctrl= {stk; tid}; state} as ams) wl =
    let Llair.{callee; actuals; areturn; return; recursive} = call in
    let Llair.{name; formals; freturn; locals; entry} = callee in
    [%Trace.call fun {pf} ->
      pf " t%i@[<2>@ %a from %a with state@]@;<1 2>%a" tid
        Llair.Func.pp_call call Llair.Function.pp return.dst.parent.name
        D.pp state]
    ;
    let dnf_states =
      if Config.function_summaries then D.dnf state else D.Set.of_ state
    in
    let domain_call =
      D.call tid ~globals ~actuals ~areturn ~formals ~freturn ~locals
    in
    D.Set.fold dnf_states wl ~f:(fun state wl ->
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
            let edge = {dst= Runnable {ip; stk; tid}; src} in
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

  let exec_return exp ({ctrl= {ip; stk; tid}; state} as ams) wl =
    let block = Llair.IP.block ip in
    let func = block.parent in
    let Llair.{name; formals; freturn; locals} = func in
    [%Trace.call fun {pf} -> pf " t%i@ from: %a" tid Llair.Function.pp name]
    ;
    let summarize post_state =
      if not Config.function_summaries then post_state
      else
        let function_summary, post_state =
          D.create_summary tid ~locals ~formals post_state
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
          {ams with ctrl= {ams.ctrl with stk}; state= retn_state}
          wl
    | None ->
        if Config.function_summaries then summarize exit_state |> ignore ;
        Work.add ~retreating:false
          {ams with ctrl= {dst= Terminated tid; src= block}}
          wl )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_throw exc ({ctrl= {ip; stk; tid}; state} as ams) wl =
    let func = (Llair.IP.block ip).parent in
    let Llair.{name; formals; freturn; fthrow; locals} = func in
    [%Trace.call fun {pf} -> pf "@ from %a" Llair.Function.pp name]
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
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_assume cond jump ({ctrl= {tid}; state} as ams) wl =
    match D.exec_assume tid state cond with
    | Some state -> exec_jump jump {ams with state} wl
    | None ->
        [%Trace.info " infeasible %a@\n@[%a@]" Llair.Exp.pp cond D.pp state] ;
        wl

  let exec_thread_create reg {Llair.entry; locals} return
      ({ctrl= {tid}; state; threads} as ams) wl =
    let child_tid, threads = Threads.create entry threads in
    let child =
      Llair.Exp.integer (Llair.Reg.typ reg) (Z.of_int child_tid)
    in
    let state = D.exec_move tid (IArray.of_ (reg, child)) state in
    let state = D.enter_scope child_tid locals state in
    exec_jump return {ams with state; threads} wl

  let exec_thread_join thread return ({ctrl= {tid}; state; threads} as ams)
      wl =
    List.fold (D.resolve_int tid state thread) wl ~f:(fun join_tid wl ->
        match Threads.join join_tid threads with
        | Some threads -> exec_jump return {ams with threads} wl
        | None -> wl )

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
                    ~parent:(Llair.Function.name jump.dst.parent.name)
                    ~name:jump.dst.lbl ) )
              jump ams wl )
    | Call ({callee; actuals; areturn; return} as call) -> (
      match
        (Llair.Function.name callee.name, IArray.to_array actuals, areturn)
      with
      | "sledge_thread_create", [|callee|], Some reg -> (
        match resolve_callee pgm tid callee state with
        | [] -> exec_skip_func areturn return ams wl
        | callees ->
            List.fold callees wl ~f:(fun callee wl ->
                exec_thread_create reg callee return ams wl ) )
      | "sledge_thread_join", [|thread|], None ->
          exec_thread_join thread return ams wl
      | _ -> exec_call call ams wl )
    | ICall ({callee; areturn; return} as call) -> (
      match resolve_callee pgm tid callee state with
      | [] -> exec_skip_func areturn return ams wl
      | callees ->
          List.fold callees wl ~f:(fun callee wl ->
              exec_call {call with callee} ams wl ) )
    | Return {exp} -> exec_return exp ams wl
    | Throw {exc} -> exec_throw exc ams wl
    | Unreachable -> wl

  let rec exec_ip pgm ({ctrl= {ip; stk; tid}; state} as ams) wl =
    match Llair.IP.inst ip with
    | Some inst -> (
        [%Trace.info
          " t%i %a@\n@[%a@]%a" tid Llair.IP.pp ip pp_state state
            Llair.Inst.pp inst] ;
        Report.step_inst ip ;
        match D.exec_inst tid inst state with
        | Ok state ->
            let ip = Llair.IP.succ ip in
            if Llair.IP.is_schedule_point ip then
              let src = Llair.IP.block ip in
              let edge = {dst= Runnable {ip; stk; tid}; src} in
              Work.add ~retreating:false {ams with ctrl= edge; state} wl
            else exec_ip pgm {ams with ctrl= {ams.ctrl with ip}; state} wl
        | Error alarm ->
            Report.alarm alarm ;
            wl )
    | None ->
        [%Trace.info
          " t%i %a@\n@[%a@]%a" tid Llair.IP.pp ip pp_state state
            Llair.Term.pp (Llair.IP.block ip).term] ;
        exec_term pgm ams wl

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
      D.call ThreadID.init ~summaries ~globals ~actuals ~areturn ~formals
        ~freturn ~locals (D.init pgm.globals)
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
