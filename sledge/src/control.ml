(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Iterative Breadth-First Bounded Exploration

    The analysis' semantics of control flow. *)

module Make (Opts : Domain_intf.Opts) (Dom : Domain_intf.Dom) = struct
  module Stack : sig
    type t

    val pp : t pp

    type as_inlined_location = t [@@deriving compare, equal, sexp_of]

    val empty : t
    val push_call : Llair.func Llair.call -> Dom.from_call -> t -> t option
    val pop_return : t -> (Dom.from_call * Llair.jump * t) option

    val pop_throw :
         t
      -> 'a
      -> unwind:
           (   Llair.Reg.t iarray
            -> Llair.Reg.Set.t
            -> Dom.from_call
            -> 'a
            -> 'a)
      -> (Dom.from_call * Llair.jump * t * 'a) option
  end = struct
    type t =
      | Return of
          { recursive: bool  (** return from a possibly-recursive call *)
          ; dst: Llair.Jump.t
          ; formals: Llair.Reg.t iarray
          ; locals: Llair.Reg.Set.t
          ; from_call: Dom.from_call
          ; stk: t }
      | Throw of Llair.Jump.t * t
      | Empty
    [@@deriving sexp_of]

    let rec pp ppf = function
      | Return {recursive= false; dst; stk= s} ->
          Format.fprintf ppf "R#%i%a" dst.dst.sort_index pp s
      | Return {recursive= true; dst; stk= s} ->
          Format.fprintf ppf "R↑#%i%a" dst.dst.sort_index pp s
      | Throw (dst, s) ->
          Format.fprintf ppf "T#%i%a" dst.dst.sort_index pp s
      | Empty -> ()

    type as_inlined_location = t [@@deriving sexp_of]

    (* Treat a stack as a code location in a hypothetical expansion of the
       program where all non-recursive functions have been completely
       inlined. In particular, this means to compare stacks as if all Return
       frames for recursive calls had been removed. Additionally, the
       from_call info in Return frames is ignored. *)
    let rec compare_as_inlined_location x y =
      if x == y then 0
      else
        match (x, y) with
        | Return {recursive= true; stk= x}, y
         |x, Return {recursive= true; stk= y} ->
            compare_as_inlined_location x y
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

    let invariant s =
      let@ () = Invariant.invariant [%here] s [%sexp_of: t] in
      match s with
      | Return _ | Throw (_, Return _) | Empty -> ()
      | Throw _ -> fail "malformed stack: %a" pp s ()

    let empty = Empty |> check invariant

    let push_return Llair.{callee= {formals; locals}; return; recursive}
        from_call stk =
      Return {recursive; dst= return; formals; locals; from_call; stk}
      |> check invariant

    let push_throw jmp stk =
      (match jmp with None -> stk | Some jmp -> Throw (jmp, stk))
      |> check invariant

    let push_call (Llair.{return; throw} as call) from_call stk =
      [%Trace.call fun {pf} -> pf "@ %a" pp stk]
      ;
      let rec count_f_in_stack acc f = function
        | Return {stk= next_frame; dst= dest_block} ->
            count_f_in_stack
              (if Llair.Jump.equal dest_block f then acc + 1 else acc)
              f next_frame
        | _ -> acc
      in
      let n = count_f_in_stack 0 return stk in
      ( if n > Opts.bound then (
        Report.hit_bound n ;
        None )
      else Some (push_throw throw (push_return call from_call stk)) )
      |>
      [%Trace.retn fun {pf} _ ->
        pf "%d of %a on stack" n Llair.Jump.pp return]

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
  end

  module Work : sig
    type t

    val init : Dom.t -> Llair.block -> t

    type x

    val skip : x
    val seq : x -> x -> x

    val add :
         ?prev:Llair.block
      -> retreating:bool
      -> Stack.t
      -> Dom.t
      -> Llair.block
      -> x

    val run : f:(Stack.t -> Dom.t -> Llair.block -> x) -> t -> unit
  end = struct
    module Edge = struct
      module T = struct
        type t =
          { dst: Llair.Block.t
          ; src: Llair.Block.t option
          ; stk: Stack.as_inlined_location }
        [@@deriving compare, equal, sexp_of]
      end

      include T

      let pp fs {dst; src} =
        Format.fprintf fs "#%i %%%s <--%a" dst.sort_index dst.lbl
          (Option.pp "%a" (fun fs (src : Llair.Block.t) ->
               Format.fprintf fs " #%i %%%s" src.sort_index src.lbl ))
          src
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

    module PrioQueue : sig
      (** an edge at a depth with the domain and depths state it yielded *)
      type elt = {depth: int; edge: Edge.t; state: Dom.t; depths: Depths.t}

      type t

      val pp : t pp

      val create : unit -> t
      (** create an empty queue *)

      val add : elt -> t -> t
      (** add an element *)

      val remove : elt -> t -> t
      (** remove an element *)

      val pop : t -> (elt * elt list * t) option
      (** the top element, the other elements with the same destination, the
          queue without the top element *)
    end = struct
      type elt = {depth: int; edge: Edge.t; state: Dom.t; depths: Depths.t}
      [@@deriving compare, equal, sexp_of]

      module Elt = struct
        type t = elt [@@deriving compare, equal, sexp_of]

        let pp ppf {depth; edge} =
          Format.fprintf ppf "%i: %a" depth Edge.pp edge
      end

      module Elts = Set.Make (Elt)

      type t = {queue: elt FHeap.t; removed: Elts.t}

      let pp ppf {queue; removed} =
        let rev_elts =
          FHeap.fold queue ~init:[] ~f:(fun rev_elts elt ->
              if Elts.mem elt removed then rev_elts else elt :: rev_elts )
        in
        Format.fprintf ppf "@[%a@]" (List.pp " ::@ " Elt.pp)
          (List.rev rev_elts)

      let create () =
        {queue= FHeap.create ~cmp:compare_elt; removed= Elts.empty}

      let add elt {queue; removed} =
        let removed' = Elts.remove elt removed in
        if removed' == removed then {queue= FHeap.add queue elt; removed}
        else {queue; removed= removed'}

      let remove elt {queue; removed} =
        {queue; removed= Elts.add elt removed}

      let rec pop {queue; removed} =
        let* top, queue = FHeap.pop queue in
        let removed' = Elts.remove top removed in
        if removed' != removed then pop {queue; removed= removed'}
        else
          let elts =
            FHeap.fold queue ~init:[] ~f:(fun elts elt ->
                if
                  Llair.Block.equal top.edge.dst elt.edge.dst
                  && not (Elts.mem elt removed)
                then elt :: elts
                else elts )
          in
          Some (top, elts, {queue; removed})
    end

    type t = PrioQueue.t
    type x = Depths.t -> t -> t

    let skip _ w = w
    let seq x y d w = y d (x d w)

    let add ?prev ~retreating stk state curr depths queue =
      let edge = {Edge.dst= curr; src= prev; stk} in
      let depth = Option.value (Depths.find edge depths) ~default:0 in
      let depth = if retreating then depth + 1 else depth in
      if depth <= Opts.bound then (
        [%Trace.info
          "@[<6>enqueue %i: %a [%a]@ | %a@]" depth Edge.pp edge Stack.pp
            edge.stk PrioQueue.pp queue] ;
        let depths = Depths.add ~key:edge ~data:depth depths in
        PrioQueue.add {depth; edge; state; depths} queue )
      else (
        [%Trace.info "prune: %i: %a" depth Edge.pp edge] ;
        Report.hit_bound Opts.bound ;
        queue )

    let init state curr =
      add ~retreating:false Stack.empty state curr Depths.empty
        (PrioQueue.create ())

    let rec run ~f queue =
      match PrioQueue.pop queue with
      | Some ({depth; edge; state; depths}, elts, queue) ->
          [%Trace.info
            "@[<6>dequeue %i: %a [%a]@ | %a@]" depth Edge.pp edge Stack.pp
              edge.stk PrioQueue.pp queue] ;
          let state, depths, queue =
            List.fold elts (state, depths, queue)
              ~f:(fun elt (state, depths, queue) ->
                match Dom.join elt.state state with
                | Some state ->
                    let depths = Depths.join elt.depths depths in
                    let queue = PrioQueue.remove elt queue in
                    (state, depths, queue)
                | None -> (state, depths, queue) )
          in
          run ~f (f edge.stk state edge.dst depths queue)
      | None ->
          [%Trace.info "queue empty"] ;
          ()
  end

  let exec_jump stk state block Llair.{dst; retreating} =
    Work.add ~prev:block ~retreating stk state dst

  let summary_table = Llair.Function.Tbl.create ()

  let exec_call stk state block call globals =
    let Llair.{callee; actuals; areturn; return; recursive} = call in
    let Llair.{name; formals; freturn; locals; entry} = callee in
    [%Trace.call fun {pf} ->
      pf "@[<2>@ %a from %a with state@]@;<1 2>%a" Llair.Func.pp_call call
        Llair.Function.pp return.dst.parent.name Dom.pp state]
    ;
    let dnf_states =
      if Opts.function_summaries then Dom.dnf state else [state]
    in
    let domain_call =
      Dom.call ~globals ~actuals ~areturn ~formals ~freturn ~locals
    in
    List.fold dnf_states Work.skip ~f:(fun state acc ->
        match
          if not Opts.function_summaries then None
          else
            let maybe_summary_post =
              let state = fst (domain_call ~summaries:false state) in
              let* summary = Llair.Function.Tbl.find summary_table name in
              List.find_map ~f:(Dom.apply_summary state) summary
            in
            [%Trace.info
              "Maybe summary post: %a" (Option.pp "%a" Dom.pp)
                maybe_summary_post] ;
            maybe_summary_post
        with
        | None ->
            let state, from_call =
              domain_call ~summaries:Opts.function_summaries state
            in
            Work.seq acc
              ( match Stack.push_call call from_call stk with
              | Some stk ->
                  Work.add stk ~prev:block ~retreating:recursive state entry
              | None -> (
                match Dom.recursion_beyond_bound with
                | `skip -> Work.seq acc (exec_jump stk state block return)
                | `prune -> Work.skip ) )
        | Some post -> Work.seq acc (exec_jump stk post block return) )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_skip_func :
         Stack.t
      -> Dom.t
      -> Llair.block
      -> Llair.Reg.t option
      -> Llair.jump
      -> Work.x =
   fun stk state block areturn return ->
    Report.unknown_call block.term ;
    let state = Option.fold ~f:Dom.exec_kill areturn state in
    exec_jump stk state block return

  let exec_call stk state block ({Llair.callee; areturn; return; _} as call)
      globals =
    if Llair.Func.is_undefined callee then
      exec_skip_func stk state block areturn return
    else exec_call stk state block call globals

  let pp_st () =
    [%Trace.printf
      "@[<v>%t@]" (fun fs ->
          Llair.Function.Tbl.iteri summary_table ~f:(fun ~key ~data ->
              Format.fprintf fs "@[<v>%a:@ @[%a@]@]@ " Llair.Function.pp key
                (List.pp "@," Dom.pp_summary)
                data ) )]

  let exec_return stk pre_state (block : Llair.block) exp =
    let Llair.{name; formals; freturn; locals} = block.parent in
    [%Trace.call fun {pf} -> pf "@ from: %a" Llair.Function.pp name]
    ;
    let summarize post_state =
      if not Opts.function_summaries then post_state
      else
        let function_summary, post_state =
          Dom.create_summary ~locals ~formals post_state
        in
        Llair.Function.Tbl.add_multi ~key:name ~data:function_summary
          summary_table ;
        pp_st () ;
        post_state
    in
    let exit_state =
      match (freturn, exp) with
      | Some freturn, Some return_val ->
          Dom.exec_move (IArray.of_ (freturn, return_val)) pre_state
      | None, None -> pre_state
      | _ -> violates Llair.Func.invariant block.parent
    in
    ( match Stack.pop_return stk with
    | Some (from_call, retn_site, stk) ->
        let post_state = summarize (Dom.post locals from_call exit_state) in
        let retn_state = Dom.retn formals freturn from_call post_state in
        exec_jump stk retn_state block retn_site
    | None ->
        (* Create and store a function summary for main *)
        if
          Opts.function_summaries
          && List.mem ~eq:String.equal
               (Llair.Function.name name)
               Opts.entry_points
        then summarize exit_state |> (ignore : Dom.t -> unit) ;
        Work.skip )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_throw stk pre_state (block : Llair.block) exc =
    let func = block.parent in
    [%Trace.call fun {pf} -> pf "@ from %a" Llair.Function.pp func.name]
    ;
    let unwind formals scope from_call state =
      Dom.retn formals (Some func.fthrow) from_call
        (Dom.post scope from_call state)
    in
    ( match Stack.pop_throw stk ~unwind pre_state with
    | Some (from_call, retn_site, stk, unwind_state) ->
        let fthrow = func.fthrow in
        let exit_state =
          Dom.exec_move (IArray.of_ (fthrow, exc)) unwind_state
        in
        let post_state = Dom.post func.locals from_call exit_state in
        let retn_state =
          Dom.retn func.formals func.freturn from_call post_state
        in
        exec_jump stk retn_state block retn_site
    | None -> Work.skip )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_assume cond jump stk state block =
    match Dom.exec_assume state cond with
    | Some state -> exec_jump stk state block jump
    | None ->
        [%Trace.info
          "@[<2>infeasible assume %a@\n@[%a@]@]" Llair.Exp.pp cond Dom.pp
            state] ;
        Work.skip

  let exec_term : Llair.program -> Stack.t -> Dom.t -> Llair.block -> Work.x
      =
   fun pgm stk state block ->
    [%Trace.info
      "@[<2>exec term@\n@[%a@]@\n%a@]" Dom.pp state Llair.Term.pp block.term] ;
    Report.step_term block ;
    match block.term with
    | Switch {key; tbl; els} ->
        IArray.fold
          ~f:(fun (case, jump) x ->
            exec_assume (Llair.Exp.eq key case) jump stk state block
            |> Work.seq x )
          tbl
          (exec_assume
             (IArray.fold tbl Llair.Exp.true_ ~f:(fun (case, _) b ->
                  Llair.Exp.and_ (Llair.Exp.dq key case) b ))
             els stk state block)
    | Iswitch {ptr; tbl} ->
        IArray.fold tbl Work.skip ~f:(fun (jump : Llair.jump) x ->
            exec_assume
              (Llair.Exp.eq ptr
                 (Llair.Exp.label
                    ~parent:(Llair.Function.name jump.dst.parent.name)
                    ~name:jump.dst.lbl))
              jump stk state block
            |> Work.seq x )
    | Call ({callee} as call) ->
        exec_call stk state block call
          (Domain_used_globals.by_function Opts.globals callee.name)
    | ICall ({callee; areturn; return} as call) -> (
        let lookup name = Llair.Func.find name pgm.functions in
        match Dom.resolve_callee lookup callee state with
        | [] -> exec_skip_func stk state block areturn return
        | callees ->
            List.fold callees Work.skip ~f:(fun callee x ->
                exec_call stk state block {call with callee}
                  (Domain_used_globals.by_function Opts.globals callee.name)
                |> Work.seq x ) )
    | Return {exp} -> exec_return stk state block exp
    | Throw {exc} -> exec_throw stk state block exc
    | Unreachable -> Work.skip

  let exec_inst :
         Llair.block
      -> Llair.inst
      -> Dom.t
      -> (Dom.t, Dom.t * Llair.inst) result =
   fun block inst state ->
    [%Trace.info
      "@[<2>exec inst@\n@[%a@]@\n%a@]" Dom.pp state Llair.Inst.pp inst] ;
    Report.step_inst block inst ;
    Dom.exec_inst inst state
    |> function
    | Some state -> Result.Ok state | None -> Result.Error (state, inst)

  let exec_block :
      Llair.program -> Stack.t -> Dom.t -> Llair.block -> Work.x =
   fun pgm stk state block ->
    [%trace]
      ~call:(fun {pf} ->
        pf "@ #%i %%%s in %a" block.sort_index block.lbl Llair.Function.pp
          block.parent.name )
      ~retn:(fun {pf} _ ->
        pf "#%i %%%s in %a" block.sort_index block.lbl Llair.Function.pp
          block.parent.name )
    @@ fun () ->
    match
      Iter.fold_result ~f:(exec_inst block)
        (IArray.to_iter block.cmnd)
        state
    with
    | Ok state -> exec_term pgm stk state block
    | Error (state, inst) ->
        Report.invalid_access_inst (Dom.report_fmt_thunk state) inst ;
        Work.skip

  let harness : Llair.program -> Work.t option =
   fun pgm ->
    List.find_map
      ~f:(fun entry_point -> Llair.Func.find entry_point pgm.functions)
      Opts.entry_points
    |> function
    | Some {name; formals; freturn; locals; entry}
      when IArray.is_empty formals ->
        Some
          (Work.init
             (fst
                (Dom.call ~summaries:Opts.function_summaries
                   ~globals:
                     (Domain_used_globals.by_function Opts.globals name)
                   ~actuals:IArray.empty ~areturn:None ~formals:IArray.empty
                   ~freturn ~locals (Dom.init pgm.globals)))
             entry)
    | _ -> None

  let exec_pgm : Llair.program -> unit =
   fun pgm ->
    match harness pgm with
    | Some work -> Work.run ~f:(exec_block pgm) work
    | None -> fail "no applicable harness" ()

  let compute_summaries pgm : Dom.summary list Llair.Function.Map.t =
    assert Opts.function_summaries ;
    exec_pgm pgm ;
    Llair.Function.Tbl.fold summary_table Llair.Function.Map.empty
      ~f:(fun ~key ~data map ->
        match data with
        | [] -> map
        | _ -> Llair.Function.Map.add ~key ~data map )
end
[@@inlined]
