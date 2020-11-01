(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Iterative Breadth-First Bounded Exploration

    The analysis' semantics of control flow. *)

type exec_opts =
  { bound: int
  ; skip_throw: bool
  ; function_summaries: bool
  ; entry_points: string list
  ; globals: Domain_used_globals.r }

module Make (Dom : Domain_intf.Dom) = struct
  module Stack : sig
    type t
    type as_inlined_location = t [@@deriving compare, sexp_of]

    val empty : t

    val push_call :
      Llair.func Llair.call -> bound:int -> Dom.from_call -> t -> t option

    val pop_return : t -> (Dom.from_call * Llair.jump * t) option

    val pop_throw :
         t
      -> 'a
      -> unwind:
           (   Llair.Reg.t list
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
          ; formals: Llair.Reg.t list
          ; locals: Llair.Reg.Set.t
          ; from_call: Dom.from_call
          ; stk: t }
      | Throw of Llair.Jump.t * t
      | Empty
    [@@deriving sexp_of]

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

    let rec print_abbrev fs = function
      | Return {recursive= false; stk= s} ->
          print_abbrev fs s ;
          Format.pp_print_char fs 'R'
      | Return {recursive= true; stk= s} ->
          print_abbrev fs s ;
          Format.pp_print_string fs "R↑"
      | Throw (_, s) ->
          print_abbrev fs s ;
          Format.pp_print_char fs 'T'
      | Empty -> ()

    let invariant s =
      let@ () = Invariant.invariant [%here] s [%sexp_of: t] in
      match s with
      | Return _ | Throw (_, Return _) | Empty -> ()
      | Throw _ -> fail "malformed stack: %a" print_abbrev s ()

    let empty = Empty |> check invariant

    let push_return Llair.{callee= {formals; locals}; return; recursive}
        from_call stk =
      Return {recursive; dst= return; formals; locals; from_call; stk}
      |> check invariant

    let push_throw jmp stk =
      (match jmp with None -> stk | Some jmp -> Throw (jmp, stk))
      |> check invariant

    let push_call (Llair.{return; throw} as call) ~bound from_call stk =
      [%Trace.call fun {pf} -> pf "%a" print_abbrev stk]
      ;
      let rec count_f_in_stack acc f = function
        | Return {stk= next_frame; dst= dest_block} ->
            count_f_in_stack
              (if Llair.Jump.equal dest_block f then acc + 1 else acc)
              f next_frame
        | _ -> acc
      in
      let n = count_f_in_stack 0 return stk in
      ( if n > bound then None
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

    val init : Dom.t -> Llair.block -> int -> t

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
        [@@deriving compare, sexp_of]
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

      type t = int M.t

      let empty = M.empty
      let find = M.find
      let add = M.add

      let join x y =
        M.merge x y ~f:(fun _ -> function
          | `Left d | `Right d -> Some d
          | `Both (d1, d2) -> Some (Int.max d1 d2) )
    end

    type priority = int * Edge.t [@@deriving compare]
    type priority_queue = priority FHeap.t
    type waiting_states = (Dom.t * Depths.t) list Llair.Block.Map.t
    type t = priority_queue * waiting_states * int
    type x = Depths.t -> t -> t

    let empty_waiting_states : waiting_states = Llair.Block.Map.empty
    let pp_priority fs (n, e) = Format.fprintf fs "%i: %a" n Edge.pp e

    let pp fs pq =
      Format.fprintf fs "@[%a@]"
        (List.pp " ::@ " pp_priority)
        (FHeap.to_list pq)

    let skip _ w = w
    let seq x y d w = y d (x d w)

    let add ?prev ~retreating stk state curr depths ((pq, ws, bound) as work)
        =
      let edge = {Edge.dst= curr; src= prev; stk} in
      let depth = Option.value (Depths.find edge depths) ~default:0 in
      let depth = if retreating then depth + 1 else depth in
      if depth > bound then (
        [%Trace.info "prune: %i: %a" depth Edge.pp edge] ;
        work )
      else
        let pq = FHeap.add pq (depth, edge) in
        [%Trace.info "@[<6>enqueue %i: %a@ | %a@]" depth Edge.pp edge pp pq] ;
        let depths = Depths.add ~key:edge ~data:depth depths in
        let ws =
          Llair.Block.Map.add_multi ~key:curr ~data:(state, depths) ws
        in
        (pq, ws, bound)

    let init state curr bound =
      add ~retreating:false Stack.empty state curr Depths.empty
        (FHeap.create ~cmp:compare_priority, empty_waiting_states, bound)

    let rec run ~f (pq0, ws, bnd) =
      match FHeap.pop pq0 with
      | Some ((_, ({Edge.dst; stk} as edge)), pq) -> (
        match Llair.Block.Map.find_and_remove dst ws with
        | Some (q :: qs, ws) ->
            let join (qa, da) (q, d) = (Dom.join q qa, Depths.join d da) in
            let skipped, (qs, depths) =
              List.fold qs ([], q) ~f:(fun curr (skipped, joined) ->
                  match join curr joined with
                  | Some joined, depths -> (skipped, (joined, depths))
                  | None, _ -> (curr :: skipped, joined) )
            in
            let ws = Llair.Block.Map.add_exn ~key:dst ~data:skipped ws in
            run ~f (f stk qs dst depths (pq, ws, bnd))
        | _ ->
            [%Trace.info "done: %a" Edge.pp edge] ;
            run ~f (pq, ws, bnd) )
      | None ->
          [%Trace.info "queue empty"] ;
          ()
  end

  let exec_jump stk state block Llair.{dst; retreating} =
    Work.add ~prev:block ~retreating stk state dst

  module RegTbl = HashTable.Make (Llair.Reg)

  let summary_table = RegTbl.create ()

  let exec_call opts stk state block call globals =
    let Llair.{callee; actuals; areturn; return; recursive} = call in
    let Llair.{name; formals; freturn; locals; entry} = callee in
    [%Trace.call fun {pf} ->
      pf "%a from %a with state@ %a" Llair.Reg.pp name.reg Llair.Reg.pp
        return.dst.parent.name.reg Dom.pp state]
    ;
    let dnf_states =
      if opts.function_summaries then Dom.dnf state else [state]
    in
    let domain_call =
      Dom.call ~globals ~actuals ~areturn ~formals ~freturn ~locals
    in
    List.fold dnf_states Work.skip ~f:(fun state acc ->
        match
          if not opts.function_summaries then None
          else
            let maybe_summary_post =
              let state = fst (domain_call ~summaries:false state) in
              let* summary = RegTbl.find summary_table name.reg in
              List.find_map ~f:(Dom.apply_summary state) summary
            in
            [%Trace.info
              "Maybe summary post: %a" (Option.pp "%a" Dom.pp)
                maybe_summary_post] ;
            maybe_summary_post
        with
        | None ->
            let state, from_call =
              domain_call ~summaries:opts.function_summaries state
            in
            Work.seq acc
              ( match
                  Stack.push_call call ~bound:opts.bound from_call stk
                with
              | Some stk ->
                  Work.add stk ~prev:block ~retreating:recursive state entry
              | None -> (
                match Dom.recursion_beyond_bound with
                | `skip -> Work.seq acc (exec_jump stk state block return)
                | `prune -> Work.skip ) )
        | Some post -> Work.seq acc (exec_jump stk post block return) )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let pp_st () =
    [%Trace.printf
      "@[<v>%t@]" (fun fs ->
          RegTbl.iteri summary_table ~f:(fun ~key ~data ->
              Format.fprintf fs "@[<v>%a:@ @[%a@]@]@ " Llair.Reg.pp key
                (List.pp "@," Dom.pp_summary)
                data ) )]

  let exec_return ~opts stk pre_state (block : Llair.block) exp =
    let Llair.{name; formals; freturn; locals} = block.parent in
    [%Trace.call fun {pf} -> pf "from: %a" Llair.Reg.pp name.reg]
    ;
    let summarize post_state =
      if not opts.function_summaries then post_state
      else
        let globals =
          Domain_used_globals.by_function opts.globals name.reg
        in
        let function_summary, post_state =
          Dom.create_summary ~locals post_state
            ~formals:
              (Llair.Reg.Set.union (Llair.Reg.Set.of_list formals) globals)
        in
        RegTbl.add_multi ~key:name.reg ~data:function_summary summary_table ;
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
          opts.function_summaries
          && List.exists opts.entry_points
               ~f:(String.equal (Llair.Reg.name name.reg))
        then summarize exit_state |> (ignore : Dom.t -> unit) ;
        Work.skip )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_throw stk pre_state (block : Llair.block) exc =
    let func = block.parent in
    [%Trace.call fun {pf} -> pf "from %a" Llair.Reg.pp func.name.reg]
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

  let exec_term :
         exec_opts
      -> Llair.program
      -> Stack.t
      -> Dom.t
      -> Llair.block
      -> Work.x =
   fun opts pgm stk state block ->
    [%Trace.info
      "@[<2>exec term@\n@[%a@]@\n%a@]" Dom.pp state Llair.Term.pp block.term] ;
    Report.step () ;
    match block.term with
    | Switch {key; tbl; els} ->
        IArray.fold
          ~f:(fun (case, jump) x ->
            match Dom.exec_assume state (Llair.Exp.eq key case) with
            | Some state -> exec_jump stk state block jump |> Work.seq x
            | None -> x )
          tbl
          ( match
              Dom.exec_assume state
                (IArray.fold tbl Llair.Exp.true_ ~f:(fun (case, _) b ->
                     Llair.Exp.and_ (Llair.Exp.dq key case) b ))
            with
          | Some state -> exec_jump stk state block els
          | None -> Work.skip )
    | Iswitch {ptr; tbl} ->
        IArray.fold tbl Work.skip ~f:(fun (jump : Llair.jump) x ->
            match
              Dom.exec_assume state
                (Llair.Exp.eq ptr
                   (Llair.Exp.label
                      ~parent:(Llair.Reg.name jump.dst.parent.name.reg)
                      ~name:jump.dst.lbl))
            with
            | Some state -> exec_jump stk state block jump |> Work.seq x
            | None -> x )
    | Call ({callee; actuals; areturn; return} as call) -> (
        let lookup name =
          Option.to_list (Llair.Func.find name pgm.functions)
        in
        let callees, state = Dom.resolve_callee lookup callee state in
        match callees with
        | [] -> exec_skip_func stk state block areturn return
        | callees ->
            List.fold callees Work.skip ~f:(fun callee x ->
                ( match
                    Dom.exec_intrinsic ~skip_throw:opts.skip_throw areturn
                      callee.name.reg actuals state
                  with
                | Some None ->
                    Report.invalid_access_term
                      (Dom.report_fmt_thunk state)
                      block.term ;
                    Work.skip
                | Some (Some state) when Dom.is_false state -> Work.skip
                | Some (Some state) -> exec_jump stk state block return
                | None when Llair.Func.is_undefined callee ->
                    exec_skip_func stk state block areturn return
                | None ->
                    exec_call opts stk state block {call with callee}
                      (Domain_used_globals.by_function opts.globals
                         callee.name.reg) )
                |> Work.seq x ) )
    | Return {exp} -> exec_return ~opts stk state block exp
    | Throw {exc} ->
        if opts.skip_throw then Work.skip
        else exec_throw stk state block exc
    | Unreachable -> Work.skip

  let exec_inst : Llair.inst -> Dom.t -> (Dom.t, Dom.t * Llair.inst) result
      =
   fun inst state ->
    [%Trace.info
      "@[<2>exec inst@\n@[%a@]@\n%a@]" Dom.pp state Llair.Inst.pp inst] ;
    Report.step () ;
    Dom.exec_inst inst state
    |> function
    | Some state -> Result.Ok state | None -> Result.Error (state, inst)

  let exec_block :
         exec_opts
      -> Llair.program
      -> Stack.t
      -> Dom.t
      -> Llair.block
      -> Work.x =
   fun opts pgm stk state block ->
    [%Trace.info "exec block %%%s" block.lbl] ;
    match
      Iter.fold_result ~f:exec_inst (IArray.to_iter block.cmnd) state
    with
    | Ok state -> exec_term opts pgm stk state block
    | Error (state, inst) ->
        Report.invalid_access_inst (Dom.report_fmt_thunk state) inst ;
        Work.skip

  let harness : exec_opts -> Llair.program -> (int -> Work.t) option =
   fun opts pgm ->
    List.find_map
      ~f:(fun entry_point -> Llair.Func.find entry_point pgm.functions)
      opts.entry_points
    |> function
    | Some {name= {reg}; formals= []; freturn; locals; entry} ->
        Some
          (Work.init
             (fst
                (Dom.call ~summaries:opts.function_summaries
                   ~globals:
                     (Domain_used_globals.by_function opts.globals reg)
                   ~actuals:[] ~areturn:None ~formals:[] ~freturn ~locals
                   (Dom.init pgm.globals)))
             entry)
    | _ -> None

  let exec_pgm : exec_opts -> Llair.program -> unit =
   fun opts pgm ->
    match harness opts pgm with
    | Some work -> Work.run ~f:(exec_block opts pgm) (work opts.bound)
    | None -> fail "no applicable harness" ()

  let compute_summaries opts pgm : Dom.summary list Llair.Reg.Map.t =
    assert opts.function_summaries ;
    exec_pgm opts pgm ;
    RegTbl.fold summary_table Llair.Reg.Map.empty ~f:(fun ~key ~data map ->
        match data with [] -> map | _ -> Llair.Reg.Map.add ~key ~data map )
end
