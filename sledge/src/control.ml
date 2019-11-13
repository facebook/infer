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
  ; globals: Used_globals.r }

module Make (Dom : Domain_sig.Dom) = struct
  module Stack : sig
    type t
    type as_inlined_location = t [@@deriving compare, sexp_of]

    val empty : t

    val push_call :
      Llair.func Llair.call -> bound:int -> Dom.from_call -> t -> t option

    val pop_return : t -> (Dom.from_call * Llair.jump * t) option

    val pop_throw :
         t
      -> init:'a
      -> unwind:(Reg.t list -> Reg.Set.t -> Dom.from_call -> 'a -> 'a)
      -> (Dom.from_call * Llair.jump * t * 'a) option
  end = struct
    type t =
      | Return of
          { recursive: bool  (** return from a possibly-recursive call *)
          ; dst: Llair.Jump.t
          ; formals: Reg.t list
          ; locals: Reg.Set.t
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
      Invariant.invariant [%here] s [%sexp_of: t]
      @@ fun () ->
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

    let pop_throw stk ~init ~unwind =
      let rec pop_throw_ state = function
        | Return {formals; locals; from_call; stk} ->
            pop_throw_ (unwind formals locals from_call state) stk
        | Throw (dst, Return {from_call; stk}) ->
            Some (from_call, dst, stk, state)
        | Empty -> None
        | Throw _ as stk -> violates invariant stk
      in
      pop_throw_ init stk
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
      include Comparator.Make (T)

      let pp fs {dst; src} =
        Format.fprintf fs "#%i %s <--%a" dst.sort_index dst.lbl
          (Option.pp "%a" (fun fs (src : Llair.Block.t) ->
               Format.fprintf fs " #%i %s" src.sort_index src.lbl ))
          src
    end

    module Depths = struct
      type t = int Map.M(Edge).t

      let empty = Map.empty (module Edge)
      let find = Map.find
      let set = Map.set

      let join x y =
        Map.merge x y ~f:(fun ~key:_ -> function
          | `Left d | `Right d -> Some d
          | `Both (d1, d2) -> Some (Int.max d1 d2) )
    end

    type priority = int * Edge.t [@@deriving compare]
    type priority_queue = priority Fheap.t
    type waiting_states = (Dom.t * Depths.t) list Map.M(Llair.Block).t
    type t = priority_queue * waiting_states * int
    type x = Depths.t -> t -> t

    let empty_waiting_states : waiting_states =
      Map.empty (module Llair.Block)

    let pp_priority fs (n, e) = Format.fprintf fs "%i: %a" n Edge.pp e

    let pp fs pq =
      Format.fprintf fs "@[%a@]"
        (List.pp " ::@ " pp_priority)
        (Sequence.to_list (Fheap.to_sequence pq))

    let skip _ w = w
    let seq x y d w = y d (x d w)

    let add ?prev ~retreating stk state curr depths ((pq, ws, bound) as work)
        =
      let edge = {Edge.dst= curr; src= prev; stk} in
      let depth = Option.value (Depths.find depths edge) ~default:0 in
      let depth = if retreating then depth + 1 else depth in
      if depth > bound then (
        [%Trace.info "prune: %i: %a" depth Edge.pp edge] ;
        work )
      else
        let pq = Fheap.add pq (depth, edge) in
        [%Trace.info "@[<6>enqueue %i: %a@ | %a@]" depth Edge.pp edge pp pq] ;
        let depths = Depths.set depths ~key:edge ~data:depth in
        let ws = Map.add_multi ws ~key:curr ~data:(state, depths) in
        (pq, ws, bound)

    let init state curr bound =
      add ~retreating:false Stack.empty state curr Depths.empty
        (Fheap.create ~cmp:compare_priority, empty_waiting_states, bound)

    let rec run ~f (pq0, ws, bnd) =
      match Fheap.pop pq0 with
      | Some ((_, ({Edge.dst; stk} as edge)), pq) -> (
        match Map.find_and_remove ws dst with
        | Some (q :: qs, ws) ->
            let join (qa, da) (q, d) = (Dom.join q qa, Depths.join d da) in
            let skipped, (qs, depths) =
              List.fold qs ~init:([], q) ~f:(fun (skipped, joined) curr ->
                  match join curr joined with
                  | Some joined, depths -> (skipped, (joined, depths))
                  | None, _ -> (curr :: skipped, joined) )
            in
            let ws = Map.add_exn ws ~key:dst ~data:skipped in
            run ~f (f stk qs dst depths (pq, ws, bnd))
        | _ ->
            [%Trace.info "done: %a" Edge.pp edge] ;
            run ~f (pq, ws, bnd) )
      | None -> [%Trace.info "queue empty"] ; ()
  end

  let exec_jump stk state block Llair.{dst; retreating} =
    Work.add ~prev:block ~retreating stk state dst

  let summary_table = Hashtbl.create (module Reg)

  let exec_call opts stk state block call globals =
    let Llair.{callee; actuals; areturn; return; recursive} = call in
    let Llair.{name; formals; freturn; locals; entry} = callee in
    [%Trace.call fun {pf} ->
      pf "%a from %a with state@ %a" Reg.pp name.reg Reg.pp
        return.dst.parent.name.reg Dom.pp state]
    ;
    let dnf_states =
      if opts.function_summaries then Dom.dnf state else [state]
    in
    let domain_call =
      Dom.call ~globals ~actuals ~areturn ~formals ~freturn ~locals
    in
    List.fold ~init:Work.skip dnf_states ~f:(fun acc state ->
        match
          if not opts.function_summaries then None
          else
            let maybe_summary_post =
              let state = fst (domain_call ~summaries:false state) in
              Hashtbl.find summary_table name.reg
              >>= List.find_map ~f:(Dom.apply_summary state)
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
          Hashtbl.iteri summary_table ~f:(fun ~key ~data ->
              Format.fprintf fs "@[<v>%a:@ @[%a@]@]@ " Reg.pp key
                (List.pp "@," Dom.pp_summary)
                data ) )]

  let exec_return ~opts stk pre_state (block : Llair.block) exp =
    let Llair.{name; formals; freturn; locals} = block.parent in
    [%Trace.call fun {pf} -> pf "from: %a" Reg.pp name.reg]
    ;
    let summarize post_state =
      if not opts.function_summaries then post_state
      else
        let globals = Used_globals.by_function opts.globals name.reg in
        let function_summary, post_state =
          Dom.create_summary ~locals post_state
            ~formals:(Set.union (Reg.Set.of_list formals) globals)
        in
        Hashtbl.add_multi summary_table ~key:name.reg ~data:function_summary ;
        pp_st () ;
        post_state
    in
    let exit_state =
      match (freturn, exp) with
      | Some freturn, Some return_val ->
          Dom.exec_move pre_state (Vector.of_ (freturn, return_val))
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
          && List.exists
               (Config.find_list "entry-points")
               ~f:(String.equal (Reg.name name.reg))
        then summarize exit_state |> (ignore : Dom.t -> unit) ;
        Work.skip )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let exec_throw stk pre_state (block : Llair.block) exc =
    let func = block.parent in
    [%Trace.call fun {pf} -> pf "from %a" Reg.pp func.name.reg]
    ;
    let unwind formals scope from_call state =
      Dom.retn formals (Some func.fthrow) from_call
        (Dom.post scope from_call state)
    in
    ( match Stack.pop_throw stk ~unwind ~init:pre_state with
    | Some (from_call, retn_site, stk, unwind_state) ->
        let fthrow = func.fthrow in
        let exit_state =
          Dom.exec_move unwind_state (Vector.of_ (fthrow, exc))
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
      -> Reg.t option
      -> Llair.jump
      -> Work.x =
   fun stk state block areturn return ->
    Report.unknown_call block.term ;
    let state = Option.fold ~f:Dom.exec_kill ~init:state areturn in
    exec_jump stk state block return

  let exec_term :
      exec_opts -> Llair.t -> Stack.t -> Dom.t -> Llair.block -> Work.x =
   fun opts pgm stk state block ->
    [%Trace.info "exec %a" Llair.Term.pp block.term] ;
    match block.term with
    | Switch {key; tbl; els} ->
        Vector.fold tbl
          ~f:(fun x (case, jump) ->
            match Dom.exec_assume state (Exp.eq key case) with
            | Some state -> exec_jump stk state block jump |> Work.seq x
            | None -> x )
          ~init:
            ( match
                Dom.exec_assume state
                  (Vector.fold tbl ~init:Exp.true_ ~f:(fun b (case, _) ->
                       Exp.and_ (Exp.dq key case) b ))
              with
            | Some state -> exec_jump stk state block els
            | None -> Work.skip )
    | Iswitch {ptr; tbl} ->
        Vector.fold tbl ~init:Work.skip ~f:(fun x (jump : Llair.jump) ->
            match
              Dom.exec_assume state
                (Exp.eq ptr
                   (Exp.label
                      ~parent:(Reg.name jump.dst.parent.name.reg)
                      ~name:jump.dst.lbl))
            with
            | Some state -> exec_jump stk state block jump |> Work.seq x
            | None -> x )
    | Call ({callee; actuals; areturn; return} as call) -> (
        let lookup name =
          Option.to_list (Llair.Func.find pgm.functions name)
        in
        let callees, state = Dom.resolve_callee lookup callee state in
        match callees with
        | [] -> exec_skip_func stk state block areturn return
        | callees ->
            List.fold callees ~init:Work.skip ~f:(fun x callee ->
                ( match
                    Dom.exec_intrinsic ~skip_throw:opts.skip_throw state
                      areturn callee.name.reg actuals
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
                      (Used_globals.by_function opts.globals
                         callee.name.reg) )
                |> Work.seq x ) )
    | Return {exp} -> exec_return ~opts stk state block exp
    | Throw {exc} ->
        if opts.skip_throw then Work.skip
        else exec_throw stk state block exc
    | Unreachable -> Work.skip

  let exec_inst : Dom.t -> Llair.inst -> (Dom.t, Dom.t * Llair.inst) result
      =
   fun state inst ->
    Dom.exec_inst state inst |> Result.of_option ~error:(state, inst)

  let exec_block :
      exec_opts -> Llair.t -> Stack.t -> Dom.t -> Llair.block -> Work.x =
   fun opts pgm stk state block ->
    [%Trace.info "exec %a" Llair.Block.pp block] ;
    match Vector.fold_result ~f:exec_inst ~init:state block.cmnd with
    | Ok state -> exec_term opts pgm stk state block
    | Error (state, inst) ->
        Report.invalid_access_inst (Dom.report_fmt_thunk state) inst ;
        Work.skip

  let harness : exec_opts -> Llair.t -> (int -> Work.t) option =
   fun opts pgm ->
    let entry_points = Config.find_list "entry-points" in
    List.find_map ~f:(Llair.Func.find pgm.functions) entry_points
    |> function
    | Some {name= {reg}; formals= []; freturn; locals; entry} ->
        Some
          (Work.init
             (fst
                (Dom.call ~summaries:opts.function_summaries
                   ~globals:(Used_globals.by_function opts.globals reg)
                   ~actuals:[] ~areturn:None ~formals:[] ~freturn ~locals
                   (Dom.init pgm.globals)))
             entry)
    | _ -> None

  let exec_pgm : exec_opts -> Llair.t -> unit =
   fun opts pgm ->
    [%Trace.call fun {pf} -> pf "@]@,@["]
    ;
    ( match harness opts pgm with
    | Some work -> Work.run ~f:(exec_block opts pgm) (work opts.bound)
    | None -> fail "no applicable harness" () )
    |>
    [%Trace.retn fun {pf} _ -> pf ""]

  let compute_summaries opts pgm : Dom.summary list Reg.Map.t =
    assert opts.function_summaries ;
    exec_pgm opts pgm ;
    Hashtbl.fold summary_table ~init:Reg.Map.empty ~f:(fun ~key ~data map ->
        match data with [] -> map | _ -> Map.set map ~key ~data )
end
