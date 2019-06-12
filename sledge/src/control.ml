(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Iterative Breadth-First Bounded Exploration

    The analysis' semantics of control flow. *)

type exec_opts = {bound: int; skip_throw: bool}

module Stack : sig
  type t
  type as_inlined_location = t [@@deriving compare, sexp_of]

  val empty : t
  val push_jump : Var.Set.t -> t -> t

  val push_call :
       Llair.block
    -> retreating:bool
    -> bound:int
    -> return:Llair.jump
    -> Domain.from_call
    -> ?throw:Llair.jump
    -> t
    -> t option

  val pop_return :
       t
    -> init:'a
    -> f:(Var.t option -> Var.Set.t -> Domain.from_call -> 'a -> 'a)
    -> (Llair.jump * 'a * t) option

  val pop_throw :
       t
    -> init:'a
    -> f:(Var.t option -> Var.Set.t -> Domain.from_call -> 'a -> 'a)
    -> (Llair.jump * 'a * t) option
end = struct
  type t =
    | Locals of Var.Set.t * t
    | Return of
        { retreating: bool
              (** return from a call not known to be nonrecursive *)
        ; dst: Llair.Jump.t
        ; freturn: Var.t option
        ; fthrow: Var.t
        ; from_call: Domain.from_call
        ; stk: t }
    | Throw of Llair.Jump.t * t
    | Empty
  [@@deriving sexp_of]

  type as_inlined_location = t [@@deriving sexp_of]

  (* Treat a stack as a code location in a hypothetical expansion of the
     program where all non-recursive functions have been completely inlined.
     In particular, this means to compare stacks as if all Locals frames or
     Return frames for recursive calls had been removed. Additionally, the
     from_call info in Return frames is ignored. *)
  let rec compare_as_inlined_location x y =
    if x == y then 0
    else
      match (x, y) with
      | Locals (_, x), y
       |x, Locals (_, y)
       |Return {retreating= true; stk= x}, y
       |x, Return {retreating= true; stk= y} ->
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
    | Locals (_, s) ->
        print_abbrev fs s ;
        Format.pp_print_char fs 'L'
    | Return {retreating= false; stk= s} ->
        print_abbrev fs s ;
        Format.pp_print_char fs 'R'
    | Return {retreating= true; stk= s} ->
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
    | Locals _ | Return _ | Throw (_, Return _) | Empty -> ()
    | Throw _ -> fail "malformed stack: %a" print_abbrev s ()

  let empty = Empty |> check invariant

  let push_jump lcls stk =
    (if Set.is_empty lcls then stk else Locals (lcls, stk))
    |> check invariant

  let push_return ~retreating dst freturn fthrow from_call stk =
    Return {retreating; dst; freturn; fthrow; from_call; stk}
    |> check invariant

  let push_throw jmp stk =
    (match jmp with None -> stk | Some jmp -> Throw (jmp, stk))
    |> check invariant

  let push_call {Llair.locals; parent= {freturn; fthrow}} ~retreating ~bound
      ~return from_call ?throw stk =
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
    else
      Some
        (push_jump locals
           (push_throw throw
              (push_return ~retreating return freturn fthrow from_call stk)))
    )
    |>
    [%Trace.retn fun {pf} _ ->
      pf "%d of %a on stack" n Llair.Jump.pp return]

  let pop_return stk ~init ~f =
    let rec pop_return_ scope = function
      | Locals (locals, stk) -> pop_return_ (Set.union locals scope) stk
      | Throw (_, stk) -> pop_return_ scope stk
      | Return {dst; freturn; from_call; stk} ->
          let dst =
            match freturn with
            | Some freturn -> {dst with args= Exp.var freturn :: dst.args}
            | None -> dst
          in
          Some (dst, f freturn scope from_call init, stk)
      | Empty -> None
    in
    pop_return_ Var.Set.empty stk

  let pop_throw stk ~init ~f =
    let rec pop_throw_ scope state = function
      | Locals (locals, stk) ->
          pop_throw_ (Set.union locals scope) state stk
      | Return {freturn; from_call; stk} ->
          pop_throw_ Var.Set.empty (f freturn scope from_call state) stk
      | Throw (dst, Return {fthrow; from_call; stk}) ->
          let dst = {dst with args= Exp.var fthrow :: dst.args} in
          Some (dst, f (Some fthrow) scope from_call state, stk)
      | Empty -> None
      | Throw _ as stk -> violates invariant stk
    in
    pop_throw_ Var.Set.empty init stk
end

module Work : sig
  type t

  val init : Domain.t -> Llair.block -> int -> t

  type x

  val skip : x
  val seq : x -> x -> x

  val add :
       ?prev:Llair.block
    -> retreating:bool
    -> Stack.t
    -> Domain.t
    -> Llair.block
    -> x

  val run : f:(Stack.t -> Domain.t -> Llair.block -> x) -> t -> unit
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
  type waiting_states = (Domain.t * Depths.t) list Map.M(Llair.Block).t
  type t = priority_queue * waiting_states * int
  type x = Depths.t -> t -> t

  let empty_waiting_states : waiting_states = Map.empty (module Llair.Block)
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
      | Some (state :: states, ws) ->
          let join (qa, da) (q, d) = (Domain.join q qa, Depths.join d da) in
          let qs, depths = List.fold ~f:join ~init:state states in
          run ~f (f stk qs dst depths (pq, ws, bnd))
      | _ ->
          [%Trace.info "done: %a" Edge.pp edge] ;
          run ~f (pq, ws, bnd) )
    | None -> [%Trace.info "queue empty"] ; ()
end

let exec_goto stk state block ({dst; retreating} : Llair.jump) =
  let stk = Stack.push_jump dst.locals stk in
  Work.add ~prev:block ~retreating stk state dst

let exec_jump stk state block ({dst; args} as jmp : Llair.jump) =
  let state = Domain.jump args dst.params dst.locals state in
  exec_goto stk state block jmp

let exec_call ~opts stk state block ({dst; args; retreating} : Llair.jump)
    (return : Llair.jump) throw =
  [%Trace.call fun {pf} ->
    pf "%a from %a" Var.pp dst.parent.name.var Var.pp
      return.dst.parent.name.var]
  ;
  let state, from_call = Domain.call args dst.params dst.locals state in
  ( match
      Stack.push_call ~bound:opts.bound dst ~retreating ~return from_call
        ?throw stk
    with
  | Some stk -> Work.add stk ~prev:block ~retreating state dst
  | None -> Work.skip )
  |>
  [%Trace.retn fun {pf} _ -> pf ""]

let exec_pop pop stk state (block : Llair.block) exp =
  [%Trace.call fun {pf} -> pf "from %a" Var.pp block.parent.name.var]
  ;
  ( match pop stk ~init:state ~f:(Domain.retn exp) with
  | Some ((jmp : Llair.jump), state, stk) -> exec_jump stk state block jmp
  | None -> Work.skip )
  |>
  [%Trace.retn fun {pf} _ -> pf ""]

let exec_return stk state block exp =
  exec_pop Stack.pop_return stk state block exp

let exec_throw stk state block exc =
  exec_pop Stack.pop_throw stk state block (Some exc)

let exec_skip_func :
    Stack.t -> Domain.t -> Llair.block -> Llair.jump -> Work.x =
 fun stk state block ({dst; args} as return) ->
  Report.unknown_call block.term ;
  let return =
    if List.is_empty dst.params then return
    else
      let args =
        List.fold_right dst.params ~init:args ~f:(fun param args ->
            Exp.nondet (Var.name param) :: args )
      in
      {return with args}
  in
  exec_jump stk state block return

let exec_term :
       opts:exec_opts
    -> Llair.t
    -> Stack.t
    -> Domain.t
    -> Llair.block
    -> Work.x =
 fun ~opts pgm stk state block ->
  [%Trace.info "exec %a" Llair.Term.pp block.term] ;
  match block.term with
  | Switch {key; tbl; els} ->
      Vector.fold tbl
        ~f:(fun x (case, jump) ->
          match Domain.assume state (Exp.eq key case) with
          | Some state -> exec_jump stk state block jump |> Work.seq x
          | None -> x )
        ~init:
          ( match
              Domain.assume state
                (Vector.fold tbl ~init:(Exp.bool true)
                   ~f:(fun b (case, _) -> Exp.and_ (Exp.dq key case) b))
            with
          | Some state -> exec_jump stk state block els
          | None -> Work.skip )
  | Iswitch {ptr; tbl} ->
      Vector.fold tbl ~init:Work.skip ~f:(fun x (jump : Llair.jump) ->
          match
            Domain.assume state
              (Exp.eq ptr
                 (Exp.label
                    ~parent:(Var.name jump.dst.parent.name.var)
                    ~name:jump.dst.lbl))
          with
          | Some state -> exec_jump stk state block jump |> Work.seq x
          | None -> x )
  | Call {call= {dst; args; retreating}; return; throw} -> (
    match
      let lookup name =
        Option.to_list (Llair.Func.find pgm.functions name)
      in
      Domain.resolve_callee lookup dst state
    with
    | [] -> exec_skip_func stk state block return
    | callees ->
        List.fold callees ~init:Work.skip ~f:(fun x callee ->
            ( match
                Domain.exec_intrinsic state
                  (List.hd return.dst.params)
                  callee.name.var args
              with
            | Some (Error ()) ->
                Report.invalid_access_term state block.term ;
                Work.skip
            | Some (Ok state) -> exec_goto stk state block return
            | None when Llair.Func.is_undefined callee ->
                exec_skip_func stk state block return
            | None ->
                exec_call ~opts stk state block
                  {dst= callee.entry; args; retreating}
                  return throw )
            |> Work.seq x ) )
  | Return {exp} -> exec_return stk state block exp
  | Throw {exc} ->
      if opts.skip_throw then Work.skip else exec_throw stk state block exc
  | Unreachable -> Work.skip

let exec_inst :
    Domain.t -> Llair.inst -> (Domain.t, Domain.t * Llair.inst) result =
 fun state inst ->
  Domain.exec_inst state inst
  |> Result.map_error ~f:(fun () -> (state, inst))

let exec_block :
       opts:exec_opts
    -> Llair.t
    -> Stack.t
    -> Domain.t
    -> Llair.block
    -> Work.x =
 fun ~opts pgm stk state block ->
  [%Trace.info "exec %a" Llair.Block.pp block] ;
  match Vector.fold_result ~f:exec_inst ~init:state block.cmnd with
  | Ok state -> exec_term ~opts pgm stk state block
  | Error (state, inst) ->
      Report.invalid_access_inst state inst ;
      Work.skip

let harness : Llair.t -> (int -> Work.t) option =
 fun pgm ->
  let entry_points = Config.find_list "entry_points" in
  List.find_map entry_points ~f:(fun name ->
      Llair.Func.find pgm.functions (Var.program name) )
  |> function
  | Some {entry= {params= []} as block} ->
      Some
        (Work.init
           (fst (Domain.call [] [] block.locals (Domain.init pgm.globals)))
           block)
  | _ -> None

let exec_pgm : exec_opts -> Llair.t -> unit =
 fun opts pgm ->
  [%Trace.call fun {pf} -> pf "@]@,@["]
  ;
  ( match harness pgm with
  | Some work -> Work.run ~f:(exec_block ~opts pgm) (work opts.bound)
  | None -> fail "no applicable harness" () )
  |>
  [%Trace.retn fun {pf} _ -> pf ""]
