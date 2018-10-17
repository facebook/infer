(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Iterative Breadth-First Bounded Exploration

    The analysis' semantics of control flow. *)

let bound = 10

type stack =
  | Locals of Var.Set.t * stack
  | Return of Llair.Jump.t * Domain.from_call * stack
  | Throw of Llair.Jump.t * stack
  | Empty
[@@deriving compare, sexp_of]

module Work : sig
  type t

  val init : Domain.t -> Llair.block -> t

  type x

  val skip : x
  val seq : x -> x -> x

  val add :
       ?prev:Llair.block
    -> retreating:bool
    -> stack
    -> Domain.t
    -> Llair.block
    -> x

  val run : f:(stack -> Domain.t -> Llair.block -> x) -> t -> unit
end = struct
  module Edge = struct
    module T = struct
      type t = {dst: Llair.Block.t; src: Llair.Block.t option; stk: stack}
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
        | `Both (d1, d2) -> Some (Int.min d1 d2) )
  end

  type priority = int * Edge.t [@@deriving compare]
  type priority_queue = priority Fheap.t
  type waiting_states = (Domain.t * Depths.t) list Map.M(Llair.Block).t
  type t = priority_queue * waiting_states
  type x = Depths.t -> t -> t

  let empty_waiting_states = Map.empty (module Llair.Block)
  let pp_priority fs (n, e) = Format.fprintf fs "%i: %a" n Edge.pp e

  let pp fs pq =
    Format.fprintf fs "@[%a@]"
      (List.pp " ::@ " pp_priority)
      (Sequence.to_list (Fheap.to_sequence pq))

  let skip _ w = w
  let seq x y d w = y d (x d w)

  let add ?prev ~retreating stk state curr depths ((pq, ws) as work) =
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
      (pq, ws)

  let init state curr =
    add ~retreating:false Empty state curr Depths.empty
      (Fheap.create ~cmp:compare_priority, empty_waiting_states)

  let rec run ~f (pq0, ws) =
    match Fheap.pop pq0 with
    | Some ((_, ({Edge.dst; stk} as edge)), pq) -> (
      match Map.find_and_remove ws dst with
      | Some (state :: states, ws) ->
          let join (qa, da) (q, d) = (Domain.join q qa, Depths.join d da) in
          let qs, depths = List.fold ~f:join ~init:state states in
          run ~f (f stk qs dst depths (pq, ws))
      | _ ->
          [%Trace.info "done: %a" Edge.pp edge] ;
          run ~f (pq, ws) )
    | None -> [%Trace.info "queue empty"] ; ()
end

let push_jump lcls stk =
  if Set.is_empty lcls then stk else Locals (lcls, stk)

let exec_jump stk state block ({dst; args; retreating} : Llair.jump) =
  let state, _ = Domain.call state args dst.params dst.locals in
  let stk = push_jump dst.locals stk in
  Work.add ~prev:block ~retreating stk state dst

let push_call locals ~return from_call ?throw stk =
  let push_return jmp from_call stk = Return (jmp, from_call, stk) in
  let push_throw jmp stk =
    match jmp with Some jmp -> Throw (jmp, stk) | None -> stk
  in
  push_jump locals (push_return return from_call (push_throw throw stk))

let exec_call stk state block ({dst; args; retreating} : Llair.jump) return
    throw =
  let state, from_call = Domain.call state args dst.params dst.locals in
  let stk = push_call dst.locals ~return from_call ?throw stk in
  Work.add stk ~prev:block ~retreating state dst

let pop_return stk ~init ~f =
  let rec pop_return_ scope = function
    | Locals (locals, stk) -> pop_return_ (Set.union locals scope) stk
    | Return (jmp, from_call, stk) -> Some (stk, f scope from_call init, jmp)
    | _ -> None
  in
  pop_return_ Var.Set.empty stk

let exec_return stk state block exp =
  match pop_return stk ~init:state ~f:Domain.retn with
  | Some (stk, state, ({args} as jmp)) ->
      exec_jump stk state block {jmp with args= Option.cons exp args}
  | None -> Work.skip

let rec pop_throw stk ~init ~f =
  match pop_return stk ~init ~f with
  | Some (stk, state, _) -> pop_throw stk ~init:state ~f
  | None -> (
    match stk with Throw (jmp, stk) -> Some (stk, init, jmp) | _ -> None )

let exec_throw stk state block exc =
  match pop_throw stk ~init:state ~f:Domain.retn with
  | Some (stk, state, ({args} as jmp)) ->
      exec_jump stk state block {jmp with args= exc :: args}
  | None -> Work.skip

let exec_skip_func :
    stack -> Domain.t -> Llair.block -> Llair.jump -> Work.x =
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

let exec_term : Llair.t -> stack -> Domain.t -> Llair.block -> Work.x =
 fun pgm stk state block ->
  [%Trace.info "exec %a" Llair.Term.pp block.term] ;
  match block.term with
  | Switch {key; tbl; els} ->
      Vector.fold tbl
        ~f:(fun x (case, jump) ->
          match Domain.assume state (Exp.eq key (Exp.integer case)) with
          | Some state -> exec_jump stk state block jump |> Work.seq x
          | None -> x )
        ~init:
          ( match
              Domain.assume state
                (Vector.fold tbl ~init:(Exp.bool true)
                   ~f:(fun b (case, _) ->
                     Exp.and_ (Exp.dq key (Exp.integer case)) b ))
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
            ( if Llair.Func.is_undefined callee then
              exec_skip_func stk state block return
            else
              exec_call stk state block
                {dst= callee.entry; args; retreating}
                return throw )
            |> Work.seq x ) )
  | Return {exp} -> exec_return stk state block exp
  | Throw {exc} -> exec_throw stk state block exc
  | Unreachable -> Work.skip

let exec_block : Llair.t -> stack -> Domain.t -> Llair.block -> Work.x =
 fun pgm stk state block ->
  [%Trace.info "exec %a" Llair.Block.pp block] ;
  match Vector.fold_result ~f:Domain.exec_inst ~init:state block.cmnd with
  | Ok state -> exec_term pgm stk state block
  | Error (q, i) -> Report.invalid_access i q ; Work.skip

let harness : Llair.t -> Work.t option =
 fun pgm ->
  List.find_map ["__llair_main"; "_Z12__llair_mainv"; "main"]
    ~f:(fun name ->
      Vector.find_map pgm.functions ~f:(fun func ->
          let fname = Var.name func.name.var in
          Option.some_if (String.equal name fname) (fname, func) ) )
  |> function
  | Some (("__llair_main" | "_Z12__llair_mainv" | "main"), main) ->
      let block = main.entry in
      if List.is_empty block.params then
        Some
          (Work.init
             (fst (Domain.call (Domain.init pgm.globals) [] [] block.locals))
             block)
      else None
  | _ -> None

let exec_pgm : Llair.t -> unit =
 fun pgm ->
  [%Trace.call fun {pf} -> pf "@]@,@["]
  ;
  ( match harness pgm with
  | Some work -> Work.run ~f:(exec_block pgm) work
  | None -> fail "no applicable harness" () )
  |>
  [%Trace.retn fun {pf} _ -> pf ""]
