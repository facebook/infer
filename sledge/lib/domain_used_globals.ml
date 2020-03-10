(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Used-globals abstract domain *)

type t = Reg.Set.t [@@deriving equal, sexp_of]

let pp = Set.pp Reg.pp
let report_fmt_thunk = Fn.flip pp
let empty = Reg.Set.empty

let init globals =
  [%Trace.info "pgm globals: {%a}" (Vector.pp ", " Global.pp) globals] ;
  empty

let join l r = Some (Set.union l r)
let recursion_beyond_bound = `skip
let is_false _ = false
let post _ _ state = state
let retn _ _ from_call post = Set.union from_call post
let dnf t = [t]
let add_if_global gs v = if Var.global (Reg.var v) then Set.add gs v else gs

let used_globals ?(init = empty) exp =
  Exp.fold_regs exp ~init ~f:add_if_global

let exec_assume st exp = Some (used_globals ~init:st exp)
let exec_kill st _ = st

let exec_move st reg_exps =
  Vector.fold reg_exps ~init:st ~f:(fun st (_, rhs) ->
      used_globals ~init:st rhs )

let exec_inst st inst =
  [%Trace.call fun {pf} -> pf "pre:{%a} %a" pp st Llair.Inst.pp inst]
  ;
  Some
    (Llair.Inst.fold_exps inst ~init:st ~f:(fun acc e ->
         used_globals ~init:acc e ))
  |>
  [%Trace.retn fun {pf} ->
    Option.iter ~f:(fun uses -> pf "post:{%a}" pp uses)]

let exec_intrinsic ~skip_throw:_ st _ intrinsic actuals =
  let name = Reg.name intrinsic in
  if
    List.exists
      [ "malloc"; "aligned_alloc"; "calloc"; "posix_memalign"; "realloc"
      ; "mallocx"; "rallocx"; "xallocx"; "sallocx"; "dallocx"; "sdallocx"
      ; "nallocx"; "malloc_usable_size"; "mallctl"; "mallctlnametomib"
      ; "mallctlbymib"; "malloc_stats_print"; "strlen"
      ; "__cxa_allocate_exception"; "_ZN5folly13usingJEMallocEv" ]
      ~f:(String.equal name)
  then
    List.fold actuals ~init:st ~f:(fun s a -> used_globals ~init:s a)
    |> fun res -> Some (Some res)
  else None

type from_call = t [@@deriving sexp_of]

(* Set abstract state to bottom (i.e. empty set) at function entry *)
let call ~summaries:_ ~globals:_ ~actuals ~areturn:_ ~formals:_ ~freturn:_
    ~locals:_ st =
  (empty, List.fold actuals ~init:st ~f:(fun s a -> used_globals ~init:s a))

let resolve_callee lookup ptr st =
  let st = used_globals ~init:st ptr in
  match Reg.of_exp ptr with
  | Some callee -> (lookup (Reg.name callee), st)
  | None -> ([], st)

(* A function summary is the set of global registers accessed by that
   function and its transitive callees *)
type summary = t

let pp_summary = pp
let create_summary ~locals:_ ~formals:_ state = (state, state)
let apply_summary st summ = Some (Set.union st summ)

(** Query *)

type r = Per_function of Reg.Set.t Reg.Map.t | Declared of Reg.Set.t

let by_function : r -> Reg.t -> t =
 fun s fn ->
  [%Trace.call fun {pf} -> pf "%a" Reg.pp fn]
  ;
  ( match s with
  | Declared set -> set
  | Per_function map -> (
    match Map.find map fn with
    | Some gs -> gs
    | None ->
        fail
          "main analysis reached function %a that was not reached by \
           used-globals pre-analysis "
          Reg.pp fn () ) )
  |>
  [%Trace.retn fun {pf} r -> pf "%a" Reg.Set.pp r]
