(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Used-globals abstract domain *)

type t = Llair.Reg.Set.t [@@deriving equal, sexp]

let pp = Llair.Reg.Set.pp
let report_fmt_thunk = Fun.flip pp
let empty = Llair.Reg.Set.empty

let init globals =
  [%Trace.info "pgm globals: {%a}" (IArray.pp ", " Llair.Global.pp) globals] ;
  empty

let join l r = Some (Llair.Reg.Set.union l r)
let recursion_beyond_bound = `skip
let is_false _ = false
let post _ _ state = state
let retn _ _ from_call post = Llair.Reg.Set.union from_call post
let dnf t = [t]

let add_if_global v gs =
  if Llair.Reg.is_global v then Llair.Reg.Set.add v gs else gs

let used_globals exp s = Llair.Exp.fold_regs ~f:add_if_global exp s
let exec_assume st exp = Some (used_globals exp st)
let exec_kill _ st = st

let exec_move reg_exps st =
  IArray.fold ~f:(fun (_, rhs) -> used_globals rhs) reg_exps st

let exec_inst inst st =
  [%Trace.call fun {pf} -> pf "pre:{%a} %a" pp st Llair.Inst.pp inst]
  ;
  Some (Llair.Inst.fold_exps ~f:used_globals inst st)
  |>
  [%Trace.retn fun {pf} ->
    Option.iter ~f:(fun uses -> pf "post:{%a}" pp uses)]

let exec_intrinsic ~skip_throw:_ _ intrinsic actuals st =
  let name = Llair.Reg.name intrinsic in
  if
    List.exists
      [ "malloc"
      ; "aligned_alloc"
      ; "calloc"
      ; "posix_memalign"
      ; "realloc"
      ; "mallocx"
      ; "rallocx"
      ; "xallocx"
      ; "sallocx"
      ; "dallocx"
      ; "sdallocx"
      ; "nallocx"
      ; "malloc_usable_size"
      ; "mallctl"
      ; "mallctlnametomib"
      ; "mallctlbymib"
      ; "malloc_stats_print"
      ; "strlen"
      ; "__cxa_allocate_exception"
      ; "_ZN5folly13usingJEMallocEv" ]
      ~f:(String.equal name)
  then List.fold ~f:used_globals actuals st |> fun res -> Some (Some res)
  else None

type from_call = t [@@deriving sexp]

(* Set abstract state to bottom (i.e. empty set) at function entry *)
let call ~summaries:_ ~globals:_ ~actuals ~areturn:_ ~formals:_ ~freturn:_
    ~locals:_ st =
  (empty, List.fold ~f:used_globals actuals st)

let resolve_callee lookup ptr st =
  let st = used_globals ptr st in
  match Llair.Reg.of_exp ptr with
  | Some callee -> (lookup (Llair.Reg.name callee), st)
  | None -> ([], st)

(* A function summary is the set of global registers accessed by that
   function and its transitive callees *)
type summary = t

let pp_summary = pp
let create_summary ~locals:_ ~formals:_ state = (state, state)
let apply_summary st summ = Some (Llair.Reg.Set.union st summ)

(** Query *)

type r =
  | Per_function of Llair.Reg.Set.t Llair.Reg.Map.t
  | Declared of Llair.Reg.Set.t

let by_function : r -> Llair.Reg.t -> t =
 fun s fn ->
  [%Trace.call fun {pf} -> pf "%a" Llair.Reg.pp fn]
  ;
  ( match s with
  | Declared set -> set
  | Per_function map -> (
    match Llair.Reg.Map.find fn map with
    | Some gs -> gs
    | None ->
        fail
          "main analysis reached function %a that was not reached by \
           used-globals pre-analysis "
          Llair.Reg.pp fn () ) )
  |>
  [%Trace.retn fun {pf} r -> pf "%a" Llair.Reg.Set.pp r]
