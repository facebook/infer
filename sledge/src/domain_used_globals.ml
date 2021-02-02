(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Used-globals abstract domain *)

type t = Llair.Global.Set.t [@@deriving compare, equal, sexp]

let pp = Llair.Global.Set.pp
let report_fmt_thunk = Fun.flip pp
let empty = Llair.Global.Set.empty

let init globals =
  [%Trace.info
    "pgm globals: {%a}" (IArray.pp ", " Llair.GlobalDefn.pp) globals] ;
  empty

let join l r = Some (Llair.Global.Set.union l r)
let recursion_beyond_bound = `skip
let post _ _ state = state
let retn _ _ from_call post = Llair.Global.Set.union from_call post
let dnf t = [t]

let used_globals exp s =
  Llair.Exp.fold_exps exp s ~f:(fun e s ->
      match Llair.Global.of_exp e with
      | Some g -> Llair.Global.Set.add g s
      | None -> s )

let exec_assume st exp = Some (used_globals exp st)
let exec_kill _ st = st

let exec_move reg_exps st =
  IArray.fold ~f:(fun (_, rhs) -> used_globals rhs) reg_exps st

let exec_inst inst st =
  [%Trace.call fun {pf} -> pf "@ pre:{%a} %a" pp st Llair.Inst.pp inst]
  ;
  Some (Llair.Inst.fold_exps ~f:used_globals inst st)
  |>
  [%Trace.retn fun {pf} ->
    Option.iter ~f:(fun uses -> pf "post:{%a}" pp uses)]

type from_call = t [@@deriving sexp]

(* Set abstract state to bottom (i.e. empty set) at function entry *)
let call ~summaries:_ ~globals:_ ~actuals ~areturn:_ ~formals:_ ~freturn:_
    ~locals:_ st =
  (empty, IArray.fold ~f:used_globals actuals st)

let resolve_callee _ _ _ = []

(* A function summary is the set of global registers accessed by that
   function and its transitive callees *)
type summary = t

let pp_summary = pp
let create_summary ~locals:_ ~formals:_ state = (state, state)
let apply_summary st summ = Some (Llair.Global.Set.union st summ)

(** Query *)

let by_function : Domain_intf.used_globals -> Llair.Function.t -> t =
 fun s fn ->
  [%Trace.call fun {pf} -> pf "@ %a" Llair.Function.pp fn]
  ;
  ( match s with
  | Declared set -> set
  | Per_function map -> (
    match Llair.Function.Map.find fn map with
    | Some gs -> gs
    | None ->
        fail
          "main analysis reached function %a that was not reached by \
           used-globals pre-analysis "
          Llair.Function.pp fn () ) )
  |>
  [%Trace.retn fun {pf} r -> pf "%a" Llair.Global.Set.pp r]
