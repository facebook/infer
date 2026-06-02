(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Function-wide pre-pass that classifies formal-parameter [Reg] [Load]s by whether they look like
    the swift_refcounted metadata-extract idiom: [%m = load ptr, ptr %X; store ptr %m, ptr %slot]
    where [%X] is a formal and [%m]'s only uses are as the value operand of Stores.

    [Llair2Textual] consults the resulting set to decide whether to emit the extra deref that
    distinguishes "read bytes at [*X]" (metadata-extract, the LLVM semantic) from "read the formal
    value back" (the Textual slot convention's default, used by many ordinary swiftc lowerings).

    Function-wide because [swift_allocObject] sits between Load and Store as a Llair terminator,
    returning into a separate block — block-local scope would miss the pair. *)

open! IStd
module ProcState = Llair2TextualState.ProcState
module Var = Llair2TextualVar
module IntSet = Stdlib.Set.Make (Int)

type t = IntSet.t

(* DFS over [func]'s CFG, collecting every reachable block. We can't just walk
   [func.cfg] as a flat list because some Llair representations exclude the
   entry block from it; chasing terminators is the safe shape. *)
let collect_blocks func =
  let seen = Stdlib.Hashtbl.create 16 in
  let rec walk (block : Llair.block) acc =
    if Stdlib.Hashtbl.mem seen block.lbl then acc
    else (
      Stdlib.Hashtbl.add seen block.lbl () ;
      let acc = block :: acc in
      let jumps =
        match block.term with
        | Switch {tbl; els; _} ->
            els :: List.map (StdUtils.iarray_to_list tbl) ~f:snd
        | Iswitch {tbl; _} ->
            StdUtils.iarray_to_list tbl
        | Call {return; throw; _} ->
            return :: Option.to_list throw
        | Return _ | Throw _ | Abort _ | Unreachable _ ->
            []
      in
      List.fold jumps ~init:acc ~f:(fun acc (jump : Llair.jump) -> walk jump.dst acc) )
  in
  walk func.Llair.entry []


let classify ~(proc_state : ProcState.t) func =
  let blocks = collect_blocks func in
  let all_insts = List.concat_map blocks ~f:(fun b -> StdUtils.iarray_to_list b.Llair.cmnd) in
  (* Step 1: candidates are Loads whose [ptr] is a formal-Reg. *)
  let candidate_set =
    List.fold all_insts ~init:IntSet.empty ~f:(fun acc inst ->
        match inst with
        | Llair.Load {reg; ptr= Llair.Exp.Reg {name; id; typ}; _} ->
            let var_name = Var.reg_to_var_name (Llair.Reg.mk typ id name) in
            if Textual.VarName.Map.mem var_name proc_state.ProcState.formals then
              IntSet.add (Llair.Reg.id reg) acc
            else acc
        | _ ->
            acc )
  in
  if IntSet.is_empty candidate_set then candidate_set
  else
    (* Step 2: fold over every inst (across all blocks) and every block's
       terminator, accumulating where each candidate is consumed: as a Store
       value, or anywhere else (Field base, GEP base, call arg, terminator,
       etc.). *)
    let candidates_in exp =
      Llair.Exp.fold_regs exp IntSet.empty ~f:(fun reg acc ->
          let i = Llair.Reg.id reg in
          if IntSet.mem i candidate_set then IntSet.add i acc else acc )
    in
    let add_in es acc =
      List.fold es ~init:acc ~f:(fun acc e -> IntSet.union acc (candidates_in e))
    in
    let from_inst (store_value, elsewhere) = function
      | Llair.Store {exp; ptr; _} ->
          (IntSet.union store_value (candidates_in exp), add_in [ptr] elsewhere)
      | Llair.Move {reg_exps; _} | Llair.MovePhi {reg_exps; _} ->
          let es = List.map (StdUtils.iarray_to_list reg_exps) ~f:snd in
          (store_value, add_in es elsewhere)
      | Llair.Load {ptr; _} | Llair.Free {ptr; _} ->
          (store_value, add_in [ptr] elsewhere)
      | Llair.AtomicRMW {ptr; exp; _} ->
          (store_value, add_in [ptr; exp] elsewhere)
      | Llair.AtomicCmpXchg {ptr; cmp; exp; _} ->
          (store_value, add_in [ptr; cmp; exp] elsewhere)
      | Llair.Alloc {num; _} ->
          (store_value, add_in [num] elsewhere)
      | Llair.Builtin {args; _} ->
          (store_value, add_in (StdUtils.iarray_to_list args) elsewhere)
      | Llair.Nondet _ ->
          (store_value, elsewhere)
    in
    let from_term (store_value, elsewhere) (b : Llair.block) =
      let es =
        match b.term with
        | Switch {key; tbl; _} ->
            key :: List.map (StdUtils.iarray_to_list tbl) ~f:fst
        | Iswitch {ptr; _} ->
            [ptr]
        | Call {actuals; _} ->
            StdUtils.iarray_to_list actuals
        | Return {exp= Some e; _} ->
            [e]
        | Return {exp= None; _} | Throw _ | Abort _ | Unreachable _ ->
            []
      in
      (store_value, add_in es elsewhere)
    in
    let init = (IntSet.empty, IntSet.empty) in
    let store_value, elsewhere = List.fold all_insts ~init ~f:from_inst in
    let store_value, elsewhere = List.fold blocks ~init:(store_value, elsewhere) ~f:from_term in
    IntSet.diff store_value elsewhere


let mem reg set = IntSet.mem (Llair.Reg.id reg) set

let is_empty = IntSet.is_empty

let empty = IntSet.empty
