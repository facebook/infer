open! Utils

module F = Format
module L = Logging

module Domain = struct (* implementation of join, widen, <=, pp *)
  type astate = { in_catch : bool ; seen_call : bool; }
  let is_bottom = false
  let initial = { in_catch = false; seen_call = false; }
  let (<=) ~lhs ~rhs = lhs == rhs
  let join astate1 astate2 =
    if (astate1.is_catch=true && astate2.is_catch=true)
    then { astate with is_catch = true; seen_call = astate1.seen_call || astate2.seen_call }
    else { astate with is_catch = false; seen_call = astate1.seen_call || astate2.seen_call }
  let widen ~prev ~next ~num_iters:_ = join prev next
  let pp fmt (astate1)= function
    F.printf fmt "(State: is_catch=%s, seen_call=%s)" (bool_of_string astate1.is_catch, bool_of_string astate1.seen_call)
end

module TransferFunctions (CFG : ProcCfg.S) = struct
  let exec_instr astate proc_data node instr =
    let is_catch_pvar _pvar =
      if (string_is_prefix "CatchVar" (Pvar.to_string _pvar)) then true else false
    in
    let is_start_of_catch = function
      | Sil.Set (Exp.Lvar lhs_pvar, _, _, _) -> is_catch_pvar lhs_pvar
      | _ -> false in
    let is_maybe_end_of_catch = function
      | Sil.Abstract _ -> true
      | _ -> false in

    if is_start_of_catch then {astate with is_catch=true;} else {astate with is_catch=astate.is_catch || false}
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Forward(ProcCfg.Exceptional))
    (TransferFunctions)

let checker { Callbacks.proc_desc; tenv; } =
  ignore(Analyzer.exec_pdesc (ProcData.make_default proc_desc tenv))
