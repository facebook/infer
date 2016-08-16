open! Utils

module F = Format
module L = Logging

module Domain = struct (* implementation of join, widen, <=, pp *)
  type astate = { in_catch : bool ; seen_call : bool; }
  let is_bottom astate1 = false
  let initial = { in_catch = false; seen_call = false; }
  let (<=) ~lhs ~rhs = lhs == rhs
  let join astate1 astate2 =
    if (astate1.in_catch=true && astate2.in_catch=true)
    then { in_catch = true; seen_call = astate1.seen_call || astate2.seen_call }
    else { in_catch = false; seen_call = astate1.seen_call || astate2.seen_call }
  let widen ~prev ~next ~num_iters:_ = join prev next
  let pp fmt astate1 = F.fprintf fmt "(State: in_catch=%b, seen_call=%b)" astate1.in_catch astate1.seen_call
end

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = ProcData.no_extras

  let exec_instr astate proc_data node instr =
    let in_catch_pvar _pvar =
      if (string_is_prefix "CatchVar" (Pvar.to_string _pvar)) then true else false
    in
    let is_start_of_catch = function
      | Sil.Set (Exp.Lvar lhs_pvar, _, _, _) -> in_catch_pvar lhs_pvar
      | _ -> false in
    let is_maybe_end_of_catch = function
      | Sil.Abstract _ -> true
      | _ -> false in

    if (is_start_of_catch instr)
    then { Domain.in_catch = true; Domain.seen_call = false; }
    else { Domain.in_catch = false; Domain.seen_call = false; }
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Exceptional)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

let checker { Callbacks.proc_desc; tenv; } =
  ignore(Analyzer.exec_pdesc (ProcData.make_default proc_desc tenv))
