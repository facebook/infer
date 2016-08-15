open! Utils

module F = Format
module L = Logging

(** backward analysis for computing set of maybe-live variables at each program point *)

module Domain = AbstractDomain.FiniteSet(Var.Set)

(* Check if current instruction is within a catch or not *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = ProcData.no_extras

  (* let in_catch astate =
       state -> within_catch = true
     let not_in_catch astate =
       state -> within_catch = false
     RETURN: astate' -> new state after performing changes on state
     *)

  let exec_instr astate proc_data node instr =
    (* if instr is a start or end of catch, modify state else do nothing *)

    let is_catch_pvar _pvar : PVar.t =
      (* Check if name of current Program Variable is "Catch" *)
      (* Note: This is a slightly hacky way  of checking *)
      if (_pvar Pvar.to_string = "Catch") then true else false
    in

    let is_start_of_catch = function
      | Sil.Set (Exp.Lvar lhs_pvar, _, _, _) -> is_catch_pvar lhs_pvar
      | _ -> false in

    let is_maybe_end_of_catch = function
      | Sil.Abstract _ -> true
      | _ -> false in

    astate

    (*
      Core logic:
      If start_of_catch is encountered, change state and return new state
      Else if end_of_catch is encountered, change state and return new state
      Else return same state
      | is_start_of_catch ->
      | is_maybe_end_of_catch ->
      | _ ->
    *)

    

end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Backward(ProcCfg.Exceptional))
    (Scheduler.ReversePostorder)
    (TransferFunctions)

let checker { Callbacks.proc_desc; tenv; } =
  ignore(Analyzer.exec_pdesc (ProcData.make_default proc_desc tenv))
