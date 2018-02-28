(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging
module VarSet = AbstractDomain.FiniteSet (Var)

module CapabilityDomain = struct
  type astate =
    | Invalid  (** neither owned nor borrowed; we can't do anything with this value *)
    | BorrowedFrom of VarSet.astate
        (** not owned, but borrowed from existing reference(s). for now, permits both reads and writes *)
    | Owned
        (** owned. permits reads, writes, and ownership transfer (e.g. call a destructor, delete, or free) *)

  (** Owned <= BorrowedFrom <= Invalid *)
  let ( <= ) ~lhs ~rhs =
    match (lhs, rhs) with
    | _, Invalid ->
        true
    | Invalid, _ ->
        false
    | BorrowedFrom s1, BorrowedFrom s2 ->
        VarSet.( <= ) ~lhs:s1 ~rhs:s2
    | Owned, _ ->
        true
    | _, Owned ->
        false


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      match (astate1, astate2) with
      | BorrowedFrom s1, BorrowedFrom s2 ->
          BorrowedFrom (VarSet.union s1 s2)
      | Owned, astate | astate, Owned ->
          astate
      | Invalid, _ | _, Invalid ->
          Invalid


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | Invalid ->
        F.fprintf fmt "Invalid"
    | BorrowedFrom vars ->
        F.fprintf fmt "BorrowedFrom(%a)" VarSet.pp vars
    | Owned ->
        F.fprintf fmt "Owned"
end

(** map from program variable to its capability *)
module Domain = struct
  include AbstractDomain.Map (Var) (CapabilityDomain)

  let report_use_after_lifetime var loc summary =
    let message =
      F.asprintf "Variable %a is used after its lifetime has ended at %a" Var.pp var Location.pp
        loc
    in
    let ltr = [Errlog.make_trace_element 0 loc "Use of invalid variable" []] in
    let exn = Exceptions.Checkers (IssueType.use_after_lifetime, Localise.verbatim_desc message) in
    Reporting.log_error summary ~loc ~ltr exn


  (* complain if we do not have the right capability to access [var] *)
  let check_var_lifetime var loc summary astate =
    let open CapabilityDomain in
    match find var astate with
    | Invalid ->
        report_use_after_lifetime var loc summary
    | BorrowedFrom borrowed_vars ->
        (* warn if any of the borrowed vars are Invalid *)
        let is_invalid v =
          match find v astate with
          | Invalid ->
              true
          | BorrowedFrom _
          (* TODO: can do deeper checking here, but have to worry about borrow cycles *)
          | Owned ->
              false
          | exception Not_found ->
              false
        in
        if VarSet.exists is_invalid borrowed_vars then report_use_after_lifetime var loc summary
    | Owned ->
        ()
    | exception Not_found ->
        ()


  let access_path_add_read ((base_var, _), _) loc summary astate =
    check_var_lifetime base_var loc summary astate ;
    astate


  let exp_add_reads exp loc summary astate =
    List.fold
      ~f:(fun acc access_path -> access_path_add_read access_path loc summary acc)
      (HilExp.get_access_paths exp) ~init:astate


  let actuals_add_reads actuals loc summary astate =
    (* TODO: handle reads in actuals + return values properly. This is nontrivial because the
       frontend sometimes chooses to translate return as pass-by-ref on a dummy actual *)
    List.fold actuals
      ~f:(fun acc actual_exp -> exp_add_reads actual_exp loc summary acc)
      ~init:astate


  (* handle assigning directly to a base var *)
  let handle_var_assign lhs_var rhs_exp loc summary astate =
    exp_add_reads rhs_exp loc summary astate |> remove lhs_var
end

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = Specs.summary

  let transfers_ownership pname =
    (* TODO: support delete[], free, and (in some cases) std::move *)
    Typ.Procname.equal pname BuiltinDecl.__delete
    ||
    match pname with
    | Typ.Procname.ObjC_Cpp clang_pname ->
        Typ.Procname.ObjC_Cpp.is_destructor clang_pname
    | _ ->
        false


  let exec_instr (astate: Domain.astate) (proc_data: extras ProcData.t) _ (instr: HilInstr.t) =
    let summary = proc_data.extras in
    match instr with
    | Assign (Base (lhs_var, _), rhs_exp, loc) ->
        Domain.handle_var_assign lhs_var rhs_exp loc summary astate
    | Assign (lhs_access_exp, rhs_exp, loc) ->
        (* assign to field, array, indirectly with &/*, or a combination *)
        Domain.exp_add_reads rhs_exp loc summary astate
        |> Domain.access_path_add_read (AccessExpression.to_access_path lhs_access_exp) loc summary
    | Call (_, Direct callee_pname, [(AccessExpression Base (lhs_var, _))], _, loc)
      when transfers_ownership callee_pname ->
        Domain.check_var_lifetime lhs_var loc summary astate ;
        Domain.add lhs_var CapabilityDomain.Invalid astate
    | Call (_, Direct callee_pname, [(AccessExpression Base (lhs_var, _)); rhs_exp], _, loc)
      when String.equal (Typ.Procname.get_method callee_pname) "operator=" ->
        Domain.handle_var_assign lhs_var rhs_exp loc summary astate
    | Call (None, Direct callee_pname, lhs_exp :: actuals, _, loc)
      when Typ.Procname.is_constructor callee_pname ->
        (* frontend translates constructors as assigning-by-ref to first actual *)
        let constructed_var =
          match lhs_exp with
          | AccessExpression Base (var, _) ->
              var
          | _ ->
              L.die InternalError "Unexpected: constructor called on %a" HilExp.pp lhs_exp
        in
        Domain.actuals_add_reads actuals loc summary astate
        |> Domain.add constructed_var CapabilityDomain.Owned
    | Call (ret_opt, _, actuals, _, loc)
      -> (
        let astate' = Domain.actuals_add_reads actuals loc summary astate in
        match ret_opt with Some (base_var, _) -> Domain.remove base_var astate' | None -> astate' )
    | Assume (assume_exp, _, _, loc) ->
        Domain.exp_add_reads assume_exp loc summary astate
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Exceptional) (TransferFunctions)

let checker {Callbacks.proc_desc; tenv; summary} =
  let proc_data = ProcData.make proc_desc tenv summary in
  let initial = Domain.empty in
  ignore (Analyzer.compute_post proc_data ~initial) ;
  summary
