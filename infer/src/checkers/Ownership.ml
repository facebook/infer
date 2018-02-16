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

type permission =
  | TransferOwnership
      (** permission to permanently transfer ownership (e.g. call a destructor, delete, or free) *)
  | Read  (** permission to read *)

module Permission = struct
  type astate = permission

  let ( <= ) ~lhs ~rhs =
    match (lhs, rhs) with
    | Read, TransferOwnership ->
        true
    | Read, Read | TransferOwnership, TransferOwnership ->
        true
    | TransferOwnership, Read ->
        false


  let join astate1 astate2 =
    match (astate1, astate2) with
    | Read, Read ->
        Read
    | TransferOwnership, _ | _, TransferOwnership ->
        TransferOwnership


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | TransferOwnership ->
        F.fprintf fmt "TransferOwnership"
    | Read ->
        F.fprintf fmt "Read"
end

(** map from variable to permission required based on the way the variable is used. for example, if
    we see delete x, then x needs permission "TransferOwnership" *)
module Domain = struct
  include AbstractDomain.Map (Var) (Permission)

  let log_use_after_lifetime var loc summary =
    let message = F.asprintf "Variable %a is used after its lifetime has ended" Var.pp var in
    let ltr = [Errlog.make_trace_element 0 loc "Use of invalid variable" []] in
    let exn = Exceptions.Checkers (IssueType.use_after_lifetime, Localise.verbatim_desc message) in
    Reporting.log_error summary ~loc ~ltr exn


  (* don't allow strong updates via add; only remove *)
  let add var new_permission loc summary astate =
    let permission =
      try
        let old_permission = find var astate in
        ( match (old_permission, new_permission) with
        | TransferOwnership, (Read | TransferOwnership) ->
            log_use_after_lifetime var loc summary
        | _ ->
            () ) ;
        Permission.join new_permission old_permission
      with Not_found -> new_permission
    in
    add var permission astate


  let access_path_add_read ((base_var, _), _) loc summary astate =
    add base_var Read loc summary astate


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
    (* TODO: support delete[], free, and destructors *)
    Typ.Procname.equal pname BuiltinDecl.__delete


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
        Domain.add lhs_var TransferOwnership loc summary astate
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
        Domain.actuals_add_reads actuals loc summary astate |> Domain.remove constructed_var
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
