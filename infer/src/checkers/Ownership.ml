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

  let make_borrowed_vars vars =
    assert (not (VarSet.is_empty vars)) ;
    BorrowedFrom vars


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

let rec is_function_typ = function
  | {Typ.desc= Tptr (t, _)} ->
      is_function_typ t
  | {Typ.desc= Tstruct typename} ->
      String.is_prefix ~prefix:"std::function" (Typ.Name.name typename)
  | _ ->
      false


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


  let base_add_read (base_var, typ) loc summary astate =
    (* don't warn if it's a read of a std::function type. we model closures as borrowing their
       captured vars, but simply reading a closure doesn't actually use these vars. this means that
       we'll miss bugs involving invalid uses of std::function's, but that seems ok *)
    if not (is_function_typ typ) then check_var_lifetime base_var loc summary astate ;
    astate


  let access_path_add_read (base, _) loc summary astate = base_add_read base loc summary astate

  let exp_add_reads exp loc summary astate =
    List.fold
      ~f:(fun acc access_expr ->
        access_path_add_read (AccessExpression.to_access_path access_expr) loc summary acc )
      (HilExp.get_access_exprs exp) ~init:astate


  let actuals_add_reads actuals loc summary astate =
    (* TODO: handle reads in actuals + return values properly. This is nontrivial because the
       frontend sometimes chooses to translate return as pass-by-ref on a dummy actual *)
    List.fold actuals
      ~f:(fun acc actual_exp -> exp_add_reads actual_exp loc summary acc)
      ~init:astate


  let borrow_vars lhs_var rhs_vars astate =
    (* borrow all of the vars read on the rhs *)
    if VarSet.is_empty rhs_vars then remove lhs_var astate
    else add lhs_var (CapabilityDomain.make_borrowed_vars rhs_vars) astate


  let borrow_exp lhs_var rhs_exp astate =
    let rhs_vars =
      List.fold (HilExp.get_access_exprs rhs_exp)
        ~f:(fun acc access_expr ->
          let (var, _), _ = AccessExpression.to_access_path access_expr in
          VarSet.add var acc )
        ~init:VarSet.empty
    in
    borrow_vars lhs_var rhs_vars astate


  (* handle assigning directly to a base var *)
  let handle_var_assign lhs_var rhs_exp loc summary astate =
    if Var.is_return lhs_var then exp_add_reads rhs_exp loc summary astate
    else
      match rhs_exp with
      | HilExp.AccessExpression AccessExpression.AddressOf address_taken_exp ->
          borrow_exp lhs_var (AccessExpression address_taken_exp) astate
      | HilExp.AccessExpression AccessExpression.Base (rhs_var, _)
        when not (Var.appears_in_source_code rhs_var) -> (
        try
          (* assume assignments with non-source vars on the RHS transfer capabilities to the LHS
             var *)
          (* copy capability from RHS to LHS *)
          let base_capability = find rhs_var astate in
          add lhs_var base_capability astate
        with Not_found ->
          (* no existing capability on RHS. don't make any assumptions about LHS capability *)
          remove lhs_var astate )
      | HilExp.AccessExpression AccessExpression.Base _ ->
          borrow_exp lhs_var rhs_exp astate
      | HilExp.Closure (_, captured_vars) ->
          (* TODO: can be folded into the case above once we have proper AccessExpressions *)
          let vars_captured_by_ref =
            List.fold captured_vars
              ~f:(fun acc ((var, typ), _) ->
                match typ.Typ.desc with Typ.Tptr _ -> VarSet.add var acc | _ -> acc )
              ~init:VarSet.empty
          in
          borrow_vars lhs_var vars_captured_by_ref astate
      | _ ->
          (* TODO: support capability transfer between source vars, other assignment modes *)
          exp_add_reads rhs_exp loc summary astate |> remove lhs_var
end

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = Specs.summary

  let acquires_ownership pname =
    (* TODO: support new[], malloc, others? *)
    Typ.Procname.equal pname BuiltinDecl.__new


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
    | Call (Some (lhs_var, _), Direct callee_pname, actuals, _, loc)
      when acquires_ownership callee_pname ->
        Domain.actuals_add_reads actuals loc summary astate
        |> Domain.add lhs_var CapabilityDomain.Owned
    | Call (None, Direct callee_pname, lhs_exp :: actuals, _, loc)
      when Typ.Procname.is_constructor callee_pname
      -> (
        (* similar to acquires ownership case *)
        let astate' = Domain.actuals_add_reads actuals loc summary astate in
        (* frontend translates constructors as assigning-by-ref to first actual *)
        match lhs_exp with
        | AccessExpression
            ( AccessExpression.AddressOf Base (constructed_var, _)
            | Base (constructed_var, _)
            (* TODO: get rid of second case once AccessExpression conversion is complete *) ) ->
            Domain.add constructed_var CapabilityDomain.Owned astate'
        | _ ->
            astate' )
    | Call (_, Direct callee_pname, [(AccessExpression Base ((lhs_var, _) as lhs_base))], _, loc)
      when transfers_ownership callee_pname ->
        Domain.base_add_read lhs_base loc summary astate
        |> Domain.add lhs_var CapabilityDomain.Invalid
    | Call
        ( _
        , Direct Typ.Procname.ObjC_Cpp callee_pname
        , [(AccessExpression Base (lhs_var, _)); rhs_exp]
        , _
        , loc )
      when Typ.Procname.ObjC_Cpp.is_operator_equal callee_pname ->
        (* TODO: once we go interprocedural, this case should only apply for operator='s with an
           empty summary *)
        Domain.handle_var_assign lhs_var rhs_exp loc summary astate
    | Call
        ( _
        , Direct Typ.Procname.ObjC_Cpp callee_pname
        , (AccessExpression Base (lhs_var, _)) :: _
        , _
        , loc )
      when Typ.Procname.ObjC_Cpp.is_cpp_lambda callee_pname ->
        (* invoking a lambda; check that it's captured vars are valid *)
        Domain.check_var_lifetime lhs_var loc summary astate ;
        astate
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
