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

module Base = struct
  type t = AccessPath.base

  let compare = AccessPath.compare_base

  let pp = AccessPath.pp_base
end

module VarSet = AbstractDomain.FiniteSet (Base)

module CapabilityDomain = struct
  type astate =
    | InvalidatedAt of Location.t
        (** neither owned nor borrowed; we can't safely do anything with this value. tagged with the
        location where invalidation occurred. *)
    | BorrowedFrom of VarSet.astate
        (** not owned, but borrowed from existing reference(s). for now, permits both reads and writes *)
    | Owned
        (** owned. permits reads, writes, and ownership transfer (e.g. call a destructor, delete, or free) *)

  let make_borrowed_vars vars =
    assert (not (VarSet.is_empty vars)) ;
    BorrowedFrom vars


  (** Owned <= BorrowedFrom <= InvalidatedAt *)
  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | InvalidatedAt loc1, InvalidatedAt loc2 ->
          Location.compare loc1 loc2 <= 0
      | _, InvalidatedAt _ ->
          true
      | InvalidatedAt _, _ ->
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
      | InvalidatedAt loc1, InvalidatedAt loc2 ->
          (* pick the "higher" program point that occurs syntactically later *)
          if Location.compare loc1 loc2 >= 0 then astate1 else astate2
      | (InvalidatedAt _ as invalid), _ | _, (InvalidatedAt _ as invalid) ->
          invalid


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | InvalidatedAt loc ->
        F.fprintf fmt "InvalidatedAt(%a)" Location.pp loc
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
  include AbstractDomain.Map (Base) (CapabilityDomain)

  let report_use_after_lifetime (var, _) ~use_loc ~invalidated_loc summary =
    let message =
      F.asprintf "Variable %a is used at line %a after its lifetime ended at %a" Var.pp var
        Location.pp use_loc Location.pp invalidated_loc
    in
    let ltr =
      [ Errlog.make_trace_element 0 invalidated_loc "End of variable lifetime" []
      ; Errlog.make_trace_element 0 use_loc "Use of invalid variable" [] ]
    in
    let exn = Exceptions.Checkers (IssueType.use_after_lifetime, Localise.verbatim_desc message) in
    Reporting.log_error summary ~loc:use_loc ~ltr exn


  (* complain if we do not have the right capability to access [var] *)
  let check_var_lifetime base_var use_loc summary astate =
    let open CapabilityDomain in
    match find base_var astate with
    | InvalidatedAt invalidated_loc ->
        report_use_after_lifetime base_var ~use_loc ~invalidated_loc summary
    | BorrowedFrom borrowed_vars ->
        (* warn if any of the borrowed vars are Invalid *)
        let report_invalidated v =
          match find v astate with
          | InvalidatedAt invalidated_loc ->
              report_use_after_lifetime base_var ~use_loc ~invalidated_loc summary
          | BorrowedFrom _
          (* TODO: can do deeper checking here, but have to worry about borrow cycles *)
          | Owned ->
              ()
          | exception Not_found ->
              ()
        in
        VarSet.iter report_invalidated borrowed_vars
    | Owned ->
        ()
    | exception Not_found ->
        ()


  let base_add_read ((_, typ) as base_var) loc summary astate =
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


  let borrow_vars lhs_base rhs_vars astate =
    (* borrow all of the vars read on the rhs *)
    if VarSet.is_empty rhs_vars then remove lhs_base astate
    else add lhs_base (CapabilityDomain.make_borrowed_vars rhs_vars) astate


  (* handle assigning directly to a base var *)
  let handle_var_assign lhs_base rhs_exp loc summary astate =
    if Var.is_return (fst lhs_base) then exp_add_reads rhs_exp loc summary astate
    else
      match rhs_exp with
      | HilExp.AccessExpression (Base rhs_base | AddressOf Base rhs_base)
        when not (Var.appears_in_source_code (fst rhs_base)) -> (
        try
          (* assume assignments with non-source vars on the RHS transfer capabilities to the LHS
             var *)
          (* copy capability from RHS to LHS *)
          let base_capability = find rhs_base astate in
          add lhs_base base_capability astate
        with Not_found ->
          (* no existing capability on RHS. don't make any assumptions about LHS capability *)
          remove lhs_base astate )
      | HilExp.Closure (_, captured_vars) ->
          (* TODO: can be folded into the case above once we have proper AccessExpressions *)
          let vars_captured_by_ref =
            List.fold captured_vars
              ~f:(fun acc (((_, typ) as base), _) ->
                match typ.Typ.desc with Typ.Tptr _ -> VarSet.add base acc | _ -> acc )
              ~init:VarSet.empty
          in
          borrow_vars lhs_base vars_captured_by_ref astate
      | _ ->
          (* TODO: support capability transfer between source vars, other assignment modes *)
          exp_add_reads rhs_exp loc summary astate |> remove lhs_base


  let release_ownership base loc summary astate =
    base_add_read base loc summary astate |> add base (CapabilityDomain.InvalidatedAt loc)
end

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = Specs.summary

  let is_aggregate (_, typ) =
    match typ.Typ.desc with
    | Tstruct _ ->
        (* TODO: we could compute this precisely by recursively checking all of the fields of the
           type, but that's going to be expensive. will add it to the frontend instead *)
        true
    | _ ->
        false


  let get_assigned_base (access_expression: AccessExpression.t) =
    match access_expression with
    | Base base ->
        Some base
    | _ ->
        let base = AccessExpression.get_base access_expression in
        (* assume assigning to any field of an aggregate struct re-initalizes the struct *)
        Option.some_if (is_aggregate base) base


  let acquire_ownership call_exp return_opt actuals loc summary astate =
    match call_exp with
    | HilInstr.Direct pname ->
        (* TODO: support new[], malloc, others? *)
        if Typ.Procname.equal pname BuiltinDecl.__new then
          let astate' = Domain.actuals_add_reads actuals loc summary astate in
          match return_opt with
          | Some return_base ->
              Some (Domain.add return_base CapabilityDomain.Owned astate')
          | None ->
              Some astate'
        else if Typ.Procname.equal pname BuiltinDecl.__placement_new then
          match (List.rev actuals, return_opt) with
          | (HilExp.AccessExpression Base placement_base) :: other_actuals, Some return_base ->
              (* placement new creates an alias between return var and placement var. model as
                 return borrowing from placement *)
              Domain.actuals_add_reads other_actuals loc summary astate
              |> Domain.add placement_base CapabilityDomain.Owned
              |> Domain.borrow_vars return_base (VarSet.singleton placement_base) |> Option.some
          | _ :: other_actuals, Some return_base ->
              Domain.actuals_add_reads other_actuals loc summary astate
              |> Domain.add return_base CapabilityDomain.Owned |> Option.some
          | _ ->
              L.die InternalError "Placement new without placement or return in %a %a"
                Typ.Procname.pp pname Location.pp loc
        else if Typ.Procname.is_constructor pname then
          match actuals with
          | (HilExp.AccessExpression AccessExpression.AddressOf access_expression) :: other_actuals
                -> (
            match get_assigned_base access_expression with
            | Some constructed_base ->
                let astate' = Domain.actuals_add_reads other_actuals loc summary astate in
                Some (Domain.add constructed_base CapabilityDomain.Owned astate')
            | None ->
                Some astate )
          | _ ->
              Some astate
        else None
    | HilInstr.Indirect _ ->
        None


  let is_destructor = function
    | Typ.Procname.ObjC_Cpp clang_pname ->
        Typ.Procname.ObjC_Cpp.is_destructor clang_pname
        && not
             (* Our frontend generates synthetic inner destructors to model invocation of base class
             destructors correctly; see D5834555/D7189239 *)
             (Typ.Procname.ObjC_Cpp.is_inner_destructor clang_pname)
    | _ ->
        false


  let is_early_return call_exp =
    match call_exp with
    | HilInstr.Direct pname ->
        Typ.Procname.equal pname BuiltinDecl.exit
        || Typ.Procname.equal pname BuiltinDecl.objc_cpp_throw
        || Typ.Procname.equal pname BuiltinDecl.abort
    | _ ->
        false


  let exec_instr (astate: Domain.astate) (proc_data: extras ProcData.t) _ (instr: HilInstr.t) =
    let summary = proc_data.extras in
    match instr with
    | Assign (lhs_access_exp, rhs_exp, loc) -> (
      match get_assigned_base lhs_access_exp with
      | Some lhs_base ->
          Domain.handle_var_assign lhs_base rhs_exp loc summary astate
      | None ->
          (* assign to field, array, indirectly with &/*, or a combination *)
          Domain.exp_add_reads rhs_exp loc summary astate
          |> Domain.access_path_add_read
               (AccessExpression.to_access_path lhs_access_exp)
               loc summary )
    | Call (_, Direct callee_pname, [(AccessExpression Base lhs_base)], _, loc)
      when Typ.Procname.equal callee_pname BuiltinDecl.__delete ->
        (* TODO: support delete[], free, and (in some cases) std::move *)
        Domain.release_ownership lhs_base loc summary astate
    | Call (_, Direct callee_pname, [(AccessExpression AddressOf Base lhs_base)], _, loc)
      when is_destructor callee_pname ->
        Domain.release_ownership lhs_base loc summary astate
    | Call
        ( _
        , Direct Typ.Procname.ObjC_Cpp callee_pname
        , [(AccessExpression AddressOf Base lhs_base); rhs_exp]
        , _
        , loc )
      when Typ.Procname.ObjC_Cpp.is_operator_equal callee_pname ->
        (* TODO: once we go interprocedural, this case should only apply for operator='s with an
           empty summary *)
        Domain.handle_var_assign lhs_base rhs_exp loc summary astate
    | Call
        ( _
        , Direct Typ.Procname.ObjC_Cpp callee_pname
        , (AccessExpression AddressOf Base lhs_base) :: _
        , _
        , loc )
      when Typ.Procname.ObjC_Cpp.is_cpp_lambda callee_pname ->
        (* invoking a lambda; check that it's captured vars are valid *)
        Domain.check_var_lifetime lhs_base loc summary astate ;
        astate
    | Call (ret_opt, call_exp, actuals, _, loc) -> (
      match acquire_ownership call_exp ret_opt actuals loc summary astate with
      | Some astate' ->
          astate'
      | None ->
          if is_early_return call_exp then
            (* thrown exception, abort(), or exit; return _|_ *)
            Domain.empty
          else
            let astate' = Domain.actuals_add_reads actuals loc summary astate in
            match ret_opt with Some base -> Domain.remove base astate' | None -> astate' )
    | Assume (assume_exp, _, _, loc) ->
        Domain.exp_add_reads assume_exp loc summary astate


  let pp_session_name _node fmt = F.pp_print_string fmt "ownership"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Exceptional) (TransferFunctions)

let checker {Callbacks.proc_desc; tenv; summary} =
  let proc_data = ProcData.make proc_desc tenv summary in
  let initial = Domain.empty in
  ignore (Analyzer.compute_post proc_data ~initial) ;
  summary
