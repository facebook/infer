(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface
open PulseDomainInterface

type t = AbductiveDomain.t

module Import = struct
  type access_mode = Read | Write | NoAccess

  type 'abductive_domain_t execution_domain_base_t = 'abductive_domain_t ExecutionDomain.base_t =
    | ContinueProgram of 'abductive_domain_t
    | ExitProgram of AbductiveDomain.summary
    | AbortProgram of AbductiveDomain.summary
    | LatentAbortProgram of {astate: AbductiveDomain.summary; latent_issue: LatentIssue.t}
    | LatentInvalidAccess of
        { astate: AbductiveDomain.summary
        ; address: AbstractValue.t
        ; must_be_valid: Trace.t
        ; calling_context: (CallEvent.t * Location.t) list }
    | ISLLatentMemoryError of AbductiveDomain.summary

  type 'astate base_error = 'astate AccessResult.error =
    | PotentialInvalidAccess of {astate: 'astate; address: AbstractValue.t; must_be_valid: Trace.t}
    | PotentialInvalidAccessSummary of
        {astate: AbductiveDomain.summary; address: AbstractValue.t; must_be_valid: Trace.t}
    | ReportableError of {astate: 'astate; diagnostic: Diagnostic.t}
    | ISLError of 'astate

  include IResult.Let_syntax

  let ( let<*> ) x f = match x with Error _ as err -> [err] | Ok y -> f y

  let ( let<+> ) x f =
    match x with Error _ as err -> [err] | Ok y -> [Ok (ExecutionDomain.ContinueProgram (f y))]
end

include Import

let check_addr_access access_mode location (address, history) astate =
  let access_trace = Trace.Immediate {location; history} in
  let* astate =
    AddressAttributes.check_valid access_trace address astate
    |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
           ReportableError
             { diagnostic=
                 Diagnostic.AccessToInvalidAddress
                   {calling_context= []; invalidation; invalidation_trace; access_trace}
             ; astate } )
  in
  match access_mode with
  | Read ->
      AddressAttributes.check_initialized access_trace address astate
      |> Result.map_error ~f:(fun () ->
             ReportableError
               { diagnostic=
                   Diagnostic.ReadUninitializedValue {calling_context= []; trace= access_trace}
               ; astate } )
  | Write ->
      Ok (AbductiveDomain.initialize address astate)
  | NoAccess ->
      Ok astate


let check_and_abduce_addr_access_isl access_mode location (address, history) ?(null_noop = false)
    astate =
  let access_trace = Trace.Immediate {location; history} in
  AddressAttributes.check_valid_isl access_trace address ~null_noop astate
  |> List.map ~f:(fun access_result ->
         let* astate = AccessResult.of_abductive_access_result access_trace access_result in
         match access_mode with
         | Read ->
             AddressAttributes.check_initialized access_trace address astate
             |> Result.map_error ~f:(fun () ->
                    ReportableError
                      { diagnostic=
                          Diagnostic.ReadUninitializedValue
                            {calling_context= []; trace= access_trace}
                      ; astate } )
         | Write ->
             Ok (AbductiveDomain.initialize address astate)
         | NoAccess ->
             Ok astate )


module Closures = struct
  module Memory = AbductiveDomain.Memory

  let fake_capture_field_prefix = "__capture_"

  let string_of_capture_mode = function
    | Pvar.ByReference ->
        "by_ref_"
    | Pvar.ByValue ->
        "by_value_"


  let fake_captured_by_ref_field_prefix =
    Printf.sprintf "%s%s" fake_capture_field_prefix (string_of_capture_mode Pvar.ByReference)


  let mk_fake_field ~id mode =
    Fieldname.make
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "function"]))
      (Printf.sprintf "%s%s%d" fake_capture_field_prefix (string_of_capture_mode mode) id)


  let is_captured_by_ref_fake_access (access : _ HilExp.Access.t) =
    match access with
    | FieldAccess fieldname
      when String.is_prefix ~prefix:fake_captured_by_ref_field_prefix
             (Fieldname.to_string fieldname) ->
        true
    | _ ->
        false


  let mk_capture_edges captured =
    List.foldi captured ~init:Memory.Edges.empty ~f:(fun id edges (mode, addr, trace) ->
        Memory.Edges.add (HilExp.Access.FieldAccess (mk_fake_field ~id mode)) (addr, trace) edges )


  let check_captured_addresses action lambda_addr (astate : t) =
    match AbductiveDomain.find_post_cell_opt lambda_addr astate with
    | None ->
        Ok astate
    | Some (edges, attributes) ->
        let+ () =
          IContainer.iter_result ~fold:Attributes.fold attributes ~f:(function
            | Attribute.Closure _ ->
                IContainer.iter_result ~fold:Memory.Edges.fold edges ~f:(fun (access, addr_trace) ->
                    if is_captured_by_ref_fake_access access then
                      let+ _ = check_addr_access Read action addr_trace astate in
                      ()
                    else Ok () )
            | _ ->
                Ok () )
        in
        astate


  let record location pname captured astate =
    let captured_addresses =
      List.filter_map captured ~f:(fun (captured_as, (address_captured, trace_captured), mode) ->
          let new_trace = ValueHistory.Capture {captured_as; mode; location} :: trace_captured in
          Some (mode, address_captured, new_trace) )
    in
    let closure_addr_hist = (AbstractValue.mk_fresh (), [ValueHistory.Assignment location]) in
    let fake_capture_edges = mk_capture_edges captured_addresses in
    let astate =
      AbductiveDomain.set_post_cell closure_addr_hist
        (fake_capture_edges, Attributes.singleton (Closure pname))
        location astate
    in
    (astate, closure_addr_hist)
end

let eval_var location hist var astate = Stack.eval location hist var astate

let eval_access mode location addr_hist access astate =
  let+ astate = check_addr_access mode location addr_hist astate in
  Memory.eval_edge addr_hist access astate


let eval_access_biad_isl mode location addr_hist access astate =
  let map_ok addr_hist access results =
    List.map results ~f:(fun result ->
        let+ astate = result in
        Memory.eval_edge addr_hist access astate )
  in
  let results = check_and_abduce_addr_access_isl mode location addr_hist astate in
  map_ok addr_hist access results


let eval mode location exp0 astate =
  let rec eval mode exp astate =
    match (exp : Exp.t) with
    | Var id ->
        Ok (eval_var location (* error in case of missing history? *) [] (Var.of_id id) astate)
    | Lvar pvar ->
        Ok
          (eval_var location
             [ValueHistory.VariableAccessed (pvar, location)]
             (Var.of_pvar pvar) astate)
    | Lfield (exp', field, _) ->
        let* astate, addr_hist = eval Read exp' astate in
        eval_access mode location addr_hist (FieldAccess field) astate
    | Lindex (exp', exp_index) ->
        let* astate, addr_hist_index = eval Read exp_index astate in
        let* astate, addr_hist = eval Read exp' astate in
        eval_access mode location addr_hist (ArrayAccess (StdTyp.void, fst addr_hist_index)) astate
    | Closure {name; captured_vars} ->
        let+ astate, rev_captured =
          List.fold_result captured_vars ~init:(astate, [])
            ~f:(fun (astate, rev_captured) (capt_exp, captured_as, _, mode) ->
              let+ astate, addr_trace = eval Read capt_exp astate in
              (astate, (captured_as, addr_trace, mode) :: rev_captured) )
        in
        Closures.record location name (List.rev rev_captured) astate
    | Cast (_, exp') ->
        eval mode exp' astate
    | Const (Cint i) ->
        let v = AbstractValue.Constants.get_int i in
        let+ astate =
          PulseArithmetic.and_eq_int v i astate
          >>| AddressAttributes.invalidate
                (v, [ValueHistory.Assignment location])
                (ConstantDereference i) location
        in
        (astate, (v, []))
    | UnOp (unop, exp, _typ) ->
        let* astate, (addr, hist) = eval Read exp astate in
        let unop_addr = AbstractValue.mk_fresh () in
        let+ astate = PulseArithmetic.eval_unop unop_addr unop addr astate in
        (astate, (unop_addr, hist))
    | BinOp (bop, e_lhs, e_rhs) ->
        let* astate, (addr_lhs, hist_lhs) = eval Read e_lhs astate in
        (* NOTE: keeping track of only [hist_lhs] into the binop is not the best *)
        let* astate, (addr_rhs, _hist_rhs) = eval Read e_rhs astate in
        let binop_addr = AbstractValue.mk_fresh () in
        let+ astate =
          PulseArithmetic.eval_binop binop_addr bop (AbstractValueOperand addr_lhs)
            (AbstractValueOperand addr_rhs) astate
        in
        (astate, (binop_addr, hist_lhs))
    | Const _ | Sizeof _ | Exn _ ->
        Ok (astate, (AbstractValue.mk_fresh (), (* TODO history *) []))
  in
  eval mode exp0 astate


let eval_to_operand mode location exp astate =
  match (exp : Exp.t) with
  | Const (Cint i) ->
      Ok (astate, PulseArithmetic.LiteralOperand i)
  | exp ->
      let+ astate, (value, _) = eval mode location exp astate in
      (astate, PulseArithmetic.AbstractValueOperand value)


let prune location ~condition astate =
  let rec prune_aux ~negated exp astate =
    match (exp : Exp.t) with
    | BinOp (bop, exp_lhs, exp_rhs) ->
        let* astate, lhs_op = eval_to_operand Read location exp_lhs astate in
        let* astate, rhs_op = eval_to_operand Read location exp_rhs astate in
        PulseArithmetic.prune_binop ~negated bop lhs_op rhs_op astate
    | UnOp (LNot, exp', _) ->
        prune_aux ~negated:(not negated) exp' astate
    | exp ->
        prune_aux ~negated (Exp.BinOp (Ne, exp, Exp.zero)) astate
  in
  prune_aux ~negated:false condition astate


let eval_deref location exp astate =
  let* astate, addr_hist = eval Read location exp astate in
  let+ astate = check_addr_access Read location addr_hist astate in
  Memory.eval_edge addr_hist Dereference astate


let eval_structure_isl mode loc exp astate =
  match (exp : Exp.t) with
  | Lfield (exp', field, _) ->
      let+ astate, addr_hist = eval mode loc exp' astate in
      let astates = eval_access_biad_isl mode loc addr_hist (FieldAccess field) astate in
      (false, astates)
  | Lindex (exp', exp_index) ->
      let* astate, addr_hist_index = eval mode loc exp_index astate in
      let+ astate, addr_hist = eval mode loc exp' astate in
      let astates =
        eval_access_biad_isl mode loc addr_hist
          (ArrayAccess (StdTyp.void, fst addr_hist_index))
          astate
      in
      (false, astates)
  | _ ->
      let+ astate, (addr, history) = eval mode loc exp astate in
      (true, [Ok (astate, (addr, history))])


let eval_deref_biad_isl location access addr_hist astate =
  check_and_abduce_addr_access_isl Read location addr_hist astate
  |> List.map ~f:(fun result ->
         let+ astate = result in
         Memory.eval_edge addr_hist access astate )


let eval_deref_isl location exp astate =
  let<*> is_structured, ls_astate_addr_hist = eval_structure_isl Read location exp astate in
  let eval_deref_function (astate, addr_hist) =
    if is_structured then eval_deref_biad_isl location Dereference addr_hist astate
    else [eval_deref location exp astate]
  in
  List.concat_map ls_astate_addr_hist ~f:(fun result ->
      let<*> astate_addr = result in
      eval_deref_function astate_addr )


let realloc_pvar tenv pvar typ location astate =
  let addr = AbstractValue.mk_fresh () in
  let astate =
    Stack.add (Var.of_pvar pvar) (addr, [ValueHistory.VariableDeclared (pvar, location)]) astate
  in
  AbductiveDomain.set_uninitialized tenv (`LocalDecl (pvar, Some addr)) typ location astate


let write_id id new_addr_loc astate = Stack.add (Var.of_id id) new_addr_loc astate

let havoc_id id loc_opt astate =
  (* Topl needs to track the return value of a method; even if nondet now, it may be pruned later. *)
  if Topl.is_active () || Stack.mem (Var.of_id id) astate then
    write_id id (AbstractValue.mk_fresh (), loc_opt) astate
  else astate


let write_access location addr_trace_ref access addr_trace_obj astate =
  check_addr_access Write location addr_trace_ref astate
  >>| Memory.add_edge addr_trace_ref access addr_trace_obj location


let write_access_biad_isl location addr_trace_ref access addr_trace_obj astate =
  let astates = check_and_abduce_addr_access_isl Write location addr_trace_ref astate in
  List.map astates ~f:(fun result ->
      let+ astate = result in
      Memory.add_edge addr_trace_ref access addr_trace_obj location astate )


let write_deref location ~ref:addr_trace_ref ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref Dereference addr_trace_obj astate


let write_deref_biad_isl location ~ref:(addr_ref, addr_ref_history) access ~obj:addr_trace_obj
    astate =
  write_access_biad_isl location (addr_ref, addr_ref_history) access addr_trace_obj astate


let write_field location ~ref:addr_trace_ref field ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref (FieldAccess field) addr_trace_obj astate


let write_arr_index location ~ref:addr_trace_ref ~index ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref (ArrayAccess (StdTyp.void, index)) addr_trace_obj astate


let havoc_field location addr_trace field trace_obj astate =
  write_field location ~ref:addr_trace field ~obj:(AbstractValue.mk_fresh (), trace_obj) astate


let allocate procname location addr_trace astate =
  AddressAttributes.allocate procname addr_trace location astate


let add_dynamic_type typ address astate = AddressAttributes.add_dynamic_type typ address astate

let remove_allocation_attr address astate = AddressAttributes.remove_allocation_attr address astate

let invalidate location cause addr_trace astate =
  check_addr_access NoAccess location addr_trace astate
  >>| AddressAttributes.invalidate addr_trace cause location


let invalidate_biad_isl location cause (address, history) astate =
  check_and_abduce_addr_access_isl NoAccess location (address, history) ~null_noop:true astate
  |> List.map ~f:(fun result ->
         let+ astate = result in
         AddressAttributes.invalidate (address, history) cause location astate )


let invalidate_access location cause ref_addr_hist access astate =
  let astate, (addr_obj, _) = Memory.eval_edge ref_addr_hist access astate in
  invalidate location cause (addr_obj, snd ref_addr_hist) astate


let invalidate_array_elements location cause addr_trace astate =
  let+ astate = check_addr_access NoAccess location addr_trace astate in
  match Memory.find_opt (fst addr_trace) astate with
  | None ->
      astate
  | Some edges ->
      Memory.Edges.fold edges ~init:astate ~f:(fun astate (access, dest_addr_trace) ->
          match (access : Memory.Access.t) with
          | ArrayAccess _ ->
              AddressAttributes.invalidate dest_addr_trace cause location astate
          | _ ->
              astate )


let shallow_copy location addr_hist astate =
  let+ astate = check_addr_access Read location addr_hist astate in
  let cell =
    match AbductiveDomain.find_post_cell_opt (fst addr_hist) astate with
    | None ->
        (Memory.Edges.empty, Attributes.empty)
    | Some cell ->
        cell
  in
  let copy = (AbstractValue.mk_fresh (), [ValueHistory.Assignment location]) in
  (AbductiveDomain.set_post_cell copy cell location astate, copy)


let check_address_escape escape_location proc_desc address history astate =
  let is_assigned_to_global address astate =
    let points_to_address pointer address astate =
      Memory.find_edge_opt pointer Dereference astate
      |> Option.exists ~f:(fun (pointee, _) -> AbstractValue.equal pointee address)
    in
    Stack.exists
      (fun var (pointer, _) -> Var.is_global var && points_to_address pointer address astate)
      astate
  in
  let check_address_of_cpp_temporary () =
    AddressAttributes.find_opt address astate
    |> Option.fold_result ~init:() ~f:(fun () attrs ->
           IContainer.iter_result ~fold:Attributes.fold attrs ~f:(fun attr ->
               match attr with
               | Attribute.AddressOfCppTemporary (variable, _)
                 when not (is_assigned_to_global address astate) ->
                   (* The returned address corresponds to a C++ temporary. It will have gone out of
                      scope by now except if it was bound to a global. *)
                   Error
                     (ReportableError
                        { diagnostic=
                            Diagnostic.StackVariableAddressEscape
                              {variable; location= escape_location; history}
                        ; astate })
               | _ ->
                   Ok () ) )
  in
  let check_address_of_stack_variable () =
    let proc_name = Procdesc.get_proc_name proc_desc in
    IContainer.iter_result ~fold:(IContainer.fold_of_pervasives_map_fold Stack.fold) astate
      ~f:(fun (variable, (var_address, _)) ->
        if
          AbstractValue.equal var_address address
          && ( Var.is_cpp_temporary variable
             || Var.is_local_to_procedure proc_name variable
                && not (Procdesc.is_captured_var proc_desc variable) )
        then (
          L.d_printfln_escaped "Stack variable address &%a detected at address %a" Var.pp variable
            AbstractValue.pp address ;
          Error
            (ReportableError
               { diagnostic=
                   Diagnostic.StackVariableAddressEscape
                     {variable; location= escape_location; history}
               ; astate }) )
        else Ok () )
  in
  let+ () = check_address_of_cpp_temporary () >>= check_address_of_stack_variable in
  astate


let mark_address_of_cpp_temporary history variable address astate =
  AddressAttributes.add_one address (AddressOfCppTemporary (variable, history)) astate


let mark_address_of_stack_variable history variable location address astate =
  AddressAttributes.add_one address (AddressOfStackVariable (variable, location, history)) astate


let check_memory_leak_unreachable unreachable_addrs location astate =
  let check_memory_leak result attributes =
    let allocated_not_freed_opt =
      Attributes.fold attributes ~init:(None (* allocation trace *), false (* freed *))
        ~f:(fun acc attr ->
          match (attr : Attribute.t) with
          | Allocated (procname, trace) ->
              (Some (procname, trace), snd acc)
          | Invalid (CFree, _) ->
              (fst acc, true)
          | _ ->
              acc )
    in
    match allocated_not_freed_opt with
    | Some (procname, trace), false ->
        (* allocated but not freed *)
        Error
          (ReportableError
             { diagnostic= Diagnostic.MemoryLeak {procname; location; allocation_trace= trace}
             ; astate })
    | _ ->
        result
  in
  List.fold unreachable_addrs ~init:(Ok ()) ~f:(fun res addr ->
      match AbductiveDomain.AddressAttributes.find_opt addr astate with
      | Some unreachable_attrs ->
          check_memory_leak res unreachable_attrs
      | None ->
          res )


let get_dynamic_type_unreachable_values vars astate =
  (* For each unreachable address we find a root variable for it; if there is
     more than one, it doesn't matter which *)
  let find_var_opt astate addr =
    Stack.fold
      (fun var (var_addr, _) var_opt ->
        if AbstractValue.equal addr var_addr then Some var else var_opt )
      astate None
  in
  let astate' = Stack.remove_vars vars astate in
  let _, _, unreachable_addrs = AbductiveDomain.discard_unreachable astate' in
  let res =
    List.fold unreachable_addrs ~init:[] ~f:(fun res addr ->
        (let open IOption.Let_syntax in
        let* attrs = AbductiveDomain.AddressAttributes.find_opt addr astate in
        let* typ = Attributes.get_dynamic_type attrs in
        let+ var = find_var_opt astate addr in
        (var, addr, typ) :: res)
        |> Option.value ~default:res )
  in
  List.map ~f:(fun (var, _, typ) -> (var, typ)) res


let remove_vars tenv vars location orig_astate =
  let astate =
    (* Simplification of [IsInstanceOf(var, typ)] term is necessary here, as a variable can die before
       the normalization function is called. This could cause [IsInstanceOf(var, typ)] terms that
       reference dead vars to be collected before they are evaluated to detect a contradiction *)
    List.fold vars ~init:(AbductiveDomain.simplify_instanceof tenv orig_astate)
      ~f:(fun astate var ->
        match Stack.find_opt var astate with
        | Some (address, history) ->
            let astate =
              if Var.appears_in_source_code var && AbductiveDomain.is_local var astate then
                mark_address_of_stack_variable history var location address astate
              else astate
            in
            if Var.is_cpp_temporary var then
              mark_address_of_cpp_temporary history var address astate
            else astate
        | _ ->
            astate )
  in
  let astate' = Stack.remove_vars vars astate in
  if phys_equal astate' astate then Ok astate
  else
    let astate, _, unreachable_addrs = AbductiveDomain.discard_unreachable astate' in
    let+ () = check_memory_leak_unreachable unreachable_addrs location orig_astate in
    astate


let get_captured_actuals location ~captured_vars ~actual_closure astate =
  let* astate, this_value_addr = eval_access Read location actual_closure Dereference astate in
  let+ _, astate, captured_vars_with_actuals =
    List.fold_result captured_vars ~init:(0, astate, [])
      ~f:(fun (id, astate, captured) (var, mode) ->
        let+ astate, captured_actual =
          eval_access Read location this_value_addr
            (FieldAccess (Closures.mk_fake_field ~id mode))
            astate
        in
        (id + 1, astate, (var, captured_actual) :: captured) )
  in
  (astate, captured_vars_with_actuals)
