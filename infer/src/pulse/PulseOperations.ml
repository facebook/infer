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
open PulseOperationResult.Import

type t = AbductiveDomain.t

let check_addr_access path ?must_be_valid_reason access_mode location (address, history) astate =
  let access_trace = Trace.Immediate {location; history} in
  let* astate =
    AddressAttributes.check_valid path ?must_be_valid_reason access_trace address astate
    |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
           ReportableError
             { diagnostic=
                 Diagnostic.AccessToInvalidAddress
                   { calling_context= []
                   ; invalid_address= Decompiler.find address astate
                   ; invalidation
                   ; invalidation_trace
                   ; access_trace
                   ; must_be_valid_reason }
             ; astate } )
    |> AccessResult.of_result
  in
  match access_mode with
  | Read ->
      AddressAttributes.check_initialized path access_trace address astate
      |> Result.map_error ~f:(fun () ->
             ReportableError
               { diagnostic=
                   Diagnostic.ReadUninitializedValue {calling_context= []; trace= access_trace}
               ; astate } )
      |> AccessResult.of_result_f ~f:(fun _ ->
             (* do not report further uninitialized reads errors on this value *)
             AbductiveDomain.initialize address astate )
  | Write ->
      Ok (AbductiveDomain.initialize address astate)
  | NoAccess ->
      Ok astate


let check_and_abduce_addr_access_isl path access_mode location (address, history)
    ?(null_noop = false) astate =
  let access_trace = Trace.Immediate {location; history} in
  AddressAttributes.check_valid_isl path access_trace address ~null_noop astate
  |> List.map ~f:(fun access_result ->
         let* astate = AccessResult.of_abductive_access_result access_trace access_result in
         match access_mode with
         | Read ->
             AddressAttributes.check_initialized path access_trace address astate
             |> Result.map_error ~f:(fun () ->
                    ReportableError
                      { diagnostic=
                          Diagnostic.ReadUninitializedValue
                            {calling_context= []; trace= access_trace}
                      ; astate } )
             |> AccessResult.of_result_f ~f:(fun _ ->
                    (* do not report further uninitialized reads errors on this value *)
                    AbductiveDomain.initialize address astate )
         | Write ->
             Ok (AbductiveDomain.initialize address astate)
         | NoAccess ->
             Ok astate )


module Closures = struct
  module Memory = AbductiveDomain.Memory

  let is_captured_by_ref_fake_access (access : _ HilExp.Access.t) =
    match access with
    | FieldAccess fieldname ->
        Fieldname.is_fake_capture_field_by_ref fieldname
    | _ ->
        false


  let mk_capture_edges captured =
    List.foldi captured ~init:Memory.Edges.empty ~f:(fun id edges (mode, typ, addr, trace) ->
        Memory.Edges.add
          (HilExp.Access.FieldAccess (Fieldname.mk_fake_capture_field ~id typ mode))
          (addr, trace) edges )


  let check_captured_addresses path action lambda_addr (astate : t) =
    match AbductiveDomain.find_post_cell_opt lambda_addr astate with
    | None ->
        Ok astate
    | Some (edges, attributes) ->
        Attributes.fold attributes ~init:(Ok astate) ~f:(fun astate_result (attr : Attribute.t) ->
            match attr with
            | Closure _ ->
                Memory.Edges.fold edges ~init:astate_result
                  ~f:(fun astate_result (access, addr_trace) ->
                    if is_captured_by_ref_fake_access access then
                      astate_result >>= check_addr_access path Read action addr_trace
                    else astate_result )
            | _ ->
                astate_result )


  let record ({PathContext.timestamp; conditions} as path) location pname captured astate =
    let captured_addresses =
      List.filter_map captured
        ~f:(fun (captured_as, (address_captured, trace_captured), typ, mode) ->
          let new_trace =
            ValueHistory.sequence ~context:conditions
              (Capture {captured_as; mode; location; timestamp})
              trace_captured
          in
          Some (mode, typ, address_captured, new_trace) )
    in
    let closure_addr_hist =
      (AbstractValue.mk_fresh (), ValueHistory.singleton (Assignment (location, timestamp)))
    in
    let fake_capture_edges = mk_capture_edges captured_addresses in
    let astate =
      AbductiveDomain.set_post_cell path closure_addr_hist
        (fake_capture_edges, Attributes.singleton (Closure pname))
        location astate
    in
    (astate, closure_addr_hist)
end

let pulse_model_type = Typ.CStruct (QualifiedCppName.of_list ["__infer_pulse_model"])

module ModeledField = struct
  let string_length = Fieldname.make pulse_model_type "__infer_model_string_length"

  let internal_string = Fieldname.make pulse_model_type "__infer_model_backing_string"

  let internal_ref_count = Fieldname.make pulse_model_type "__infer_model_reference_count"

  let delegated_release = Fieldname.make pulse_model_type "__infer_model_delegated_release"
end

let conservatively_initialize_args arg_values ({AbductiveDomain.post} as astate) =
  let reachable_values =
    BaseDomain.reachable_addresses_from (Caml.List.to_seq arg_values) (post :> BaseDomain.t)
  in
  AbstractValue.Set.fold AbductiveDomain.initialize reachable_values astate


let eval_access path ?must_be_valid_reason mode location addr_hist access astate =
  let+ astate = check_addr_access path ?must_be_valid_reason mode location addr_hist astate in
  Memory.eval_edge addr_hist access astate


let eval_deref_access path ?must_be_valid_reason mode location addr_hist access astate =
  let* astate, addr_hist = eval_access path Read location addr_hist access astate in
  eval_access path ?must_be_valid_reason mode location addr_hist Dereference astate


let eval_access_biad_isl path mode location addr_hist access astate =
  let map_ok addr_hist access results =
    List.map results ~f:(fun result ->
        let+ astate = result in
        Memory.eval_edge addr_hist access astate )
  in
  let results = check_and_abduce_addr_access_isl path mode location addr_hist astate in
  map_ok addr_hist access results


let eval_var ({PathContext.timestamp} as path) location pvar astate =
  Stack.eval path location
    (ValueHistory.singleton (VariableAccessed (pvar, location, timestamp)))
    (Var.of_pvar pvar) astate


let eval path mode location exp0 astate =
  let rec eval ({PathContext.timestamp} as path) mode exp astate =
    match (exp : Exp.t) with
    | Var id ->
        Sat
          (Ok
             (Stack.eval path location (* error in case of missing history? *) ValueHistory.epoch
                (Var.of_id id) astate ) )
    | Lvar pvar ->
        Sat (Ok (eval_var path location pvar astate))
    | Lfield (exp', field, _) ->
        let+* astate, addr_hist = eval path Read exp' astate in
        eval_access path mode location addr_hist (FieldAccess field) astate
    | Lindex (exp', exp_index) ->
        let** astate, addr_hist_index = eval path Read exp_index astate in
        let+* astate, addr_hist = eval path Read exp' astate in
        eval_access path mode location addr_hist
          (ArrayAccess (StdTyp.void, fst addr_hist_index))
          astate
    | Closure {name; captured_vars} ->
        let++ astate, rev_captured =
          List.fold captured_vars
            ~init:(Sat (Ok (astate, [])))
            ~f:(fun result (capt_exp, captured_as, typ, mode) ->
              let** astate, rev_captured = result in
              let++ astate, addr_trace = eval path Read capt_exp astate in
              (astate, (captured_as, addr_trace, typ, mode) :: rev_captured) )
        in
        let astate, v_hist = Closures.record path location name (List.rev rev_captured) astate in
        let astate =
          conservatively_initialize_args
            (List.rev_map rev_captured ~f:(fun (_, (addr, _), _, _) -> addr))
            astate
        in
        (astate, v_hist)
    | Const (Cfun proc_name) ->
        (* function pointers are represented as closures with no captured variables *)
        Sat (Ok (Closures.record path location proc_name [] astate))
    | Cast (_, exp') ->
        eval path mode exp' astate
    | Const (Cint i) ->
        let v = AbstractValue.Constants.get_int i in
        let invalidation = Invalidation.ConstantDereference i in
        let++ astate =
          PulseArithmetic.and_eq_int v i astate
          >>|| AddressAttributes.invalidate
                 (v, ValueHistory.singleton (Assignment (location, timestamp)))
                 invalidation location
        in
        (astate, (v, ValueHistory.singleton (Invalidated (invalidation, location, timestamp))))
    | Const (Cstr s) ->
        (* TODO: record actual string value; since we are making strings be a record in memory
           instead of pure values some care has to be added to access string values once written *)
        let v = AbstractValue.mk_fresh () in
        let=* astate, (len_addr, hist) =
          eval_access path Write location
            (v, ValueHistory.singleton (Assignment (location, timestamp)))
            (FieldAccess ModeledField.string_length) astate
        in
        let len_int = IntLit.of_int (String.length s) in
        let++ astate = PulseArithmetic.and_eq_int len_addr len_int astate in
        (astate, (v, hist))
    | Const ((Cfloat _ | Cclass _) as c) ->
        let v = AbstractValue.mk_fresh () in
        let++ astate = PulseArithmetic.and_eq_const v c astate in
        (astate, (v, ValueHistory.singleton (Assignment (location, timestamp))))
    | UnOp (unop, exp, _typ) ->
        let** astate, (addr, hist) = eval path Read exp astate in
        let unop_addr = AbstractValue.mk_fresh () in
        let++ astate, unop_addr = PulseArithmetic.eval_unop unop_addr unop addr astate in
        (astate, (unop_addr, hist))
    | BinOp (bop, e_lhs, e_rhs) ->
        let** astate, (addr_lhs, hist_lhs) = eval path Read e_lhs astate in
        let** astate, (addr_rhs, hist_rhs) = eval path Read e_rhs astate in
        let binop_addr = AbstractValue.mk_fresh () in
        let++ astate, binop_addr =
          PulseArithmetic.eval_binop binop_addr bop (AbstractValueOperand addr_lhs)
            (AbstractValueOperand addr_rhs) astate
        in
        (astate, (binop_addr, ValueHistory.binary_op bop hist_lhs hist_rhs))
    | Exn exp ->
        eval path Read exp astate
    | Sizeof _ ->
        Sat (Ok (astate, (AbstractValue.mk_fresh (), (* TODO history *) ValueHistory.epoch)))
  in
  eval path mode exp0 astate


let eval_to_operand path location exp astate =
  match (exp : Exp.t) with
  | Const c ->
      Sat (Ok (astate, PulseArithmetic.ConstOperand c, ValueHistory.epoch))
  | exp ->
      let++ astate, (value, hist) = eval path Read location exp astate in
      (astate, PulseArithmetic.AbstractValueOperand value, hist)


let prune path location ~condition astate =
  let rec prune_aux ~negated exp astate =
    match (exp : Exp.t) with
    | BinOp (bop, exp_lhs, exp_rhs) ->
        let** astate, lhs_op, lhs_hist = eval_to_operand path location exp_lhs astate in
        let** astate, rhs_op, rhs_hist = eval_to_operand path location exp_rhs astate in
        let++ astate = PulseArithmetic.prune_binop ~negated bop lhs_op rhs_op astate in
        let hist =
          match (lhs_hist, rhs_hist) with
          | ValueHistory.Epoch, hist | hist, ValueHistory.Epoch ->
              (* if one history is empty then just propagate the other one (which could also be
                 empty) *)
              hist
          | _ ->
              ValueHistory.binary_op bop lhs_hist rhs_hist
        in
        (astate, hist)
    | UnOp (LNot, exp', _) ->
        prune_aux ~negated:(not negated) exp' astate
    | exp ->
        prune_aux ~negated (Exp.BinOp (Ne, exp, Exp.zero)) astate
  in
  prune_aux ~negated:false condition astate


let eval_deref path ?must_be_valid_reason location exp astate =
  let+* astate, addr_hist = eval path Read location exp astate in
  let+ astate = check_addr_access path ?must_be_valid_reason Read location addr_hist astate in
  Memory.eval_edge addr_hist Dereference astate


let eval_proc_name path location call_exp astate =
  match (call_exp : Exp.t) with
  | Const (Cfun proc_name) | Closure {name= proc_name} ->
      Sat (Ok (astate, Some proc_name))
  | _ ->
      let++ astate, (f, _) = eval path Read location call_exp astate in
      (astate, AddressAttributes.get_closure_proc_name f astate)


let eval_structure_isl path mode loc exp astate =
  match (exp : Exp.t) with
  | Lfield (exp', field, _) ->
      let++ astate, addr_hist = eval path mode loc exp' astate in
      let astates = eval_access_biad_isl path mode loc addr_hist (FieldAccess field) astate in
      (false, astates)
  | Lindex (exp', exp_index) ->
      let** astate, addr_hist_index = eval path mode loc exp_index astate in
      let++ astate, addr_hist = eval path mode loc exp' astate in
      let astates =
        eval_access_biad_isl path mode loc addr_hist
          (ArrayAccess (StdTyp.void, fst addr_hist_index))
          astate
      in
      (false, astates)
  | _ ->
      let++ astate, (addr, history) = eval path mode loc exp astate in
      (true, [Ok (astate, (addr, history))])


let eval_deref_biad_isl path location access addr_hist astate =
  check_and_abduce_addr_access_isl path Read location addr_hist astate
  |> List.map ~f:(fun result ->
         let+ astate = result in
         Memory.eval_edge addr_hist access astate )


let eval_deref_isl path location exp astate =
  let<**> is_structured, ls_astate_addr_hist = eval_structure_isl path Read location exp astate in
  let eval_deref_function (astate, addr_hist) =
    if is_structured then eval_deref_biad_isl path location Dereference addr_hist astate
    else eval_deref path location exp astate |> SatUnsat.to_list
  in
  List.concat_map ls_astate_addr_hist ~f:(fun result ->
      let<*> astate_addr = result in
      eval_deref_function astate_addr )


let realloc_pvar tenv ({PathContext.timestamp} as path) pvar typ location astate =
  let addr = AbstractValue.mk_fresh () in
  let astate =
    Stack.add (Var.of_pvar pvar)
      (addr, ValueHistory.singleton (VariableDeclared (pvar, location, timestamp)))
      astate
  in
  AbductiveDomain.set_uninitialized tenv path (`LocalDecl (pvar, Some addr)) typ location astate


let write_id id new_addr_loc astate = Stack.add (Var.of_id id) new_addr_loc astate

let read_id id astate = Stack.find_opt (Var.of_id id) astate

let havoc_id id loc_opt astate =
  (* Topl needs to track the return value of a method; even if nondet now, it may be pruned later. *)
  if Topl.is_active () || Stack.mem (Var.of_id id) astate then
    write_id id (AbstractValue.mk_fresh (), loc_opt) astate
  else astate


let write_access path location addr_trace_ref access addr_trace_obj astate =
  check_addr_access path Write location addr_trace_ref astate
  >>| Memory.add_edge path addr_trace_ref access addr_trace_obj location


let write_access_biad_isl path location addr_trace_ref access addr_trace_obj astate =
  let astates = check_and_abduce_addr_access_isl path Write location addr_trace_ref astate in
  List.map astates ~f:(fun result ->
      let+ astate = result in
      Memory.add_edge path addr_trace_ref access addr_trace_obj location astate )


let write_deref path location ~ref:addr_trace_ref ~obj:addr_trace_obj astate =
  write_access path location addr_trace_ref Dereference addr_trace_obj astate


let write_deref_biad_isl path location ~ref:(addr_ref, addr_ref_history) access ~obj:addr_trace_obj
    astate =
  write_access_biad_isl path location (addr_ref, addr_ref_history) access addr_trace_obj astate


let write_field path location ~ref:addr_trace_ref field ~obj:addr_trace_obj astate =
  write_access path location addr_trace_ref (FieldAccess field) addr_trace_obj astate


let write_deref_field path location ~ref:addr_trace_ref field ~obj:addr_trace_obj astate =
  let* astate, addr_hist =
    eval_access path Read location addr_trace_ref (FieldAccess field) astate
  in
  write_deref path location ~ref:addr_hist ~obj:addr_trace_obj astate


let write_arr_index path location ~ref:addr_trace_ref ~index ~obj:addr_trace_obj astate =
  write_access path location addr_trace_ref (ArrayAccess (StdTyp.void, index)) addr_trace_obj astate


let havoc_deref_field path location addr_trace field trace_obj astate =
  write_deref_field path location ~ref:addr_trace field
    ~obj:(AbstractValue.mk_fresh (), trace_obj)
    astate


let always_reachable address astate = AddressAttributes.always_reachable address astate

let allocate allocator location addr astate =
  AddressAttributes.allocate allocator addr location astate


let java_resource_release ~recursive address astate =
  let if_valid_access_then_eval addr access astate =
    Option.map (Memory.find_edge_opt addr access astate) ~f:fst
  in
  let if_valid_field_then_load obj field astate =
    let open IOption.Let_syntax in
    let* field_addr = if_valid_access_then_eval obj (FieldAccess field) astate in
    if_valid_access_then_eval field_addr Dereference astate
  in
  let rec loop seen obj astate =
    if AbstractValue.Set.mem obj seen || AddressAttributes.is_java_resource_released obj astate then
      astate
    else
      let astate = AddressAttributes.java_resource_release obj astate in
      match if_valid_field_then_load obj ModeledField.delegated_release astate with
      | Some delegation ->
          (* beware: if the field is not valid, a regular call to Java.load_field will generate a
             fresh abstract value and we will loop forever, even if we use the [seen] set *)
          if recursive then loop (AbstractValue.Set.add obj seen) delegation astate else astate
      | None ->
          astate
  in
  loop AbstractValue.Set.empty address astate


let csharp_resource_release ~recursive address astate =
  let if_valid_access_then_eval addr access astate =
    Option.map (Memory.find_edge_opt addr access astate) ~f:fst
  in
  let if_valid_field_then_load obj field astate =
    let open IOption.Let_syntax in
    let* field_addr = if_valid_access_then_eval obj (FieldAccess field) astate in
    if_valid_access_then_eval field_addr Dereference astate
  in
  let rec loop seen obj astate =
    if AbstractValue.Set.mem obj seen || AddressAttributes.is_csharp_resource_released obj astate
    then astate
    else
      let astate = AddressAttributes.csharp_resource_release obj astate in
      match if_valid_field_then_load obj ModeledField.delegated_release astate with
      | Some delegation ->
          (* beware: if the field is not valid, a regular call to CSharp.load_field will generate a
             fresh abstract value and we will loop forever, even if we use the [seen] set *)
          if recursive then loop (AbstractValue.Set.add obj seen) delegation astate else astate
      | None ->
          astate
  in
  loop AbstractValue.Set.empty address astate


let add_dynamic_type typ address astate = AddressAttributes.add_dynamic_type typ address astate

let add_dynamic_type_source_file typ source_file address astate =
  AddressAttributes.add_dynamic_type_source_file typ source_file address astate


let add_ref_counted address astate = AddressAttributes.add_ref_counted address astate

let is_ref_counted address astate = AddressAttributes.is_ref_counted address astate

let remove_allocation_attr address astate = AddressAttributes.remove_allocation_attr address astate

type invalidation_access =
  | MemoryAccess of
      { pointer: AbstractValue.t * ValueHistory.t
      ; access: BaseMemory.Access.t
      ; hist_obj_default: ValueHistory.t }
  | StackAddress of Var.t * ValueHistory.t
  | UntraceableAccess

let record_invalidation ({PathContext.timestamp; conditions} as path) access_path location cause
    astate =
  match access_path with
  | StackAddress (x, hist0) ->
      let astate, (addr, hist) = Stack.eval path location hist0 x astate in
      let hist' =
        ValueHistory.sequence ~context:conditions (Invalidated (cause, location, timestamp)) hist
      in
      Stack.add x (addr, hist') astate
  | MemoryAccess {pointer; access; hist_obj_default} ->
      let addr_obj, hist_obj =
        match Memory.find_edge_opt (fst pointer) access astate with
        | Some addr_hist ->
            addr_hist
        | None ->
            (AbstractValue.mk_fresh (), hist_obj_default)
      in
      let hist' =
        ValueHistory.sequence ~context:conditions
          (Invalidated (cause, location, timestamp))
          hist_obj
      in
      Memory.add_edge path pointer access (addr_obj, hist') location astate
  | UntraceableAccess ->
      astate


let invalidate path access_path location cause addr_trace astate =
  check_addr_access path NoAccess location addr_trace astate
  >>| AddressAttributes.invalidate addr_trace cause location
  >>| record_invalidation path access_path location cause


let invalidate_biad_isl path location cause (address, history) astate =
  check_and_abduce_addr_access_isl path NoAccess location (address, history) ~null_noop:true astate
  |> List.map ~f:(fun result ->
         let+ astate = result in
         AddressAttributes.invalidate (address, history) cause location astate )


let invalidate_access path location cause ref_addr_hist access astate =
  let astate, (addr_obj, hist_obj) = Memory.eval_edge ref_addr_hist access astate in
  invalidate path
    (MemoryAccess {pointer= ref_addr_hist; access; hist_obj_default= hist_obj})
    location cause
    (addr_obj, snd ref_addr_hist)
    astate


let invalidate_deref_access path location cause ref_addr_hist access astate =
  let astate, addr_hist = Memory.eval_edge ref_addr_hist access astate in
  let astate, (addr_obj, hist_obj) = Memory.eval_edge addr_hist Dereference astate in
  invalidate path
    (MemoryAccess {pointer= ref_addr_hist; access; hist_obj_default= hist_obj})
    location cause
    (addr_obj, snd ref_addr_hist)
    astate


let invalidate_array_elements path location cause addr_trace astate =
  let+ astate = check_addr_access path NoAccess location addr_trace astate in
  match Memory.find_opt (fst addr_trace) astate with
  | None ->
      astate
  | Some edges ->
      Memory.Edges.fold edges ~init:astate ~f:(fun astate (access, dest_addr_trace) ->
          match (access : Memory.Access.t) with
          | ArrayAccess _ as access ->
              AddressAttributes.invalidate dest_addr_trace cause location astate
              |> record_invalidation path
                   (MemoryAccess {pointer= addr_trace; access; hist_obj_default= snd dest_addr_trace}
                   )
                   location cause
          | _ ->
              astate )


let shallow_copy ({PathContext.timestamp} as path) location addr_hist astate =
  let+ astate = check_addr_access path Read location addr_hist astate in
  let cell_opt = AbductiveDomain.find_post_cell_opt (fst addr_hist) astate in
  let copy =
    (AbstractValue.mk_fresh (), ValueHistory.singleton (Assignment (location, timestamp)))
  in
  ( Option.value_map cell_opt ~default:astate ~f:(fun cell ->
        AbductiveDomain.set_post_cell path copy cell location astate )
  , copy )


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
    |> Option.value_map ~default:(Result.Ok ()) ~f:(fun attrs ->
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
                        ; astate } )
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
               ; astate } ) )
        else Ok () )
  in
  let+ () =
    let open Result.Monad_infix in
    check_address_of_cpp_temporary () >>= check_address_of_stack_variable
    |> AccessResult.of_result_f ~f:(fun _ -> ())
  in
  astate


let mark_address_of_cpp_temporary history variable address astate =
  AddressAttributes.add_one address (AddressOfCppTemporary (variable, history)) astate


let mark_address_of_stack_variable history variable location address astate =
  AddressAttributes.add_one address (AddressOfStackVariable (variable, location, history)) astate


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
  let unreachable_addrs = AbductiveDomain.get_unreachable_attributes astate' in
  let res =
    List.fold unreachable_addrs ~init:[] ~f:(fun res addr ->
        (let open IOption.Let_syntax in
        let* attrs = AbductiveDomain.AddressAttributes.find_opt addr astate in
        let* typ, _ = Attributes.get_dynamic_type_source_file attrs in
        let+ var = find_var_opt astate addr in
        (var, addr, typ) :: res)
        |> Option.value ~default:res )
  in
  List.map ~f:(fun (var, _, typ) -> (var, typ)) res


(** raised by [filter_live_addresses] to stop early when looking for leaks *)
exception NoLeak

let filter_live_addresses ~is_dead_root potential_leak_addrs astate =
  (* stop as soon as we find out that all locations that could potentially cause a leak are still
     live *)
  if AbstractValue.Set.is_empty potential_leak_addrs then raise_notrace NoLeak ;
  let potential_leaks = ref potential_leak_addrs in
  let mark_reachable addr =
    potential_leaks := AbstractValue.Set.remove addr !potential_leaks ;
    if AbstractValue.Set.is_empty !potential_leaks then raise_notrace NoLeak
  in
  let pre = (astate.AbductiveDomain.pre :> BaseDomain.t) in
  let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
  (* filter out addresses live in the post *)
  ignore
    (BaseDomain.GraphVisit.fold
       ~var_filter:(fun var -> not (is_dead_root var))
       post ~init:()
       ~f:(fun _ () addr _ ->
         mark_reachable addr ;
         Continue () )
       ~finish:(fun () -> ()) ) ;
  let collect_reachable_from addrs base_state =
    BaseDomain.GraphVisit.fold_from_addresses addrs base_state ~init:()
      ~already_visited:AbstractValue.Set.empty
      ~f:(fun () addr _ ->
        mark_reachable addr ;
        Continue () )
      ~finish:(fun () -> ())
    |> fst
  in
  (* any address reachable in the pre-condition is not dead as callers can still be holding on to
     them; so any address reachable from anything reachable from the precondition is live *)
  let reachable_in_pre =
    (* start from the *values* of variables, not their addresses; addresses of formals are
       meaningless for callers so are not reachable outside the current function *)
    let formal_values =
      BaseStack.to_seq pre.stack
      |> Seq.flat_map (fun (_, (formal_addr, _)) ->
             match BaseMemory.find_opt formal_addr pre.heap with
             | None ->
                 Seq.empty
             | Some edges ->
                 BaseMemory.Edges.to_seq edges
                 |> Seq.map (fun (_access, (value, _)) ->
                        mark_reachable value ;
                        value ) )
    in
    collect_reachable_from formal_values pre
  in
  let reachable_from_reachable_in_pre =
    collect_reachable_from (AbstractValue.Set.to_seq reachable_in_pre) post
  in
  ignore reachable_from_reachable_in_pre ;
  !potential_leaks


let mark_potential_leaks location ~dead_roots astate =
  let is_dead_root var = List.mem ~equal:Var.equal dead_roots var in
  (* only consider locations that could actually cause a leak if unreachable *)
  let allocated_reachable_from_dead_root =
    BaseDomain.reachable_addresses ~var_filter:is_dead_root
      (astate.AbductiveDomain.post :> BaseDomain.t)
    |> AbstractValue.Set.filter (fun addr ->
           AddressAttributes.get_allocation addr astate |> Option.is_some )
  in
  match filter_live_addresses ~is_dead_root allocated_reachable_from_dead_root astate with
  | exception NoLeak ->
      astate
  | potential_leaks ->
      (* delay reporting leak as to avoid false positives we need to massage the state some more;
         TODO: this can make use miss reporting memory leaks if another error is found *)
      AbstractValue.Set.fold
        (fun addr astate -> AddressAttributes.add_unreachable_at addr location astate)
        potential_leaks astate


let remove_vars vars location astate =
  let astate = mark_potential_leaks location ~dead_roots:vars astate in
  (* remember addresses that will marked invalid later *)
  let astate =
    List.fold vars ~init:astate ~f:(fun astate var ->
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
  Stack.remove_vars vars astate


let get_var_captured_actuals path location ~captured_formals ~actual_closure astate =
  let+ _, astate, captured_actuals =
    PulseResult.list_fold captured_formals ~init:(0, astate, [])
      ~f:(fun (id, astate, captured) (_, mode, typ) ->
        let+ astate, captured_actual =
          eval_access path Read location actual_closure
            (FieldAccess (Fieldname.mk_fake_capture_field ~id typ mode))
            astate
        in
        (id + 1, astate, (captured_actual, typ) :: captured) )
  in
  (* captured_actuals is currently in reverse order compared with the given
     captured_formals because it is built during the above fold (equivalent to a
     fold_left). We reverse it back to have a direct correspondece between the
     two lists' elements *)
  (astate, List.rev captured_actuals)


let get_closure_captured_actuals path location ~captured_actuals astate =
  let++ astate, captured_actuals =
    PulseOperationResult.list_fold captured_actuals ~init:(astate, [])
      ~f:(fun (astate, captured_actuals) (exp, _, typ, _) ->
        let++ astate, captured_actual = eval path Read location exp astate in
        (astate, (captured_actual, typ) :: captured_actuals) )
  in
  (* captured_actuals is currently in reverse order compared with its original
     order. We reverse it back to have it in the same order as the given
     captured_actuals and therefore not break the element-wise correspondence
     between the captured_actuals and captured_formals in the caller *)
  (astate, List.rev captured_actuals)


type call_kind =
  [ `Closure of (Exp.t * Pvar.t * Typ.t * CapturedVar.capture_mode) list
  | `Var of Ident.t
  | `ResolvedProcname ]

let get_captured_actuals procname path location ~captured_formals ~call_kind ~actuals astate =
  if
    Procname.is_objc_block procname
    || Procname.is_specialized_with_function_parameters procname
    || Procname.is_erlang procname
  then
    match call_kind with
    | `Closure captured_actuals ->
        get_closure_captured_actuals path location ~captured_actuals astate
    | `Var id ->
        let+* astate, actual_closure = eval path Read location (Exp.Var id) astate in
        get_var_captured_actuals path location ~captured_formals ~actual_closure astate
    | `ResolvedProcname ->
        Sat (Ok (astate, []))
  else
    match actuals with
    | (actual_closure, _) :: _ when not (List.is_empty captured_formals) ->
        Sat
          ((* Assumption: the first parameter will be a closure *)
           let* astate, actual_closure =
             eval_access path Read location actual_closure Dereference astate
           in
           get_var_captured_actuals path location ~captured_formals ~actual_closure astate )
    | _ ->
        Sat (Ok (astate, []))
