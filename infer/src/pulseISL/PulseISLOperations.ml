(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
open IResult.Let_syntax
open PulseISLBasicInterface
open PulseISLDomainInterface

type t = AbductiveDomain.t

type 'a access_result = ('a, Diagnostic.t * t) result

let ok_continue post = Ok [ExecutionDomain.ContinueProgram post]

(** Check that the [address] is not known to be invalid *)
let check_addr_access location (address, history) astate =
  let access_trace = Trace.Immediate {location; history} in
  AddressAttributes.check_valid access_trace address astate
  |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
         (Diagnostic.AccessToInvalidAddress {invalidation; invalidation_trace; access_trace}, astate)
         )

let check_and_abduce_addr_access procname location (address, history) ?null_noop:(null_noop=false) astate =
  let access_trace = Trace.Immediate {location; history} in
  AddressAttributes.check_valid_and_abduce procname access_trace address ~null_noop:null_noop (PulseISLArithmetic.is_known_neq_zero astate address) (PulseISLArithmetic.is_known_zero astate address) astate
  |> Result.map_error ~f:(fun (invalidation, invalidation_trace, astate) ->
         (Diagnostic.AccessToInvalidAddress {invalidation; invalidation_trace; access_trace}, astate)
           )

module Closures = struct
  module Memory = AbductiveDomain.Memory

  let fake_capture_field_prefix = "__capture_"

  let mk_fake_field ~id =
    Fieldname.make
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "function"]))
      (Printf.sprintf "%s%d" fake_capture_field_prefix id)


  let is_captured_fake_access (access : _ HilExp.Access.t) =
    match access with
    | FieldAccess fieldname
      when String.is_prefix ~prefix:fake_capture_field_prefix (Fieldname.to_string fieldname) ->
        true
    | _ ->
        false


  let mk_capture_edges captured =
    List.foldi captured ~init:Memory.Edges.empty ~f:(fun id edges captured_addr_trace ->
        Memory.Edges.add (HilExp.Access.FieldAccess (mk_fake_field ~id)) captured_addr_trace edges
    )


  let check_captured_addresses action lambda_addr (astate : t) =
    match AbductiveDomain.find_post_cell_opt lambda_addr astate with
    | None ->
        Ok astate
    | Some (edges, attributes) ->
        let+ () =
          IContainer.iter_result ~fold:Attributes.fold attributes ~f:(function
            | Attribute.Closure _ ->
                IContainer.iter_result ~fold:Memory.Edges.fold edges
                  ~f:(fun (access, addr_trace) ->
                    if is_captured_fake_access access then
                      let+ _ = check_addr_access action addr_trace astate in
                      ()
                    else Ok () )
            | _ ->
                Ok () )
        in
        astate


  let record location pname captured astate =
    let captured_addresses =
      List.rev_filter_map captured
        ~f:(fun (captured_as, (address_captured, trace_captured), mode) ->
          match mode with
          | Pvar.ByValue  ->
              None
          | Pvar.ByReference ->
              let new_trace = ValueHistory.Capture {captured_as; location} :: trace_captured in
              Some (address_captured, new_trace) )
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

let eval_var location hist var astate =
  Stack.eval location hist var astate


let eval_access location addr_hist access astate =
  let+ astate = check_addr_access location addr_hist astate in
  Memory.eval_edge addr_hist access astate

let get_array_access addr ati astate =
  match Memory.find_edges_opt addr astate with
    | Some edges -> (
        let kv_op = List.find ~f:(fun (acc,_) ->
           match acc with
           | HilExp.Access.ArrayAccess (_, dest_addr) ->
              PulseISLArithmetic.is_equal_to astate dest_addr ati 
           | _ -> false) (Memory.Edges.bindings edges)
        in match kv_op with
           | None -> None
           | Some (acc, _) ->
              (match acc with
               | HilExp.Access.ArrayAccess (_, dest_addr) ->
                    Some dest_addr
               | _ ->
                  None))
    | None ->  None

let update_abducible_var lhs_var address astate=
  if BaseDomain.is_reachable address astate.AbductiveDomain.abducible_vars (astate.AbductiveDomain.post :> BaseDomain.t) then
      AbductiveDomain.add_abducible_var lhs_var astate
  else astate

let eval location exp0 astate =
  let rec eval exp astate =
    match (exp : Exp.t) with
    | Var id ->
         Ok (eval_var location (* error in case of missing history? *) [] (Var.of_id id) astate)
    | Lvar pvar ->
        Ok (eval_var location [ValueHistory.VariableAccessed (pvar, location)]  (Var.of_pvar pvar) astate)
    | Lfield (exp', field, _) ->
        let* astate, addr_hist = eval exp' astate in
        let+ astate = check_addr_access location addr_hist astate in
        Memory.eval_edge addr_hist (FieldAccess field) astate
    | Lindex (exp', exp_index) ->
        let* astate, addr_hist_index = eval exp_index astate in
        let* astate, addr_hist = eval exp' astate in
        let+ astate = check_addr_access location addr_hist astate in
        Memory.eval_edge addr_hist (ArrayAccess (Typ.void, fst addr_hist_index)) astate
    | Closure {name; captured_vars} ->
        let+ astate, rev_captured =
          List.fold_result captured_vars ~init:(astate, [])
            ~f:(fun (astate, rev_captured) (capt_exp, captured_as, _ , mode) ->
              let+ astate, addr_trace = eval capt_exp astate in
              (astate, (captured_as, addr_trace, mode) :: rev_captured) )
        in
        Closures.record location name (List.rev rev_captured) astate
    | Cast (_, exp') ->
        eval exp' astate
    | Const (Cint i) ->
        let v = AbstractValue.Constants.get_int i in
        let astate =
          PulseISLArithmetic.and_eq_int v i astate
          |> AddressAttributes.invalidate
               (v, [ValueHistory.Assignment location])
               (ConstantDereference i) location
        in
        Ok (astate, (v, []))
    | UnOp (unop, exp, _typ) ->
        let+ astate, (addr, hist) = eval exp astate in
        let unop_addr = AbstractValue.mk_fresh () in
        (PulseISLArithmetic.eval_unop unop_addr unop addr astate, (unop_addr, hist))
    | BinOp (bop, e_lhs, e_rhs) ->
        let* astate, (addr_lhs, hist_lhs) = eval e_lhs astate in
        (* NOTE: keeping track of only [hist_lhs] into the binop is not the best *)
        let+ astate, (addr_rhs, _hist_rhs) = eval e_rhs astate in
        let binop_addr = AbstractValue.mk_fresh () in
        ( PulseISLArithmetic.eval_binop binop_addr bop (AbstractValueOperand addr_lhs)
            (AbstractValueOperand addr_rhs) astate
        , (binop_addr, hist_lhs) )
    | Const _ | Sizeof _ | Exn _ ->
        Ok (astate, (AbstractValue.mk_fresh (), (* TODO history *) []))
  in
  eval exp0 astate


let eval_to_operand location exp astate =
  match (exp : Exp.t) with
  | Const (Cint i) ->
      Ok (astate, PulseISLArithmetic.LiteralOperand i)
  | exp ->
      let+ astate, (value, _) = eval location exp astate in
      (astate, PulseISLArithmetic.AbstractValueOperand value)


let prune location ~condition astate =
  let rec prune_aux ~negated exp astate =
    match (exp : Exp.t) with
    | BinOp (bop, exp_lhs, exp_rhs) ->
        let* astate, lhs_op = eval_to_operand location exp_lhs astate in
        let+ astate, rhs_op = eval_to_operand location exp_rhs astate in
        PulseISLArithmetic.prune_binop ~negated bop lhs_op rhs_op astate
    | UnOp (LNot, exp', _) ->
        prune_aux ~negated:(not negated) exp' astate
    | exp ->
        prune_aux ~negated (Exp.BinOp (Ne, exp, Exp.zero)) astate
  in
  prune_aux ~negated:false condition astate


let eval_deref location exp astate =
  let* astate, addr_hist = eval location exp astate in
  let+ astate = check_addr_access location addr_hist astate in
  Memory.eval_edge addr_hist Dereference astate

let eval_deref_abduce procname location access addr_hist astate=
  let+ astates = check_and_abduce_addr_access procname location addr_hist astate in
  (List.map ~f:(fun ast ->
       match ast.AbductiveDomain.status with
       | Ok ->
          Memory.eval_edge addr_hist access ast
       | Er ->
          (ast, addr_hist)
       ) astates)

let eval_structure pdesc loc exp astate =
  let procname = Procdesc.get_proc_name pdesc in
  let map_ok access addr_hist astates=
    (List.map ~f:(fun ast ->
           match ast.AbductiveDomain.status with
             | Ok -> Memory.eval_edge addr_hist access ast
             | Er -> (ast, addr_hist)
         ) astates)
  in
  match (exp : Exp.t) with
  | Lfield (exp', field, _) ->
     let* astate, addr_hist = eval loc exp' astate in
     let+ astates = check_and_abduce_addr_access procname loc addr_hist astate in
     false, map_ok (HilExp.Access.FieldAccess field) addr_hist astates
  | Lindex (exp', exp_index) ->
     let* astate, addr_hist_index = eval loc exp_index astate in
     let* astate, addr_hist = eval loc exp' astate in
     let+ astates = check_and_abduce_addr_access procname loc addr_hist astate in
     false, map_ok (HilExp.Access.ArrayAccess (Typ.void, fst addr_hist_index)) addr_hist astates
  | _ -> (
       let+ (astate, (addr, history )) = eval loc exp astate in
       true, [(astate, (addr, history ))])
      
let eval_deref_biad pdesc location exp astate =
  let procname = Procdesc.get_proc_name pdesc in
  let* is_biad, ls_astate_addr_hist = eval_structure pdesc location exp astate in
  List.fold ls_astate_addr_hist ~init:(Ok [])
      ~f:(fun acc (astate, addr_hist) ->
          match acc with
          | Ok acc_astates ->
             (match astate.AbductiveDomain.status with
              | Ok ->
                 let+ astates =
                   if is_biad then eval_deref_abduce procname location Dereference addr_hist astate
                   else
                       let+ astate = eval_deref location exp astate in
                       [astate]
                 in
                 acc_astates@astates
              | Er -> Ok (acc_astates@[(astate, addr_hist)])
             )
          | Error _ as a -> a)
  
let add_attr_post addr attr astate=
  AbductiveDomain.AddressAttributes.add_one addr attr astate
  
let realloc_pvar pvar location astate =
  let n_addr = AbstractValue.mk_fresh () in
  let access_trace = Trace.Immediate {location; history=[]} in
  Stack.add (Var.of_pvar pvar)
      (n_addr, [ValueHistory.VariableDeclared (pvar, location)]) astate
  |> AbductiveDomain.Memory.register_address n_addr
  |>  add_attr_post n_addr (MustBeValid access_trace)


let write_id id ((addr, _) as new_addr_loc) astate =
  let var = (Var.of_id id) in
  update_abducible_var var addr astate
  |> Stack.add var new_addr_loc

let write_id_list id astate_new_addr_loc_list =
  List.map astate_new_addr_loc_list ~f:(fun (astate, new_addr_loc) ->
           write_id id new_addr_loc astate)

let havoc_id id loc_opt astate =
  if Stack.mem (Var.of_id id) astate then
      write_id id (AbstractValue.mk_fresh (), loc_opt) astate
  else astate


let write_access location addr_trace_ref access addr_trace_obj astate =
  check_addr_access location addr_trace_ref astate
  >>| Memory.add_edge addr_trace_ref access addr_trace_obj location

let check_memory_abdleak_attrs location astate result attributes=
  let allocated_not_freed_opt =
    Attributes.fold attributes ~init:(None (* allocation trace *), false (* freed *))
        ~f:(fun acc attr ->
            match (attr : Attribute.t) with
            | Allocated (procname, trace) ->
               (Some (procname, trace, true), snd acc)
            | AbdAllocated (procname, trace) ->
               (Some (procname, trace, false), snd acc)
            | Invalid (CFree, _) ->
               (fst acc, true)
            | _ ->
               acc )
  in 
  match allocated_not_freed_opt with
  | Some (procname, trace, is_intra), false ->
     (* abdallocated but not freed *)
     if is_intra then
         Error (Diagnostic.MemoryLeak {procname; location; allocation_trace= trace}, astate)
     else (
         match result with
         | Ok astate ->
            Ok (AbductiveDomain.set_status AbductiveDomain.PostStatus.Er astate)
         | _ ->
            result
     )
  | _ ->
     result
  
let check_memory_leak location unreachable_addrs astate=
  let check_memory_abdleak addr result =
    let opt_attributes = BaseAddressAttributes.find_opt addr (astate.AbductiveDomain.post :> BaseDomain.t).attrs in
    match opt_attributes with
      | None -> result
      | Some attributes -> check_memory_abdleak_attrs location astate result attributes
  in
  match astate.AbductiveDomain.status with
    | AbductiveDomain.PostStatus.Ok ->
       AbstractValue.Set.fold check_memory_abdleak unreachable_addrs (Ok astate)
    | AbductiveDomain.PostStatus.Er -> Ok astate

let write_access_update location addr_trace_ref access addr_trace_obj astate=
  let live_addresses = BaseDomain.reachable_addresses (astate.AbductiveDomain.post :> BaseDomain.t) in
  let astate = Memory.add_edge addr_trace_ref access addr_trace_obj location astate in
  let live_addresses_after = BaseDomain.reachable_addresses (astate.AbductiveDomain.post :> BaseDomain.t) in
  let unreachable_addrs = AbstractValue.Set.diff live_addresses live_addresses_after in
  check_memory_leak location unreachable_addrs astate
                                     
let write_access_biad procname location addr_trace_ref access addr_trace_obj astate =
  let rec map_rec astates f acc=
    match astates with
    | ast::rest ->
       (match f ast with
        | Ok astate ->
           map_rec rest f (acc@[astate])
        | Error r ->
           Error r
       )
    | [] ->
       Ok acc
  in
  match check_and_abduce_addr_access procname location addr_trace_ref astate with
  | Ok astates ->
     (map_rec astates (fun ast ->
              match ast.AbductiveDomain.status with
              | Ok ->
                   write_access_update location addr_trace_ref access addr_trace_obj ast
              | Er ->
                 Ok ast
          ) [] )
  | Error reason -> Error reason

let write_deref location ~ref:(addr_ref, addr_ref_history) ~obj:addr_trace_obj astate =
  write_access location (addr_ref, addr_ref_history) Dereference addr_trace_obj astate
  
let write_deref_biad procname location ~ref:(addr_ref, addr_ref_history) access ~obj:addr_trace_obj astate =
  write_access_biad procname location (addr_ref, addr_ref_history) access  addr_trace_obj astate
 
let write_field location ~ref:addr_trace_ref field ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref (FieldAccess field) addr_trace_obj astate


let write_arr_index location ~ref:addr_trace_ref ~index ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref (ArrayAccess (Typ.void, index)) addr_trace_obj astate


let havoc_field location addr_trace field trace_obj astate =
  write_field location ~ref:addr_trace field ~obj:(AbstractValue.mk_fresh (), trace_obj) astate

let invalidate_post location cause addr_trace astate =
  check_addr_access location addr_trace astate
  >>| AddressAttributes.invalidate addr_trace cause location

let invalidate procname location cause ?null_noop:(null_noop=false) (address, history) astate =
  let+ astates = check_and_abduce_addr_access procname location (address, history) ~null_noop:null_noop astate in
  (List.map ~f:(fun ast ->
           match ast.AbductiveDomain.status with
           | Ok ->
              AddressAttributes.invalidate (address, history) cause location ast
           | Er ->
              ast
       ) astates)

let allocate procname location addr_trace astate =
  AddressAttributes.allocate procname addr_trace location astate

let add_dynamic_type typ address astate =
  AddressAttributes.add_dynamic_type typ address astate

let remove_allocation_attr address astate =
  AddressAttributes.remove_allocation_attr address astate

let invalidate_access location cause ref_addr_hist access astate =
  let astate, (addr_obj, _) = Memory.eval_edge ref_addr_hist access astate in
  invalidate_post location cause (addr_obj, snd ref_addr_hist) astate


let invalidate_array_elements location cause addr_trace astate =
  let+ astate = check_addr_access location addr_trace astate in
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
  let+ astate = check_addr_access location addr_hist astate in
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
                     ( Diagnostic.StackVariableAddressEscape
                         {variable; location= escape_location; history}
                     , astate )
               | _ ->
                   Ok () ) )
  in
  let check_address_of_stack_variable () =
    let proc_name = Procdesc.get_proc_name proc_desc in
    IContainer.iter_result ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Stack.fold) astate
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
            (Diagnostic.StackVariableAddressEscape {variable; location= escape_location; history}
            , astate ) )
        else Ok () )
  in
  let+ () = check_address_of_cpp_temporary () >>= check_address_of_stack_variable in
  astate

let check_address_escape_list escape_location proc_desc address history astates =
  let rec helper astates=
    match astates with
      | ast::rest ->
         (match check_address_escape escape_location proc_desc address history ast with
          | Ok _ ->
             helper rest
          | Error x ->
             Error x)
      | [] ->
         Ok ()
  in
  let+ () = helper astates in
  astates
  
let mark_address_of_cpp_temporary history variable address astate =
  AddressAttributes.add_one address (AddressOfCppTemporary (variable, history)) astate


let mark_address_of_stack_variable history variable location address astate =
  AddressAttributes.add_one address (AddressOfStackVariable (variable, location, history)) astate

let check_memory_leak_unreachable unreachable_addrs location astate =
  let check_memory_leak result attributes = check_memory_abdleak_attrs location astate result attributes in
  match astate.AbductiveDomain.status with
  | AbductiveDomain.PostStatus.Ok ->
     List.fold unreachable_addrs ~init:(Ok astate) ~f:(fun res addr ->
      match AbductiveDomain.AddressAttributes.find_opt addr astate with
      | Some unreachable_attrs ->
          check_memory_leak res unreachable_attrs
      | None ->
          res)
  | AbductiveDomain.PostStatus.Er -> Ok astate


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


let remove_vars (* pdesc *) vars location orig_astate =
  let astate =
    List.fold vars ~init:orig_astate ~f:(fun astate var ->
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
  if phys_equal astate' astate then Ok [astate]
  else
    let astate, live_addresses, unreachable_addrs = AbductiveDomain.discard_unreachable astate' in
    let+ _ = check_memory_leak_unreachable unreachable_addrs location orig_astate in
    (* discard aduction errors on local vars *)
    (match astate.AbductiveDomain.status with
     | PostStatus.Er when not (BaseAddressAttributes.exist_invalid live_addresses (astate.pre :> BaseDomain.t).attrs) ->
          []
     | _ ->
        [astate])


let is_ptr_to_const formal_typ_opt =
  Option.value_map formal_typ_opt ~default:false ~f:(fun (formal_typ : Typ.t) ->
      match formal_typ.desc with Typ.Tptr (t, _) -> Typ.is_const t.quals | _ -> false )


let unknown_call call_loc reason ~ret ~actuals ~formals_opt astate =
  let event = ValueHistory.Call {f= reason; location= call_loc; in_call= []} in
  let havoc_ret (ret, _) astate = havoc_id ret [event] astate in
  let havoc_actual_if_ptr (actual, actual_typ) formal_typ_opt astate =
    (* We should not havoc when the corresponding formal is a
       pointer to const *)
    if
      (not (Language.curr_language_is Java))
      && Typ.is_pointer actual_typ
      && not (is_ptr_to_const formal_typ_opt)
    then
      (* HACK: write through the pointer even if it is invalid (except in Java). This is to avoid raising issues when
         havoc'ing pointer parameters (which normally causes a [check_valid] call. *)
      let fresh_value = AbstractValue.mk_fresh () in
      Memory.add_edge actual Dereference (fresh_value, [event]) call_loc astate
      |> add_attr_post fresh_value (MustBeValid (Trace.Immediate {location=call_loc; history=[]}))
    else astate
  in
  let add_skipped_proc astate =
    match reason with
    | CallEvent.SkippedKnownCall proc_name ->
        AbductiveDomain.add_skipped_call proc_name
          (Trace.Immediate {location= call_loc; history= []})
          astate
    | _ ->
        astate
  in
  L.d_printfln "skipping unknown procedure@." ;
  ( match formals_opt with
  | None ->
      List.fold actuals
        ~f:(fun astate actual_typ -> havoc_actual_if_ptr actual_typ None astate)
        ~init:astate
  | Some formals -> (
    match
      List.fold2 actuals formals
        ~f:(fun astate actual_typ (_, formal_typ) ->
          havoc_actual_if_ptr actual_typ (Some formal_typ) astate )
        ~init:astate
    with
    | Unequal_lengths ->
        L.d_printfln "ERROR: formals have length %d but actuals have length %d"
          (List.length formals) (List.length actuals) ;
        astate
    | Ok result ->
        result ) )
  |> havoc_ret ret |> add_skipped_proc


let apply_callee callee_pname call_loc callee_exec_state ~ret ~formals ~actuals astate =
  let apply callee_prepost ~f =
    PulseISLInterproc.apply_prepost callee_pname call_loc ~callee_prepost ~formals ~actuals astate
    >>| function
    | None ->
        (* couldn't apply pre/post pair *) None
    | Some (post, return_val_opt, subst) ->
       let event = ValueHistory.Call {f= Call callee_pname; location= call_loc; in_call= []} in
       let post =
         match return_val_opt with
           | Some (return_val, return_hist) ->
              write_id (fst ret) (return_val, event :: return_hist) post
           | None ->
              havoc_id (fst ret) [event] post
       in
       Some ((f post), subst)
  in
  let open ExecutionDomain in
  match callee_exec_state with
  | AbortProgram _ ->
      (* Callee has failed; don't propagate the failure *)
      Ok (Some (callee_exec_state, AbstractValue.Map.empty))
  | ContinueProgram astate ->
      apply astate ~f:(fun astate -> ContinueProgram astate)
  | ExitProgram astate ->
      apply astate ~f:(fun astate -> ExitProgram astate)

let check_all_invalid callees_callers_match callee_proc_name call_location astate =
  (match astate.AbductiveDomain.status with
   | AbductiveDomain.PostStatus.Ok -> Ok ()
   | AbductiveDomain.PostStatus.Er -> (
       AbstractValue.Map.fold
           (fun _ ((addr_caller, hist_caller)) astate_result ->
               match astate_result with
               | Error _ ->
                  astate_result
               | Ok () -> (
                   match BaseAddressAttributes.get_invalid addr_caller (astate.AbductiveDomain.post :> BaseDomain.t).attrs with
                   | None ->  astate_result
                   | Some (invalidation, callee_access_trace) ->
                      let access_trace =
                        Trace.ViaCall
                            { in_call= callee_access_trace
                            ; f= Call callee_proc_name
                            ; location= call_location
                            ; history= hist_caller }
                      in Error (invalidation, callee_access_trace, access_trace, addr_caller)
            ) )
            callees_callers_match (Ok ()))
  )
  |> Result.map_error ~f:(fun (invalidation, invalidation_trace, access_trace, _) ->
     ( Diagnostic.AccessToInvalidAddress
                       {invalidation; invalidation_trace; access_trace}
     , astate ) )

let check_all_states_all_invalid callee_proc_name call_location (astates, args_matches) =
  let rec map_error astates args_matches acc=
    match astates,args_matches with
    | astate::rest1, subst::rest2 -> (
        match astate with
        | ExecutionDomain.ContinueProgram astate -> (
            match check_all_invalid subst callee_proc_name call_location astate with
            | Ok () ->
               Ok ()
            | (Error _) as x ->
               map_error rest1 rest2 x)
        | _ -> Ok ()
    )
    | [], [] -> acc
    | _ -> assert false
  in
  map_error astates args_matches (Ok ())
  
let call ~callee_data call_loc callee_pname ~ret ~actuals ~formals_opt (astate : AbductiveDomain.t)
    : (ExecutionDomain.t list, Diagnostic.t * t) result =
  match callee_data with
  | Some (callee_proc_desc, exec_states) -> (
      let formals =
        Procdesc.get_formals callee_proc_desc
        |> List.map ~f:(fun (mangled, _) -> Pvar.mk mangled callee_pname |> Var.of_pvar)
      in
      let is_blacklist =
        Option.exists Config.pil_cut_to_one_path_procedures_pattern ~f:(fun regex ->
            Str.string_match regex (Procname.to_string callee_pname) 0 )
      in
      (* call {!AbductiveDomain.PrePost.apply} on each pre/post pair in the summary. *)
      let* astates, args_matches  = IContainer.fold_result_until exec_states ~fold:List.fold ~init:([], [])
        ~f:(fun (posts, args_matches) callee_exec_state ->
          (* apply all pre/post specs *)
          match
            apply_callee callee_pname call_loc callee_exec_state ~formals ~actuals ~ret astate
          with
          | Ok None ->
              (* couldn't apply pre/post pair *)
              Continue (Ok (posts, args_matches))
          | Ok (Some (post, a)) when is_blacklist ->
              L.d_printfln "Keep only one disjunct because %a is in blacklist" Procname.pp
                callee_pname ;
              Stop ([post], [a])
          | Ok (Some (post, subst)) ->
              Continue (Ok (post :: posts, subst :: args_matches))
          | Error _ as x ->
              Continue x )
        ~finish:(fun x -> x) in
      match check_all_states_all_invalid (* actuals *) callee_pname call_loc (astates, args_matches) with
          | Ok () ->  Ok astates
          | (Error _) as x -> x        
  )
  | None ->
      (* no spec found for some reason (unknown function, ...) *)
      L.d_printfln "No spec found for %a@\n" Procname.pp callee_pname ;
      unknown_call call_loc (SkippedKnownCall callee_pname) ~ret ~actuals ~formals_opt astate
      |> ok_continue
