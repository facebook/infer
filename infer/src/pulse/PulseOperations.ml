(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
open IResult.Let_syntax
open PulseBasicInterface
open PulseDomainInterface

type t = AbductiveDomain.t

type 'a access_result = ('a, Diagnostic.t * t) result

let ok_continue post = Ok [ExecutionDomain.ContinueProgram post]

let check_addr_access location (address, history) astate =
  let access_trace = Trace.Immediate {location; history} in
  AddressAttributes.check_valid access_trace address astate
  |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
         ( Diagnostic.AccessToInvalidAddress
             {calling_context= []; invalidation; invalidation_trace; access_trace}
         , astate ) )

(* let is_top_post astate=
 *   let imm_params = astate.AbductiveDomain.imm_params in
 *   if Var.Set.is_empty imm_params then
 *     true
 *   else
 *     let imm_addresses = BaseDomain.reachable_addresses_from (fun var -> Var.Set.mem var imm_params) (astate.AbductiveDomain.post :> BaseDomain.t) in
 *     let imm_addresses = AbstractValue.Set.diff imm_addresses astate.AbductiveDomain.mod_addrs in
 *     let post_pc = PathCondition.simplify ~keep:imm_addresses astate.AbductiveDomain.path_condition in
 *     PathCondition.is_true post_pc *)

let is_top_pre astate=
  let pre_formal_addresses = BaseDomain.reachable_addresses_from (fun var -> Var.Set.mem var astate.AbductiveDomain.nonref_formals) (astate.AbductiveDomain.pre :> BaseDomain.t) in
  BaseAddressAttributes.is_empty_heap_attrs pre_formal_addresses (astate.AbductiveDomain.pre :> BaseDomain.t).attrs && PulseArithmetic.has_no_assumptions (astate :> AbductiveDomain.t)

let is_must_bug astate=
  is_top_pre astate (* && is_top_post astate *)
  
let check_and_abduce_addr_access procname location (address, history) ?null_noop:(null_noop=false) astate =
  let access_trace = Trace.Immediate {location; history} in
  (* let+ astates = *)
  let res = AddressAttributes.check_valid_and_abduce procname access_trace address ~null_noop:null_noop (PulseArithmetic.is_known_neq_zero astate address) (PulseArithmetic.is_known_zero astate address) astate in
  match res with
    | Error (invalidation, invalidation_trace, astate) ->
       if is_must_bug astate then
           Error (Diagnostic.AccessToInvalidAddress {calling_context= []; invalidation; invalidation_trace; access_trace}, astate)
       else Ok [AddressAttributes.add_one address (Attribute.Invalid (invalidation, access_trace)) astate]
    | Ok astates ->
       Ok astates

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
        Memory.Edges.add (HilExp.Access.FieldAccess (mk_fake_field ~id)) captured_addr_trace edges )


  let check_captured_addresses action lambda_addr (astate : t) =
    match AbductiveDomain.find_post_cell_opt lambda_addr astate with
    | None ->
        Ok astate
    | Some (edges, attributes) ->
        let+ () =
          IContainer.iter_result ~fold:Attributes.fold attributes ~f:(function
            | Attribute.Closure _ ->
                IContainer.iter_result ~fold:Memory.Edges.fold edges ~f:(fun (access, addr_trace) ->
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
      List.filter_map captured ~f:(fun (captured_as, (address_captured, trace_captured), mode) ->
          let new_trace = ValueHistory.Capture {captured_as; mode; location} :: trace_captured in
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

let eval_var location hist var astate = Stack.eval location hist var astate


let eval_access location addr_hist access astate =
  let+ astate = check_addr_access location addr_hist astate in
  Memory.eval_edge addr_hist access astate

let get_array_access addr ati astate =
  match Memory.find_edges_opt addr astate with
  | Some edges -> (
      let kv_op = List.find ~f:(fun (acc,_) -> match acc with
          | HilExp.Access.ArrayAccess (_, dest_addr) -> PulseArithmetic.is_equal_to astate dest_addr ati 
          | _ -> false) (Memory.Edges.bindings edges)
      in match kv_op with
      | None -> None
      | Some (acc, _) -> ( match acc with
          | HilExp.Access.ArrayAccess (_, dest_addr) -> Some dest_addr
          | _ -> None)
    )
  | None ->  None

let eval location exp0 astate =
  let rec eval exp astate =
    match (exp : Exp.t) with
    | Var id ->
        Ok (eval_var location (* error in case of missing history? *) [] (Var.of_id id) astate)
    | Lvar pvar ->
        Ok (eval_var location [ValueHistory.VariableAccessed (pvar, location)] (Var.of_pvar pvar) astate)
    | Lfield (exp', field, _) ->
        let* astate, addr_hist = eval exp' astate in
        let+ astate = check_addr_access location addr_hist astate in
        Memory.eval_edge addr_hist (FieldAccess field) astate
    | Lindex (exp', exp_index) ->
        let* astate, addr_hist_index = eval exp_index astate in
        let* astate, addr_hist = eval exp' astate in
        let+ astate = check_addr_access location addr_hist astate in
        Memory.eval_edge addr_hist (ArrayAccess (StdTyp.void, fst addr_hist_index)) astate
    | Closure {name; captured_vars} ->
        let+ astate, rev_captured =
          List.fold_result captured_vars ~init:(astate, [])
            ~f:(fun (astate, rev_captured) (capt_exp, captured_as, _, mode) ->
                let+ astate, addr_trace = eval capt_exp astate in
                (astate, (captured_as, addr_trace, mode) :: rev_captured) )
        in
        Closures.record location name (List.rev rev_captured) astate
    | Cast (_, exp') ->
        eval exp' astate
    | Const (Cint i) ->
        let v = AbstractValue.Constants.get_int i in
        let astate =
          PulseArithmetic.and_eq_int v i astate
          |> AddressAttributes.invalidate
               (v, [ValueHistory.Assignment location])
               (ConstantDereference i) location
        in
        Ok (astate, (v, []))
    | UnOp (unop, exp, _typ) ->
        let+ astate, (addr, hist) = eval exp astate in
        let unop_addr = AbstractValue.mk_fresh () in
        (PulseArithmetic.eval_unop unop_addr unop addr astate, (unop_addr, hist))
    | BinOp (bop, e_lhs, e_rhs) ->
        let* astate, (addr_lhs, hist_lhs) = eval e_lhs astate in
        (* NOTE: keeping track of only [hist_lhs] into the binop is not the best *)
        let+ astate, (addr_rhs, _hist_rhs) = eval e_rhs astate in
        let binop_addr = AbstractValue.mk_fresh () in
        ( PulseArithmetic.eval_binop binop_addr bop (AbstractValueOperand addr_lhs)
            (AbstractValueOperand addr_rhs) astate
        , (binop_addr, hist_lhs) )
    | Const _ | Sizeof _ | Exn _ ->
        Ok (astate, (AbstractValue.mk_fresh (), (* TODO history *) []))
  in
  eval exp0 astate


let eval_to_operand location exp astate =
  match (exp : Exp.t) with
  | Const (Cint i) ->
      Ok (astate, PulseArithmetic.LiteralOperand i)
  | exp ->
      let+ astate, (value, _) = eval location exp astate in
      (astate, PulseArithmetic.AbstractValueOperand value)


let prune location ~condition astate =
  let rec prune_aux ~negated exp astate =
    match (exp : Exp.t) with
    | BinOp (bop, exp_lhs, exp_rhs) ->
        let* astate, lhs_op = eval_to_operand location exp_lhs astate in
        let+ astate, rhs_op = eval_to_operand location exp_rhs astate in
        PulseArithmetic.prune_binop ~negated bop lhs_op rhs_op astate
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

let eval_deref_abduce pname location access addr_hist astate=
  let+ astates = check_and_abduce_addr_access pname location addr_hist astate in
  (List.map ~f:(fun ast ->
           match ast.AbductiveDomain.status with
           | Ok -> Memory.eval_edge addr_hist access ast
           | Er _ -> (ast, addr_hist)
       ) astates)

let eval_structure pdesc loc exp astate =
  let procname = Procdesc.get_proc_name pdesc in
  let map_ok access addr_hist astates=
    (List.map ~f:(fun ast ->
           match ast.AbductiveDomain.status with
             | Ok -> Memory.eval_edge addr_hist access ast
             | Er _ -> (ast, addr_hist)
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
     true, [(astate, (addr, history ))]
   )
      
let eval_deref_biad pdesc location exp astate =
  let pname = Procdesc.get_proc_name pdesc in
  let* is_biad, ls_astate_addr_hist = eval_structure pdesc location exp astate in
  List.fold ls_astate_addr_hist ~init:(Ok [])
      ~f:(fun acc (astate, addr_hist) ->
          match acc with
            | Ok acc_astates ->
               (match astate.AbductiveDomain.status with
                  | Ok ->
                     let+ astates =
                       if is_biad then eval_deref_abduce pname location Dereference addr_hist astate
                       else
                           let+ astate = eval_deref location exp astate in
                           [astate]
                     in
                     acc_astates@astates
                  | Er _ -> Ok (acc_astates@[(astate, addr_hist)])
               )
            | Error _ as a -> a)
let add_attr_post addr attr astate=
  AbductiveDomain.AddressAttributes.add_one addr attr astate
  
let realloc_pvar pvar location astate =
  if not Config.pulse_isl then
    Stack.add (Var.of_pvar pvar)
      (AbstractValue.mk_fresh (), [ValueHistory.VariableDeclared (pvar, location)])
      astate
   else
     (if Var.Set.mem (Var.of_pvar pvar) astate.AbductiveDomain.local_ptrvars then
       astate
     else
       let n_addr = AbstractValue.mk_fresh () in
       let access_trace = Trace.Immediate {location; history=[]} in
       Stack.add (Var.of_pvar pvar)
          (n_addr, [ValueHistory.VariableDeclared (pvar, location)]) astate
       |> AbductiveDomain.Memory.register_address n_addr
       |>  add_attr_post n_addr (MustBeValid access_trace))

let write_id id new_addr_loc astate = Stack.add (Var.of_id id) new_addr_loc astate

let write_id_list id astate_new_addr_loc_list =
  List.map astate_new_addr_loc_list ~f:(fun (astate, new_addr_loc) ->
           write_id id new_addr_loc astate)

let havoc_id id loc_opt astate =
  if Stack.mem (Var.of_id id) astate then write_id id (AbstractValue.mk_fresh (), loc_opt) astate
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
       let err = Diagnostic.MemoryLeak {procname; location; allocation_trace= trace} in
       if is_intra then
           if is_must_bug astate then
               Error (err, astate)
           else Ok (AbductiveDomain.set_status (AbductiveDomain.PostStatus.Er (Some err)) astate)
       else (
           match result with
             | Ok astate ->
                Ok (AbductiveDomain.set_status (AbductiveDomain.PostStatus.Er (Some err)) astate)
             | _ -> result
       )
    | _ ->
       result
  
let check_memory_leak pname location unreachable_addrs astate=
  if Procname.is_java pname then
      Ok astate
  else
      (* ignore pointer arithmetic for now *)
      let unreachable_addrs = AbstractValue.Set.diff unreachable_addrs (PathCondition.get_variables astate.AbductiveDomain.path_condition) in
      let check_memory_abdleak addr result =
        let opt_attributes = BaseAddressAttributes.find_opt addr (astate.AbductiveDomain.post :> BaseDomain.t).attrs in
        match opt_attributes with
        | None -> result
        | Some attributes -> check_memory_abdleak_attrs location astate result attributes
      in
      match astate.AbductiveDomain.status with
        | AbductiveDomain.PostStatus.Ok ->
           AbstractValue.Set.fold check_memory_abdleak unreachable_addrs (Ok astate)
        | AbductiveDomain.PostStatus.Er _ -> Ok astate

let write_access_update pname location addr_trace_ref access addr_trace_obj astate=
  let live_addresses = BaseDomain.reachable_addresses_from Var.appears_in_source_code (astate.AbductiveDomain.post :> BaseDomain.t) in
  let astate = Memory.add_edge addr_trace_ref access addr_trace_obj location astate
             (* |> Memory.map_pre_heap ~f:(BaseMemory.add_edge (fst addr_trace_ref) access addr_trace_obj) *)  in
  let live_addresses_after = BaseDomain.reachable_addresses_from Var.appears_in_source_code (astate.AbductiveDomain.post :> BaseDomain.t) in
  let unreachable_addrs = AbstractValue.Set.diff live_addresses live_addresses_after in
  check_memory_leak pname location unreachable_addrs astate
                                     
let write_access_biad pname location addr_trace_ref access addr_trace_obj astate =
  let rec map_rec astates f acc=
    match astates with
      | ast::rest ->
         ( match f ast with
             | Ok astate -> map_rec rest f (acc@[astate])
             | Error r -> Error r
         )
      | [] -> Ok acc
  in
  match check_and_abduce_addr_access pname location addr_trace_ref astate with
    | Ok astates -> (map_rec astates (fun ast ->
           match ast.AbductiveDomain.status with
             | Ok -> write_access_update pname location addr_trace_ref access addr_trace_obj ast
             | Er _ -> Ok ast
                         ) [] )
    | Error reason -> Error reason

let write_deref location ~ref:addr_trace_ref ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref Dereference addr_trace_obj astate

let write_deref_biad pdesc location ~ref:(addr_ref, addr_ref_history) access ~obj:addr_trace_obj astate =
  write_access_biad pdesc location (addr_ref, addr_ref_history) access  addr_trace_obj astate

let write_field location ~ref:addr_trace_ref field ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref (FieldAccess field) addr_trace_obj astate


let write_arr_index location ~ref:addr_trace_ref ~index ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref (ArrayAccess (StdTyp.void, index)) addr_trace_obj astate


let havoc_field location addr_trace field trace_obj astate =
  write_field location ~ref:addr_trace field ~obj:(AbstractValue.mk_fresh (), trace_obj) astate

let invalidate location cause addr_trace astate =
  check_addr_access location addr_trace astate
  >>| AddressAttributes.invalidate addr_trace cause location

let invalidate_biad procname location cause ?null_noop:(null_noop=false) (address, history) astate =
  let+ astates = check_and_abduce_addr_access procname location (address, history) ~null_noop:null_noop astate in
  (List.map ~f:(fun ast ->
           match ast.AbductiveDomain.status with
             | Ok -> AddressAttributes.invalidate (address, history) cause location ast
             | Er _ -> ast
       ) astates)


let allocate procname location addr_trace astate =
  AddressAttributes.allocate procname addr_trace location astate


let add_dynamic_type typ address astate = AddressAttributes.add_dynamic_type typ address astate

let remove_allocation_attr address astate = AddressAttributes.remove_allocation_attr address astate

let invalidate_access location cause ref_addr_hist access astate =
  let astate, (addr_obj, _) = Memory.eval_edge ref_addr_hist access astate in
  invalidate location cause (addr_obj, snd ref_addr_hist) astate


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
            ( Diagnostic.StackVariableAddressEscape {variable; location= escape_location; history}
            , astate ) )
        else Ok () )
  in
  let+ () = check_address_of_cpp_temporary () >>= check_address_of_stack_variable in
  astate

let check_address_escape_list escape_location proc_desc address history astates =
  let rec helper astates=
    match astates with
    | ast::rest -> (match check_address_escape escape_location proc_desc address history ast with
                    | Ok _ -> helper rest
                    | Error x -> Error x
    )
    | [] -> Ok ()
  in
  let+ () = helper astates in
  astates
  
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
        Error (Diagnostic.MemoryLeak {procname; location; allocation_trace= trace}, astate)
    | _ ->
        result
  in
  List.fold unreachable_addrs ~init:(Ok ()) ~f:(fun res addr ->
      match AbductiveDomain.AddressAttributes.find_opt addr astate with
      | Some unreachable_attrs ->
          check_memory_leak res unreachable_attrs
      | None ->
          res )

let check_memory_leak_unreachable_comb pname unreachable_addrs location astate =
  if Procname.is_java pname then
      Ok astate
  else
      let check_memory_leak result attributes = check_memory_abdleak_attrs location astate result attributes in
      match astate.AbductiveDomain.status with
      | AbductiveDomain.PostStatus.Ok ->
         List.fold unreachable_addrs (* ~init:(Ok ()) *) ~init:(Ok astate) ~f:(fun res addr ->
                 match AbductiveDomain.AddressAttributes.find_opt addr astate with
                 | Some unreachable_attrs ->
                    check_memory_leak res unreachable_attrs
                 | None ->
                    res )
      | AbductiveDomain.PostStatus.Er _ -> Ok astate

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


let remove_vars pname vars location orig_astate =
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
    (let astate, live_addresses, unreachable_addrs = AbductiveDomain.discard_unreachable astate' in
    if not Config.pulse_isl then
      let+ () = check_memory_leak_unreachable unreachable_addrs location orig_astate in
      [astate]
    else
      (let+ _ = check_memory_leak_unreachable_comb pname unreachable_addrs location orig_astate in
      (* discard aduction errors on local vars *)
      (match astate.AbductiveDomain.status with
       | AbductiveDomain.PostStatus.Er _ when not (BaseAddressAttributes.exist_invalid live_addresses (astate.AbductiveDomain.pre :> BaseDomain.t).attrs) -> []
       | _ ->  [astate])))


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


let apply_callee ~caller_proc_desc callee_pname call_loc callee_exec_state ~ret
    ~captured_vars_with_actuals ~formals ~actuals astate =
  let apply callee_prepost ~f =
    PulseInterproc.apply_prepost callee_pname call_loc ~callee_prepost ~captured_vars_with_actuals
      ~formals ~actuals astate
    >>= function
    | None ->
        (* couldn't apply pre/post pair *) Ok None
    | Some (post, return_val_opt, subst) ->
        let event = ValueHistory.Call {f= Call callee_pname; location= call_loc; in_call= []} in
        let post =
          match return_val_opt with
          | Some (return_val, return_hist) ->
              write_id (fst ret) (return_val, event :: return_hist) post
          | None ->
              havoc_id (fst ret) [event] post
        in
        (f post subst)
  in
  let open ExecutionDomain in
  match callee_exec_state with
  | AbortProgram astate ->
      apply
        (astate :> AbductiveDomain.t)
        ~f:(fun astate ->
          let astate_summary = AbductiveDomain.summary_of_post caller_proc_desc astate in
          Ok (Some (AbortProgram astate_summary)) )
  | ContinueProgram astate ->
      apply astate ~f:(fun astate subst -> Ok (Some (ContinueProgram astate, subst)))
  | ExitProgram astate ->
      apply astate ~f:(fun astate subst -> Ok (Some (ExitProgram astate, subst)))
  | LatentAbortProgram {astate; latent_issue} ->
      apply
        (astate :> AbductiveDomain.t)
        ~f:(fun astate subst ->
          let astate_summary = AbductiveDomain.summary_of_post caller_proc_desc astate in
          if PulseArithmetic.is_unsat_cheap (astate_summary :> AbductiveDomain.t) then Ok None
          else
            let latent_issue =
              LatentIssue.add_call (CallEvent.Call callee_pname, call_loc) latent_issue
            in
            if LatentIssue.should_report astate_summary then
              Error (LatentIssue.to_diagnostic latent_issue, (astate_summary :> AbductiveDomain.t))
            else Ok (Some ((LatentAbortProgram {astate= astate_summary; latent_issue}), subst)))

let check_all_invalid callees_callers_match callee_proc_name call_location astate =
  let res = (match astate.AbductiveDomain.status with
    | AbductiveDomain.PostStatus.Ok -> Ok astate
    | AbductiveDomain.PostStatus.Er _ -> (
        AbstractValue.Map.fold
            (fun addr_caller hist_caller astate_result ->
               match astate_result with
               | Error _ ->
                   astate_result
               | Ok astate -> (
                   match BaseAddressAttributes.get_invalid addr_caller (astate.AbductiveDomain.post :> BaseDomain.t).attrs with
                   | None -> astate_result
                   | Some (invalidation, invalidation_trace) ->
                       let access_trace =
                         Trace.ViaCall
                           { in_call= invalidation_trace
                           ; f= Call callee_proc_name
                           ; location= call_location
                           ; history= hist_caller }
                       in
                       Error (invalidation, invalidation_trace, access_trace, addr_caller, astate)
                 ) )
            callees_callers_match (Ok astate))
    )
  in
  (* |> Result.map_error ~f:(fun (invalidation, invalidation_trace, access_trace, addr_caller) -> *)
  match res with
  | Error (invalidation, invalidation_trace, access_trace, addr_caller, astate) ->
      (L.d_printfln "ERROR: caller's %a er!" AbstractValue.pp addr_caller ;
       if is_must_bug astate then
         Error ( Diagnostic.AccessToInvalidAddress
                   {calling_context= []; invalidation; invalidation_trace; access_trace}
               , AbductiveDomain.set_status (AbductiveDomain.PostStatus.Er (Some (Diagnostic.AccessToInvalidAddress {calling_context= []; invalidation; invalidation_trace; access_trace}))) astate )
       else
         Ok (AbductiveDomain.set_status (AbductiveDomain.PostStatus.Er (Some (Diagnostic.AccessToInvalidAddress {calling_context= []; invalidation; invalidation_trace; access_trace}))) astate))
  | Ok astate ->
      Ok astate
(* ) *)

let check_imply_er callers_invalids callee_proc_name call_location call_astate =
  match call_astate with
  | ExecutionDomain.ContinueProgram astate -> (
        let+ astate = check_all_invalid callers_invalids callee_proc_name call_location astate in
        (ExecutionDomain.ContinueProgram astate)
    )
    | _ -> Ok call_astate
  
  
let get_captured_actuals location ~captured_vars ~actual_closure astate =
  let* astate, this_value_addr = eval_access location actual_closure Dereference astate in
  let+ _, astate, captured_vars_with_actuals =
    List.fold_result captured_vars ~init:(0, astate, []) ~f:(fun (id, astate, captured) var ->
        let+ astate, captured_actual =
          eval_access location this_value_addr (FieldAccess (Closures.mk_fake_field ~id)) astate
        in
        (id + 1, astate, (var, captured_actual) :: captured) )
  in
  (astate, captured_vars_with_actuals)


let call ~caller_proc_desc ~(callee_data : (Procdesc.t * PulseSummary.t) option) call_loc
    callee_pname ~ret ~actuals ~formals_opt (astate : AbductiveDomain.t) :
  (ExecutionDomain.t list, Diagnostic.t * t) result =
  match callee_data with
  | Some (callee_proc_desc, exec_states) -> (
      let formals =
        Procdesc.get_formals callee_proc_desc
        |> List.map ~f:(fun (mangled, _) -> Pvar.mk mangled callee_pname |> Var.of_pvar)
      in
      let captured_vars =
        Procdesc.get_captured callee_proc_desc
        |> List.map ~f:(fun (mangled, _, _) ->
            let pvar = Pvar.mk mangled callee_pname in
            Var.of_pvar pvar )
      in
      let* astate, captured_vars_with_actuals =
        match actuals with
        | (actual_closure, _) :: _
          when not (Procname.is_objc_block callee_pname || List.is_empty captured_vars) ->
            (* Assumption: the first parameter will be a closure *)
            get_captured_actuals call_loc ~captured_vars ~actual_closure astate
        | _ ->
            Ok (astate, [])
      in
      let is_blacklist =
        Option.exists Config.pulse_cut_to_one_path_procedures_pattern ~f:(fun regex ->
            Str.string_match regex (Procname.to_string callee_pname) 0 )
      in
      (* call {!AbductiveDomain.PrePost.apply} on each pre/post pair in the summary. *)
      IContainer.fold_result_until
        (exec_states :> ExecutionDomain.t list)
        ~fold:List.fold ~init:[]
        ~f:(fun posts callee_exec_state ->
          (* apply all pre/post specs *)
          match
            apply_callee ~caller_proc_desc callee_pname call_loc callee_exec_state
              ~captured_vars_with_actuals ~formals ~actuals ~ret astate
          with
          | Ok None ->
              (* couldn't apply pre/post pair *)
              Continue (Ok posts)
          | Ok (Some (post, invalids)) when is_blacklist ->
              (L.d_printfln "Keep only one disjunct because %a is in blacklist" Procname.pp
                callee_pname ;
              if not Config.pulse_isl then
                Stop [post]
              else
	       match check_imply_er invalids callee_pname call_loc post with
               | Ok post ->
                  Stop [post]
               | Error _ as x ->
                  Continue x)
          | Ok (Some (post, invalids)) ->
              (if not Config.pulse_isl then
                Continue (Ok (post :: posts))
              else
		(match check_imply_er invalids callee_pname call_loc post with
             | Ok post ->
                Continue (Ok (post :: posts))
             | Error _ as x ->
                Continue x))
          | Error _ as x ->
              Continue x )
        ~finish:(fun x -> x)
  )
  | None ->
      (* no spec found for some reason (unknown function, ...) *)
      L.d_printfln "No spec found for %a@\n" Procname.pp callee_pname ;
      unknown_call call_loc (SkippedKnownCall callee_pname) ~ret ~actuals ~formals_opt astate
      |> ok_continue

let merge_spec location astates=
  let extract_abd_attr addr_attrs formals base_domain=
    let pre_formal_addresses = BaseDomain.reachable_addresses_from (fun var -> Var.Set.mem var formals ) base_domain in
    BaseAddressAttributes.fold (fun addr attrs acc ->
            if AbstractValue.Set.mem addr pre_formal_addresses then
                BaseAddressAttributes.union addr attrs acc
            else
                acc
        ) base_domain.BaseDomain.attrs addr_attrs
  in
  let find_top_addrs addr_abd_attrs=
    BaseAddressAttributes.sfold (fun addr attrs acc_tops ->
        let abd_attr, null_attr, free_attr =
         Attribute.Set.fold
           (fun attr (acc_abd, acc_null, acc_free) ->(
                match attr with
                | Attribute.AbdAllocated _ ->
                    (true, acc_null, acc_free)
                | Attribute.Invalid (CFree, _) ->
                    (acc_abd, acc_null, true)
                | Attribute.Invalid (CppDelete, _) ->
                    (acc_abd, acc_null, true)
                | Attribute.Invalid (ConstantDereference i, _) when IntLit.iszero i ->
                    (acc_abd, true, acc_free)
                | _ ->
                    (acc_abd, acc_null, acc_free))) attrs (false, false, false)
        in
        if abd_attr && null_attr && free_attr then
          AbstractValue.Set.add addr acc_tops
        else
          acc_tops
      ) addr_abd_attrs AbstractValue.Set.empty
  in
  let extract_error acc er_astate=
    let add_non_duplicate er=
      if  List.exists acc ~f:(fun er' -> Diagnostic.equal er er') then
          acc
      else acc@[er]
    in
    match er_astate.AbductiveDomain.status with
      | Er (Some raise_err) ->
         add_non_duplicate raise_err
      | Er None ->
         (match BaseAddressAttributes.find_first_invalid (er_astate.AbductiveDomain.post :> BaseDomain.t).attrs with
          | Some (_, invalidation, trace) ->
             let loc = Trace.get_outer_location trace in
             let acc_loc = Location.none loc.Location.file in
             let invalidation_trace = Trace.set_outer_location acc_loc trace in
             let new_er = Diagnostic.AccessToInvalidAddress {calling_context= []; invalidation; invalidation_trace; access_trace = trace} in
             add_non_duplicate new_er
          | None ->
             acc)
      | _ -> acc
  in
  (* merge post *)
  (* let post_addr_abd_attrs = List.fold astates ~init:(BaseAddressAttributes.sempty) ~f:(fun acc astate -> extract_abd_attr acc astate.AbductiveDomain.nonref_formals (astate.AbductiveDomain.post :> BaseDomain.t)) in
   * (\* find all address v such that v=abdalloca \/ v=null \/ v=free *\)
   * let post_top_addrs = find_top_addrs post_addr_abd_attrs in
   * let simpl_astates = AbstractValue.Set.fold (fun addr acc_posts ->
   *     List.map acc_posts ~f:(fun astate -> AddressAttributes.map_post_attrs ~f:(fun attrs ->
   *         BaseAddressAttributes.remove addr attrs
   *       ) astate )
   *   ) post_top_addrs astates  in
   * let er_top_posts, _ =
   *   match List.fold2 astates simpl_astates ~init:([], []) ~f:(fun (acc, rem_acc) astate simpl_astate  ->
   *       if is_top_post simpl_astate then
   *         (acc@[astate], rem_acc)
   *       else
   *         (acc, rem_acc@[astate])) with
   *   | List.Or_unequal_lengths.Ok res -> res
   *   | _ -> assert false
   * in *)
  (*merge pre*)
  let invalidations =
    match astates with
    | [astate] when is_top_pre astate ->
        extract_error [] astate
    | _ ->
        let er_top_posts = astates in
        let addr_abd_attrs = List.fold er_top_posts ~init:(BaseAddressAttributes.sempty) ~f:(fun acc astate -> extract_abd_attr acc astate.AbductiveDomain.nonref_formals (astate.AbductiveDomain.pre :> BaseDomain.t)) in
        let top_addrs = find_top_addrs addr_abd_attrs in
        (* find all address v such that v=abdalloca \/ v=null \/ v=free *)
        let er_top_pres = AbstractValue.Set.fold (fun addr acc_posts ->
            List.map acc_posts ~f:(fun astate -> AddressAttributes.map_pre_attrs ~f:(fun attrs ->
                BaseAddressAttributes.remove addr attrs
              ) astate )
          ) top_addrs er_top_posts  in
        let er_top_pres = List.filter er_top_pres ~f:(is_top_pre) in
        List.fold er_top_pres ~init:[] ~f:extract_error in
  match invalidations with
  | [] -> None
  | [er] -> Some er
  | or_errs -> 
      Some (Diagnostic.OrError (or_errs, location))

