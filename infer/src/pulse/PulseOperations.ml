(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
open Result.Monad_infix
open PulseBasicInterface
open PulseDomainInterface

type t = AbductiveDomain.t

type 'a access_result = ('a, Diagnostic.t) result

(** Check that the [address] is not known to be invalid *)
let check_addr_access location (address, history) astate =
  let access_trace = Trace.Immediate {location; history} in
  Memory.check_valid access_trace address astate
  |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
         Diagnostic.AccessToInvalidAddress {invalidation; invalidation_trace; access_trace} )


module Closures = struct
  module Memory = AbductiveDomain.Memory

  let fake_capture_field_prefix = "__capture_"

  let mk_fake_field ~id =
    Typ.Fieldname.Clang.from_class_name
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "function"]))
      (Printf.sprintf "%s%d" fake_capture_field_prefix id)


  let is_captured_fake_access (access : _ HilExp.Access.t) =
    match access with
    | FieldAccess fieldname
      when String.is_prefix ~prefix:fake_capture_field_prefix (Typ.Fieldname.to_string fieldname) ->
        true
    | _ ->
        false


  let mk_capture_edges captured =
    let fake_fields =
      List.rev_mapi captured ~f:(fun id captured_addr_trace ->
          (HilExp.Access.FieldAccess (mk_fake_field ~id), captured_addr_trace) )
    in
    Memory.Edges.of_seq (Caml.List.to_seq fake_fields)


  let check_captured_addresses action lambda_addr astate =
    match Memory.find_opt lambda_addr astate with
    | None ->
        Ok astate
    | Some (edges, attributes) ->
        IContainer.iter_result ~fold:Attributes.fold attributes ~f:(function
          | Attribute.Closure _ ->
              IContainer.iter_result
                ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold) edges
                ~f:(fun (access, addr_trace) ->
                  if is_captured_fake_access access then
                    check_addr_access action addr_trace astate >>| fun _ -> ()
                  else Ok () )
          | _ ->
              Ok () )
        >>| fun () -> astate


  let record location pname captured astate =
    let captured_addresses =
      List.rev_filter_map captured
        ~f:(fun (captured_as, (address_captured, trace_captured), mode) ->
          match mode with
          | `ByValue ->
              None
          | `ByReference ->
              let new_trace = ValueHistory.Capture {captured_as; location} :: trace_captured in
              Some (address_captured, new_trace) )
    in
    let closure_addr_hist = (AbstractValue.mk_fresh (), [ValueHistory.Assignment location]) in
    let fake_capture_edges = mk_capture_edges captured_addresses in
    let astate =
      Memory.set_cell closure_addr_hist
        (fake_capture_edges, Attributes.singleton (Closure pname))
        location astate
    in
    (astate, closure_addr_hist)
end

let eval_var var astate = Stack.eval var astate

let eval_access location addr_hist access astate =
  check_addr_access location addr_hist astate
  >>| fun astate -> Memory.eval_edge addr_hist access astate


type operand = LiteralOperand of IntLit.t | AbstractValueOperand of AbstractValue.t

let eval_arith_operand location binop_addr binop_hist bop op_lhs op_rhs astate =
  let arith_of_op op astate =
    match op with
    | LiteralOperand i ->
        Some (Arithmetic.equal_to i)
    | AbstractValueOperand v ->
        Memory.get_arithmetic v astate |> Option.map ~f:fst
  in
  match
    Option.both (arith_of_op op_lhs astate) (arith_of_op op_rhs astate)
    |> Option.bind ~f:(fun (addr_lhs, addr_rhs) -> Arithmetic.binop bop addr_lhs addr_rhs)
  with
  | None ->
      astate
  | Some binop_a ->
      let binop_trace = Trace.Immediate {location; history= binop_hist} in
      let astate = Memory.add_attribute binop_addr (Arithmetic (binop_a, binop_trace)) astate in
      astate


let eval_bo_itv_binop binop_addr bop op_lhs op_rhs astate =
  let bo_itv_of_op op astate =
    match op with
    | LiteralOperand i ->
        Itv.of_int_lit i
    | AbstractValueOperand v ->
        Memory.get_bo_itv v astate
  in
  match Itv.arith_binop bop (bo_itv_of_op op_lhs astate) (bo_itv_of_op op_rhs astate) with
  | None ->
      astate
  | Some itv ->
      Memory.add_attribute binop_addr (BoItv itv) astate


let eval_binop location binop op_lhs op_rhs binop_hist astate =
  let binop_addr = AbstractValue.mk_fresh () in
  let astate =
    eval_arith_operand location binop_addr binop_hist binop op_lhs op_rhs astate
    |> eval_bo_itv_binop binop_addr binop op_lhs op_rhs
  in
  (astate, (binop_addr, binop_hist))


let eval location exp0 astate =
  let rec eval exp astate =
    match (exp : Exp.t) with
    | Var id ->
        Ok (eval_var (* error in case of missing history? *) [] (Var.of_id id) astate)
    | Lvar pvar ->
        Ok (eval_var [ValueHistory.VariableAccessed (pvar, location)] (Var.of_pvar pvar) astate)
    | Lfield (exp', field, _) ->
        eval exp' astate
        >>= fun (astate, addr_hist) ->
        check_addr_access location addr_hist astate
        >>| fun astate -> Memory.eval_edge addr_hist (FieldAccess field) astate
    | Lindex (exp', exp_index) ->
        eval exp_index astate
        >>= fun (astate, addr_hist_index) ->
        eval exp' astate
        >>= fun (astate, addr_hist) ->
        check_addr_access location addr_hist astate
        >>| fun astate ->
        Memory.eval_edge addr_hist (ArrayAccess (Typ.void, fst addr_hist_index)) astate
    | Closure {name; captured_vars} ->
        List.fold_result captured_vars ~init:(astate, [])
          ~f:(fun (astate, rev_captured) (capt_exp, captured_as, _) ->
            eval capt_exp astate
            >>| fun (astate, addr_trace) ->
            let mode =
              (* HACK: the frontend follows this discipline *)
              match (capt_exp : Exp.t) with Lvar _ -> `ByReference | _ -> `ByValue
            in
            (astate, (captured_as, addr_trace, mode) :: rev_captured) )
        >>| fun (astate, rev_captured) ->
        Closures.record location name (List.rev rev_captured) astate
    | Cast (_, exp') ->
        eval exp' astate
    | Const (Cint i) ->
        (* TODO: make identical const the same address *)
        let addr = AbstractValue.mk_fresh () in
        let astate =
          Memory.add_attribute addr
            (Arithmetic (Arithmetic.equal_to i, Immediate {location; history= []}))
            astate
          |> Memory.add_attribute addr (BoItv (Itv.of_int_lit i))
          |> Memory.invalidate
               (addr, [ValueHistory.Assignment location])
               (ConstantDereference i) location
        in
        Ok (astate, (addr, []))
    | UnOp (unop, exp, _typ) -> (
        eval exp astate
        >>| fun (astate, (addr, hist)) ->
        let unop_addr = AbstractValue.mk_fresh () in
        match
          Memory.get_arithmetic addr astate
          |> Option.bind ~f:(function a, _ -> Arithmetic.unop unop a)
        with
        | None ->
            (astate, (unop_addr, (* TODO history *) []))
        | Some unop_a ->
            let unop_hist = (* TODO: add event *) hist in
            let unop_trace = Trace.Immediate {location; history= unop_hist} in
            let astate = Memory.add_attribute unop_addr (Arithmetic (unop_a, unop_trace)) astate in
            (astate, (unop_addr, unop_hist)) )
    | BinOp (bop, e_lhs, e_rhs) ->
        eval e_lhs astate
        >>= fun (astate, (addr_lhs, hist_lhs)) ->
        eval e_rhs astate
        >>| fun ( astate
                , ( addr_rhs
                  , (* NOTE: arbitrarily track only the history of the lhs, maybe not the brightest idea *)
                  _ ) ) ->
        eval_binop location bop (AbstractValueOperand addr_lhs) (AbstractValueOperand addr_rhs)
          hist_lhs astate
    | Const _ | Sizeof _ | Exn _ ->
        Ok (astate, (AbstractValue.mk_fresh (), (* TODO history *) []))
  in
  eval exp0 astate


let eval_arith location exp astate =
  match (exp : Exp.t) with
  | Const (Cint i) ->
      Ok
        ( astate
        , None
        , Some
            ( Arithmetic.equal_to i
            , Trace.Immediate {location; history= [ValueHistory.Assignment location]} ) )
  | exp -> (
      eval location exp astate
      >>| fun (astate, (addr, _)) ->
      match Memory.get_arithmetic addr astate with
      | Some a ->
          (astate, Some addr, Some a)
      | None ->
          (astate, Some addr, None) )


let record_abduced event location addr_opt orig_arith_hist_opt arith_opt astate =
  match Option.both addr_opt arith_opt with
  | None ->
      astate
  | Some (addr, arith) ->
      let trace =
        match orig_arith_hist_opt with
        | None ->
            Trace.Immediate {location; history= [event]}
        | Some (_, trace) ->
            Trace.add_event event trace
      in
      let attribute = Attribute.Arithmetic (arith, trace) in
      Memory.abduce_attribute addr attribute astate |> Memory.add_attribute addr attribute


let prune ~is_then_branch if_kind location ~condition astate =
  let rec prune_aux ~negated exp astate =
    match (exp : Exp.t) with
    | BinOp (bop, e1, e2) -> (
        eval_arith location e1 astate
        >>= fun (astate, addr1, eval1) ->
        eval_arith location e2 astate
        >>| fun (astate, addr2, eval2) ->
        match
          Arithmetic.abduce_binop_is_true ~negated bop (Option.map ~f:fst eval1)
            (Option.map ~f:fst eval2)
        with
        | Unsatisfiable ->
            (astate, false)
        | Satisfiable (abduced1, abduced2) ->
            let event = ValueHistory.Conditional {is_then_branch; if_kind; location} in
            let astate =
              record_abduced event location addr1 eval1 abduced1 astate
              |> record_abduced event location addr2 eval2 abduced2
            in
            (astate, true) )
    | UnOp (LNot, exp', _) ->
        prune_aux ~negated:(not negated) exp' astate
    | exp ->
        let zero = Exp.Const (Cint IntLit.zero) in
        prune_aux ~negated (Exp.BinOp (Ne, exp, zero)) astate
  in
  prune_aux ~negated:false condition astate


let eval_deref location exp astate =
  eval location exp astate
  >>= fun (astate, addr_hist) ->
  check_addr_access location addr_hist astate
  >>| fun astate -> Memory.eval_edge addr_hist Dereference astate


let realloc_pvar pvar location astate =
  Stack.add (Var.of_pvar pvar)
    (AbstractValue.mk_fresh (), [ValueHistory.VariableDeclared (pvar, location)])
    astate


let write_id id new_addr_loc astate = Stack.add (Var.of_id id) new_addr_loc astate

let havoc_id id loc_opt astate =
  if Stack.mem (Var.of_id id) astate then write_id id (AbstractValue.mk_fresh (), loc_opt) astate
  else astate


let write_access location addr_trace_ref access addr_trace_obj astate =
  check_addr_access location addr_trace_ref astate
  >>| Memory.add_edge addr_trace_ref access addr_trace_obj location


let write_deref location ~ref:addr_trace_ref ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref Dereference addr_trace_obj astate


let write_field location addr_trace_ref field addr_trace_obj astate =
  write_access location addr_trace_ref (FieldAccess field) addr_trace_obj astate


let havoc_deref location addr_trace trace_obj astate =
  write_deref location ~ref:addr_trace ~obj:(AbstractValue.mk_fresh (), trace_obj) astate


let havoc_field location addr_trace field trace_obj astate =
  write_field location addr_trace field (AbstractValue.mk_fresh (), trace_obj) astate


let invalidate location cause addr_trace astate =
  check_addr_access location addr_trace astate >>| Memory.invalidate addr_trace cause location


let invalidate_deref location cause ref_addr_hist astate =
  let astate, (addr_obj, _) = Memory.eval_edge ref_addr_hist Dereference astate in
  invalidate location cause (addr_obj, snd ref_addr_hist) astate


let invalidate_array_elements location cause addr_trace astate =
  check_addr_access location addr_trace astate
  >>| fun astate ->
  match Memory.find_opt (fst addr_trace) astate with
  | None ->
      astate
  | Some (edges, _) ->
      Memory.Edges.fold
        (fun access dest_addr_trace astate ->
          match (access : Memory.Access.t) with
          | ArrayAccess _ ->
              Memory.invalidate dest_addr_trace cause location astate
          | _ ->
              astate )
        edges astate


let shallow_copy location addr_hist astate =
  check_addr_access location addr_hist astate
  >>| fun astate ->
  let cell =
    match Memory.find_opt (fst addr_hist) astate with
    | None ->
        (Memory.Edges.empty, Attributes.empty)
    | Some cell ->
        cell
  in
  let copy = (AbstractValue.mk_fresh (), [ValueHistory.Assignment location]) in
  (Memory.set_cell copy cell location astate, copy)


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
    Memory.find_opt address astate
    |> Option.fold_result ~init:() ~f:(fun () (_, attrs) ->
           IContainer.iter_result ~fold:Attributes.fold attrs ~f:(fun attr ->
               match attr with
               | Attribute.AddressOfCppTemporary (variable, _)
                 when not (is_assigned_to_global address astate) ->
                   (* The returned address corresponds to a C++ temporary. It will have gone out of
                      scope by now except if it was bound to a global. *)
                   Error
                     (Diagnostic.StackVariableAddressEscape
                        {variable; location= escape_location; history})
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
            (Diagnostic.StackVariableAddressEscape {variable; location= escape_location; history}) )
        else Ok () )
  in
  check_address_of_cpp_temporary () >>= check_address_of_stack_variable >>| fun () -> astate


let mark_address_of_cpp_temporary history variable address astate =
  Memory.add_attribute address (AddressOfCppTemporary (variable, history)) astate


let mark_address_of_stack_variable history variable location address astate =
  Memory.add_attribute address (AddressOfStackVariable (variable, location, history)) astate


let remove_vars vars location astate =
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
  let astate' = Stack.remove_vars vars astate in
  if phys_equal astate' astate then astate else AbductiveDomain.discard_unreachable astate'


let unknown_call call_loc reason ~ret ~actuals astate =
  let event = ValueHistory.Call {f= reason; location= call_loc; in_call= []} in
  let havoc_ret (ret, _) astate = havoc_id ret [event] astate in
  let havoc_actual_if_ptr (actual, typ) astate =
    if Typ.is_pointer typ then
      (* TODO: to avoid false negatives, we should not havoc when the corresponding formal is a
         pointer to const *)
      (* HACK: write through the pointer even if it is invalid. This is to avoid raising issues when
         havoc'ing pointer parameters (which normally causes a [check_valid] call. *)
      let fresh_value = AbstractValue.mk_fresh () in
      Memory.add_edge actual Dereference (fresh_value, [event]) call_loc astate
    else astate
  in
  L.d_printfln "skipping unknown procedure@." ;
  List.fold actuals ~f:(fun astate actual_typ -> havoc_actual_if_ptr actual_typ astate) ~init:astate
  |> havoc_ret ret


let call ~caller_summary call_loc callee_pname ~ret ~actuals astate =
  match PulsePayload.read_full ~caller_summary ~callee_pname with
  | Some (callee_proc_desc, preposts) ->
      let formals =
        Procdesc.get_formals callee_proc_desc
        |> List.map ~f:(fun (mangled, _) -> Pvar.mk mangled callee_pname |> Var.of_pvar)
      in
      (* call {!AbductiveDomain.PrePost.apply} on each pre/post pair in the summary. *)
      List.fold_result preposts ~init:[] ~f:(fun posts pre_post ->
          (* apply all pre/post specs *)
          AbductiveDomain.PrePost.apply callee_pname call_loc pre_post ~formals ~actuals astate
          >>| function
          | None ->
              (* couldn't apply pre/post pair *) posts
          | Some (post, return_val_opt) ->
              let event =
                ValueHistory.Call {f= Call callee_pname; location= call_loc; in_call= []}
              in
              let post =
                match return_val_opt with
                | Some (return_val, return_hist) ->
                    write_id (fst ret) (return_val, event :: return_hist) post
                | None ->
                    havoc_id (fst ret) [event] post
              in
              post :: posts )
  | None ->
      (* no spec found for some reason (unknown function, ...) *)
      L.d_printfln "No spec found for %a@\n" Typ.Procname.pp callee_pname ;
      Ok [unknown_call call_loc (SkippedKnownCall callee_pname) ~ret ~actuals astate]
