(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module AbstractAddress = PulseDomain.AbstractAddress
module Attribute = PulseDomain.Attribute
module Attributes = PulseDomain.Attributes
module InterprocAction = PulseDomain.InterprocAction
module ValueHistory = PulseDomain.ValueHistory
module Memory = PulseAbductiveDomain.Memory
module Stack = PulseAbductiveDomain.Stack
open Result.Monad_infix

include (* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)
  struct
  [@@@warning "-60"]

  (** Do not use {!PulseDomain} directly, go through {!PulseAbductiveDomain} instead *)
  module PulseDomain = struct end [@@deprecated "Use PulseAbductiveDomain instead."]
end

type t = PulseAbductiveDomain.t

type 'a access_result = ('a, PulseDiagnostic.t) result

(** Check that the [address] is not known to be invalid *)
let check_addr_access action (address, history) astate =
  Memory.check_valid action address astate
  |> Result.map_error ~f:(fun invalidated_by ->
         PulseDiagnostic.AccessToInvalidAddress {invalidated_by; accessed_by= {action; history}} )


module Closures = struct
  open Result.Monad_infix
  module Memory = PulseAbductiveDomain.Memory

  let fake_capture_field_prefix = "__capture_"

  let mk_fake_field ~id =
    Typ.Fieldname.Clang.from_class_name
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "function"]))
      (Printf.sprintf "%s%d" fake_capture_field_prefix id)


  let is_captured_fake_access (access : _ HilExp.Access.t) =
    match access with
    | FieldAccess fieldname
      when String.is_prefix ~prefix:fake_capture_field_prefix (Typ.Fieldname.to_string fieldname)
      ->
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
    let closure_addr = AbstractAddress.mk_fresh () in
    let fake_capture_edges = mk_capture_edges captured_addresses in
    let astate =
      Memory.set_cell closure_addr
        (fake_capture_edges, Attributes.singleton (Closure pname))
        astate
    in
    (astate, (closure_addr, (* TODO: trace *) []))
end

let eval_var var astate = Stack.eval var astate

let eval_access location addr_trace access astate =
  let addr = fst addr_trace in
  let action = InterprocAction.Immediate {imm= (); location} in
  check_addr_access action addr_trace astate >>| fun astate -> Memory.eval_edge addr access astate


let eval location exp0 astate =
  let action = InterprocAction.Immediate {imm= (); location} in
  let rec eval exp astate =
    match (exp : Exp.t) with
    | Var id ->
        Ok (eval_var (Var.of_id id) astate)
    | Lvar pvar ->
        Ok (eval_var (Var.of_pvar pvar) astate)
    | Lfield (exp', field, _) ->
        eval exp' astate
        >>= fun (astate, addr_trace) ->
        check_addr_access action addr_trace astate
        >>| fun astate -> Memory.eval_edge (fst addr_trace) (FieldAccess field) astate
    | Lindex (exp', exp_index) ->
        eval exp_index astate
        >>= fun (astate, addr_trace_index) ->
        check_addr_access action addr_trace_index astate
        >>= fun astate ->
        eval exp' astate
        >>= fun (astate, addr_trace) ->
        check_addr_access action addr_trace astate
        >>| fun astate ->
        Memory.eval_edge (fst addr_trace) (ArrayAccess (Typ.void, fst addr_trace_index)) astate
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
    | Sizeof _ | UnOp _ | BinOp _ | Exn _ | Const _ ->
        Ok (astate, (AbstractAddress.mk_fresh (), (* TODO history *) []))
  in
  eval exp0 astate


let eval_deref location exp astate =
  eval location exp astate
  >>= fun (astate, addr_trace) ->
  let action = InterprocAction.Immediate {imm= (); location} in
  check_addr_access action addr_trace astate
  >>| fun astate -> Memory.eval_edge (fst addr_trace) Dereference astate


let realloc_var var location astate =
  Stack.add var (AbstractAddress.mk_fresh (), [ValueHistory.VariableDeclaration location]) astate


let write_id id new_addr_loc astate = Stack.add (Var.of_id id) new_addr_loc astate

let havoc_id id loc_opt astate =
  if Stack.mem (Var.of_id id) astate then write_id id (AbstractAddress.mk_fresh (), loc_opt) astate
  else astate


let action_of_address location = InterprocAction.Immediate {imm= (); location}

let write_access location addr_trace_ref access addr_trace_obj astate =
  let action = action_of_address location in
  check_addr_access action addr_trace_ref astate
  >>| Memory.add_edge (fst addr_trace_ref) access addr_trace_obj


let write_deref location ~ref:addr_trace_ref ~obj:addr_trace_obj astate =
  write_access location addr_trace_ref Dereference addr_trace_obj astate


let write_field location addr_trace_ref field addr_trace_obj astate =
  write_access location addr_trace_ref (FieldAccess field) addr_trace_obj astate


let havoc_deref location addr_trace trace_obj astate =
  write_deref location ~ref:addr_trace ~obj:(AbstractAddress.mk_fresh (), trace_obj) astate


let havoc_field location addr_trace field trace_obj astate =
  write_field location addr_trace field (AbstractAddress.mk_fresh (), trace_obj) astate


let invalidate location cause addr_trace astate =
  let action = action_of_address location in
  check_addr_access action addr_trace astate >>| Memory.invalidate addr_trace cause


let invalidate_deref location cause (addr_ref, history) astate =
  let astate, (addr_obj, _) = Memory.eval_edge addr_ref Dereference astate in
  invalidate location cause (addr_obj, history) astate


let invalidate_array_elements location cause addr_trace astate =
  let action = action_of_address location in
  check_addr_access action addr_trace astate
  >>| fun astate ->
  match Memory.find_opt (fst addr_trace) astate with
  | None ->
      astate
  | Some (edges, _) ->
      Memory.Edges.fold
        (fun access dest_addr_trace astate ->
          match (access : Memory.Access.t) with
          | ArrayAccess _ ->
              Memory.invalidate dest_addr_trace cause astate
          | _ ->
              astate )
        edges astate


let shallow_copy location addr_hist astate =
  let action = action_of_address location in
  check_addr_access action addr_hist astate
  >>| fun astate ->
  let cell =
    match Memory.find_opt (fst addr_hist) astate with
    | None ->
        (Memory.Edges.empty, Attributes.empty)
    | Some cell ->
        cell
  in
  let copy = AbstractAddress.mk_fresh () in
  (Memory.set_cell copy cell astate, copy)


let check_address_escape escape_location proc_desc address history astate =
  let is_assigned_to_global address astate =
    let points_to_address pointer address astate =
      Memory.find_edge_opt pointer Dereference astate
      |> Option.exists ~f:(fun (pointee, _) -> AbstractAddress.equal pointee address)
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
                     (PulseDiagnostic.StackVariableAddressEscape
                        {variable; location= escape_location; history})
               | _ ->
                   Ok () ) )
  in
  let check_address_of_stack_variable () =
    let proc_name = Procdesc.get_proc_name proc_desc in
    IContainer.iter_result ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Stack.fold) astate
      ~f:(fun (variable, (var_address, _)) ->
        if
          AbstractAddress.equal var_address address
          && ( Var.is_cpp_temporary variable
             || Var.is_local_to_procedure proc_name variable
                && not (Procdesc.is_captured_var proc_desc variable) )
        then (
          L.d_printfln_escaped "Stack variable address &%a detected at address %a" Var.pp variable
            AbstractAddress.pp address ;
          Error
            (PulseDiagnostic.StackVariableAddressEscape
               {variable; location= escape_location; history}) )
        else Ok () )
  in
  check_address_of_cpp_temporary () >>= check_address_of_stack_variable >>| fun () -> astate


let mark_address_of_cpp_temporary history variable address astate =
  Memory.add_attributes address
    (Attributes.singleton (AddressOfCppTemporary (variable, history)))
    astate


let remove_vars vars astate =
  let astate =
    List.fold vars ~init:astate ~f:(fun astate var ->
        match Stack.find_opt var astate with
        | Some (address, history) when Var.is_cpp_temporary var ->
            mark_address_of_cpp_temporary history var address astate
        | _ ->
            astate )
  in
  let astate' = Stack.remove_vars vars astate in
  if phys_equal astate' astate then astate else PulseAbductiveDomain.discard_unreachable astate'


let call ~caller_summary call_loc callee_pname ~ret ~actuals astate =
  match PulsePayload.read_full ~caller_summary ~callee_pname with
  | Some (callee_proc_desc, preposts) ->
      let formals =
        Procdesc.get_formals callee_proc_desc
        |> List.map ~f:(fun (mangled, _) -> Pvar.mk mangled callee_pname |> Var.of_pvar)
      in
      (* call {!PulseAbductiveDomain.PrePost.apply} on each pre/post pair in the summary. *)
      List.fold_result preposts ~init:[] ~f:(fun posts pre_post ->
          (* apply all pre/post specs *)
          PulseAbductiveDomain.PrePost.apply callee_pname call_loc pre_post ~formals ~actuals
            astate
          >>| fun (post, return_val_opt) ->
          let event = ValueHistory.Call {f= Call callee_pname; location= call_loc} in
          let post =
            match return_val_opt with
            | Some return_val ->
                write_id (fst ret) (return_val, [event]) post
            | None ->
                havoc_id (fst ret) [event] post
          in
          post :: posts )
  | None ->
      (* no spec found for some reason (unknown function, ...) *)
      L.d_printfln "No spec found for %a@\n" Typ.Procname.pp callee_pname ;
      let event = ValueHistory.Call {f= SkippedKnownCall callee_pname; location= call_loc} in
      Ok [havoc_id (fst ret) [event] astate]
