(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module AbstractAddress = PulseDomain.AbstractAddress
module Attribute = PulseDomain.Attribute
module Attributes = PulseDomain.Attributes
module Memory = PulseDomain.Memory
module Stack = PulseDomain.Stack
open Result.Monad_infix

type t = PulseDomain.t = {heap: Memory.t; stack: Stack.t}

type 'a access_result = ('a, PulseDiagnostic.t) result

(** Check that the address is not known to be invalid *)
let check_addr_access actor (address, trace) astate =
  match Memory.get_invalidation address astate.heap with
  | Some invalidated_by ->
      Error
        (PulseDiagnostic.AccessToInvalidAddress {invalidated_by; accessed_by= actor; address; trace})
  | None ->
      Ok astate


(** Walk the heap starting from [addr] and following [path]. Stop either at the element before last
   and return [new_addr] if [overwrite_last] is [Some new_addr], or go until the end of the path if it
   is [None]. Create more addresses into the heap as needed to follow the [path]. Check that each
   address reached is valid. *)
let rec walk ~dereference_to_ignore actor ~on_last addr_trace path astate =
  let check_addr_access_optional actor addr_trace astate =
    match dereference_to_ignore with
    | Some 0 ->
        Ok astate
    | _ ->
        check_addr_access actor addr_trace astate
  in
  match (path, on_last) with
  | [], `Access ->
      Ok (astate, addr_trace)
  | [], `Overwrite _ ->
      L.die InternalError "Cannot overwrite last address in empty path"
  | [a], `Overwrite new_addr_trace ->
      check_addr_access_optional actor addr_trace astate
      >>| fun astate ->
      let heap = Memory.add_edge_and_back_edge (fst addr_trace) a new_addr_trace astate.heap in
      ({astate with heap}, new_addr_trace)
  | a :: path, _ -> (
      check_addr_access_optional actor addr_trace astate
      >>= fun astate ->
      let dereference_to_ignore =
        Option.map ~f:(fun index -> max 0 (index - 1)) dereference_to_ignore
      in
      let addr = fst addr_trace in
      match Memory.find_edge_opt addr a astate.heap with
      | None ->
          let addr_trace' = (AbstractAddress.mk_fresh (), []) in
          let heap = Memory.add_edge_and_back_edge addr a addr_trace' astate.heap in
          let astate = {astate with heap} in
          walk ~dereference_to_ignore actor ~on_last addr_trace' path astate
      | Some addr_trace' ->
          walk ~dereference_to_ignore actor ~on_last addr_trace' path astate )


let write_var var new_addr_trace astate =
  let astate, var_address_of =
    match Stack.find_opt var astate.stack with
    | Some (addr, _) ->
        (astate, addr)
    | None ->
        let addr = AbstractAddress.mk_fresh () in
        let stack = Stack.add var (addr, None) astate.stack in
        ({astate with stack}, addr)
  in
  (* Update heap with var_address_of -*-> new_addr *)
  let heap = Memory.add_edge var_address_of HilExp.Access.Dereference new_addr_trace astate.heap in
  {astate with heap}


let ends_with_addressof = function HilExp.AccessExpression.AddressOf _ -> true | _ -> false

let last_dereference access_list =
  let rec last_dereference_inner access_list index result =
    match access_list with
    | [] ->
        result
    | HilExp.Access.Dereference :: rest ->
        last_dereference_inner rest (index + 1) (Some index)
    | _ :: rest ->
        last_dereference_inner rest (index + 1) result
  in
  last_dereference_inner access_list 0 None


let rec to_accesses location access_expr astate =
  let exception Failed_fold of PulseDiagnostic.t in
  try
    HilExp.AccessExpression.to_accesses_fold access_expr ~init:astate
      ~f_array_offset:(fun astate hil_exp_opt ->
        match hil_exp_opt with
        | None ->
            (astate, AbstractAddress.mk_fresh ())
        | Some hil_exp -> (
          match eval_hil_exp location hil_exp astate with
          | Ok result ->
              result
          | Error diag ->
              raise (Failed_fold diag) ) )
    |> Result.return
  with Failed_fold diag -> Error diag


(** add addresses to the state to give an address to the destination of the given access path *)
and walk_access_expr ~on_last astate access_expr location =
  to_accesses location access_expr astate
  >>= fun (astate, (access_var, _), access_list) ->
  let dereference_to_ignore =
    if ends_with_addressof access_expr then last_dereference access_list else None
  in
  if Config.write_html then
    L.d_printfln "Accessing %a -> [%a]" Var.pp access_var
      (Pp.seq ~sep:"," Memory.Access.pp)
      access_list ;
  match (on_last, access_list) with
  | `Overwrite new_addr_trace, [] ->
      Ok (write_var access_var new_addr_trace astate, new_addr_trace)
  | `Access, _ | `Overwrite _, _ :: _ -> (
      let astate, base_addr_trace =
        match Stack.find_opt access_var astate.stack with
        | Some (addr, init_loc_opt) ->
            let trace =
              Option.value_map init_loc_opt ~default:[] ~f:(fun init_loc ->
                  [PulseTrace.VariableDeclaration init_loc] )
            in
            (astate, (addr, trace))
        | None ->
            let addr = AbstractAddress.mk_fresh () in
            let stack = Stack.add access_var (addr, None) astate.stack in
            ({astate with stack}, (addr, []))
      in
      match access_list with
      | [HilExp.Access.TakeAddress] ->
          Ok (astate, base_addr_trace)
      | _ ->
          let actor = {PulseDiagnostic.access_expr; location} in
          walk ~dereference_to_ignore actor ~on_last base_addr_trace
            (HilExp.Access.Dereference :: access_list)
            astate )


(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to.

    Return an error state if it traverses some known invalid address or if the end destination is
    known to be invalid. *)
and materialize_address astate access_expr = walk_access_expr ~on_last:`Access astate access_expr

and read location access_expr astate =
  materialize_address astate access_expr location
  >>= fun (astate, addr_trace) ->
  let actor = {PulseDiagnostic.access_expr; location} in
  check_addr_access actor addr_trace astate >>| fun astate -> (astate, addr_trace)


and read_all location access_exprs astate =
  List.fold_result access_exprs ~init:astate ~f:(fun astate access_expr ->
      read location access_expr astate >>| fst )


and eval_hil_exp location (hil_exp : HilExp.t) astate =
  match hil_exp with
  | AccessExpression access_expr ->
      read location access_expr astate >>| fun (astate, (addr, _)) -> (astate, addr)
  | _ ->
      read_all location (HilExp.get_access_exprs hil_exp) astate
      >>| fun astate -> (astate, AbstractAddress.mk_fresh ())


(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to, and replace that with the given
    address.

    Return an error state if it traverses some known invalid address. *)
let overwrite_address astate access_expr new_addr_trace =
  walk_access_expr ~on_last:(`Overwrite new_addr_trace) astate access_expr


(** Add the given address to the set of know invalid addresses. *)
let mark_invalid actor address astate =
  {astate with heap= Memory.invalidate address actor astate.heap}


let havoc_var trace var astate = write_var var (AbstractAddress.mk_fresh (), trace) astate

let havoc trace location (access_expr : HilExp.AccessExpression.t) astate =
  overwrite_address astate access_expr (AbstractAddress.mk_fresh (), trace) location >>| fst


let write location access_expr addr astate =
  overwrite_address astate access_expr addr location >>| fun (astate, _) -> astate


let invalidate cause location access_expr astate =
  materialize_address astate access_expr location
  >>= fun (astate, addr_trace) ->
  check_addr_access {access_expr; location} addr_trace astate
  >>| mark_invalid cause (fst addr_trace)


let invalidate_array_elements cause location access_expr astate =
  materialize_address astate access_expr location
  >>= fun (astate, addr_trace) ->
  check_addr_access {access_expr; location} addr_trace astate
  >>| fun astate ->
  match Memory.find_opt (fst addr_trace) astate.heap with
  | None ->
      astate
  | Some (edges, _) ->
      Memory.Edges.fold
        (fun access (dest_addr, _) astate ->
          match (access : Memory.Access.t) with
          | ArrayAccess _ ->
              mark_invalid cause dest_addr astate
          | _ ->
              astate )
        edges astate


let check_address_of_local_variable proc_desc address astate =
  let proc_location = Procdesc.get_loc proc_desc in
  let proc_name = Procdesc.get_proc_name proc_desc in
  let check_address_of_cpp_temporary () =
    Memory.find_opt address astate.heap
    |> Option.fold_result ~init:() ~f:(fun () (_, attrs) ->
           Container.fold_result ~fold:(IContainer.fold_of_pervasives_fold ~fold:Attributes.fold)
             attrs ~init:() ~f:(fun () attr ->
               match attr with
               | Attribute.AddressOfCppTemporary (variable, location_opt) ->
                   let location = Option.value ~default:proc_location location_opt in
                   Error (PulseDiagnostic.StackVariableAddressEscape {variable; location})
               | _ ->
                   Ok () ) )
  in
  let check_address_of_stack_variable () =
    Container.fold_result ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Stack.fold)
      astate.stack ~init:() ~f:(fun () (variable, (var_address, init_location)) ->
        if
          AbstractAddress.equal var_address address
          && ( Var.is_cpp_temporary variable
             || Var.is_local_to_procedure proc_name variable
                && not (Procdesc.is_captured_var proc_desc variable) )
        then
          let location = Option.value ~default:proc_location init_location in
          Error (PulseDiagnostic.StackVariableAddressEscape {variable; location})
        else Ok () )
  in
  check_address_of_cpp_temporary () >>= check_address_of_stack_variable >>| fun () -> astate


let mark_address_of_cpp_temporary location variable address heap =
  Memory.add_attributes address
    (Attributes.singleton (AddressOfCppTemporary (variable, location)))
    heap


let remove_vars vars astate =
  let heap =
    List.fold vars ~init:astate.heap ~f:(fun heap var ->
        match Stack.find_opt var astate.stack with
        | Some (address, location) when Var.is_cpp_temporary var ->
            (* TODO: it would be good to record the location of the temporary creation in the
                 stack and save it here in the attribute for reporting *)
            mark_address_of_cpp_temporary location var address heap
        | _ ->
            heap )
  in
  let stack = List.fold ~f:(fun stack var -> Stack.remove var stack) ~init:astate.stack vars in
  if phys_equal stack astate.stack && phys_equal heap astate.heap then astate else {stack; heap}


let record_var_decl_location location var astate =
  let addr =
    match Stack.find_opt var astate.stack with
    | Some (addr, _) ->
        addr
    | None ->
        AbstractAddress.mk_fresh ()
  in
  let stack = Stack.add var (addr, Some location) astate.stack in
  {astate with stack}


module Closures = struct
  open Result.Monad_infix

  let check_captured_addresses location lambda addr astate =
    match Memory.find_opt addr astate.heap with
    | None ->
        Ok astate
    | Some (_, attributes) ->
        IContainer.iter_result ~fold:(IContainer.fold_of_pervasives_fold ~fold:Attributes.fold)
          attributes ~f:(function
          | Attribute.Closure (_, captured) ->
              IContainer.iter_result ~fold:List.fold captured ~f:(fun addr_trace ->
                  check_addr_access {access_expr= lambda; location} addr_trace astate
                  >>| fun _ -> () )
          | _ ->
              Ok () )
        >>| fun () -> astate


  let write location access_expr pname captured astate =
    let closure_addr = AbstractAddress.mk_fresh () in
    write location access_expr
      (closure_addr, [PulseTrace.Assignment {lhs= access_expr; location}])
      astate
    >>| fun astate ->
    { astate with
      heap=
        Memory.add_attributes closure_addr
          (Attributes.singleton (Closure (pname, captured)))
          astate.heap }


  let record location access_expr pname captured astate =
    List.fold_result captured ~init:(astate, [])
      ~f:(fun ((astate, captured) as result) (captured_as, captured_exp) ->
        match captured_exp with
        | HilExp.AccessExpression (AddressOf access_expr as captured_access_expr) ->
            read location access_expr astate
            >>= fun (astate, (address, trace)) ->
            let new_trace =
              PulseTrace.Capture {captured_as; captured= captured_access_expr; location} :: trace
            in
            Ok (astate, (address, new_trace) :: captured)
        | _ ->
            Ok result )
    >>= fun (astate, captured_addresses) ->
    write location access_expr pname captured_addresses astate
end

module StdVector = struct
  open Result.Monad_infix

  let is_reserved location vector_access_expr astate =
    read location vector_access_expr astate
    >>| fun (astate, (addr, _)) -> (astate, Memory.is_std_vector_reserved addr astate.heap)


  let mark_reserved location vector_access_expr astate =
    read location vector_access_expr astate
    >>| fun (astate, (addr, _)) -> {astate with heap= Memory.std_vector_reserve addr astate.heap}
end
