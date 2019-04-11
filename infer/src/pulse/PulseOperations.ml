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

(** Check that the address is not known to be invalid *)
let check_addr_access action (address, trace) astate =
  Memory.check_valid action address astate
  |> Result.map_error ~f:(fun invalidated_by ->
         PulseDiagnostic.AccessToInvalidAddress {invalidated_by; accessed_by= action; trace} )


(** Walk the heap starting from [addr] and following [path]. Stop either at the element before last
   and return [new_addr] if [overwrite_last] is [Some new_addr], or go until the end of the path if it
   is [None]. Create more addresses into the heap as needed to follow the [path]. Check that each
   address reached is valid. *)
let rec walk ~dereference_to_ignore action ~on_last addr_trace path astate =
  let check_addr_access_optional action addr_trace astate =
    match dereference_to_ignore with
    | Some 0 ->
        Ok astate
    | _ ->
        check_addr_access action addr_trace astate
  in
  match (path, on_last) with
  | [], `Access ->
      Ok (astate, addr_trace)
  | [], `Overwrite _ ->
      L.die InternalError "Cannot overwrite last address in empty path"
  | [a], `Overwrite new_addr_trace ->
      check_addr_access_optional action addr_trace astate
      >>| fun astate ->
      let astate = Memory.add_edge (fst addr_trace) a new_addr_trace astate in
      (astate, new_addr_trace)
  | a :: path, _ ->
      check_addr_access_optional action addr_trace astate
      >>= fun astate ->
      let dereference_to_ignore =
        Option.map ~f:(fun index -> max 0 (index - 1)) dereference_to_ignore
      in
      let astate, addr_trace' = Memory.materialize_edge (fst addr_trace) a astate in
      walk ~dereference_to_ignore action ~on_last addr_trace' path astate


let write_var var new_addr_trace astate =
  let astate, (var_address_of, _) = Stack.materialize var astate in
  (* Update heap with var_address_of -*-> new_addr *)
  Memory.add_edge var_address_of HilExp.Access.Dereference new_addr_trace astate


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
        let astate, (addr, init_loc_opt) = Stack.materialize access_var astate in
        let trace =
          Option.value_map init_loc_opt ~default:[] ~f:(fun init_loc ->
              [PulseTrace.VariableDeclaration init_loc] )
        in
        (astate, (addr, trace))
      in
      match access_list with
      | [HilExp.Access.TakeAddress] ->
          Ok (astate, base_addr_trace)
      | _ ->
          let action = PulseTrace.Immediate {imm= access_expr; location} in
          walk ~dereference_to_ignore action ~on_last base_addr_trace
            (HilExp.Access.Dereference :: access_list)
            astate )


(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to.

    Return an error state if it traverses some known invalid address or if the end destination is
    known to be invalid. *)
and materialize_address astate access_expr = walk_access_expr ~on_last:`Access astate access_expr

and read location access_expr astate = materialize_address astate access_expr location

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
let mark_invalid action address astate = Memory.invalidate address action astate

let realloc_var var location astate =
  Stack.add var (AbstractAddress.mk_fresh (), Some location) astate


let havoc_var trace var astate = write_var var (AbstractAddress.mk_fresh (), trace) astate

let havoc trace location (access_expr : HilExp.AccessExpression.t) astate =
  overwrite_address astate access_expr (AbstractAddress.mk_fresh (), trace) location >>| fst


let write location access_expr addr astate =
  overwrite_address astate access_expr addr location >>| fun (astate, _) -> astate


let invalidate cause location access_expr astate =
  materialize_address astate access_expr location
  >>= fun (astate, addr_trace) ->
  check_addr_access (Immediate {imm= access_expr; location}) addr_trace astate
  >>| mark_invalid cause (fst addr_trace)


let invalidate_array_elements cause location access_expr astate =
  materialize_address astate access_expr location
  >>= fun (astate, addr_trace) ->
  check_addr_access (Immediate {imm= access_expr; location}) addr_trace astate
  >>| fun astate ->
  match Memory.find_opt (fst addr_trace) astate with
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
    Memory.find_opt address astate
    |> Option.fold_result ~init:() ~f:(fun () (_, attrs) ->
           IContainer.iter_result ~fold:Attributes.fold attrs ~f:(fun attr ->
               match attr with
               | Attribute.AddressOfCppTemporary (variable, location_opt) ->
                   let location = Option.value ~default:proc_location location_opt in
                   Error (PulseDiagnostic.StackVariableAddressEscape {variable; location})
               | _ ->
                   Ok () ) )
  in
  let check_address_of_stack_variable () =
    IContainer.iter_result ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Stack.fold) astate
      ~f:(fun (variable, (var_address, init_location)) ->
        if
          AbstractAddress.equal var_address address
          && ( Var.is_cpp_temporary variable
             || Var.is_local_to_procedure proc_name variable
                && not (Procdesc.is_captured_var proc_desc variable) )
        then (
          let location = Option.value ~default:proc_location init_location in
          L.d_printfln_escaped "Stack Variable &%a detected at address %a" Var.pp variable
            AbstractAddress.pp address ;
          Error (PulseDiagnostic.StackVariableAddressEscape {variable; location}) )
        else Ok () )
  in
  check_address_of_cpp_temporary () >>= check_address_of_stack_variable >>| fun () -> astate


let mark_address_of_cpp_temporary location variable address astate =
  Memory.add_attributes address
    (Attributes.singleton (AddressOfCppTemporary (variable, location)))
    astate


let remove_vars vars astate =
  let astate =
    List.fold vars ~init:astate ~f:(fun heap var ->
        match Stack.find_opt var astate with
        | Some (address, location) when Var.is_cpp_temporary var ->
            (* TODO: it would be good to record the location of the temporary creation in the
                 stack and save it here in the attribute for reporting *)
            mark_address_of_cpp_temporary location var address astate
        | _ ->
            heap )
  in
  let astate' = Stack.remove_vars vars astate in
  if phys_equal astate' astate then astate else PulseAbductiveDomain.discard_unreachable astate'


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


  let check_captured_addresses location lambda addr astate =
    match Memory.find_opt addr astate with
    | None ->
        Ok astate
    | Some (edges, attributes) ->
        IContainer.iter_result ~fold:Attributes.fold attributes ~f:(function
          | Attribute.Closure _ ->
              IContainer.iter_result
                ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold) edges
                ~f:(fun (access, addr_trace) ->
                  if is_captured_fake_access access then
                    check_addr_access (Immediate {imm= lambda; location}) addr_trace astate
                    >>| fun _ -> ()
                  else Ok () )
          | _ ->
              Ok () )
        >>| fun () -> astate


  let write location access_expr pname captured astate =
    let closure_addr = AbstractAddress.mk_fresh () in
    write location access_expr
      (closure_addr, [PulseTrace.Assignment {lhs= access_expr; location}])
      astate
    >>| fun astate ->
    let fake_capture_edges = mk_capture_edges captured in
    Memory.set_cell closure_addr (fake_capture_edges, Attributes.singleton (Closure pname)) astate


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
  module Memory = PulseAbductiveDomain.Memory

  let is_reserved location vector_access_expr astate =
    read location vector_access_expr astate
    >>| fun (astate, (addr, _)) -> (astate, Memory.is_std_vector_reserved addr astate)


  let mark_reserved location vector_access_expr astate =
    read location vector_access_expr astate
    >>| fun (astate, (addr, _)) -> Memory.std_vector_reserve addr astate
end

module Interproc = struct
  open Result.Monad_infix

  (* compute addresses for actuals and then call {!PulseAbductiveDomain.PrePost.apply} on each
     pre/post pair in the summary. *)
  let call callee_pname ~formals ~ret ~actuals _flags call_loc astate pre_posts =
    L.d_printfln "PulseOperations.call" ;
    List.fold_result actuals ~init:(astate, []) ~f:(fun (astate, rev_actual_addresses) actual ->
        match actual with
        | HilExp.AccessExpression access_expr ->
            read call_loc access_expr astate
            >>| fun (astate, (addr, trace)) -> (astate, Some (addr, trace) :: rev_actual_addresses)
        | _ ->
            Ok (astate, None :: rev_actual_addresses) )
    >>= fun (astate, rev_actual_addresses) ->
    let actuals = List.rev rev_actual_addresses in
    read call_loc HilExp.AccessExpression.(address_of_base ret) astate
    >>= fun (astate, ret) ->
    List.fold_result pre_posts ~init:[] ~f:(fun posts pre_post ->
        (* apply all pre/post specs *)
        PulseAbductiveDomain.PrePost.apply callee_pname call_loc pre_post ~formals ~ret ~actuals
          astate
        >>| fun post -> post :: posts )
end
