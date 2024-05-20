(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

let prune_eq_list_values astate values =
  let prune_eq astate val1 val2 =
    PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand val1)
      (AbstractValueOperand val2) astate
  in
  match values with
  | [] ->
      astate
  | head :: tail ->
      List.fold tail ~init:astate ~f:(fun astate value ->
          prune_eq astate head value |> PulseOperationResult.sat_ok |> Option.value ~default:astate )


let rec initialize_heap_path heap_path astate =
  match (heap_path : Specialization.HeapPath.t) with
  | Pvar pvar ->
      let opt_addr = Stack.find_opt (Var.of_pvar pvar) astate in
      let default () = (AbstractValue.mk_fresh (), ValueHistory.epoch) in
      (astate, Option.value_or_thunk opt_addr ~default)
  | FieldAccess (fieldname, heap_path) ->
      let astate, src_addr = initialize_heap_path heap_path astate in
      let access = Access.FieldAccess fieldname in
      Memory.eval_edge src_addr access astate
  | Dereference heap_path ->
      let astate, src_addr = initialize_heap_path heap_path astate in
      Memory.eval_edge src_addr Dereference astate


let apply {Specialization.Pulse.aliases; dynamic_types} location astate =
  let astate =
    Option.value_map aliases ~default:astate ~f:(fun aliases ->
        List.fold aliases ~init:astate ~f:(fun astate alias ->
            let astate, values =
              List.fold alias ~init:(astate, []) ~f:(fun (astate, values) heap_path ->
                  let astate, addr = initialize_heap_path heap_path astate in
                  let astate, (value, _) = Memory.eval_edge addr Dereference astate in
                  (astate, value :: values) )
            in
            prune_eq_list_values astate values ) )
  in
  let astate =
    Specialization.HeapPath.Map.fold
      (fun heap_path typename astate ->
        let astate, (addr, _) = initialize_heap_path heap_path astate in
        let typ = Typ.mk_struct typename in
        PulseArithmetic.and_dynamic_type_is_unsafe addr typ location astate )
      dynamic_types astate
  in
  astate
