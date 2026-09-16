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
      let opt_addr =
        Stack.find_opt (Var.of_pvar pvar) astate |> Option.map ~f:ValueOrigin.addr_hist
      in
      let default () = (AbstractValue.mk_fresh (), ValueHistory.epoch) in
      (astate, Option.value_or_thunk opt_addr ~default)
  | FieldAccess (fieldname, heap_path) ->
      let astate, src_addr = initialize_heap_path heap_path astate in
      let access = Access.FieldAccess fieldname in
      Memory.eval_edge src_addr access astate
  | Dereference heap_path ->
      let astate, src_addr = initialize_heap_path heap_path astate in
      Memory.eval_edge src_addr Dereference astate


(* Well-known global that bridges specialization (seeds the caller's variadic actuals)
   and the C `va_start`/`va_arg` models (read them back). See #1937. *)
let va_args_global_pvar = Pvar.mk_global (Mangled.from_string "__infer_va_args_global")

(* Seed the global va-args array with the caller's extra (variadic) actuals so that,
   during specialized re-analysis of a C variadic callee, each `va_arg` read connects
   to the corresponding caller argument. *)
let seed_variadic_actuals variadic_actuals location astate =
  match variadic_actuals with
  | [] ->
      astate
  | _ ->
      let astate, global_addr =
        let opt =
          Stack.find_opt (Var.of_pvar va_args_global_pvar) astate
          |> Option.map ~f:ValueOrigin.addr_hist
        in
        match opt with
        | Some ah ->
            (astate, ah)
        | None ->
            let ah = (AbstractValue.mk_fresh (), ValueHistory.epoch) in
            (astate, ah)
      in
      List.foldi variadic_actuals ~init:astate ~f:(fun k astate heap_path ->
          let astate, actual_ah = initialize_heap_path heap_path astate in
          let index = (AbstractValue.mk_fresh (), ValueHistory.epoch) in
          let astate =
            PulseArithmetic.and_eq_int (fst index) (IntLit.of_int k) astate
            |> PulseOperationResult.sat_ok |> Option.value ~default:astate
          in
          let access = Access.ArrayAccess (StdTyp.void, fst index) in
          Memory.add_edge PathContext.initial global_addr access actual_ah location astate )


let apply {Specialization.Pulse.aliases; dynamic_types; variadic_actuals} location astate =
  let astate =
    Option.value_map aliases ~default:astate ~f:(fun aliases ->
        List.fold aliases ~init:astate ~f:(fun astate alias ->
            let astate, values =
              List.fold alias ~init:(astate, []) ~f:(fun (astate, values) heap_path ->
                  let astate, (value, _) = initialize_heap_path heap_path astate in
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
  let astate = seed_variadic_actuals variadic_actuals location astate in
  astate
