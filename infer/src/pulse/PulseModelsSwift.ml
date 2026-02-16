(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseOperationResult.Import
open PulseModelsImport
module DSL = PulseModelsDSL

let alloc size : model_no_non_disj =
 fun model_data astate ->
  let<++> astate =
    Basic.alloc_not_null ~initialize:false ~desc:"alloc" SwiftAlloc (Some size) model_data astate
  in
  astate


let unknown _args () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = fresh () in
  assign_ret res


let function_ptr_call args () : unit DSL.model_monad =
  (* TODO: implement this *)
  unknown args ()


let closure_call orig_args () : unit DSL.model_monad =
  let open DSL.Syntax in
  match orig_args with
  | proc_name_arg :: args -> (
      let* arg_dynamic_type_data = get_dynamic_type ~ask_specialization:true proc_name_arg in
      match arg_dynamic_type_data with
      | Some {Formula.typ= {desc= Typ.Tstruct (SwiftClosure csig)}} -> (
          let proc_name = Procname.Swift (SwiftProcname.mk_function csig) in
          let args = List.mapi ~f:(fun i arg -> (Format.sprintf "arg_%d" i, arg)) args in
          Logging.d_printfln "calling %a with args = %a" Procname.pp proc_name
            (Pp.comma_seq (Pp.pair ~fst:String.pp ~snd:DSL.pp_aval))
            args ;
          let* res_opt = swift_call proc_name args |> is_unsat in
          match res_opt with
          | Some res ->
              assign_ret res
          | None -> (
              (* if the call is unsat, it could be because of mismatched arguments, because the specialised
                 closure doesn't capture any variables, and so the captured argument is not needed. In this
                 case, we try the call again, by removing the last argument, which will be null. *)
              let args = List.take args (List.length args - 1) in
              Logging.d_printfln "calling %a again with args = %a" Procname.pp proc_name
                (Pp.comma_seq (Pp.pair ~fst:String.pp ~snd:DSL.pp_aval))
                args ;
              let* res_opt = swift_call proc_name args |> is_unsat in
              match res_opt with Some res -> assign_ret res | None -> unreachable ) )
      | _ ->
          Logging.d_printfln "no method name found for closure %a" DSL.pp_aval proc_name_arg ;
          function_ptr_call args () )
  | [] ->
      function_ptr_call orig_args ()


let dynamic_call arg orig_args () : unit DSL.model_monad =
  let open DSL.Syntax in
  let dynamic_call_with_type name offset self args =
    let args = List.mapi ~f:(fun i arg -> (Format.sprintf "arg_%d" i, arg)) (args @ [self]) in
    let* proc_name = tenv_resolve_method_with_offset name offset in
    match proc_name with
    | Some proc_name ->
        Logging.d_printfln "calling %a with args = %a" Procname.pp proc_name
          (Pp.comma_seq (Pp.pair ~fst:String.pp ~snd:DSL.pp_aval))
          args ;
        let* res = swift_call proc_name args in
        assign_ret res
    | None ->
        Logging.d_printfln "proc_name not found for name = %a and offset = %a" Typ.Name.pp name
          Int.pp offset ;
        unknown args ()
  in
  let* offset_opt = as_constant_int arg in
  match (offset_opt, List.rev orig_args) with
  | Some offset, self :: actuals -> (
      let args = List.rev actuals in
      let* arg_dynamic_type_data = get_dynamic_type ~ask_specialization:true self in
      match arg_dynamic_type_data with
      | Some {Formula.typ= {desc= Tstruct name}} ->
          dynamic_call_with_type name offset self args
      | _ -> (
          let* arg_static_type = get_static_type self in
          match arg_static_type with
          | Some (Typ.SwiftClass class_name as name)
            when not (SwiftClassName.equal (SwiftClassName.of_string "ptr_elt") class_name) ->
              dynamic_call_with_type name offset self args
          | _ ->
              Logging.d_printfln
                "method to call not found, no dynamic or static type found for %a, returning a \
                 fresh value"
                DSL.pp_aval self ;
              unknown args () ) )
  | _, _ ->
      closure_call (arg :: orig_args) ()


let derived_enum_equals arg1 arg2 () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = binop Binop.Eq arg1 arg2 in
  assign_ret res


let builtins_matcher builtin args : unit -> unit DSL.model_monad =
  let builtin_s = SwiftProcname.show_builtin builtin in
  match (builtin : SwiftProcname.builtin) with
  | NonDet ->
      unknown args
  | InitTuple ->
      unknown args
  | DynamicCall -> (
    match args with arg :: args -> dynamic_call arg args | [] -> unknown args )
  | DerivedEnumEquals -> (
      let arg1, arg2, args = ProcnameDispatcherBuiltins.expect_at_least_2_args args builtin_s in
      (* we are modelling the case for simple enums where there are two args here, in the case
         of complex enums there can be more args, but we are not modelling that yet. *)
      match args with
      | [] ->
          derived_enum_equals arg1 arg2
      | _ ->
          unknown args )
  | ObjcMsgSend ->
      (* TODO T251645387 *)
      unknown args
  | ObjcMsgSendSuper2 ->
      unknown args
  | Memcpy ->
      unknown args


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [+BuiltinDecl.(match_builtin __swift_alloc) <>$ capt_exp $--> alloc]
  |> List.map ~f:(fun matcher ->
         matcher
         |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist
         |> ProcnameDispatcher.Call.map_matcher ~f:lift_model )
