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
      let* proc_name_opt = as_constant_string proc_name_arg in
      match proc_name_opt with
      | Some proc_name ->
          let proc_name =
            Procname.Swift (SwiftProcname.mk_function (Mangled.from_string proc_name))
          in
          let args = List.mapi ~f:(fun i arg -> (Format.sprintf "arg_%d" i, arg)) args in
          Logging.d_printfln "calling %a with args = %a" Procname.pp proc_name
            (Pp.comma_seq (Pp.pair ~fst:String.pp ~snd:DSL.pp_aval))
            args ;
          let* res = swift_call proc_name args in
          assign_ret res
      | None ->
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
          | Some name ->
              dynamic_call_with_type name offset self args
          | None ->
              Logging.d_printfln
                "method to call not found, no dynamic or static type found for %a, returning a \
                 fresh value"
                DSL.pp_aval self ;
              unknown args () ) )
  | _, _ ->
      closure_call (arg :: orig_args) ()


let builtins_matcher builtin args : unit -> unit DSL.model_monad =
  let builtin_s = SwiftProcname.show_builtin builtin in
  match (builtin : SwiftProcname.builtin) with
  | NonDet ->
      unknown args
  | InitTuple ->
      unknown args
  | DynamicCall ->
      let arg, args = ProcnameDispatcherBuiltins.expect_at_least_1_arg args builtin_s in
      dynamic_call arg args


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [+BuiltinDecl.(match_builtin __swift_alloc) <>$ capt_exp $--> alloc]
  |> List.map ~f:(fun matcher ->
         matcher
         |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist
         |> ProcnameDispatcher.Call.map_matcher ~f:lift_model )
