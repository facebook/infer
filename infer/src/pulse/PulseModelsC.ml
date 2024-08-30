(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module DSL = PulseModelsDSL
module FuncArg = ProcnameDispatcher.Call.FuncArg

let free deleted_access : model = Basic.free_or_delete `Free CFree deleted_access

let invalidate path access_path location cause addr_trace : unit DSL.model_monad =
  let open DSL.Syntax in
  PulseOperations.invalidate path access_path location cause addr_trace |> exec_command


let alloc_common_dsl ~null_case ~initialize allocator size_exp_opt : unit DSL.model_monad =
  let open DSL.Syntax in
  let* {path; location; ret= ret_id, _} = get_data in
  let astate_alloc = Basic.return_alloc_not_null allocator ~initialize size_exp_opt in
  if null_case then
    let result_null =
      let* ret_addr = fresh ~more:"(null case)" () in
      assign_ret ret_addr @@> and_eq_int ret_addr IntLit.zero
      @@> invalidate path
            (StackAddress (Var.of_id ret_id, snd ret_addr))
            location (ConstantDereference IntLit.zero) ret_addr
    in
    disj [astate_alloc; result_null]
  else astate_alloc


let alloc_common ~null_case ~initialize ~desc allocator size_exp_opt : model =
  let open DSL.Syntax in
  start_named_model desc @@ alloc_common_dsl ~null_case ~initialize allocator size_exp_opt


let malloc ~null_case size_exp =
  alloc_common ~null_case ~initialize:false ~desc:"malloc" CMalloc (Some size_exp)


let custom_malloc ~null_case size_exp model_data astate =
  alloc_common ~null_case ~initialize:false ~desc:"custom malloc"
    (CustomMalloc model_data.callee_procname) (Some size_exp) model_data astate


let custom_alloc_not_null desc model_data astate =
  alloc_common ~initialize:false ~null_case:false ~desc (CustomMalloc model_data.callee_procname)
    None model_data astate


let realloc_common ~desc allocator pointer size : model =
 fun data astate non_disj ->
  free pointer data astate non_disj
  |> NonDisjDomain.bind ~f:(fun result non_disj ->
         let ( let<*> ) x f = bind_sat_result non_disj (Sat x) f in
         let<*> exec_state = result in
         match (exec_state : ExecutionDomain.t) with
         | ContinueProgram astate ->
             alloc_common ~null_case:true ~initialize:false ~desc allocator (Some size) data astate
               non_disj
         | ExceptionRaised _
         | ExitProgram _
         | AbortProgram _
         | LatentAbortProgram _
         | LatentInvalidAccess _
         | LatentSpecializedTypeIssue _ ->
             ([Ok exec_state], non_disj) )


let realloc = realloc_common ~desc:"realloc" CRealloc

let custom_realloc pointer size data astate =
  realloc_common ~desc:"custom realloc" (CustomRealloc data.callee_procname) pointer size data
    astate


let call_c_function_ptr {FuncArg.arg_payload= function_ptr} actuals : model =
 fun {path; analysis_data; location; ret= (ret_id, _) as ret; dispatch_call_eval_args} astate
     non_disj ->
  let callee_proc_name_opt =
    match PulseArithmetic.get_dynamic_type (ValueOrigin.value function_ptr) astate with
    | Some {typ= {desc= Typ.Tstruct (Typ.CFunction csig)}} ->
        Some (Procname.C csig)
    | _ ->
        None
  in
  match callee_proc_name_opt with
  | Some callee_proc_name ->
      dispatch_call_eval_args analysis_data path ret (Const (Cfun callee_proc_name)) actuals
        location CallFlags.default astate non_disj (Some callee_proc_name)
  | None ->
      (* we don't know what procname this function pointer resolves to *)
      let res =
        (* dereference call expression to catch nil issues *)
        let<+> astate, _ =
          PulseOperations.eval_access path Read location
            (ValueOrigin.addr_hist function_ptr)
            Dereference astate
        in
        let desc = Procname.to_string BuiltinDecl.__call_c_function_ptr in
        let hist = Hist.single_event path (Hist.call_event path location desc) in
        let astate = PulseOperations.havoc_id ret_id hist astate in
        let astate =
          AbductiveDomain.add_need_dynamic_type_specialization (ValueOrigin.value function_ptr)
            astate
        in
        let astate =
          let unknown_effect = Attribute.UnknownEffect (Model desc, hist) in
          List.fold actuals ~init:astate ~f:(fun acc FuncArg.{arg_payload= actual} ->
              AddressAttributes.add_one (ValueOrigin.value actual) unknown_effect acc )
        in
        astate
      in
      (res, non_disj)


(** a few models from (g)libc and beyond *)
include struct
  open DSL.Syntax

  let valid_arg arg : model = start_model @@ check_valid arg

  let valid_args2 arg1 arg2 : model = start_model @@ check_valid arg1 @@> check_valid arg2

  let non_det_ret : model = start_model @@ assign_ret @= fresh ()

  let zero_or_minus_one_ret : model =
    start_model @@ disj [assign_ret @= int (-1); assign_ret @= int 0]


  let non_det_or_minus_one_ret : model =
    start_model @@ disj [assign_ret @= int (-1); assign_ret @= fresh ()]


  let ret_alloc_or_null allocator =
    disj [assign_ret @= null; Basic.return_alloc_not_null allocator None ~initialize:true]


  let fclose stream : model =
    start_model @@ Basic.free FClose stream
    @@> disj [assign_ret @= int (-1 (* EOF *)); assign_ret @= int 0]


  let fgetpos stream pos =
    start_model @@ check_valid stream
    @@>
    let* obj = fresh () in
    store ~ref:(to_aval pos) obj @@> disj [assign_ret @= int 0; assign_ret @= int (-1)]


  let gets str : model =
    start_model @@ check_valid str @@> disj [assign_ret @= null; assign_ret (to_aval str)]


  let fgets str stream : model =
    start_model @@ check_valid stream @@> check_valid str
    @@> disj [assign_ret @= null; assign_ret (to_aval str)]
    @@> data_dependency str [str; stream]


  let fopen path mode : model =
    start_model @@ check_valid path @@> check_valid mode @@> ret_alloc_or_null FileDescriptor
    @@> data_dependency_to_ret [path]


  let fprintf stream format args =
    start_model @@ check_valid stream @@> check_valid format
    @@> data_dependency stream (format :: args)
    @@> assign_ret (* pretend [fprintf] always succeeds *) @= fresh_nonneg ()


  let fputs s stream =
    start_model @@ check_valid stream @@> check_valid s @@> data_dependency stream [s]
    @@> assign_ret (* pretend [fputs] always succeeds *) @= fresh_nonneg ()


  let putc c stream : model =
    start_model @@ check_valid stream
    @@> disj [assign_ret @= int (-1); assign_ret (to_aval c)]
    @@> data_dependency stream [c]
end

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let open DSL.Syntax in
  let match_regexp_opt r_opt (_tenv, proc_name) _ =
    Option.exists r_opt ~f:(fun r ->
        let s = Procname.to_string proc_name in
        Str.string_match r s 0 )
  in
  let rev_compose1 model2 model1 = compose1 model1 model2 in
  let ignore_arg model _arg = model in
  let ignore_args2 model _arg1 _arg2 = model in
  let taint_ret_from_arg arg = start_model @@ data_dependency_to_ret [arg] in
  let map_context_tenv f (x, _) = f x in
  [ +BuiltinDecl.(match_builtin free) <>$ capt_arg $--> free
  ; +match_regexp_opt Config.pulse_model_free_pattern <>$ capt_arg $+...$--> free
  ; -"realloc" <>$ capt_arg $+ capt_exp $--> realloc
  ; +match_regexp_opt Config.pulse_model_realloc_pattern
    <>$ capt_arg $+ capt_exp $+...$--> custom_realloc
  ; +BuiltinDecl.(match_builtin __call_c_function_ptr) $ capt_arg $++$--> call_c_function_ptr
  ; -"clearerr" <>$ capt_arg_payload $--> valid_arg
  ; -"fclose" <>$ capt_arg $--> fclose
  ; -"feof" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"ferror" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"fgetc" <>$ capt_arg_payload
    $--> (valid_arg |> rev_compose1 (ignore_arg non_det_ret) |> rev_compose1 taint_ret_from_arg)
  ; -"fgetpos" <>$ capt_arg_payload $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"fgetpos" <>$ capt_arg_payload $+ capt_arg_payload $--> fgetpos
  ; -"fgets" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload $--> fgets
  ; -"fileno" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"fopen" <>$ capt_arg_payload $+ capt_arg_payload $--> fopen
  ; -"fprintf" <>$ capt_arg_payload $+ capt_arg_payload $+++$--> fprintf
  ; -"fputc" <>$ capt_arg_payload $+ capt_arg_payload $--> putc
  ; -"fputs" <>$ capt_arg_payload $+ capt_arg_payload $--> fputs
  ; -"fseek" <>$ capt_arg_payload $+ any_arg $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"fsetpos" <>$ capt_arg_payload $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"fsetpos" <>$ capt_arg_payload $+ capt_arg_payload
    $--> compose2 valid_args2 (ignore_args2 zero_or_minus_one_ret)
  ; -"ftell" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_or_minus_one_ret)
  ; -"getc" <>$ capt_arg_payload
    $--> (valid_arg |> rev_compose1 (ignore_arg non_det_ret) |> rev_compose1 taint_ret_from_arg)
  ; -"gets" <>$ capt_arg_payload $--> gets
  ; -"printf" &--> start_model @@ assign_ret @= fresh ()
  ; -"putc" <>$ capt_arg_payload $+ capt_arg_payload $--> putc
  ; -"rewind" <>$ capt_arg_payload $--> valid_arg
  ; -"ungetc" <>$ any_arg $+ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"vfprintf" <>$ capt_arg_payload $+ capt_arg_payload $+++$--> fprintf ]
  @ ( [ +BuiltinDecl.(match_builtin malloc)
        <>$ capt_exp
        $--> malloc ~null_case:(not Config.pulse_unsafe_malloc)
      ; +match_regexp_opt Config.pulse_model_malloc_pattern
        <>$ capt_exp
        $+...$--> custom_malloc ~null_case:(not Config.pulse_unsafe_malloc)
      ; +map_context_tenv PatternMatch.ObjectiveC.is_core_graphics_create_or_copy
        &--> custom_alloc_not_null "CGCreate/Copy"
      ; +map_context_tenv PatternMatch.ObjectiveC.is_core_foundation_create_or_copy
        &--> custom_alloc_not_null "CFCreate/Copy"
      ; +BuiltinDecl.(match_builtin malloc_no_fail) <>$ capt_exp $--> malloc ~null_case:false
      ; +match_regexp_opt Config.pulse_model_alloc_pattern &--> custom_alloc_not_null "custom alloc"
      ]
    |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist) )
