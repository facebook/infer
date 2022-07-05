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
module StringSet = Caml.Set.Make (String)


let write_field path field new_val location addr astate =
  let* astate, field_addr =
    PulseOperations.eval_access path Write location addr (FieldAccess field) astate
  in
  PulseOperations.write_deref path location ~ref:field_addr ~obj:new_val astate


let call_may_throw_exception (exn : CSharpClassName.t) : model =
 fun {location; path; analysis_data} astate ->
  let ret_addr = AbstractValue.mk_fresh () in
  let exn_name = CSharpClassName.to_string exn in
  let desc = Printf.sprintf "throw_%s" exn_name in
  let ret_alloc_hist = Hist.single_alloc path location desc in
  let typ = Typ.mk_struct (Typ.CSharpClass exn) in
  let astate = PulseOperations.add_dynamic_type typ ret_addr astate in
  let ret_var = Procdesc.get_ret_var analysis_data.proc_desc in
  let astate, ref = PulseOperations.eval_var path location ret_var astate in
  let obj = (ret_addr, ret_alloc_hist) in
  let<*> astate = PulseOperations.write_deref path location ~ref ~obj astate in
  [Ok (ExceptionRaised astate)]

(* let throw : model = fun _ astate -> [Ok (ExceptionRaised astate)] *)


module Resource = struct
  let allocate_aux ~exn_class_name ((this, _) as this_obj) delegation_opt : model =
   fun ({location; callee_procname; path} as model_data) astate ->
    let[@warning "-8"] (Some (Typ.CSharpClass class_name)) =
      Procname.get_class_type_name callee_procname
    in
    let allocator = Attribute.CSharpResource class_name in
    let post = PulseOperations.allocate allocator location this astate in
    let delegated_state =
      let<+> post =
        Option.value_map delegation_opt ~default:(Ok post) ~f:(fun obj ->
            write_field path PulseOperations.ModeledField.delegated_release obj location this_obj
              post )
      in
      post
    in
    let exn_state =
      Option.value_map exn_class_name ~default:[] ~f:(fun cn ->
          call_may_throw_exception (CSharpClassName.from_string cn) model_data astate )
    in
    let ret = delegated_state @ exn_state
    in
    ret


  let allocate ?exn_class_name this_arg : model = allocate_aux ~exn_class_name this_arg None

  let allocate_with_delegation ?exn_class_name () this_arg delegation : model =
    allocate_aux ~exn_class_name this_arg (Some delegation)


  let use ~exn_class_name : model =
    let exn = CSharpClassName.from_string exn_class_name in
    fun model_data astate ->
      Ok (ContinueProgram astate) :: call_may_throw_exception exn model_data astate


  let release ?exn_class_name this : model =
   fun model_data astate ->
    let exn_state =
      Option.value_map exn_class_name ~default:[] ~f:(fun cn ->
          call_may_throw_exception (CSharpClassName.from_string cn) model_data astate )
    in
    (PulseOperations.java_resource_release ~recursive:true (fst this) astate |> Basic.ok_continue) @ exn_state


  let _release_this_only this : model =
   fun _ astate ->
    PulseOperations.java_resource_release ~recursive:false (fst this) astate |> Basic.ok_continue
end

let string_length_access = HilExp.Access.FieldAccess PulseOperations.ModeledField.string_length

let string_is_null_or_whitespace ~desc ((addr, hist) as addr_hist) : model =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let ret_val = AbstractValue.mk_fresh () in
  let astate_null =
    PulseArithmetic.prune_eq_zero addr astate
    >>== PulseArithmetic.and_eq_int ret_val IntLit.one
    >>|| PulseOperations.write_id ret_id (ret_val, Hist.add_event path event hist)
    >>|| ExecutionDomain.continue |> SatUnsat.to_list
  in
  let astate_not_null =
    let<**> astate = PulseArithmetic.prune_positive addr astate in
    let<*> astate, (len_addr, hist) =
      PulseOperations.eval_access path Read location addr_hist string_length_access astate
    in
    let astate = PulseOperations.write_id ret_id (ret_val, Hist.add_event path event hist) astate in
    let astate_empty =
      PulseArithmetic.prune_eq_zero len_addr astate
      >>== PulseArithmetic.and_eq_int ret_val IntLit.one
      >>|| ExecutionDomain.continue
    in
    let astate_not_empty =
      PulseArithmetic.prune_positive len_addr astate
      >>== PulseArithmetic.and_eq_int ret_val IntLit.zero
      >>|| ExecutionDomain.continue
    in
    [astate_empty; astate_not_empty] |> SatUnsat.filter
  in
  astate_null @ astate_not_null

let iDisposablesIgnore = 
    [ "System.IO.MemoryStream"
    ; "System.IO.StringReader"
    ; "System.IO.StringWriter" ]


let input_resource_usage_modeled =
    StringSet.of_list [ "WriteByte"; "Write"; "Read" ]

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let map_context_tenv f (x, _) = f x in
  [ +map_context_tenv (PatternMatch.CSharp.implements "System.String")
    &:: "IsNullOrWhiteSpace" <>$ capt_arg_payload
    $--> string_is_null_or_whitespace ~desc:"String.IsNullOrWhiteSpace"
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.String")
    &:: "IsNullOrEmpty" <>$ capt_arg_payload
    $--> string_is_null_or_whitespace ~desc:"String.IsNullOrEmpty"
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.Diagnostics.Debug")
    &:: "Assert" <>$ capt_arg $--> Basic.assert_
  (* IDisposables that take care of passed resource (stream), and may throw exception *)
  ; +map_context_tenv (PatternMatch.CSharp.implements_one_of
      [ "System.IO.StreamReader"
      ; "System.IO.StreamWriter"
      ; "System.IO.BinaryReader"
      ; "System.IO.BinaryWriter"
        ])
    &:: ".ctor" <>$ capt_arg_payload $+ capt_arg_payload
    $+...$--> Resource.allocate_with_delegation (*~exn_class_name:"System.ArgumentException"*) ()
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.Net.Sockets.NetworkStream")
    &:: ".ctor" <>$ capt_arg_payload $+ capt_arg_payload
    $+...$--> Resource.allocate_with_delegation ~exn_class_name:"System.IO.IOException" ()
  (* Things that may throw an exception *)
  ; +map_context_tenv (PatternMatch.CSharp.implements_one_of
      [ "System.IO.FileStream"
      ; "System.IO.IsolatedStorage.IsolatedStorageFileStream"
      ])
    &:: ".ctor" <>$ capt_arg_payload
    $+...$--> Resource.allocate ~exn_class_name:"System.IO.FileNotFoundException"
    (* Usage of IDisposables *)
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.IO.Stream")
  &::+ (fun _ str -> StringSet.mem str input_resource_usage_modeled)
    <>$ any_arg $+...$--> Resource.use ~exn_class_name:"System.IO.IOException"
  (* Some IDisposables that don't _need_ to be disposed, so don't track *)
  ; +map_context_tenv (PatternMatch.CSharp.implements_one_of iDisposablesIgnore)
    &:: ".ctor" <>$ any_arg $+...$--> Basic.skip
    (* Base case for IDisposables *)
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.IDisposable")
    &:: ".ctor" <>$ capt_arg_payload $+...$--> Resource.allocate
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.IDisposable")
    &:: "close" <>$ capt_arg_payload $--> Resource.release
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.IDisposable")
    &:: "dispose" <>$ capt_arg_payload $--> Resource.release
    ]
