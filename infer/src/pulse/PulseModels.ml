(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Result.Monad_infix
open PulseBasicInterface
open PulseDomainInterface

type arg_payload = AbstractValue.t * ValueHistory.t

type exec_fun =
     caller_summary:Summary.t
  -> Location.t
  -> ret:Ident.t * Typ.t
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t list PulseOperations.access_result

type model = exec_fun

module Misc = struct
  let shallow_copy model_desc dest_pointer_hist src_pointer_hist : model =
   fun ~caller_summary:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model model_desc; location; in_call= []} in
    PulseOperations.eval_access location src_pointer_hist Dereference astate
    >>= fun (astate, obj) ->
    PulseOperations.shallow_copy location obj astate
    >>= fun (astate, obj_copy) ->
    PulseOperations.write_deref location ~ref:dest_pointer_hist
      ~obj:(fst obj_copy, event :: snd obj_copy)
      astate
    >>| fun astate -> [PulseOperations.havoc_id ret_id [event] astate]


  let early_exit : model = fun ~caller_summary:_ _ ~ret:_ _ -> Ok []

  let return_int : Int64.t -> model =
   fun i64 ~caller_summary:_ location ~ret:(ret_id, _) astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let astate =
      Memory.add_attribute ret_addr
        (Arithmetic (Arithmetic.equal_to (IntLit.of_int64 i64), Immediate {location; history= []}))
        astate
    in
    Ok [PulseOperations.write_id ret_id (ret_addr, []) astate]


  let skip : model = fun ~caller_summary:_ _ ~ret:_ astate -> Ok [astate]

  let id_first_arg arg_access_hist : model =
   fun ~caller_summary:_ _ ~ret astate ->
    Ok [PulseOperations.write_id (fst ret) arg_access_hist astate]
end

module C = struct
  let free deleted_access : model =
   fun ~caller_summary:_ location ~ret:_ astate ->
    PulseOperations.invalidate location Invalidation.CFree deleted_access astate >>| List.return
end

module Cplusplus = struct
  let delete deleted_access : model =
   fun ~caller_summary:_ location ~ret:_ astate ->
    PulseOperations.invalidate location Invalidation.CppDelete deleted_access astate >>| List.return


  let placement_new actuals : model =
   fun ~caller_summary:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "<placement new>()"; location; in_call= []} in
    match List.rev actuals with
    | ProcnameDispatcher.Call.FuncArg.{arg_payload= address, hist} :: _ ->
        Ok [PulseOperations.write_id ret_id (address, event :: hist) astate]
    | _ ->
        Ok [PulseOperations.havoc_id ret_id [event] astate]
end

module StdBasicString = struct
  let internal_string =
    Typ.Fieldname.Clang.from_class_name
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "basic_string"]))
      "__infer_model_backing_string"


  let internal_string_access = HilExp.Access.FieldAccess internal_string

  let to_internal_string location bstring astate =
    PulseOperations.eval_access location bstring internal_string_access astate


  let data this_hist : model =
   fun ~caller_summary:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "std::basic_string::data()"; location; in_call= []} in
    to_internal_string location this_hist astate
    >>= fun (astate, string_addr_hist) ->
    PulseOperations.eval_access location string_addr_hist Dereference astate
    >>| fun (astate, (string, hist)) ->
    [PulseOperations.write_id ret_id (string, event :: hist) astate]


  let destructor this_hist : model =
   fun ~caller_summary:_ location ~ret:_ astate ->
    let model = CallEvent.Model "std::basic_string::~basic_string()" in
    let call_event = ValueHistory.Call {f= model; location; in_call= []} in
    to_internal_string location this_hist astate
    >>= fun (astate, (string_addr, string_hist)) ->
    let string_addr_hist = (string_addr, call_event :: string_hist) in
    PulseOperations.invalidate_deref location CppDelete string_addr_hist astate
    >>= fun astate ->
    PulseOperations.invalidate location CppDelete string_addr_hist astate >>| fun astate -> [astate]
end

module StdFunction = struct
  let operator_call lambda_ptr_hist actuals : model =
   fun ~caller_summary location ~ret astate ->
    let havoc_ret (ret_id, _) astate =
      let event = ValueHistory.Call {f= Model "std::function::operator()"; location; in_call= []} in
      [PulseOperations.havoc_id ret_id [event] astate]
    in
    PulseOperations.eval_access location lambda_ptr_hist Dereference astate
    >>= fun (astate, (lambda, _)) ->
    PulseOperations.Closures.check_captured_addresses location lambda astate
    >>= fun astate ->
    match PulseAbductiveDomain.Memory.get_closure_proc_name lambda astate with
    | None ->
        (* we don't know what proc name this lambda resolves to *) Ok (havoc_ret ret astate)
    | Some callee_proc_name ->
        let actuals =
          List.map actuals ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
              (arg_payload, typ) )
        in
        PulseOperations.call ~caller_summary location callee_proc_name ~ret ~actuals astate
end

module StdVector = struct
  let internal_array =
    Typ.Fieldname.Clang.from_class_name
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "vector"]))
      "__infer_model_backing_array"


  let internal_array_access = HilExp.Access.FieldAccess internal_array

  let to_internal_array location vector astate =
    PulseOperations.eval_access location vector internal_array_access astate


  let element_of_internal_array location vector index astate =
    to_internal_array location vector astate
    >>= fun (astate, vector_internal_array) ->
    PulseOperations.eval_access location vector_internal_array
      (ArrayAccess (Typ.void, index))
      astate


  let reallocate_internal_array trace vector vector_f location astate =
    to_internal_array location vector astate
    >>= fun (astate, array_address) ->
    PulseOperations.invalidate_array_elements location (StdVector vector_f) array_address astate
    >>= PulseOperations.invalidate_deref location (StdVector vector_f) array_address
    >>= PulseOperations.havoc_field location vector internal_array trace


  let invalidate_references vector_f vector : model =
   fun ~caller_summary:_ location ~ret:_ astate ->
    let crumb =
      ValueHistory.Call
        { f= Model (Format.asprintf "%a()" Invalidation.pp_std_vector_function vector_f)
        ; location
        ; in_call= [] }
    in
    reallocate_internal_array [crumb] vector vector_f location astate >>| List.return


  let at ~desc vector index : model =
   fun ~caller_summary:_ location ~ret astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    element_of_internal_array location vector (fst index) astate
    >>| fun (astate, (addr, hist)) ->
    [PulseOperations.write_id (fst ret) (addr, event :: hist) astate]


  let reserve vector : model =
   fun ~caller_summary:_ location ~ret:_ astate ->
    let crumb = ValueHistory.Call {f= Model "std::vector::reserve()"; location; in_call= []} in
    reallocate_internal_array [crumb] vector Reserve location astate
    >>| PulseAbductiveDomain.Memory.std_vector_reserve (fst vector)
    >>| List.return


  let push_back vector : model =
   fun ~caller_summary:_ location ~ret:_ astate ->
    let crumb = ValueHistory.Call {f= Model "std::vector::push_back()"; location; in_call= []} in
    if PulseAbductiveDomain.Memory.is_std_vector_reserved (fst vector) astate then
      (* assume that any call to [push_back] is ok after one called [reserve] on the same vector
         (a perfect analysis would also make sure we don't exceed the reserved size) *)
      Ok [astate]
    else
      (* simulate a re-allocation of the underlying array every time an element is added *)
      reallocate_internal_array [crumb] vector PushBack location astate >>| List.return
end

module ProcNameDispatcher = struct
  let dispatch : (Tenv.t, model, arg_payload) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let match_builtin builtin _ s = String.equal s (Typ.Procname.get_method builtin) in
    make_dispatcher
      [ +match_builtin BuiltinDecl.free <>$ capt_arg_payload $--> C.free
      ; +match_builtin BuiltinDecl.__delete <>$ capt_arg_payload $--> Cplusplus.delete
      ; +match_builtin BuiltinDecl.__placement_new &++> Cplusplus.placement_new
      ; +match_builtin BuiltinDecl.objc_cpp_throw <>--> Misc.early_exit
      ; +match_builtin BuiltinDecl.__cast <>$ capt_arg_payload $+...$--> Misc.id_first_arg
      ; +match_builtin BuiltinDecl.abort <>--> Misc.early_exit
      ; +match_builtin BuiltinDecl.exit <>--> Misc.early_exit
      ; -"folly" &:: "DelayedDestruction" &:: "destroy" &--> Misc.skip
      ; -"folly" &:: "Optional" &:: "reset" &--> Misc.skip
      ; -"folly" &:: "SocketAddress" &:: "~SocketAddress" &--> Misc.skip
      ; -"std" &:: "basic_string" &:: "data" <>$ capt_arg_payload $--> StdBasicString.data
      ; -"std" &:: "basic_string" &:: "~basic_string" <>$ capt_arg_payload
        $--> StdBasicString.destructor
      ; -"std" &:: "function" &:: "operator()" $ capt_arg_payload $++$--> StdFunction.operator_call
      ; -"std" &:: "function" &:: "operator=" $ capt_arg_payload $+ capt_arg_payload
        $--> Misc.shallow_copy "std::function::operator="
      ; -"std" &:: "integral_constant" < any_typ &+ capt_int
        >::+ (fun _ name -> String.is_prefix ~prefix:"operator_" name)
        <>--> Misc.return_int
      ; -"std" &:: "vector" &:: "assign" <>$ capt_arg_payload
        $+...$--> StdVector.invalidate_references Assign
      ; -"std" &:: "vector" &:: "clear" <>$ capt_arg_payload
        $--> StdVector.invalidate_references Clear
      ; -"std" &:: "vector" &:: "emplace" $ capt_arg_payload
        $+...$--> StdVector.invalidate_references Emplace
      ; -"std" &:: "vector" &:: "emplace_back" $ capt_arg_payload
        $+...$--> StdVector.invalidate_references EmplaceBack
      ; -"std" &:: "vector" &:: "insert" <>$ capt_arg_payload
        $+...$--> StdVector.invalidate_references Insert
      ; -"std" &:: "vector" &:: "operator[]" <>$ capt_arg_payload $+ capt_arg_payload
        $--> StdVector.at ~desc:"std::vector::at()"
      ; -"std" &:: "vector" &:: "shrink_to_fit" <>$ capt_arg_payload
        $--> StdVector.invalidate_references ShrinkToFit
      ; -"std" &:: "vector" &:: "push_back" <>$ capt_arg_payload $+...$--> StdVector.push_back
      ; -"std" &:: "vector" &:: "reserve" <>$ capt_arg_payload $+...$--> StdVector.reserve
      ; +PatternMatch.implements_collection
        &:: "get" <>$ capt_arg_payload $+ capt_arg_payload
        $--> StdVector.at ~desc:"Collection.get()" ]
end

let dispatch tenv proc_name args = ProcNameDispatcher.dispatch tenv proc_name args
