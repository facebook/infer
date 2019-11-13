(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Result.Monad_infix
open PulseBasicInterface

type exec_fun =
     caller_summary:Summary.t
  -> Location.t
  -> ret:Ident.t * Typ.t
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t list PulseOperations.access_result

type model = exec_fun

module Misc = struct
  let shallow_copy model_desc : model =
   fun ~caller_summary:_ location ~ret:(ret_id, _) ~actuals astate ->
    let event = ValueHistory.Call {f= Model model_desc; location; in_call= []} in
    ( match actuals with
    | [(dest_pointer_hist, _); (src_pointer_hist, _)] ->
        PulseOperations.eval_access location src_pointer_hist Dereference astate
        >>= fun (astate, obj) ->
        PulseOperations.shallow_copy location obj astate
        >>= fun (astate, obj_copy) ->
        PulseOperations.write_deref location ~ref:dest_pointer_hist
          ~obj:(fst obj_copy, event :: snd obj_copy)
          astate
    | _ ->
        Ok astate )
    >>| fun astate -> [PulseOperations.havoc_id ret_id [event] astate]


  let early_exit : model = fun ~caller_summary:_ _ ~ret:_ ~actuals:_ _ -> Ok []

  let skip : model = fun ~caller_summary:_ _ ~ret:_ ~actuals:_ astate -> Ok [astate]

  let id_first_arg : model =
   fun ~caller_summary:_ _ ~ret ~actuals astate ->
    match actuals with
    | (arg_access_hist, _) :: _ ->
        Ok [PulseOperations.write_id (fst ret) arg_access_hist astate]
    | _ ->
        Ok [astate]
end

module C = struct
  let free : model =
   fun ~caller_summary:_ location ~ret:_ ~actuals astate ->
    match actuals with
    | [(deleted_access, _)] ->
        PulseOperations.invalidate location Invalidation.CFree deleted_access astate >>| List.return
    | _ ->
        Ok [astate]
end

module Cplusplus = struct
  let delete : model =
   fun ~caller_summary:_ location ~ret:_ ~actuals astate ->
    match actuals with
    | [(deleted_access, _)] ->
        PulseOperations.invalidate location Invalidation.CppDelete deleted_access astate
        >>| List.return
    | _ ->
        Ok [astate]


  let placement_new : model =
   fun ~caller_summary:_ location ~ret:(ret_id, _) ~actuals astate ->
    let event = ValueHistory.Call {f= Model "<placement new>()"; location; in_call= []} in
    match List.rev actuals with
    | ((address, hist), _) :: _ ->
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


  let data : model =
   fun ~caller_summary:_ location ~ret:(ret_id, _) ~actuals astate ->
    let event = ValueHistory.Call {f= Model "std::basic_string::data()"; location; in_call= []} in
    match actuals with
    | [(this_hist, _)] ->
        to_internal_string location this_hist astate
        >>= fun (astate, string_addr_hist) ->
        PulseOperations.eval_access location string_addr_hist Dereference astate
        >>| fun (astate, (string, hist)) ->
        [PulseOperations.write_id ret_id (string, event :: hist) astate]
    | _ ->
        Ok [PulseOperations.havoc_id ret_id [event] astate]


  let destructor : model =
   fun ~caller_summary:_ location ~ret:(ret_id, _) ~actuals astate ->
    let model = CallEvent.Model "std::basic_string::~basic_string()" in
    let call_event = ValueHistory.Call {f= model; location; in_call= []} in
    match actuals with
    | [(this_hist, _)] ->
        to_internal_string location this_hist astate
        >>= fun (astate, (string_addr, string_hist)) ->
        let string_addr_hist = (string_addr, call_event :: string_hist) in
        PulseOperations.invalidate_deref location CppDelete string_addr_hist astate
        >>= fun astate ->
        PulseOperations.invalidate location CppDelete string_addr_hist astate
        >>| fun astate -> [astate]
    | _ ->
        Ok [PulseOperations.havoc_id ret_id [call_event] astate]
end

module StdFunction = struct
  let operator_call : model =
   fun ~caller_summary location ~ret ~actuals astate ->
    let havoc_ret (ret_id, _) astate =
      let event = ValueHistory.Call {f= Model "std::function::operator()"; location; in_call= []} in
      [PulseOperations.havoc_id ret_id [event] astate]
    in
    match actuals with
    | [] ->
        Ok (havoc_ret ret astate)
    | (lambda_ptr_hist, _) :: actuals -> (
        PulseOperations.eval_access location lambda_ptr_hist Dereference astate
        >>= fun (astate, (lambda, _)) ->
        PulseOperations.Closures.check_captured_addresses location lambda astate
        >>= fun astate ->
        match PulseAbductiveDomain.Memory.get_closure_proc_name lambda astate with
        | None ->
            (* we don't know what proc name this lambda resolves to *) Ok (havoc_ret ret astate)
        | Some callee_proc_name ->
            PulseOperations.call ~caller_summary location callee_proc_name ~ret ~actuals astate )
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


  let invalidate_references vector_f : model =
   fun ~caller_summary:_ location ~ret:_ ~actuals astate ->
    match actuals with
    | (vector, _) :: _ ->
        let crumb =
          ValueHistory.Call
            { f= Model (Format.asprintf "%a()" Invalidation.pp_std_vector_function vector_f)
            ; location
            ; in_call= [] }
        in
        reallocate_internal_array [crumb] vector vector_f location astate >>| List.return
    | _ ->
        Ok [astate]


  let at ~desc : model =
   fun ~caller_summary:_ location ~ret ~actuals astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    match actuals with
    | [(vector, _); (index, _)] ->
        element_of_internal_array location vector (fst index) astate
        >>| fun (astate, (addr, hist)) ->
        [PulseOperations.write_id (fst ret) (addr, event :: hist) astate]
    | _ ->
        Ok [PulseOperations.havoc_id (fst ret) [event] astate]


  let reserve : model =
   fun ~caller_summary:_ location ~ret:_ ~actuals astate ->
    match actuals with
    | [(vector, _); _value] ->
        let crumb = ValueHistory.Call {f= Model "std::vector::reserve()"; location; in_call= []} in
        reallocate_internal_array [crumb] vector Reserve location astate
        >>| PulseAbductiveDomain.Memory.std_vector_reserve (fst vector)
        >>| List.return
    | _ ->
        Ok [astate]


  let push_back : model =
   fun ~caller_summary:_ location ~ret:_ ~actuals astate ->
    match actuals with
    | [(vector, _); _value] ->
        let crumb =
          ValueHistory.Call {f= Model "std::vector::push_back()"; location; in_call= []}
        in
        if PulseAbductiveDomain.Memory.is_std_vector_reserved (fst vector) astate then
          (* assume that any call to [push_back] is ok after one called [reserve] on the same vector
             (a perfect analysis would also make sure we don't exceed the reserved size) *)
          Ok [astate]
        else
          (* simulate a re-allocation of the underlying array every time an element is added *)
          reallocate_internal_array [crumb] vector PushBack location astate >>| List.return
    | _ ->
        Ok [astate]
end

module ProcNameDispatcher = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.ProcName.dispatcher =
    let open ProcnameDispatcher.ProcName in
    make_dispatcher
      [ -"folly" &:: "DelayedDestruction" &:: "destroy" &--> Misc.skip
      ; -"folly" &:: "Optional" &:: "reset" &--> Misc.skip
      ; -"folly" &:: "SocketAddress" &:: "~SocketAddress" &--> Misc.skip
      ; -"std" &:: "basic_string" &:: "data" &--> StdBasicString.data
      ; -"std" &:: "basic_string" &:: "~basic_string" &--> StdBasicString.destructor
      ; -"std" &:: "function" &:: "operator()" &--> StdFunction.operator_call
      ; -"std" &:: "function" &:: "operator=" &--> Misc.shallow_copy "std::function::operator="
      ; -"std" &:: "vector" &:: "assign" &--> StdVector.invalidate_references Assign
      ; -"std" &:: "vector" &:: "clear" &--> StdVector.invalidate_references Clear
      ; -"std" &:: "vector" &:: "emplace" &--> StdVector.invalidate_references Emplace
      ; -"std" &:: "vector" &:: "emplace_back" &--> StdVector.invalidate_references EmplaceBack
      ; -"std" &:: "vector" &:: "insert" &--> StdVector.invalidate_references Insert
      ; -"std" &:: "vector" &:: "operator[]" &--> StdVector.at ~desc:"std::vector::at()"
      ; -"std" &:: "vector" &:: "push_back" &--> StdVector.push_back
      ; -"std" &:: "vector" &:: "reserve" &--> StdVector.reserve
      ; -"std" &:: "vector" &:: "shrink_to_fit" &--> StdVector.invalidate_references ShrinkToFit
      ; +PatternMatch.implements_collection &:: "get" <>--> StdVector.at ~desc:"Collection.get()"
      ; -"__cast" <>--> Misc.id_first_arg ]
end

let builtins_dispatcher =
  let builtins =
    [ (BuiltinDecl.__delete, Cplusplus.delete)
    ; (BuiltinDecl.__placement_new, Cplusplus.placement_new)
    ; (BuiltinDecl.abort, Misc.early_exit)
    ; (BuiltinDecl.exit, Misc.early_exit)
    ; (BuiltinDecl.free, C.free)
    ; (BuiltinDecl.objc_cpp_throw, Misc.early_exit) ]
  in
  let builtins_map =
    Hashtbl.create
      ( module struct
        include Typ.Procname

        let hash = Caml.Hashtbl.hash

        let sexp_of_t _ = assert false
      end )
  in
  List.iter builtins ~f:(fun (builtin, model) ->
      let open PolyVariantEqual in
      assert (Hashtbl.add builtins_map ~key:builtin ~data:model = `Ok) ) ;
  fun proc_name -> Hashtbl.find builtins_map proc_name


let dispatch tenv proc_name flags =
  match builtins_dispatcher proc_name with
  | Some _ as result ->
      result
  | None -> (
    match ProcNameDispatcher.dispatch tenv proc_name with
    | Some _ as result ->
        result
    | None ->
        if flags.CallFlags.cf_noreturn then Some Misc.early_exit else None )
