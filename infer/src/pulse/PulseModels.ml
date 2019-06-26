(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Result.Monad_infix

type exec_fun =
     Location.t
  -> ret:Ident.t * Typ.t
  -> actuals:(PulseDomain.AddrTracePair.t * Typ.t) list
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t list PulseOperations.access_result

type model = exec_fun

module Misc = struct
  let early_exit : model = fun _ ~ret:_ ~actuals:_ _ -> Ok []
end

module C = struct
  let free : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [(deleted_access, _)] ->
        PulseOperations.invalidate location
          (PulseDomain.InterprocAction.Immediate {imm= PulseDomain.Invalidation.CFree; location})
          deleted_access astate
        >>| List.return
    | _ ->
        Ok [astate]
end

module Cplusplus = struct
  let delete : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [(deleted_access, _)] ->
        PulseOperations.invalidate location
          (PulseDomain.InterprocAction.Immediate {imm= PulseDomain.Invalidation.CppDelete; location})
          deleted_access astate
        >>| List.return
    | _ ->
        Ok [astate]


  let operator_call : model =
   fun location ~ret:(ret_id, _) ~actuals astate ->
    ( match actuals with
    | (lambda, _) :: _ ->
        PulseOperations.Closures.check_captured_addresses
          (PulseDomain.InterprocAction.Immediate {imm= (); location})
          (fst lambda) astate
    | _ ->
        Ok astate )
    >>| fun astate ->
    let event = PulseDomain.ValueHistory.Call {f= `Model "<lambda>"; location} in
    [PulseOperations.havoc_id ret_id [event] astate]


  let placement_new : model =
   fun location ~ret:(ret_id, _) ~actuals astate ->
    let event = PulseDomain.ValueHistory.Call {f= `Model "<placement new>"; location} in
    match List.rev actuals with
    | ((address, _), _) :: _ ->
        Ok [PulseOperations.write_id ret_id (address, [event]) astate]
    | _ ->
        Ok [PulseOperations.havoc_id ret_id [event] astate]
end

module StdVector = struct
  let internal_array =
    Typ.Fieldname.Clang.from_class_name
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "vector"]))
      "__internal_array"


  let to_internal_array location vector astate =
    PulseOperations.eval_access location vector (FieldAccess internal_array) astate


  let element_of_internal_array location vector index astate =
    to_internal_array location vector astate
    >>= fun (astate, vector_internal_array) ->
    PulseOperations.eval_access location vector_internal_array
      (ArrayAccess (Typ.void, index))
      astate


  let reallocate_internal_array trace vector vector_f location astate =
    to_internal_array location vector astate
    >>= fun (astate, array_address) ->
    let invalidation =
      PulseDomain.InterprocAction.Immediate
        {imm= PulseDomain.Invalidation.StdVector vector_f; location}
    in
    PulseOperations.invalidate_array_elements location invalidation array_address astate
    >>= PulseOperations.invalidate_deref location invalidation array_address
    >>= PulseOperations.havoc_field location vector internal_array trace


  let invalidate_references vector_f : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | (vector, _) :: _ ->
        let crumb =
          PulseDomain.ValueHistory.Call
            { f=
                `Model
                  (Format.asprintf "%a" PulseDomain.Invalidation.pp_std_vector_function vector_f)
            ; location }
        in
        reallocate_internal_array [crumb] vector vector_f location astate >>| List.return
    | _ ->
        Ok [astate]


  let at : model =
   fun location ~ret ~actuals astate ->
    let event = PulseDomain.ValueHistory.Call {f= `Model "std::vector::at"; location} in
    match actuals with
    | [(vector, _); (index, _)] ->
        element_of_internal_array location vector (fst index) astate
        >>| fun (astate, (addr, _)) -> [PulseOperations.write_id (fst ret) (addr, [event]) astate]
    | _ ->
        Ok [PulseOperations.havoc_id (fst ret) [event] astate]


  let reserve : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [(vector, _); _value] ->
        let crumb = PulseDomain.ValueHistory.Call {f= `Model "std::vector::reserve"; location} in
        reallocate_internal_array [crumb] vector Reserve location astate
        >>| PulseAbductiveDomain.Memory.std_vector_reserve (fst vector)
        >>| List.return
    | _ ->
        Ok [astate]


  let push_back : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [(vector, _); _value] ->
        let crumb = PulseDomain.ValueHistory.Call {f= `Model "std::vector::push_back"; location} in
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

module DelayedDestruction = struct
  let destroy : model = fun _ ~ret:_ ~actuals:_ astate -> Ok [astate]
end

module ProcNameDispatcher = struct
  let dispatch : (unit, model) ProcnameDispatcher.ProcName.dispatcher =
    let open ProcnameDispatcher.ProcName in
    make_dispatcher
      [ -"folly" &:: "DelayedDestruction" &:: "destroy" &--> DelayedDestruction.destroy
      ; -"std" &:: "function" &:: "operator()" &--> Cplusplus.operator_call
      ; -"std" &:: "vector" &:: "assign" &--> StdVector.invalidate_references Assign
      ; -"std" &:: "vector" &:: "clear" &--> StdVector.invalidate_references Clear
      ; -"std" &:: "vector" &:: "emplace" &--> StdVector.invalidate_references Emplace
      ; -"std" &:: "vector" &:: "emplace_back" &--> StdVector.invalidate_references EmplaceBack
      ; -"std" &:: "vector" &:: "insert" &--> StdVector.invalidate_references Insert
      ; -"std" &:: "vector" &:: "operator[]" &--> StdVector.at
      ; -"std" &:: "vector" &:: "push_back" &--> StdVector.push_back
      ; -"std" &:: "vector" &:: "reserve" &--> StdVector.reserve
      ; -"std" &:: "vector" &:: "shrink_to_fit" &--> StdVector.invalidate_references ShrinkToFit ]
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


let dispatch proc_name flags =
  match builtins_dispatcher proc_name with
  | Some _ as result ->
      result
  | None -> (
    match ProcNameDispatcher.dispatch () proc_name with
    | Some _ as result ->
        result
    | None ->
        if flags.CallFlags.cf_noreturn then Some Misc.early_exit else None )
