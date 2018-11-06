(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Result.Monad_infix
module L = Logging

type exec_fun =
     Location.t
  -> ret:Var.t * Typ.t
  -> actuals:HilExp.t list
  -> PulseDomain.t
  -> PulseDomain.t PulseDomain.access_result

type model = exec_fun

module C = struct
  let free : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [AccessExpression deleted_access] ->
        PulseDomain.invalidate (CFree (deleted_access, location)) location deleted_access astate
    | _ ->
        Ok astate


  let variable_initialization : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [AccessExpression (AddressOf access_expr)] ->
        PulseDomain.havoc location access_expr astate
    | _ ->
        L.die InternalError
          "The frontend is not supposed to produce __variable_initialization(e) where e is not of \
           the form `&exp`. Got [%a]."
          (Pp.seq ~sep:", " HilExp.pp) actuals
end

module Cplusplus = struct
  let delete : model =
   fun location ~ret:_ ~actuals astate ->
    PulseDomain.read_all location (List.concat_map actuals ~f:HilExp.get_access_exprs) astate
    >>= fun astate ->
    match actuals with
    | [AccessExpression deleted_access] ->
        PulseDomain.invalidate
          (CppDelete (deleted_access, location))
          location deleted_access astate
    | _ ->
        Ok astate


  let placement_new : model =
   fun location ~ret:(ret_var, _) ~actuals astate ->
    let read_all args astate =
      PulseDomain.read_all location (List.concat_map args ~f:HilExp.get_access_exprs) astate
    in
    match List.rev actuals with
    | HilExp.AccessExpression expr :: other_actuals ->
        PulseDomain.read location expr astate
        >>= fun (astate, address) ->
        Ok (PulseDomain.write_var ret_var address astate) >>= read_all other_actuals
    | _ :: other_actuals ->
        read_all other_actuals astate >>| PulseDomain.havoc_var ret_var
    | _ ->
        Ok astate
end

module StdVector = struct
  let internal_array =
    Typ.Fieldname.Clang.from_class_name
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "vector"]))
      "__internal_array"


  let to_internal_array vector = AccessExpression.field_offset vector internal_array

  let deref_internal_array vector =
    AccessExpression.(array_offset (dereference (to_internal_array vector)) Typ.void [])


  let at : model =
   fun location ~ret ~actuals astate ->
    match actuals with
    | [AccessExpression vector; _index] ->
        PulseDomain.read location (deref_internal_array vector) astate
        >>= fun (astate, loc) -> PulseDomain.write location (AccessExpression.base ret) loc astate
    | _ ->
        Ok (PulseDomain.havoc_var (fst ret) astate)


  let push_back : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [AccessExpression vector; _value] ->
        let array = to_internal_array vector in
        (* all elements should be invalidated *)
        let array_elements = deref_internal_array vector in
        let invalidation = PulseInvalidation.StdVectorPushBack (vector, location) in
        PulseDomain.invalidate invalidation location array_elements astate
        >>= PulseDomain.havoc location array
    | _ ->
        Ok astate
end

module ProcNameDispatcher = struct
  let dispatch : (unit, model) ProcnameDispatcher.ProcName.dispatcher =
    let open ProcnameDispatcher.ProcName in
    make_dispatcher
      [ -"std" &:: "vector" &:: "operator[]" &--> StdVector.at
      ; -"std" &:: "vector" &:: "push_back" &--> StdVector.push_back ]
end

let builtins_dispatcher =
  let builtins =
    [ (BuiltinDecl.__delete, Cplusplus.delete)
    ; (BuiltinDecl.__placement_new, Cplusplus.placement_new)
    ; (BuiltinDecl.__variable_initialization, C.variable_initialization)
    ; (BuiltinDecl.free, C.free) ]
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


let dispatch proc_name =
  match builtins_dispatcher proc_name with
  | Some _ as result ->
      result
  | None ->
      ProcNameDispatcher.dispatch () proc_name
