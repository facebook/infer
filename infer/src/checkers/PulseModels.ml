(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Result.Monad_infix

type exec_fun =
     Location.t
  -> ret:Var.t * Typ.t
  -> actuals:HilExp.t list
  -> PulseDomain.t
  -> PulseDomain.t PulseDomain.access_result

type model = exec_fun

module Cplusplus = struct
  let delete : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [AccessExpression deleted_access] ->
        PulseDomain.invalidate location deleted_access astate
    | _ ->
        Ok astate
end

module StdVector = struct
  let internal_array =
    Typ.Fieldname.Clang.from_class_name
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "vector"]))
      "__internal_array"


  let deref_internal_array vector =
    AccessExpression.ArrayOffset
      (AccessExpression.FieldOffset (vector, internal_array), Typ.void, [])


  let at : model =
   fun location ~ret ~actuals astate ->
    match actuals with
    | [AccessExpression vector; _index] ->
        PulseDomain.read location (deref_internal_array vector) astate
        >>= fun (astate, loc) -> PulseDomain.write location (AccessExpression.Base ret) loc astate
    | _ ->
        Ok astate


  let push_back : model =
   fun location ~ret:_ ~actuals astate ->
    match actuals with
    | [AccessExpression vector; _value] ->
        PulseDomain.invalidate location (deref_internal_array vector) astate
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
  let builtins = [(BuiltinDecl.__delete, Cplusplus.delete)] in
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
