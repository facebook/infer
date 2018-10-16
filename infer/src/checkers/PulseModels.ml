(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type exec_fun = ret:Var.t -> actuals:HilExp.t list -> PulseDomain.t -> PulseDomain.access_result

type model = exec_fun

module Cplusplus = struct
  let delete : model =
   fun ~ret:_ ~actuals astate ->
    match actuals with
    | [AccessExpression deleted_access] ->
        PulseDomain.invalidate deleted_access astate
    | _ ->
        Ok astate
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


let dispatch proc_name = builtins_dispatcher proc_name
