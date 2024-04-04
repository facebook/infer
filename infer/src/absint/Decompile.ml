(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** Find the function call instruction used to initialize normal variable [id], and return the
    function name and arguments *)
let find_normal_variable_funcall (node : Procdesc.Node.t) (id : Ident.t) :
    (Exp.t * Exp.t list * Location.t * CallFlags.t) option =
  let find_declaration _ = function
    | Sil.Call ((id0, _), fun_exp, args, loc, call_flags) when Ident.equal id id0 ->
        Some (fun_exp, List.map ~f:fst args, loc, call_flags)
    | _ ->
        None
  in
  let res = Procdesc.Node.find_in_node_or_preds node ~f:find_declaration in
  if Config.trace_error && is_none res then
    L.d_printfln "find_normal_variable_funcall could not find %a in node %a" Ident.pp id
      Procdesc.Node.pp node ;
  res


(** Find a program variable assignment in the current node or predecessors. *)
let find_program_variable_assignment node pvar : (Procdesc.Node.t * Ident.t) option =
  let find_instr node = function
    | Sil.Store {e1= Exp.Lvar pvar_; e2= Exp.Var id}
      when Pvar.equal pvar pvar_ && Ident.is_normal id ->
        Some (node, id)
    | _ ->
        None
  in
  Procdesc.Node.find_in_node_or_preds node ~f:find_instr
