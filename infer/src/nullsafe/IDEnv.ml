(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Environment for temporary identifiers used in instructions. Lazy implementation: only created
    when actually used. *)

type t = Exp.t Ident.Hash.t Lazy.t

let create_ proc_desc =
  let map = Ident.Hash.create 1 in
  let do_instr _ = function
    | Sil.Load {id; e} ->
        Ident.Hash.add map id e
    | Sil.Call ((res_ident, _), Exp.Const (Const.Cfun fname), [(Exp.Var src_ident, _); _], _, _)
    (* [id] = __cast([ex]) when [ex] is a frontend temporary, try to unfold it further *)
      when Procname.equal fname BuiltinDecl.__cast ->
        Ident.Hash.find_opt map src_ident
        |> Option.iter ~f:(fun ex -> Ident.Hash.add map res_ident ex)
    | _ ->
        ()
  in
  Procdesc.iter_instrs do_instr proc_desc ;
  map


(* lazy implementation, only create when used *)
let create proc_desc =
  let map = lazy (create_ proc_desc) in
  map


let lookup map_ id =
  let map = Lazy.force map_ in
  Ident.Hash.find_opt map id


let expand_expr idenv e =
  match e with Exp.Var id -> ( match lookup idenv id with Some e' -> e' | None -> e ) | _ -> e


let expand_expr_temps idenv node exp_ =
  let exp = expand_expr idenv exp_ in
  match exp with
  | Exp.Lvar pvar when Pvar.is_frontend_tmp pvar -> (
    match Decompile.find_program_variable_assignment node pvar with
    | None ->
        exp
    | Some (_, id) ->
        expand_expr idenv (Exp.Var id) )
  | _ ->
      exp


(** Return true if the expression is a temporary variable introduced by the front-end. *)
let exp_is_temp idenv e =
  match expand_expr idenv e with Exp.Lvar pvar -> Pvar.is_frontend_tmp pvar | _ -> false
