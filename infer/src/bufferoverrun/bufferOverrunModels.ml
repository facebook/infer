(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module L = Logging
module Dom = BufferOverrunDomain
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set

module Make (CFG : ProcCfg.S) = struct
  module Sem = BufferOverrunSemantics.Make (CFG)

  type model_fun =
    Typ.Procname.t -> (Ident.t * Typ.t) option -> (Exp.t * Typ.t) list -> CFG.node -> Location.t
    -> Dom.Mem.astate -> Dom.Mem.astate

  let set_uninitialized node (typ: Typ.t) loc mem =
    match typ.desc with
    | Tint _ | Tfloat _ ->
        Dom.Mem.weak_update_heap loc Dom.Val.Itv.top mem
    | _ ->
        L.(debug BufferOverrun Verbose)
          "/!\\ Do not know how to uninitialize type %a at %a@\n" (Typ.pp Pp.text) typ Location.pp
          (CFG.loc node) ;
        mem


  (* NOTE: heuristic *)
  let get_malloc_info : Exp.t -> Typ.t * Int.t option * Exp.t = function
    | Exp.BinOp (Binop.Mult, Exp.Sizeof {typ; nbytes}, length)
    | Exp.BinOp (Binop.Mult, length, Exp.Sizeof {typ; nbytes}) ->
        (typ, nbytes, length)
    | Exp.Sizeof {typ; nbytes} ->
        (typ, nbytes, Exp.one)
    | x ->
        (Typ.mk (Typ.Tint Typ.IChar), Some 1, x)


  let model_malloc pname ret params node location mem =
    match ret with
    | Some (id, _) ->
        let typ, stride, length0 = get_malloc_info (List.hd_exn params |> fst) in
        let length = Sem.eval length0 mem (CFG.loc node) in
        let traces = TraceSet.add_elem (Trace.ArrDecl location) (Dom.Val.get_traces length) in
        let v =
          Sem.eval_array_alloc pname node typ ?stride Itv.zero (Dom.Val.get_itv length) 0 1
          |> Dom.Val.set_traces traces
        in
        mem |> Dom.Mem.add_stack (Loc.of_id id) v
        |> set_uninitialized node typ (Dom.Val.get_array_locs v)
    | _ ->
        L.(debug BufferOverrun Verbose)
          "/!\\ Do not know where to model malloc at %a@\n" Location.pp (CFG.loc node) ;
        mem


  let model_realloc pname ret params node location mem =
    model_malloc pname ret (List.tl_exn params) node location mem


  let model_min _pname ret params _node location mem =
    match (ret, params) with
    | Some (id, _), [(e1, _); (e2, _)] ->
        let i1 = Sem.eval e1 mem location |> Dom.Val.get_itv in
        let i2 = Sem.eval e2 mem location |> Dom.Val.get_itv in
        let v = Itv.min_sem i1 i2 |> Dom.Val.of_itv in
        mem |> Dom.Mem.add_stack (Loc.of_id id) v
    | _ ->
        mem


  let model_set_size _pname _ret params _node location mem =
    match params with
    | [(e1, _); (e2, _)] ->
        let locs = Sem.eval_locs e1 mem location |> Dom.Val.get_pow_loc in
        let size = Sem.eval e2 mem location |> Dom.Val.get_itv in
        let arr = Dom.Mem.find_heap_set locs mem in
        let arr = Dom.Val.set_array_size size arr in
        Dom.Mem.strong_update_heap locs arr mem
    | _ ->
        mem


  let model_by_value value _pname ret _params _node _location mem =
    match ret with
    | Some (id, _) ->
        Dom.Mem.add_stack (Loc.of_id id) value mem
    | None ->
        L.(debug BufferOverrun Verbose)
          "/!\\ Do not know where to model value %a@\n" Dom.Val.pp value ;
        mem


  let model_bottom _pname _ret _params _node _location _mem = Bottom

  let model_infer_print _pname _ret params _node location mem =
    match params with
    | (e, _) :: _ ->
        L.(debug BufferOverrun Medium)
          "@[<v>=== Infer Print === at %a@,%a@]%!" Location.pp location Dom.Val.pp
          (Sem.eval e mem location) ;
        mem
    | _ ->
        mem


  let model_infer_set_array_length pname _ret params node location mem =
    match params with
    | [(Exp.Lvar array_pvar, {Typ.desc= Typ.Tarray (typ, _, stride0)}); (length_exp, _)] ->
        let length = Sem.eval length_exp mem location |> Dom.Val.get_itv in
        let stride = Option.map ~f:IntLit.to_int stride0 in
        let v = Sem.eval_array_alloc pname node typ ?stride Itv.zero length 0 1 in
        mem |> Dom.Mem.add_stack (Loc.of_pvar array_pvar) v
        |> set_uninitialized node typ (Dom.Val.get_array_locs v)
    | [_; _] ->
        L.(die InternalError) "Unexpected type of arguments for __set_array_length()"
    | _ ->
        L.(die InternalError) "Unexpected number of arguments for __set_array_length()"


  let dispatcher : model_fun QualifiedCppName.Dispatch.quals_dispatcher =
    QualifiedCppName.Dispatch.of_fuzzy_qual_names
      [ (["__inferbo_min"], model_min)
      ; (["__inferbo_set_size"], model_set_size)
      ; (["__exit"; "exit"], model_bottom)
      ; (["fgetc"], model_by_value Dom.Val.Itv.m1_255)
      ; (["infer_print"], model_infer_print)
      ; (["malloc"; "__new_array"], model_malloc)
      ; (["realloc"], model_realloc)
      ; (["__set_array_length"], model_infer_set_array_length)
      ; (["strlen"], model_by_value Dom.Val.Itv.nat) ]

end
