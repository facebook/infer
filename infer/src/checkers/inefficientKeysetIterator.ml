(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CFG = ProcCfg.Normal
module LoopNodes = AbstractDomain.FiniteSet (Procdesc.Node)

let find_loaded_pvar id = function
  | Sil.Load {id= lhs_id; e= Exp.Lvar rhs_pvar} when Ident.equal lhs_id id ->
      Some rhs_pvar
  | _ ->
      None


let find_first_arg_id ~fun_name ~class_name_f ~lhs_f = function
  | Sil.Call ((lhs_id, _), Exp.Const (Const.Cfun pname), (Exp.Var rhs_id, _) :: _, _, _)
    when lhs_f lhs_id
         && String.equal fun_name (Procname.get_method pname)
         && Procname.get_class_name pname |> Option.exists ~f:class_name_f ->
      Some rhs_id
  | _ ->
      None


let implements_map tenv s =
  PatternMatch.Java.implements_map tenv s || PatternMatch.Java.implements_androidx_map tenv s


(** If given a node that has 4 instructions and calls fun_name, pickup bcvarY, i.e. variable for the
    first argument

    {v
     n$X      = *&$bcvarY
       _      = *n$X
     n$X+1    = fun_name(n$X,....)
    *&$ir_var = n$X+1
    v} *)
let find_first_arg_pvar ?(ir_var_opt = None) node ~fun_name ~class_name_f =
  let instrs = Procdesc.Node.get_instrs node in
  if Instrs.count instrs >= 4 then
    let instr_arr = Instrs.get_underlying_not_reversed instrs in
    match instr_arr.(3) with
    | Sil.Store {e1= Exp.Lvar ir_var; e2= Exp.Var rhs_id}
      when Option.value_map ~default:true ir_var_opt ~f:(Pvar.equal ir_var) ->
        find_first_arg_id ~fun_name ~class_name_f ~lhs_f:(Ident.equal rhs_id) instr_arr.(2)
        |> Option.bind ~f:(fun arg_id -> find_loaded_pvar arg_id instr_arr.(0))
    | _ ->
        None
  else None


let report_matching_get proc_desc err_log tenv pvar loop_nodes : unit =
  LoopNodes.iter
    (fun node ->
      let instrs = Procdesc.Node.get_instrs node in
      if Instrs.count instrs >= 5 then
        let instr_arr = Instrs.get_underlying_not_reversed instrs in
        find_first_arg_id ~fun_name:"get" ~class_name_f:(implements_map tenv)
          ~lhs_f:(fun _ -> true)
          instr_arr.(3)
        |> Option.iter ~f:(fun arg_id ->
               find_loaded_pvar arg_id instr_arr.(0)
               |> Option.iter ~f:(fun arg_pvar ->
                      if Pvar.equal arg_pvar pvar then
                        let pp_m = MarkupFormatter.pp_monospaced in
                        let exp_desc =
                          F.asprintf
                            "Accessing a value using a key that was retrieved from a %a iterator. "
                            pp_m "keySet"
                        in
                        let suggestion =
                          F.asprintf
                            "It is more efficient to use an iterator on the %a of the map, \
                             avoiding the extra %a lookup."
                            pp_m "entrySet" pp_m "HashMap.get(key)"
                        in
                        let loc = Procdesc.Node.get_loc node in
                        let ltr = [Errlog.make_trace_element 0 loc "" []] in
                        Reporting.log_issue ~suggestion proc_desc err_log ~loc ~ltr
                          InefficientKeysetIterator IssueType.inefficient_keyset_iterator exp_desc ) )
      )
    loop_nodes


(** Heuristic: check up to 4 direct predecessor nodes *)
let when_dominating_preds_satisfy ?(ir_var_opt = None) idom my_node ~fun_name ~class_name_f ~f =
  let preds node =
    Procdesc.Node.get_preds node
    |> List.filter ~f:(fun node -> Dominators.dominates idom node my_node)
  in
  let rec aux node (counter : int) =
    if not (Int.equal counter 0) then
      match preds node with
      | [pred_node] -> (
        match find_first_arg_pvar pred_node ~fun_name ~class_name_f ~ir_var_opt with
        | Some pvar ->
            f pred_node pvar
        | None ->
            aux pred_node (counter - 1) )
      | _ ->
          ()
  in
  aux my_node 4


let checker {IntraproceduralAnalysis.proc_desc; tenv; err_log} =
  let cfg = CFG.from_pdesc proc_desc in
  let loop_head_to_loop_nodes =
    Loop_control.(get_loop_head_to_source_nodes cfg |> get_loop_head_to_loop_nodes)
  in
  let idom = Dominators.get_idoms proc_desc in
  Procdesc.NodeMap.iter
    (fun loop_head loop_nodes ->
      if
        find_first_arg_pvar loop_head ~fun_name:"hasNext"
          ~class_name_f:(PatternMatch.Java.implements_iterator tenv)
        |> Option.is_some
      then
        when_dominating_preds_satisfy idom loop_head ~fun_name:"iterator"
          ~class_name_f:(PatternMatch.Java.implements_set tenv) ~f:(fun itr_node itr_pvar ->
            when_dominating_preds_satisfy idom itr_node ~fun_name:"keySet"
              ~ir_var_opt:(Some itr_pvar) ~class_name_f:(implements_map tenv)
              ~f:(fun _keySet_node get_pvar ->
                report_matching_get proc_desc err_log tenv get_pvar loop_nodes ) ) )
    loop_head_to_loop_nodes
