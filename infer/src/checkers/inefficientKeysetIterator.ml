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
  | Sil.Load (lhs_id, Exp.Lvar rhs_pvar, _, _) when Ident.equal lhs_id id ->
      Some rhs_pvar
  | _ ->
      None


let find_first_arg_id ~fun_name ~lhs_f = function
  | Sil.Call ((lhs_id, _), Exp.Const (Const.Cfun pname), (Exp.Var rhs_id, _) :: _, _, _)
    when lhs_f lhs_id && String.equal fun_name (Typ.Procname.get_method pname) ->
      Some rhs_id
  | _ ->
      None


(** If given a node that has 4 instructions and calls fun_name, 
    pickup bcvarY, i.e. variable for the first argument 
     n$X      = *&$bcvarY
       _      = *n$X 
     n$X+1    = fun_name(n$X,....)
    *&$irvarZ = n$X+1
 *)
let find_first_arg_pvar node ~fun_name =
  let instrs = Procdesc.Node.get_instrs node in
  if Instrs.count instrs >= 4 then
    let instr_arr = Instrs.get_underlying_not_reversed instrs in
    match instr_arr.(3) with
    | Sil.Store (Exp.Lvar _, _, Exp.Var rhs_id, _) ->
        find_first_arg_id ~fun_name ~lhs_f:(Ident.equal rhs_id) instr_arr.(2)
        |> Option.bind ~f:(fun arg_id -> find_loaded_pvar arg_id instr_arr.(0))
    | _ ->
        None
  else None


let report_matching_get summary pvar loop_nodes : unit =
  LoopNodes.iter
    (fun node ->
      let instrs = Procdesc.Node.get_instrs node in
      if Instrs.count instrs >= 5 then
        let instr_arr = Instrs.get_underlying_not_reversed instrs in
        find_first_arg_id ~fun_name:"get" ~lhs_f:(fun _ -> true) instr_arr.(3)
        |> Option.iter ~f:(fun arg_id ->
               find_loaded_pvar arg_id instr_arr.(0)
               |> Option.iter ~f:(fun arg_pvar ->
                      if Pvar.equal arg_pvar pvar then
                        let pp_m = MarkupFormatter.pp_monospaced in
                        let exp_desc =
                          F.asprintf
                            "Accessing a value using a key that was retrieved from a %a iterator. \
                             It is more efficient to use an iterator on the %a of the map, \
                             avoiding the extra %a lookup."
                            pp_m "keySet" pp_m "entrySet" pp_m "HashMap.get(key)"
                        in
                        let loc = Procdesc.Node.get_loc node in
                        let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
                        Reporting.log_error summary ~loc ~ltr IssueType.inefficient_keyset_iterator
                          exp_desc ) ) )
    loop_nodes


let when_dominating_pred_satisfies idom my_node ~f =
  let preds =
    Procdesc.Node.get_preds my_node
    |> List.filter ~f:(fun node -> Dominators.dominates idom node my_node)
  in
  match preds with [pred_node] -> f pred_node | _ -> ()


let checker Callbacks.{summary} : Summary.t =
  let proc_desc = Summary.get_proc_desc summary in
  let cfg = CFG.from_pdesc proc_desc in
  let _, loop_head_to_loop_nodes = Loop_control.get_loop_control_maps cfg in
  let idom = Dominators.get_idoms proc_desc in
  Procdesc.NodeMap.iter
    (fun loop_head loop_nodes ->
      if find_first_arg_pvar loop_head ~fun_name:"hasNext" |> Option.is_some then
        when_dominating_pred_satisfies idom loop_head ~f:(fun itr_node ->
            if Option.is_some (find_first_arg_pvar itr_node ~fun_name:"iterator") then
              when_dominating_pred_satisfies idom itr_node ~f:(fun keySet_node ->
                  find_first_arg_pvar keySet_node ~fun_name:"keySet"
                  |> Option.iter ~f:(fun get_pvar ->
                         report_matching_get summary get_pvar loop_nodes ) ) ) )
    loop_head_to_loop_nodes ;
  summary
