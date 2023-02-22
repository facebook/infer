(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** global (per-function) state: count stores and function calls in order to identify which branch
    and which call in a branch we are analyzing *)
let node_id = ref (-1)

module DisjDomain = struct
  (** ["4";"goo2";"1";"foo1"], printed as "foo1.1.goo2.4", means we explored the first branch of foo
      followed by the 4th branch of goo *)
  type t = string list [@@deriving compare, equal]

  let pp fmt l = F.fprintf fmt "@[%a@]" (Pp.seq ~sep:"." F.pp_print_string) (List.rev l)

  let leq ~lhs ~rhs = equal lhs rhs

  let equal_fast l1 l2 = equal l1 l2

  let is_normal _ = true

  let is_exceptional _ = false

  let is_executable _ = true

  let exceptional_to_normal _ = assert false (* no exceptional state anyway *)
end

module NonDisjDomain = AbstractDomain.BottomTopLifted (AbstractDomain.Empty)

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module DisjDomain = DisjDomain
  module NonDisjDomain = NonDisjDomain

  type analysis_data = (DisjDomain.t list * NonDisjDomain.t) InterproceduralAnalysis.t

  let exec_instr (astate, astate_non_disj) analysis_data _cfg_node (instr : Sil.instr) :
      DisjDomain.t list * NonDisjDomain.t =
    let astate' =
      match instr with
      | Store _ ->
          (* only store instructions (and calls) are used as markers, to avoid cluttering tests *)
          incr node_id ;
          [string_of_int !node_id :: astate]
      | Call (_, Const (Cfun proc_name), _, _, _) -> (
        match analysis_data.InterproceduralAnalysis.analyze_dependency proc_name with
        | None ->
            [astate]
        | Some (callee_summary, _) ->
            incr node_id ;
            List.map callee_summary ~f:(fun xs ->
                xs @ (F.asprintf "%a%d" Procname.pp proc_name !node_id :: astate) ) )
      | Call _ | Load _ | Prune _ | Metadata _ ->
          [astate]
    in
    (astate', astate_non_disj)


  let pp_session_name _node fmt = F.pp_print_string fmt "Disjunctive Domain demo"
end

module DisjunctiveAnalyzer =
  AbstractInterpreter.MakeDisjunctive
    (TransferFunctions)
    (struct
      (* re-use pulse options to avoid complicating the command-line interface just for testing *)
      let join_policy = `UnderApproximateAfter Config.pulse_max_disjuncts

      (* just 2 for now, we may want to parameterize this in the future *)
      let widen_policy = `UnderApproximateAfterNumIterations 2
    end)

type domain = DisjunctiveAnalyzer.TransferFunctions.Domain.t

let pp_domain = DisjunctiveAnalyzer.TransferFunctions.Domain.pp

let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  node_id := -1 ;
  let result =
    DisjunctiveAnalyzer.compute_post analysis_data ~initial:([[]], NonDisjDomain.bottom) proc_desc
  in
  Option.iter result ~f:(fun post ->
      L.result "%a:@\n  @[<2>%a@]@\n" Procname.pp (Procdesc.get_proc_name proc_desc) pp_domain post ) ;
  result
