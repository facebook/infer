(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ResourceLeakCSDomain

  type analysis_data = ResourceLeakCSDomain.Summary.t InterproceduralAnalysis.t

  let is_closeable_typename tenv typename =
    let is_closable_interface typename _ =
      match Typ.Name.name typename with "System.IDisposable" -> true | _ -> false
    in
    PatternMatch.supertype_exists tenv is_closable_interface typename


  let is_closeable_procname tenv procname =
    match procname with
    | Procname.CSharp csharp_procname ->
        is_closeable_typename tenv (Procname.CSharp.get_class_type_name csharp_procname)
    | _ ->
        false


  let acquires_resource tenv procname =
    (* We assume all constructors of a subclass of Closeable acquire a resource *)
    Procname.is_constructor procname && is_closeable_procname tenv procname


  let releases_resource tenv procname =
    (* We assume the close method of a Closeable releases all of its resources *)
    match procname with
    | Procname.CSharp _ ->
        ( String.equal "Close" (Procname.get_method procname)
        || String.equal "Dispose" (Procname.get_method procname) )
        && is_closeable_procname tenv procname
    | _ ->
        String.equal "close" (Procname.get_method procname) && is_closeable_procname tenv procname


  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : ResourceLeakCSDomain.t)
      {InterproceduralAnalysis.proc_desc; tenv; analyze_dependency; _} _ _ (instr : HilInstr.t) =
    let is_not_enumerable =
      let contains s1 s2 =
        let re = Str.regexp_string s2 in
        try
          ignore (Str.search_forward re s1 0) ;
          false
        with Not_found_s _ | Caml.Not_found -> true
      in
      contains (Procname.to_string (Procdesc.get_proc_name proc_desc)) "IEnumerable"
      && contains (Procname.to_string (Procdesc.get_proc_name proc_desc)) "Enumerator"
    in
    match instr with
    | Call (_return, Direct callee_procname, HilExp.AccessExpression allocated :: _, _, _loc)
      when acquires_resource tenv callee_procname && is_not_enumerable ->
        let typename =
          match callee_procname with
          | Procname.CSharp csharp_procname ->
              Procname.CSharp.get_class_type_name csharp_procname
          | _ ->
              L.die InternalError "Unsupported procname kind! Only .NET is supported"
        in
        ResourceLeakCSDomain.acquire_resource
          (HilExp.AccessExpression.to_access_path allocated)
          typename astate
    | Call (_, Direct callee_procname, [actual], _, _loc)
      when releases_resource tenv callee_procname -> (
      match actual with
      | HilExp.AccessExpression access_expr ->
          ResourceLeakCSDomain.release_resource
            (HilExp.AccessExpression.to_access_path access_expr)
            astate
      | _ ->
          astate )
    | Call (return, Direct callee_procname, actuals, _, _loc) -> (
      match analyze_dependency callee_procname with
      | Some (_callee_proc_desc, callee_summary) ->
          (* interprocedural analysis produced a summary: use it *)
          ResourceLeakCSDomain.Summary.apply ~callee:callee_summary ~return ~actuals astate
      | None ->
          (* No summary for [callee_procname]; it's native code or missing for some reason *)
          astate )
    | Assign (access_expr, AccessExpression rhs_access_expr, _loc) ->
        ResourceLeakCSDomain.assign
          (HilExp.AccessExpression.to_access_path access_expr)
          (HilExp.AccessExpression.to_access_path rhs_access_expr)
          astate
    | Assign (lhs_access_path, rhs_exp, _loc) -> (
      match rhs_exp with
      | HilExp.AccessExpression access_expr ->
          ResourceLeakCSDomain.assign
            (HilExp.AccessExpression.to_access_path lhs_access_path)
            (HilExp.AccessExpression.to_access_path access_expr)
            astate
      | _ ->
          astate )
    | Assume (assume_exp, _, _, _loc) -> (
        (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)
        let rec extract_null_compare_expr expr =
          match expr with
          | HilExp.Cast (_, e) ->
              extract_null_compare_expr e
          | HilExp.BinaryOperator (Binop.Eq, HilExp.AccessExpression access_expr, exp)
          | HilExp.BinaryOperator (Binop.Eq, exp, HilExp.AccessExpression access_expr)
          | HilExp.UnaryOperator
              ( Unop.LNot
              , HilExp.BinaryOperator (Binop.Ne, HilExp.AccessExpression access_expr, exp)
              , _ )
          | HilExp.UnaryOperator
              ( Unop.LNot
              , HilExp.BinaryOperator (Binop.Ne, exp, HilExp.AccessExpression access_expr)
              , _ ) ->
              Option.some_if (HilExp.is_null_literal exp)
                (HilExp.AccessExpression.to_access_path access_expr)
          | _ ->
              None
        in
        match extract_null_compare_expr assume_exp with
        | Some ap ->
            ResourceLeakCSDomain.release_resource ap astate
        | _ ->
            astate )
    | Call (_, Indirect _, _, _, _) ->
        (* This should never happen in .NET. Fail if it does. *)
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | Metadata _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "resource leaks"
end

(** Type of CFG to analyze--ExceptionalThrowOnly to follow exceptional control-flow edges, Normal to
    ignore them *)
module CFG = ProcCfg.ExceptionalThrowOnly

(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))

(** Report an error when we have acquired more resources than we have released *)
let report_if_leak {InterproceduralAnalysis.proc_desc; err_log; _} formal_map post =
  if ResourceLeakCSDomain.has_leak formal_map post then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
    let proc_name = Procdesc.get_proc_name proc_desc in
    let resouces_and_type =
      ResourceLeakCSDomain.Summary.resource_and_type_to_str post Config.debug_mode
    in
    let message = F.asprintf "%s in method \"%a\"" resouces_and_type Procname.pp proc_name in
    Reporting.log_issue proc_desc err_log ~loc:last_loc DOTNETResourceLeaks
      IssueType.dotnet_resource_leak message


(* Callback for invoking the checker from the outside--registered in RegisterCheckers *)
let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let result =
    Analyzer.compute_post analysis_data ~initial:ResourceLeakCSDomain.initial proc_desc
  in
  Option.map result ~f:(fun post ->
      let formal_map = FormalMap.make proc_desc in
      report_if_leak analysis_data formal_map post ;
      ResourceLeakCSDomain.Summary.make formal_map post )
