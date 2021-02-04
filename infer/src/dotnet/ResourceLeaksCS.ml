(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module Hashtbl = Caml.Hashtbl

let leak_list = ref []

let type_map = ref (Hashtbl.create 100)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ResourceLeakCSDomain

  type analysis_data = ResourceLeakCSDomain.Summary.t InterproceduralAnalysis.t

  let is_closeable_typename tenv typename =
    let is_closable_interface typename _ =
      match Typ.Name.name typename with
      | "java.io.AutoCloseable" | "java.io.Closeable" ->
          true
      | "System.IDisposable" ->
          true
      | _ ->
          false
    in
    PatternMatch.supertype_exists tenv is_closable_interface typename


  let is_closeable_procname tenv procname =
    match procname with
    | Procname.Java java_procname ->
        is_closeable_typename tenv (Procname.Java.get_class_type_name java_procname)
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
      {InterproceduralAnalysis.proc_desc; tenv; analyze_dependency; _} _ (instr : HilInstr.t) =
    let assign_type_map = type_map := ResourceLeakCSDomain.get_type_map in
    assign_type_map ;
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
        let get_class_name =
          match callee_procname with
          | Procname.Java java_procname ->
              Procname.Java.get_class_name java_procname
          | Procname.CSharp csharp_procname ->
              Procname.CSharp.get_class_name csharp_procname
          | _ ->
              L.die InternalError "Unsupported procname kind! Only Java and .NET is supported"
        in
        ResourceLeakCSDomain.acquire_resource
          (HilExp.AccessExpression.to_access_path allocated)
          get_class_name astate
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
        (* This should never happen in Java. Fail if it does. *)
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | Metadata _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "resource leaks"
end

(** 5(a) Type of CFG to analyze--Exceptional to follow exceptional control-flow edges, Normal to
    ignore them *)
module CFG = ProcCfg.Normal

(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))

(** Report an error when we have acquired more resources than we have released *)
let report_if_leak {InterproceduralAnalysis.proc_desc; err_log; _} formal_map post =
  if ResourceLeakCSDomain.has_leak formal_map post then (
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
    let message =
      let concat_types =
        Hashtbl.iter
          (fun x y ->
            if ResourceLeakCSDomain.check_count x post then
              leak_list := ResourceLeakCSDomain.LeakList.append_one !leak_list y )
          !type_map
      in
      concat_types ;
      let concat_leak_list = String.concat ~sep:", " !leak_list in
      F.asprintf "Leaked %a resource(s) at type(s) %s" ResourceLeakCSDomain.pp post concat_leak_list
    in
    ResourceLeakCSDomain.reset_type_map ;
    ResourceLeakCSDomain.Summary.reset_interface_type_map ;
    leak_list := [] ;
    Reporting.log_issue proc_desc err_log ~loc:last_loc DOTNETResourceLeaks
      IssueType.dotnet_resource_leak message )
  else ResourceLeakCSDomain.reset_type_map ;
  ResourceLeakCSDomain.Summary.reset_interface_type_map


(* Callback for invoking the checker from the outside--registered in RegisterCheckers *)
let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let result =
    Analyzer.compute_post analysis_data ~initial:ResourceLeakCSDomain.initial proc_desc
  in
  Option.map result ~f:(fun post ->
      let formal_map = FormalMap.make proc_desc in
      report_if_leak analysis_data formal_map post ;
      ResourceLeakCSDomain.Summary.make formal_map post )
