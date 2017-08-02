(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter

module UseDefChain = struct
  type astate =
    | DependsOn of (Location.t * AccessPath.t)
    | NullDefCompare of (Location.t * AccessPath.t)
    | NullDefAssign of (Location.t * AccessPath.t)
    [@@deriving compare]

  let ( <= ) ~lhs ~rhs = compare_astate lhs rhs <= 0

  (* Keep only one chain in join/widen as we are going to report only one
   * trace to the user eventually. *)
  let join lhs rhs = if ( <= ) ~lhs ~rhs then rhs else lhs

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | NullDefAssign (loc, ap)
     -> F.fprintf fmt "NullDefAssign(%a, %a)" Location.pp loc AccessPath.pp ap
    | NullDefCompare (loc, ap)
     -> F.fprintf fmt "NullDefCompare(%a, %a)" Location.pp loc AccessPath.pp ap
    | DependsOn (loc, ap)
     -> F.fprintf fmt "DependsOn(%a, %a)" Location.pp loc AccessPath.pp ap
end

module Domain = AbstractDomain.Map (AccessPath) (UseDefChain)

type extras = ProcData.no_extras

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type nonrec extras = extras

  let is_access_nullable ap proc_data =
    match AccessPath.get_field_and_annotation ap proc_data.ProcData.tenv with
    | Some (_, annot_item)
     -> Annotations.ia_is_nullable annot_item
    | _
     -> false

  let nullable_usedef_chain_of exp lhs astate loc =
    match exp with
    | HilExp.Constant Cint n when IntLit.isnull n
     -> Some (UseDefChain.NullDefAssign (loc, lhs))
    | HilExp.AccessPath ap -> (
      try
        match Domain.find ap astate with
        | UseDefChain.NullDefCompare _
         -> (* Stop NullDefCompare from propagating here because we want to prevent
                 * the checker from suggesting @Nullable on y in the following case:
                 * if (x == null) ... else { y = x; } *)
            None
        | _
         -> Some (UseDefChain.DependsOn (loc, ap))
      with Not_found -> None )
    | _
     -> None

  let extract_null_compare_expr = function
    | HilExp.BinaryOperator ((Eq | Ne), HilExp.AccessPath ap, exp)
    | HilExp.BinaryOperator ((Eq | Ne), exp, HilExp.AccessPath ap)
     -> Option.some_if (HilExp.is_null_literal exp) ap
    | _
     -> None

  let exec_instr (astate: Domain.astate) proc_data _ (instr: HilInstr.t) =
    match instr with
    | Assume (expr, _, _, loc) -> (
      match extract_null_compare_expr expr with
      | Some ap when not (is_access_nullable ap proc_data)
       -> let udchain = UseDefChain.NullDefCompare (loc, ap) in
          Domain.add ap udchain astate
      | _
       -> astate )
    | Call _
     -> (* For now we just assume the callee always return non-null *)
        astate
    | Assign (lhs, rhs, loc)
     -> if not (is_access_nullable lhs proc_data) then
          match nullable_usedef_chain_of rhs lhs astate loc with
          | Some udchain
           -> Domain.add lhs udchain astate
          | None
           -> astate
        else astate
end

module Analyzer =
  AbstractInterpreter.Make (ProcCfg.Exceptional) (LowerHil.MakeDefault (TransferFunctions))

let make_error_trace astate ap ud =
  let name_of ap =
    match AccessPath.get_last_access ap with
    | Some AccessPath.FieldAccess field_name
     -> "Field " ^ Typ.Fieldname.to_flat_string field_name
    | Some AccessPath.ArrayAccess _
     -> "Some array element"
    | None
     -> "Variable"
  in
  let open UseDefChain in
  let rec error_trace_impl depth ap = function
    | NullDefAssign (loc, src)
     -> let msg = F.sprintf "%s is assigned null here" (name_of src) in
        let ltr = [Errlog.make_trace_element depth loc msg []] in
        Some (loc, ltr)
    | NullDefCompare (loc, src)
     -> let msg = F.sprintf "%s is compared to null here" (name_of src) in
        let ltr = [Errlog.make_trace_element depth loc msg []] in
        Some (loc, ltr)
    | DependsOn (loc, dep) ->
      try
        let ud' = Domain.find dep astate in
        let msg = F.sprintf "%s could be assigned here" (name_of ap) in
        let trace_elem = Errlog.make_trace_element depth loc msg [] in
        Option.map (error_trace_impl (depth + 1) dep ud') ~f:(fun (_, trace) ->
            (loc, trace_elem :: trace) )
      with Not_found -> None
  in
  error_trace_impl 0 ap ud

let pretty_field_name proc_data field_name =
  match Procdesc.get_proc_name proc_data.ProcData.pdesc with
  | Typ.Procname.Java jproc_name
   -> let proc_class_name = Typ.Procname.java_get_class_name jproc_name in
      let field_class_name = Typ.Fieldname.java_get_class field_name in
      if String.equal proc_class_name field_class_name then Typ.Fieldname.to_flat_string field_name
      else Typ.Fieldname.to_simplified_string field_name
  | _
   -> (* This format is subject to change once this checker gets to run on C/Cpp/ObjC *)
      Typ.Fieldname.to_string field_name

let checker {Callbacks.summary; proc_desc; tenv} =
  let report astate (proc_data: extras ProcData.t) =
    let report_access_path ap udchain =
      let issue_kind = Localise.to_issue_id Localise.field_should_be_nullable in
      match AccessPath.get_field_and_annotation ap proc_data.tenv with
      | Some (field_name, _) when Typ.Fieldname.Java.is_captured_parameter field_name
       -> (* Skip reporting when field comes from generated code *)
          ()
      | Some (field_name, _)
       -> (
          let message =
            F.asprintf "Field %a should be annotated with %a" MF.pp_monospaced
              (pretty_field_name proc_data field_name) MF.pp_monospaced "@Nullable"
          in
          let exn = Exceptions.Checkers (issue_kind, Localise.verbatim_desc message) in
          match make_error_trace astate ap udchain with
          | Some (loc, ltr)
           -> Reporting.log_warning summary ~loc ~ltr exn
          | None
           -> Reporting.log_warning summary exn )
      | _
       -> ()
    in
    Domain.iter report_access_path astate
  in
  let proc_name = Procdesc.get_proc_name proc_desc in
  if AndroidFramework.is_destroy_method proc_name then
    (* Skip the fields nullified in Fragment onDestroy and onDestroyView *)
    summary
  else
    (* Assume all fields are not null in the beginning *)
    let initial = (Domain.empty, IdAccessPathMapDomain.empty) in
    let proc_data = ProcData.make_default proc_desc tenv in
    match Analyzer.compute_post proc_data ~initial ~debug:false with
    | Some (post, _)
     -> report post proc_data ; summary
    | None
     -> failwithf "Analyzer failed to compute post for %a" Typ.Procname.pp proc_name
