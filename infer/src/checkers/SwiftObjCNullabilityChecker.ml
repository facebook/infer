(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

module CallStatus = struct
  type t = Unannotated | Annotated | NotAPointer [@@deriving compare, equal]

  let pp fmt = function
    | Unannotated ->
        (* Using the built-in color tags if available, otherwise raw HTML with proper escaping *)
        F.fprintf fmt "<span style=\"color: red\">Unannotated Pointer</span>"
    | Annotated ->
        F.fprintf fmt "<span style=\"color: green\">Annotated</span>"
    | NotAPointer ->
        F.fprintf fmt "Non-pointer"
end

(* 1. Turn the status into a proper Abstract Domain using a Flat lattice *)
module StatusDomain = AbstractDomain.Flat (CallStatus)

(* 2. Define the Key module for Location *)
module LocKey = struct
  type t = Location.t [@@deriving compare]

  let pp = Location.pp
end

(* 3. Now the Map will work because StatusDomain satisfies AbstractDomain.S *)
module Domain = struct
  include AbstractDomain.Map (LocKey) (StatusDomain)

  let pp fmt astate =
    if is_empty astate then F.fprintf fmt "No Swift->ObjC boundaries found."
    else
      iter
        (fun loc status_flat ->
          match StatusDomain.get status_flat with
          | Some status ->
              F.fprintf fmt "@.  Boundary at %a: %a" Location.pp loc CallStatus.pp status
          | None ->
              () )
        astate
end

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = Domain

  type analysis_data = IntraproceduralAnalysis.t

  let pp_session_name _node fmt = F.pp_print_string fmt "SwiftObjCNullability"

  let get_status (attrs : ProcAttributes.t) =
    if not (Typ.is_pointer attrs.ret_type) then CallStatus.NotAPointer
    else if
      Annotations.ia_is_nullable attrs.ret_annots || Annotations.ia_is_nonnull attrs.ret_annots
    then CallStatus.Annotated
    else CallStatus.Unannotated


  let exec_instr astate {IntraproceduralAnalysis.proc_desc; err_log} _ _ (instr : Sil.instr) =
    match instr with
    | Call (_, Exp.Const (Const.Cfun callee_pname), _, loc, _) ->
        let caller_pname = Procdesc.get_proc_name proc_desc in
        (* 1. Explicitly check the boundary *)
        if Procname.is_swift caller_pname && Procname.is_objc_method callee_pname then (
          match Attributes.load callee_pname with
          | Some attrs ->
              let status = get_status attrs in
              (* 2. Log to HTML trace for developer debugging *)
              L.d_printfln "Boundary at %a: %a" Location.pp loc CallStatus.pp status ;
              (* 3. Check for issue - No need for complex StatusDomain lifting here *)
              if
                CallStatus.equal status Unannotated
                && SwiftObjCNullabilityIssue.should_report_at loc
              then
                Reporting.log_issue proc_desc err_log ~loc SwiftObjCNullability
                  IssueType.missing_nullability_annotation
                  (SwiftObjCNullabilityIssue.message callee_pname) ;
              (* 4. Update the domain state for the Post-State HTML summary *)
              Domain.add loc (StatusDomain.v status) astate
          | None ->
              L.d_printfln "No attributes found for %a" Procname.pp callee_pname ;
              astate )
        else astate
    | _ ->
        astate
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker ({IntraproceduralAnalysis.proc_desc} as analysis_data) =
  let initial = Domain.empty in
  ignore (Analyzer.compute_post analysis_data ~initial proc_desc)
