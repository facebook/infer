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

  let get_status ~(call_flags : CallFlags.t) (attrs : ProcAttributes.t) =
    if not (Typ.is_pointer attrs.ret_type) then CallStatus.NotAPointer
    else
      (* Combine the callee's procdesc-level [ret_annots] with any per-call-site
         annotations the frontend recovered for this call (e.g. Swift recognising
         that the caller treats the result as [Optional<T>] even when the ObjC
         method itself is unannotated). *)
      let combined_annots = call_flags.cf_caller_ret_annots @ attrs.ret_annots in
      (* [_Null_unspecified] is an explicit maintainer choice to expose the return as a Swift
         implicitly-unwrapped Optional (e.g. IGListKit's [IGListSectionController.collectionContext]
         is declared [null_unspecified] precisely so idiomatic Swift consumers are not forced into
         [as!] / [fatalError]). Treat it as a deliberate annotation, not as the "missing annotation"
         case the checker exists to catch. *)
      if
        Annotations.ia_is_nullable combined_annots
        || Annotations.ia_is_nonnull combined_annots
        || Annotations.ia_is_null_unspecified combined_annots
      then CallStatus.Annotated
      else CallStatus.Unannotated


  let exec_instr astate {IntraproceduralAnalysis.proc_desc; err_log} _ _ (instr : Sil.instr) =
    match instr with
    | Call (_, Exp.Const (Const.Cfun callee_pname), _, loc, call_flags) ->
        let caller_pname = Procdesc.get_proc_name proc_desc in
        (* 1. Explicitly check the boundary *)
        if Procname.is_swift caller_pname && Procname.is_objc_method callee_pname then (
          match Attributes.load callee_pname with
          | Some attrs ->
              let status = get_status ~call_flags attrs in
              (* 2. Log to HTML trace for developer debugging *)
              L.d_printfln "Boundary at %a: %a" Location.pp loc CallStatus.pp status ;
              (* 3. Check for issue - No need for complex StatusDomain lifting here *)
              if
                CallStatus.equal status Unannotated
                && (not call_flags.cf_return_null_checked)
                && SwiftObjCNullabilityIssue.should_report_at loc
                && not (SwiftObjCNullabilityIssue.is_system_framework_callee attrs)
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
