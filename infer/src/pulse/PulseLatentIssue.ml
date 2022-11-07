(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module Decompiler = PulseAbductiveDecompiler
module DecompilerExpr = PulseDecompilerExpr
module Diagnostic = PulseDiagnostic
module L = Logging

let add_call_to_access_to_invalid_address call_subst astate invalid_access =
  let expr_callee = invalid_access.Diagnostic.invalid_address in
  L.d_printfln "adding call to invalid address %a" DecompilerExpr.pp_with_abstract_value expr_callee ;
  let expr_caller =
    match
      let open IOption.Let_syntax in
      let* addr_callee = DecompilerExpr.abstract_value_of_expr expr_callee in
      AbstractValue.Map.find_opt addr_callee call_subst
    with
    | None ->
        (* the abstract value doesn't make sense in the caller: forget about it *)
        DecompilerExpr.reset_abstract_value expr_callee
    | Some (invalid_address, caller_history) ->
        let address_caller = Decompiler.find invalid_address astate in
        L.d_printfln "invalid_address= %a; address_caller= %a; caller_history= %a" AbstractValue.pp
          invalid_address DecompilerExpr.pp address_caller ValueHistory.pp caller_history ;
        address_caller
  in
  {invalid_access with Diagnostic.invalid_address= expr_caller}


type t =
  | AccessToInvalidAddress of Diagnostic.access_to_invalid_address
  | ErlangError of Diagnostic.ErlangError.t
  | ReadUninitializedValue of Diagnostic.read_uninitialized_value
[@@deriving compare, equal, yojson_of]

let to_diagnostic = function
  | AccessToInvalidAddress access_to_invalid_address ->
      Diagnostic.AccessToInvalidAddress access_to_invalid_address
  | ErlangError erlang_error ->
      Diagnostic.ErlangError erlang_error
  | ReadUninitializedValue read_uninitialized_value ->
      Diagnostic.ReadUninitializedValue read_uninitialized_value


let pp fmt latent_issue = Diagnostic.pp fmt (to_diagnostic latent_issue)

let add_call_to_calling_context call_and_loc = function
  | AccessToInvalidAddress access ->
      AccessToInvalidAddress {access with calling_context= call_and_loc :: access.calling_context}
  | ErlangError (Badarg {calling_context; location}) ->
      ErlangError (Badarg {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badkey {calling_context; location}) ->
      ErlangError (Badkey {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badmap {calling_context; location}) ->
      ErlangError (Badmap {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badmatch {calling_context; location}) ->
      ErlangError (Badmatch {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badrecord {calling_context; location}) ->
      ErlangError (Badrecord {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Case_clause {calling_context; location}) ->
      ErlangError (Case_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Function_clause {calling_context; location}) ->
      ErlangError (Function_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (If_clause {calling_context; location}) ->
      ErlangError (If_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Try_clause {calling_context; location}) ->
      ErlangError (Try_clause {calling_context= call_and_loc :: calling_context; location})
  | ReadUninitializedValue read ->
      ReadUninitializedValue {read with calling_context= call_and_loc :: read.calling_context}


let add_call call_and_loc call_subst astate latent_issue =
  let latent_issue =
    match latent_issue with
    | AccessToInvalidAddress invalid_access ->
        let invalid_access =
          add_call_to_access_to_invalid_address call_subst astate invalid_access
        in
        AccessToInvalidAddress invalid_access
    | _ ->
        latent_issue
  in
  add_call_to_calling_context call_and_loc latent_issue


let is_manifest astate =
  PulseArithmetic.is_manifest astate
  && ( (not Config.pulse_isl)
     || AbductiveDomain.Summary.is_isl_without_allocation astate
        && ( (not Config.pulse_manifest_emp)
           || AbductiveDomain.Summary.is_pre_without_isl_abduced astate ) )


(* require a summary because we don't want to stop reporting because some non-abducible condition is
   not true as calling context cannot possibly influence such conditions *)
let should_report (astate : AbductiveDomain.Summary.t) (diagnostic : Diagnostic.t) =
  match diagnostic with
  | ConstRefableParameter _
  | CSharpResourceLeak _
  | JavaResourceLeak _
  | MemoryLeak _
  | ReadonlySharedPtrParameter _
  | RetainCycle _
  | StackVariableAddressEscape _
  | TaintFlow _
  | UnnecessaryCopy _ ->
      (* these issues are reported regardless of the calling context, not sure if that's the right
         decision yet *)
      `ReportNow
  | AccessToInvalidAddress latent ->
      if is_manifest astate then `ReportNow else `DelayReport (AccessToInvalidAddress latent)
  | ErlangError latent ->
      if is_manifest astate then `ReportNow else `DelayReport (ErlangError latent)
  | ReadUninitializedValue latent ->
      if is_manifest astate then `ReportNow else `DelayReport (ReadUninitializedValue latent)
