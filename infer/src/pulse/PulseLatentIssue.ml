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

let add_call_to_access_to_invalid_address (call_event, location) (call_subst, hist_map) astate
    invalid_access =
  let expr_callee = invalid_access.Diagnostic.invalid_address in
  L.d_printfln "adding call to invalid address %a" DecompilerExpr.pp_with_abstract_value expr_callee ;
  let expr_caller, default_caller_history =
    match
      let open IOption.Let_syntax in
      let* addr_callee = DecompilerExpr.abstract_value_of_expr expr_callee in
      AbstractValue.Map.find_opt addr_callee call_subst
    with
    | None ->
        (* the abstract value doesn't make sense in the caller: forget about it *)
        (DecompilerExpr.reset_abstract_value expr_callee, ValueHistory.epoch)
    | Some (invalid_address, caller_history) ->
        (Decompiler.find invalid_address astate, caller_history)
  in
  let access_trace =
    Trace.add_call call_event location hist_map ~default_caller_history
      invalid_access.Diagnostic.access_trace
  in
  {invalid_access with Diagnostic.access_trace; invalid_address= expr_caller}


type t =
  | AccessToInvalidAddress of Diagnostic.access_to_invalid_address
  | ErlangError of Diagnostic.ErlangError.t
[@@deriving compare, equal, yojson_of]

let to_diagnostic = function
  | AccessToInvalidAddress access_to_invalid_address ->
      Diagnostic.AccessToInvalidAddress access_to_invalid_address
  | ErlangError erlang_error ->
      Diagnostic.ErlangError erlang_error


let pp fmt latent_issue = Diagnostic.pp fmt (to_diagnostic latent_issue)

let add_call_to_calling_context call_and_loc = function
  | AccessToInvalidAddress access ->
      AccessToInvalidAddress {access with calling_context= call_and_loc :: access.calling_context}
  | ErlangError (Badarg {calling_context; location}) ->
      ErlangError (Badarg {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badgenerator {calling_context; location}) ->
      ErlangError (Badgenerator {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badkey {calling_context; location}) ->
      ErlangError (Badkey {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badmap {calling_context; location}) ->
      ErlangError (Badmap {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badmatch {calling_context; location}) ->
      ErlangError (Badmatch {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badrecord {calling_context; location}) ->
      ErlangError (Badrecord {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badreturn {calling_context; location}) ->
      ErlangError (Badreturn {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Case_clause {calling_context; location}) ->
      ErlangError (Case_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Else_clause {calling_context; location}) ->
      ErlangError (Else_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Function_clause {calling_context; location}) ->
      ErlangError (Function_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (If_clause {calling_context; location}) ->
      ErlangError (If_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Try_clause {calling_context; location}) ->
      ErlangError (Try_clause {calling_context= call_and_loc :: calling_context; location})


let add_call call_and_loc call_substs astate latent_issue =
  let latent_issue =
    match latent_issue with
    | AccessToInvalidAddress invalid_access ->
        let invalid_access =
          add_call_to_access_to_invalid_address call_and_loc call_substs astate invalid_access
        in
        AccessToInvalidAddress invalid_access
    | _ ->
        latent_issue
  in
  add_call_to_calling_context call_and_loc latent_issue


(* require a summary because we don't want to stop reporting because some non-abducible condition is
   not true as calling context cannot possibly influence such conditions *)
let should_report (astate : AbductiveDomain.Summary.t) (diagnostic : Diagnostic.t) =
  match diagnostic with
  | ConfigUsage _
  | ConstRefableParameter _
  | CSharpResourceLeak _
  | DynamicTypeMismatch _
  | JavaResourceLeak _
  | TransitiveAccess _
  | HackCannotInstantiateAbstractClass _
  | HackUnawaitedAwaitable _
  | HackUnfinishedBuilder _
  | MemoryLeak _
  | MutualRecursionCycle _
  | ReadUninitialized _
  | ReadonlySharedPtrParameter _
  | RetainCycle _
  | StackVariableAddressEscape _
  | TaintFlow _
  | UnnecessaryCopy _ ->
      (* these issues are reported regardless of the calling context, not sure if that's the right
         decision yet *)
      `ReportNow
  | AccessToInvalidAddress latent ->
      if PulseArithmetic.is_manifest astate then `ReportNow
      else `DelayReport (AccessToInvalidAddress latent)
  | ErlangError latent ->
      if PulseArithmetic.is_manifest astate then `ReportNow else `DelayReport (ErlangError latent)
