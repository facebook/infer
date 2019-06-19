(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | AccessToInvalidAddress of
      { access: HilExp.AccessExpression.t PulseDomain.explained
      ; invalidated_by: PulseDomain.Invalidation.t PulseDomain.Trace.t
      ; accessed_by: HilExp.AccessExpression.t PulseDomain.explained PulseDomain.Trace.t }
  | StackVariableAddressEscape of
      { variable: Var.t
      ; history: PulseDomain.ValueHistory.t
      ; location: Location.t }

let get_location = function
  | AccessToInvalidAddress {accessed_by} ->
      PulseDomain.InterprocAction.to_outer_location accessed_by.action
  | StackVariableAddressEscape {location} ->
      location


let get_message = function
  | AccessToInvalidAddress {access; accessed_by; invalidated_by; _} ->
      (* The goal is to get one of the following messages depending on the scenario:

         42: delete x; return x->f
         "`x->f` accesses `x`, which was invalidated at line 42 by `delete` on `x`"

         42: bar(x); return x->f
         "`x->f` accesses `x`, which was invalidated at line 42 by `delete` on `x` in call to `bar`"

         42: bar(x); foo(x);
         "call to `foo` eventually accesses `x->f` but `x` was invalidated at line 42 by `delete` on `x` in call to `bar`"

         If we don't have "x->f" but instead some non-user-visible expression, then
         "access to `x`, which was invalidated at line 42 by `delete` on `x`"

         Likewise if we don't have "x" in the second part but instead some non-user-visible expression, then
         "`x->f` accesses `x`, which was invalidated at line 42 by `delete`"
      *)
      let pp_access_trace invalidated f
          (trace : HilExp.AccessExpression.t PulseDomain.explained PulseDomain.Trace.t) =
        match trace.action with
        | Immediate {imm= access; _} -> (
          match HilExp.AccessExpression.to_source_string (access :> HilExp.AccessExpression.t) with
          | Some access_s
            when HilExp.AccessExpression.equal (access :> HilExp.AccessExpression.t) invalidated ->
              F.fprintf f "`%s` " access_s
          | Some access_s -> (
            match HilExp.AccessExpression.to_source_string invalidated with
            | Some invalidated_s ->
                F.fprintf f "`%s` accesses `%s`, which " access_s invalidated_s
            | None ->
                F.fprintf f "access to `%s`, which " access_s )
          | None -> (
            match HilExp.AccessExpression.to_source_string invalidated with
            | Some invalidated_s ->
                F.fprintf f "`%s` " invalidated_s
            | None ->
                F.fprintf f "accessing memory that " ) )
        | ViaCall {action; proc_name; _} -> (
            let access_and_invalidated_s =
              match
                ( HilExp.AccessExpression.to_source_string
                    (PulseDomain.InterprocAction.get_immediate action :> HilExp.AccessExpression.t)
                , HilExp.AccessExpression.to_source_string invalidated )
              with
              | Some access_s, Some invalidated_s ->
                  Some (access_s, invalidated_s)
              | Some s, None | None, Some s ->
                  Some (s, s)
              | None, None ->
                  None
            in
            match access_and_invalidated_s with
            | Some (access_s, invalidated_s) ->
                F.fprintf f "call to `%a` eventually accesses `%s` but `%s` " Typ.Procname.describe
                  proc_name access_s invalidated_s
            | None ->
                F.fprintf f "call to `%a` eventually accesses `%a`, which " Typ.Procname.describe
                  proc_name HilExp.AccessExpression.pp invalidated )
      in
      let pp_invalidation_trace line f trace =
        match trace.PulseDomain.Trace.action with
        | Immediate {imm= invalidation; _} ->
            F.fprintf f "%a on line %d" PulseDomain.Invalidation.describe invalidation line
        | ViaCall {action; proc_name; _} ->
            F.fprintf f "%a on line %d indirectly during the call to `%a`"
              PulseDomain.Invalidation.describe
              (PulseDomain.InterprocAction.get_immediate action)
              line Typ.Procname.describe proc_name
      in
      let line_of_trace trace =
        let {Location.line; _} =
          PulseDomain.InterprocAction.to_outer_location trace.PulseDomain.Trace.action
        in
        line
      in
      let invalidation_line = line_of_trace invalidated_by in
      F.asprintf "%a%a"
        (pp_access_trace (access :> HilExp.AccessExpression.t))
        accessed_by
        (pp_invalidation_trace invalidation_line)
        invalidated_by
  | StackVariableAddressEscape {variable; _} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "address of %a is returned by the function" pp_var variable


let get_trace = function
  | AccessToInvalidAddress {accessed_by; invalidated_by} ->
      PulseDomain.Trace.add_to_errlog ~header:"invalidation part of the trace starts here"
        (fun f invalidation ->
          F.fprintf f "memory %a" PulseDomain.Invalidation.describe invalidation )
        invalidated_by
      @@ PulseDomain.Trace.add_to_errlog ~header:"use-after-lifetime part of the trace starts here"
           (fun f (access : HilExp.AccessExpression.t PulseDomain.explained) ->
             F.fprintf f "invalid access to `%a`" HilExp.AccessExpression.pp
               (access :> HilExp.AccessExpression.t) )
           accessed_by
      @@ []
  | StackVariableAddressEscape {history; location; _} ->
      PulseDomain.ValueHistory.add_to_errlog ~nesting:0 history
      @@
      let nesting = 0 in
      [Errlog.make_trace_element nesting location "returned here" []]


let get_issue_type = function
  | AccessToInvalidAddress {invalidated_by} ->
      PulseDomain.InterprocAction.get_immediate invalidated_by.action
      |> PulseDomain.Invalidation.issue_type_of_cause
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
