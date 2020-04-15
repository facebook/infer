(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module SkippedCalls = PulseSkippedCalls

type trace = WrittenTo of PulseTrace.t | Invalid of (PulseInvalidation.t * PulseTrace.t)
[@@deriving compare]

module ModifiedVar = struct
  type t = {var: Var.t; access: unit HilExp.Access.t; trace: trace [@compare.ignore]}
  [@@deriving compare]

  let pp fmt {var} = F.fprintf fmt "@\n %a @\n" Var.pp var
end

module ModifiedVarSet = AbstractDomain.FiniteSet (ModifiedVar)
module Exited = AbstractDomain.BooleanOr

type t =
  { modified_params: ModifiedVarSet.t
  ; modified_globals: ModifiedVarSet.t
  ; skipped_calls: SkippedCalls.t
  ; exited: Exited.t }

let is_pure {modified_globals; modified_params; skipped_calls; exited} =
  ModifiedVarSet.is_empty modified_globals
  && ModifiedVarSet.is_empty modified_params
  && SkippedCalls.is_empty skipped_calls
  && Exited.is_bottom exited


let pure =
  { modified_params= ModifiedVarSet.empty
  ; modified_globals= ModifiedVarSet.empty
  ; skipped_calls= SkippedCalls.empty
  ; exited= Exited.bottom }


let join astate1 astate2 =
  if phys_equal astate1 astate2 then astate1
  else
    let {modified_globals= mg1; modified_params= mp1; skipped_calls= uk1; exited= e1} = astate1 in
    let {modified_globals= mg2; modified_params= mp2; skipped_calls= uk2; exited= e2} = astate2 in
    PhysEqual.optim2
      ~res:
        { modified_globals= ModifiedVarSet.join mg1 mg2
        ; modified_params= ModifiedVarSet.join mp1 mp2
        ; skipped_calls= SkippedCalls.union (fun _pname t1 _ -> Some t1) uk1 uk2
        ; exited= Exited.join e1 e2 }
      astate1 astate2


type param_source = Formal | Global

let pp_param_source fmt = function
  | Formal ->
      F.pp_print_string fmt "parameter"
  | Global ->
      F.pp_print_string fmt "global variable"


let add_to_errlog ~nesting param_source ModifiedVar.{var; trace} errlog =
  match trace with
  | WrittenTo access_trace ->
      PulseTrace.add_to_errlog ~include_value_history:false ~nesting
        ~pp_immediate:(fun fmt ->
          F.fprintf fmt "%a `%a` modified here" pp_param_source param_source Var.pp var )
        access_trace errlog
  | Invalid (invalidation, invalidation_trace) ->
      PulseTrace.add_to_errlog ~include_value_history:false ~nesting
        ~pp_immediate:(fun fmt ->
          F.fprintf fmt "%a `%a` %a here" pp_param_source param_source Var.pp var
            PulseInvalidation.describe invalidation )
        invalidation_trace errlog
