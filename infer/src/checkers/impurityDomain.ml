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

module PVar = struct
  type t = Pvar.t [@@deriving compare]

  let pp fmt pv = F.pp_print_string fmt (Pvar.get_simplified_name pv)
end

module ModifiedAccess = struct
  type t = {ordered_access_list: unit HilExp.Access.t list; trace: trace [@compare.ignore]}
  [@@deriving compare]

  let pp fmt {ordered_access_list} =
    let pp_sep fmt () = F.fprintf fmt "" in
    (F.pp_print_list ~pp_sep (HilExp.Access.pp (fun _ _ -> ()))) fmt ordered_access_list
end

module ModifiedVarMap = AbstractDomain.FiniteMultiMap (PVar) (ModifiedAccess)
module Exited = AbstractDomain.BooleanOr

type t =
  { modified_params: ModifiedVarMap.t
  ; modified_globals: ModifiedVarMap.t
  ; skipped_calls: SkippedCalls.t
  ; exited: Exited.t }

let is_pure {modified_globals; modified_params; skipped_calls; exited} =
  ModifiedVarMap.is_bottom modified_globals
  && ModifiedVarMap.is_bottom modified_params
  && SkippedCalls.is_empty skipped_calls
  && Exited.is_bottom exited


let pure =
  { modified_params= ModifiedVarMap.bottom
  ; modified_globals= ModifiedVarMap.bottom
  ; skipped_calls= SkippedCalls.empty
  ; exited= Exited.bottom }


let join astate1 astate2 =
  if phys_equal astate1 astate2 then astate1
  else
    let {modified_globals= mg1; modified_params= mp1; skipped_calls= uk1; exited= e1} = astate1 in
    let {modified_globals= mg2; modified_params= mp2; skipped_calls= uk2; exited= e2} = astate2 in
    PhysEqual.optim2
      ~res:
        { modified_globals= ModifiedVarMap.join mg1 mg2
        ; modified_params= ModifiedVarMap.join mp1 mp2
        ; skipped_calls= SkippedCalls.union (fun _pname t1 _ -> Some t1) uk1 uk2
        ; exited= Exited.join e1 e2 }
      astate1 astate2


type param_source = Formal | Global

let pp_param_source fmt = function
  | Formal ->
      F.pp_print_string fmt "parameter"
  | Global ->
      F.pp_print_string fmt "global variable"


let add_to_errlog ~nesting param_source pvar (ModifiedAccess.{trace} as access) errlog =
  match trace with
  | WrittenTo access_trace ->
      PulseTrace.add_to_errlog ~include_value_history:false ~nesting
        ~pp_immediate:(fun fmt ->
          F.fprintf fmt "%a `%a.%a` modified here" pp_param_source param_source PVar.pp pvar
            ModifiedAccess.pp access )
        access_trace errlog
  | Invalid (invalidation, invalidation_trace) ->
      PulseTrace.add_to_errlog ~include_value_history:false ~nesting
        ~pp_immediate:(fun fmt ->
          F.fprintf fmt "%a `%a.%a` %a here" pp_param_source param_source PVar.pp pvar
            ModifiedAccess.pp access PulseInvalidation.describe invalidation )
        invalidation_trace errlog
