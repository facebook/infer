(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type trace = WrittenTo of PulseTrace.t | Invalid of (PulseInvalidation.t * PulseTrace.t)
[@@deriving compare]

module ModifiedVar = struct
  type nonempty_action_type = trace * trace list [@@deriving compare]

  type t = {var: Var.t; trace_list: nonempty_action_type} [@@deriving compare]

  let pp fmt {var} = F.fprintf fmt "@\n %a @\n" Var.pp var
end

module ModifiedVarSet = AbstractDomain.FiniteSet (ModifiedVar)

type t = {modified_params: ModifiedVarSet.t; modified_globals: ModifiedVarSet.t}

let is_pure {modified_globals; modified_params} =
  ModifiedVarSet.is_empty modified_globals && ModifiedVarSet.is_empty modified_params


let pure = {modified_params= ModifiedVarSet.empty; modified_globals= ModifiedVarSet.empty}

let join astate1 astate2 =
  if phys_equal astate1 astate2 then astate1
  else
    let {modified_globals= mg1; modified_params= mp1} = astate1 in
    let {modified_globals= mg2; modified_params= mp2} = astate2 in
    PhysEqual.optim2
      ~res:
        {modified_globals= ModifiedVarSet.join mg1 mg2; modified_params= ModifiedVarSet.join mp1 mp2}
      astate1 astate2


type param_source = Formal | Global

let pp_param_source fmt = function
  | Formal ->
      F.pp_print_string fmt "parameter"
  | Global ->
      F.pp_print_string fmt "global variable"


let add_to_errlog ~nesting param_source ModifiedVar.{var; trace_list} errlog =
  let aux ~nesting errlog trace =
    match trace with
    | WrittenTo access_trace ->
        PulseTrace.add_to_errlog ~nesting
          ~pp_immediate:(fun fmt ->
            F.fprintf fmt "%a `%a` modified here" pp_param_source param_source Var.pp var )
          access_trace errlog
    | Invalid (invalidation, invalidation_trace) ->
        PulseTrace.add_to_errlog ~nesting
          ~pp_immediate:(fun fmt ->
            F.fprintf fmt "%a `%a` %a here" pp_param_source param_source Var.pp var
              PulseInvalidation.describe invalidation )
          invalidation_trace errlog
  in
  let first_trace, rest = trace_list in
  List.fold_left rest ~init:(aux ~nesting errlog first_trace) ~f:(aux ~nesting)


let pp fmt ({modified_globals; modified_params} as astate) =
  if is_pure astate then F.fprintf fmt "@\n pure @\n"
  else if ModifiedVarSet.is_empty modified_params then
    F.fprintf fmt "@\n impure, modified globals :%a @\n" ModifiedVarSet.pp modified_globals
  else if ModifiedVarSet.is_empty modified_globals then
    F.fprintf fmt "@\n impure, modified params :%a @\n" ModifiedVarSet.pp modified_params
  else
    F.fprintf fmt "@\n impure, modified params :%a, modified globals :%a @\n" ModifiedVarSet.pp
      modified_params ModifiedVarSet.pp modified_globals
