(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseModelsImport
open PulseBasicInterface
open PulseDomainInterface

val matchers : matcher list

val get_model_from_db :
     Procname.t
  -> PulseValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
  -> model_no_non_disj option

val get_module_attribute : Tenv.t -> tag:string -> string option
[@@warning "-unused-value-declaration"]

val get_erlang_type_or_any : AbstractValue.t -> AbductiveDomain.t -> ErlangTypeName.t
[@@warning "-unused-value-declaration"]

include sig
  [@@@warning "-unused-module"]

  module Custom : sig
    type erlang_value = known_erlang_value option

    and known_erlang_value =
      | Atom of string option
      | IntLit of string option
      | List of erlang_value list
      | Tuple of erlang_value list
      | GenServer of {module_name: string option}

    val return_value_model : erlang_value -> model_no_non_disj
    [@@warning "-unused-value-declaration"]

    val exists_db_model : Procname.t -> bool
  end
end
