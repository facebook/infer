(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type trace =
  | WrittenTo of unit PulseDomain.InterprocAction.t
  | Invalid of PulseDomain.Invalidation.t PulseDomain.Trace.t
[@@deriving compare]

module ModifiedVar : sig
  type nonempty_action_type = trace * trace list

  type t = {var: Var.t; trace_list: nonempty_action_type}
end

module ModifiedVarSet : sig
  include AbstractDomain.FiniteSetS with type elt = ModifiedVar.t
end

type t = {modified_params: ModifiedVarSet.t; modified_globals: ModifiedVarSet.t}

val pure : t

val is_pure : t -> bool

val add_to_errlog :
     nesting:int
  -> str:string
  -> ModifiedVar.t
  -> Errlog.loc_trace_elem list
  -> Errlog.loc_trace_elem list

val join : t -> t -> t
