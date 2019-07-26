(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
module Dom = BufferOverrunDomain
module Relation = BufferOverrunDomainRelation
module PO = BufferOverrunProofObligations

module ModelEnv : sig
  type model_env =
    { pname: Typ.Procname.t
    ; node_hash: int
    ; location: Location.t
    ; tenv: Tenv.t
    ; integer_type_widths: Typ.IntegerWidths.t }

  val mk_model_env :
    Typ.Procname.t -> node_hash:int -> Location.t -> Tenv.t -> Typ.IntegerWidths.t -> model_env
end

module Exec : sig
  val load_locs :
    represents_multiple_values:bool -> Ident.t -> Typ.t -> PowLoc.t -> Dom.Mem.t -> Dom.Mem.t

  val decl_local : ModelEnv.model_env -> Dom.Mem.t * int -> Loc.t * Typ.t -> Dom.Mem.t * int

  val init_c_array_fields :
       ModelEnv.model_env
    -> Itv.SymbolPath.partial option
    -> Typ.t
    -> PowLoc.t
    -> ?dyn_length:Exp.t
    -> Dom.Mem.t
    -> Dom.Mem.t

  val set_dyn_length : ModelEnv.model_env -> Typ.t -> PowLoc.t -> Itv.t -> Dom.Mem.t -> Dom.Mem.t

  val decl_string :
    ModelEnv.model_env -> do_alloc:bool -> PowLoc.t -> string -> Dom.Mem.t -> Dom.Mem.t

  val set_c_strlen : tgt:Dom.Val.t -> src:Dom.Val.t -> Dom.Mem.t -> Dom.Mem.t
end

module Check : sig
  val array_access :
       arr:Dom.Val.t
    -> idx:Dom.Val.t
    -> idx_sym_exp:Relation.SymExp.t option
    -> relation:Relation.t
    -> is_plus:bool
    -> last_included:bool
    -> latest_prune:Dom.LatestPrune.t
    -> Location.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t

  val lindex :
       Typ.IntegerWidths.t
    -> array_exp:Exp.t
    -> index_exp:Exp.t
    -> last_included:bool
    -> Dom.Mem.t
    -> Location.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t

  val lindex_byte :
       Typ.IntegerWidths.t
    -> array_exp:Exp.t
    -> byte_index_exp:Exp.t
    -> last_included:bool
    -> Dom.Mem.t
    -> Location.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t

  val binary_operation :
       Typ.IntegerWidths.t
    -> Binop.t
    -> lhs:Dom.Val.t
    -> rhs:Dom.Val.t
    -> latest_prune:Dom.LatestPrune.t
    -> Location.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t
end
