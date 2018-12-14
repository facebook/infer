(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
module Dom = BufferOverrunDomain
module Relation = BufferOverrunDomainRelation
module PO = BufferOverrunProofObligations

module Exec : sig
  val get_alist_size : Dom.Val.t -> Dom.Mem.t -> Dom.Val.t

  val load_locs : Ident.t -> PowLoc.t -> Dom.Mem.t -> Dom.Mem.t

  val load_val : Ident.t -> Dom.Val.t -> Dom.Mem.t -> Dom.Mem.t

  type decl_local =
       Typ.Procname.t
    -> node_hash:int
    -> Location.t
    -> Loc.t
    -> Typ.t
    -> inst_num:int
    -> represents_multiple_values:bool
    -> dimension:int
    -> Dom.Mem.t
    -> Dom.Mem.t * int

  val decl_local_array :
       decl_local:decl_local
    -> Typ.Procname.t
    -> node_hash:int
    -> Location.t
    -> Loc.t
    -> Typ.t
    -> length:IntLit.t option
    -> ?stride:int
    -> inst_num:int
    -> represents_multiple_values:bool
    -> dimension:int
    -> Dom.Mem.t
    -> Dom.Mem.t * int

  val decl_local_collection :
       Typ.Procname.t
    -> node_hash:int
    -> Location.t
    -> Loc.t
    -> inst_num:int
    -> represents_multiple_values:bool
    -> dimension:int
    -> Dom.Mem.t
    -> Dom.Mem.t * int

  val init_c_array_fields :
       Tenv.t
    -> Typ.IntegerWidths.t
    -> Typ.Procname.t
    -> Itv.SymbolPath.partial option
    -> node_hash:int
    -> Typ.t
    -> PowLoc.t
    -> ?dyn_length:Exp.t
    -> Dom.Mem.t
    -> Dom.Mem.t

  val set_dyn_length : Location.t -> Tenv.t -> Typ.t -> PowLoc.t -> Itv.t -> Dom.Mem.t -> Dom.Mem.t
end

module Check : sig
  val array_access :
       arr:Dom.Val.t
    -> idx:Dom.Val.t
    -> idx_sym_exp:Relation.SymExp.t option
    -> relation:Relation.t
    -> is_plus:bool
    -> last_included:bool
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

  val collection_access :
       Typ.IntegerWidths.t
    -> array_exp:Exp.t
    -> index_exp:Exp.t
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
    -> Location.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t
end
