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
  val get_alist_size : Dom.Val.t -> Dom.Mem.astate -> Dom.Val.astate

  val load_val : Ident.t -> Dom.Val.astate -> Dom.Mem.astate -> Dom.Mem.astate

  type decl_local =
       Typ.Procname.t
    -> node_hash:int
    -> Location.t
    -> Loc.t
    -> Typ.t
    -> inst_num:int
    -> represents_multiple_values:bool
    -> dimension:int
    -> Dom.Mem.astate
    -> Dom.Mem.astate * int

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
    -> Dom.Mem.astate
    -> Dom.Mem.astate * int

  val decl_local_collection :
       Typ.Procname.t
    -> node_hash:int
    -> Location.t
    -> Loc.t
    -> inst_num:int
    -> represents_multiple_values:bool
    -> dimension:int
    -> Dom.Mem.astate
    -> Dom.Mem.astate * int

  type decl_sym_val =
       Typ.Procname.t
    -> Itv.SymbolPath.partial
    -> Tenv.t
    -> node_hash:int
    -> Location.t
    -> depth:int
    -> Loc.t
    -> Typ.t
    -> Dom.Mem.astate
    -> Dom.Mem.astate

  val decl_sym_arr :
       decl_sym_val:decl_sym_val
    -> Symb.SymbolPath.c_sym_array_kind
    -> Typ.Procname.t
    -> Itv.SymbolTable.t
    -> Itv.SymbolPath.partial
    -> Tenv.t
    -> node_hash:int
    -> Location.t
    -> depth:int
    -> Loc.t
    -> Typ.t
    -> ?offset:Itv.t
    -> ?size:Itv.t
    -> ?stride:int
    -> inst_num:int
    -> new_sym_num:Counter.t
    -> new_alloc_num:Counter.t
    -> Dom.Mem.astate
    -> Dom.Mem.astate

  val decl_sym_java_ptr :
       decl_sym_val:decl_sym_val
    -> Typ.Procname.t
    -> Itv.SymbolPath.partial
    -> Tenv.t
    -> node_hash:int
    -> Location.t
    -> depth:int
    -> Loc.t
    -> Typ.t
    -> inst_num:int
    -> new_alloc_num:Counter.t
    -> Dom.Mem.astate
    -> Dom.Mem.astate

  val decl_sym_collection :
       Typ.Procname.t
    -> Itv.SymbolTable.t
    -> Itv.SymbolPath.partial
    -> Location.t
    -> Loc.t
    -> new_sym_num:Counter.t
    -> Dom.Mem.astate
    -> Dom.Mem.astate

  val init_array_fields :
       Tenv.t
    -> Typ.IntegerWidths.t
    -> Typ.Procname.t
    -> Itv.SymbolPath.partial option
    -> node_hash:int
    -> Typ.t
    -> PowLoc.t
    -> ?dyn_length:Exp.t
    -> Dom.Mem.astate
    -> Dom.Mem.astate

  val set_dyn_length :
    Location.t -> Tenv.t -> Typ.t -> PowLoc.t -> Itv.t -> Dom.Mem.astate -> Dom.Mem.astate
end

module Check : sig
  val array_access :
       arr:Dom.Val.t
    -> idx:Dom.Val.t
    -> idx_sym_exp:Relation.SymExp.t option
    -> relation:Relation.astate
    -> is_plus:bool
    -> Location.t
    -> PO.ConditionSet.t
    -> PO.ConditionSet.t

  val lindex :
       Typ.IntegerWidths.t
    -> array_exp:Exp.t
    -> index_exp:Exp.t
    -> Dom.Mem.astate
    -> Location.t
    -> PO.ConditionSet.t
    -> PO.ConditionSet.t

  val lindex_byte :
       Typ.IntegerWidths.t
    -> array_exp:Exp.t
    -> byte_index_exp:Exp.t
    -> Dom.Mem.astate
    -> Location.t
    -> PO.ConditionSet.t
    -> PO.ConditionSet.t

  val collection_access :
       Typ.IntegerWidths.t
    -> array_exp:Exp.t
    -> index_exp:Exp.t
    -> ?is_collection_add:bool
    -> Dom.Mem.astate
    -> Location.t
    -> PO.ConditionSet.t
    -> PO.ConditionSet.t

  val binary_operation :
       Typ.IntegerWidths.t
    -> Binop.t
    -> lhs:Dom.Val.t
    -> rhs:Dom.Val.t
    -> Location.t
    -> PO.ConditionSet.t
    -> PO.ConditionSet.t
end
