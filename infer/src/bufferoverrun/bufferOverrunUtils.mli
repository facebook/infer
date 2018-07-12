(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
module Dom = BufferOverrunDomain
module PO = BufferOverrunProofObligations

module Exec : sig
  val load_val : Ident.t -> Dom.Val.astate -> Dom.Mem.astate -> Dom.Mem.astate

  type decl_local =
    Typ.Procname.t -> node_hash:int -> Location.t -> Loc.t -> Typ.t -> inst_num:int
    -> dimension:int -> Dom.Mem.astate -> Dom.Mem.astate * int

  val decl_local_array :
    decl_local:decl_local -> Typ.Procname.t -> node_hash:int -> Location.t -> Loc.t -> Typ.t
    -> length:IntLit.t option -> ?stride:int -> inst_num:int -> dimension:int -> Dom.Mem.astate
    -> Dom.Mem.astate * int

  type decl_sym_val =
    Typ.Procname.t -> Itv.SymbolPath.partial -> Tenv.t -> node_hash:int -> Location.t -> depth:int
    -> Loc.t -> Typ.t -> Dom.Mem.astate -> Dom.Mem.astate

  val decl_sym_arr :
    decl_sym_val:decl_sym_val -> Typ.Procname.t -> Itv.SymbolPath.partial -> Tenv.t
    -> node_hash:int -> Location.t -> depth:int -> Loc.t -> Typ.t -> ?offset:Itv.t -> ?size:Itv.t
    -> inst_num:int -> new_sym_num:Itv.Counter.t -> new_alloc_num:Itv.Counter.t -> Dom.Mem.astate
    -> Dom.Mem.astate

  val init_array_fields :
    Tenv.t -> Typ.Procname.t -> node_hash:int -> Typ.t -> PowLoc.t -> ?dyn_length:Exp.t
    -> Dom.Mem.astate -> Dom.Mem.astate

  val set_dyn_length : Tenv.t -> Typ.t -> PowLoc.t -> Itv.t -> Dom.Mem.astate -> Dom.Mem.astate
end

module Check : sig
  val array_access :
    arr:Dom.Val.t -> idx:Dom.Val.t -> is_plus:bool -> Typ.Procname.t -> Location.t
    -> PO.ConditionSet.t -> PO.ConditionSet.t

  val lindex :
    array_exp:Exp.t -> index_exp:Exp.t -> Dom.Mem.astate -> Typ.Procname.t -> Location.t
    -> PO.ConditionSet.t -> PO.ConditionSet.t
end
