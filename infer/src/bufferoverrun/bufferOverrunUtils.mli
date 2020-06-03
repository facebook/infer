(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
module Dom = BufferOverrunDomain
module PO = BufferOverrunProofObligations

module ModelEnv : sig
  type model_env =
    { pname: Procname.t
    ; node_hash: int
    ; location: Location.t
    ; tenv: Tenv.t
    ; integer_type_widths: Typ.IntegerWidths.t
    ; get_summary: BufferOverrunAnalysisSummary.get_summary }

  val mk_model_env :
       Procname.t
    -> node_hash:int
    -> Location.t
    -> Tenv.t
    -> Typ.IntegerWidths.t
    -> BufferOverrunAnalysisSummary.get_summary
    -> model_env
end

module Exec : sig
  val load_locs :
       represents_multiple_values:bool
    -> modeled_range:Dom.ModeledRange.t option
    -> Ident.t
    -> Typ.t
    -> PowLoc.t
    -> Dom.Mem.t
    -> Dom.Mem.t

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
    -> Procname.t
    -> Binop.t
    -> lhs:Dom.Val.t
    -> rhs:Dom.Val.t
    -> latest_prune:Dom.LatestPrune.t
    -> Location.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t
end

type get_formals = Procname.t -> (Pvar.t * Typ.t) list option

module ReplaceCallee : sig
  (** Replaced proc name with its modified parameters.

      [is_params_ref] represents that the parameters are given as references to variables, e.g.,
      when [int i = 5;], the function of [std::make_shared<C>(i);] in C++ is translated to
      [std::make_shared<C>(&i, tgt)] in Sil where [tgt] is the variable for the target object,
      rather than [std::make_shared<C>(i, tgt)] (note that the type of [&i] is [int&]).

      The [is_params_ref] value is used to evaluate parameters correctly after replacing the callee.
      For example, when we replace [std::make_shared<C>(&i, tgt)] to the constructor call of [C],
      i.e. [C(tgt, i)], the parameters' order and types are slightly different, so which should be
      handled correctly later in the instantiation phase. *)
  type replaced = {pname: Procname.t; params: (Exp.t * Typ.t) list; is_params_ref: bool}

  val replace_make_shared : Tenv.t -> get_formals -> Procname.t -> (Exp.t * Typ.t) list -> replaced
end

val clear_cache : unit -> unit
