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
    { pname: Procname.t (* the name of the builtin *)
    ; caller_pname: Procname.t option (* caller of the builtin *)
    ; node_hash: int
    ; location: Location.t
    ; tenv: Tenv.t
    ; integer_type_widths: IntegerWidths.t
    ; get_summary: BufferOverrunAnalysisSummary.get_summary }

  val mk_model_env :
       Procname.t
    -> ?caller_pname:Procname.t
    -> node_hash:int
    -> Location.t
    -> Tenv.t
    -> IntegerWidths.t
    -> BufferOverrunAnalysisSummary.get_summary
    -> model_env
  (** Make model environment. caller_pname is relevant only when the model environment is used to
      process builtins. *)
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
  (** [load_locs id typ locs mem] loads the contents of the memory locations [locs] (which have type
      [typ]) into the identifier [id].

      If [locs] represents a single location, also create an aliasing relation between this location
      and [id]. This is useful, e.g., to constrain also the location when the identifier is
      constrained. *)

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
       IntegerWidths.t
    -> array_exp:Exp.t
    -> index_exp:Exp.t
    -> last_included:bool
    -> Dom.Mem.t
    -> Location.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t

  val lindex_byte :
       IntegerWidths.t
    -> array_exp:Exp.t
    -> byte_index_exp:Exp.t
    -> last_included:bool
    -> Dom.Mem.t
    -> Location.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t

  val binary_operation :
       IntegerWidths.t
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

      [is_args_ref] represents that the arguments are given as references to variables, e.g., when
      [int i = 5;], the function of [std::make_shared<C>(i);] in C++ is translated to
      [std::make_shared<C>(&i, tgt)] in Sil where [tgt] is the variable for the target object,
      rather than [std::make_shared<C>(i, tgt)] (note that the type of [&i] is [int&]).

      The [is_args_ref] value is used to evaluate argments correctly after replacing the callee. For
      example, when we replace [std::make_shared<C>(&i, tgt)] to the constructor call of [C], i.e.
      [C(tgt, i)], the arguments' order and types are slightly different, so which should be handled
      correctly later in the instantiation phase. *)
  type replaced = {pname: Procname.t; args: (Exp.t * Typ.t) list; is_args_ref: bool}

  val replace_make_shared : Tenv.t -> get_formals -> Procname.t -> (Exp.t * Typ.t) list -> replaced
end

val clear_cache : unit -> unit
