(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

type 'a model_monad

type astate = AbductiveDomain.t

type aval = AbstractValue.t * ValueHistory.t

module Syntax : sig
  module ModeledField = PulseOperations.ModeledField

  (** {2 Polymorphic Operations} *)

  val ( let* ) : 'a model_monad -> ('a -> 'b model_monad) -> 'b model_monad

  val ret : 'a -> 'a model_monad

  val throw : unit model_monad [@@warning "-unused-value-declaration"]

  val unreachable : 'a model_monad

  val report : Diagnostic.t -> unit model_monad

  val list_fold :
    'a list -> init:'accum -> f:('accum -> 'a -> 'accum model_monad) -> 'accum model_monad

  val list_iter : 'a list -> f:('a -> unit model_monad) -> unit model_monad

  val list_filter_map : 'a list -> f:('a -> 'b option model_monad) -> 'b list model_monad

  val option_iter : 'a option -> f:('a -> unit model_monad) -> unit model_monad

  val absvalue_set_fold :
       AbstractValue.Set.t
    -> init:'accum
    -> f:('accum -> AbstractValue.t -> 'accum model_monad)
    -> 'accum model_monad
  [@@warning "-unused-value-declaration"]

  val absvalue_set_iter :
    AbstractValue.Set.t -> f:(AbstractValue.t -> unit model_monad) -> unit model_monad

  val ignore : 'a model_monad -> unit model_monad [@@warning "-unused-value-declaration"]

  val assign_ret : aval -> unit model_monad
  (** assign the value to the return variable of the current function *)

  val dynamic_dispatch :
       cases:(Typ.name * (unit -> 'a model_monad)) list
    -> ?default:(unit -> 'a model_monad)
    -> aval
    -> 'a model_monad

  val dispatch_call :
       Ident.t * Typ.t
    -> Procname.t
    -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
    -> unit model_monad

  val apply_hack_closure : aval -> aval list -> aval model_monad

  val get_data : PulseModelsImport.model_data model_monad

  (** {2 Disjunctive reasoning} *)

  val disjuncts : 'a model_monad list -> 'a model_monad

  val start_model : unit model_monad -> PulseModelsImport.model
  (** get a model from a disjunctive model_monad *)

  val lift_to_monad : PulseModelsImport.model -> unit model_monad
  (** beware that the model may modify the [PulseModelsImport.model_data.ret] field *)

  val lift_to_monad_and_get_result : PulseModelsImport.model -> aval model_monad
  (** apply the model and return its result. fails if the model did not assign the reserved
      [model_data.ret] variable. *)

  (** {2 Operations}

      [PulseOperations] functions you need should be copied here *)

  val allocation : Attribute.allocator -> aval -> unit model_monad

  val add_dict_contain_const_keys : aval -> unit model_monad

  val add_dict_read_const_key : aval -> Fieldname.t -> unit model_monad

  val remove_dict_contain_const_keys : aval -> unit model_monad

  val is_hack_sinit_called : aval -> bool model_monad

  val set_hack_sinit_called : aval -> unit model_monad

  val add_static_type : Typ.name -> aval -> unit model_monad

  val deep_copy : ?depth_max:int -> aval -> aval model_monad

  val eval_binop : Binop.t -> aval -> aval -> aval model_monad

  val eval_binop_int : Binop.t -> aval -> IntLit.t -> aval model_monad

  val eval_read : Exp.t -> aval model_monad [@@warning "-unused-value-declaration"]

  val eval_const_int : int -> aval model_monad

  val eval_const_string : string -> aval model_monad

  val eval_string_concat : aval -> aval -> aval model_monad

  val eval_access : ?desc:string -> access_mode -> aval -> Access.t -> aval model_monad

  val eval_deref_access : access_mode -> aval -> Access.t -> aval model_monad

  val get_dynamic_type :
    ask_specialization:bool -> aval -> Formula.dynamic_type_data option model_monad

  val new_ : Exp.t -> aval model_monad

  val constructor : Typ.Name.t -> (string * aval) list -> aval model_monad
  (** [constructor_dsl typ_name fields] builds a fresh object of type [typ_name] and initializes its
      fields using list [fields] *)

  val remove_hack_builder_attributes : aval -> unit model_monad
  [@@warning "-unused-value-declaration"]

  val get_const_string : aval -> string option model_monad

  val mk_fresh : model_desc:string -> ?more:string -> unit -> aval model_monad

  val write_field : ref:aval -> obj:aval -> Fieldname.t -> unit model_monad

  val write_deref_field : ref:aval -> obj:aval -> Fieldname.t -> unit model_monad

  val get_known_fields : aval -> Access.t list model_monad
  (** Return the fields we know about. There may be more, so use with caution *)

  (** {2 PulseFormula operations} *)

  val prune_eq : aval -> aval -> unit model_monad

  val prune_eq_int : aval -> IntLit.t -> unit model_monad

  val prune_eq_string : aval -> string -> unit model_monad

  val prune_ne_string : aval -> string -> unit model_monad

  val prune_eq_zero : aval -> unit model_monad

  val prune_positive : aval -> unit model_monad

  val prune_lt : aval -> aval -> unit model_monad

  val prune_lt_int : aval -> IntLit.t -> unit model_monad

  val prune_le : aval -> aval -> unit model_monad

  val prune_gt : aval -> aval -> unit model_monad

  val prune_gt_int : aval -> IntLit.t -> unit model_monad

  val prune_ge : aval -> aval -> unit model_monad

  val prune_ge_int : aval -> IntLit.t -> unit model_monad

  val prune_ne : aval -> aval -> unit model_monad

  val prune_ne_int : aval -> IntLit.t -> unit model_monad

  val prune_ne_zero : aval -> unit model_monad

  val and_eq_int : aval -> IntLit.t -> unit model_monad

  val and_eq : aval -> aval -> unit model_monad

  val and_equal_instanceof : aval -> aval -> Typ.t -> nullable:bool -> unit model_monad

  val and_positive : aval -> unit model_monad [@@warning "-unused-value-declaration"]

  val as_constant_q : aval -> Q.t option model_monad

  val as_constant_int : aval -> int option model_monad

  val as_constant_bool : aval -> bool option model_monad

  val as_constant_string : aval -> string option model_monad

  val aval_of_int : ValueHistory.t -> int -> aval model_monad

  (** {2 Tenv operations} *)

  val tenv_resolve_field_info : Typ.name -> Fieldname.t -> Struct.field_info option model_monad

  val tenv_resolve_fieldname : Typ.name -> string -> Fieldname.t option model_monad

  val write_deref : ref:aval -> obj:aval -> unit model_monad

  (** {2 Invalidation operations} *)

  val invalidate_access : Invalidation.t -> aval -> Access.t -> unit model_monad

  (** {2 Escape Hatches}

      if necessary you can convert an operation outside of this module with the following operators *)

  val exec_command : (astate -> astate) -> unit model_monad

  val exec_partial_command : (astate -> astate PulseOperationResult.t) -> unit model_monad

  val exec_operation : (astate -> 'a * astate) -> 'a model_monad

  val exec_partial_operation : (astate -> (astate * 'a) PulseOperationResult.t) -> 'a model_monad

  val exec_pure_operation : (astate -> 'a) -> 'a model_monad

  val register_class_object_for_value : aval -> aval -> unit model_monad
  (** This is used to make hack_get_static_class behave like a pure function *)

  module Basic : sig
    val alloc_not_null :
      ?desc:string -> Attribute.allocator -> Exp.t option -> initialize:bool -> unit model_monad
  end
end

val unsafe_to_astate_transformer :
  'a model_monad -> PulseModelsImport.model_data -> astate -> ('a * astate) sat_unsat_t
(** warning: the transformation will fail if the result of the computation is not a single abstract
    state with no error and it ignores the non-disjunctive state. You should think twice before
    using it... *)
