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
open PulseModelsImport

type 'a model_monad

type astate = AbductiveDomain.t

type aval = AbstractValue.t * ValueHistory.t

module Syntax : sig
  module ModeledField = PulseOperations.ModeledField

  val to_aval : ValueOrigin.t -> aval

  (** {2 Polymorphic Operations} *)

  val ( let* ) : 'a model_monad -> ('a -> 'b model_monad) -> 'b model_monad

  val ( >>= ) : 'a model_monad -> ('a -> 'b model_monad) -> 'b model_monad

  val ( @= ) : ('a -> 'b model_monad) -> 'a model_monad -> 'b model_monad

  val ( @@> ) : unit model_monad -> 'a model_monad -> 'a model_monad
  (** sequential composition *)

  val compose1 : ('a -> model) -> ('a -> model) -> 'a -> model

  val compose2 : ('a -> 'b -> model) -> ('a -> 'b -> model) -> 'a -> 'b -> model

  val ret : 'a -> 'a model_monad

  val throw : unit model_monad [@@warning "-unused-value-declaration"]

  val unreachable : 'a model_monad

  val report : Diagnostic.t -> unit model_monad

  val report_assert_error : unit model_monad

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

  val python_call : Procname.t -> (string * aval) list -> aval model_monad

  val apply_hack_closure : aval -> aval list -> aval model_monad

  val apply_python_closure :
    aval -> (ProcAttributes.t option -> aval list model_monad) -> aval model_monad

  val get_data : model_data model_monad

  val add_model_call : ValueHistory.t -> ValueHistory.t model_monad

  (** {2 Disjunctive reasoning} *)

  val disj : 'a model_monad list -> 'a model_monad

  val start_model : (unit -> unit model_monad) -> model
  (** get a model from a disjunctive model_monad *)

  val start_named_model : string -> (unit -> unit model_monad) -> model

  val lift_to_monad : model -> unit model_monad
  (** beware that the model may modify the [model_data.ret] field *)

  val lift_to_monad_and_get_result : model -> aval model_monad
  (** apply the model and return its result. fails if the model did not assign the reserved
      [model_data.ret] variable. *)

  (** {2 Operations}

      [PulseOperations] functions you need should be copied here *)

  val allocation : Attribute.allocator -> aval -> unit model_monad

  val is_allocated : aval -> bool model_monad

  val data_dependency : ValueOrigin.t -> ValueOrigin.t list -> unit model_monad

  val data_dependency_to_ret : ValueOrigin.t list -> unit model_monad

  val add_dict_read_const_key : aval -> Fieldname.t -> unit model_monad

  val is_dict_contain_const_keys : aval -> bool model_monad

  val remove_dict_contain_const_keys : aval -> unit model_monad

  val is_hack_constinit_called : aval -> bool model_monad

  val set_hack_constinit_called : aval -> unit model_monad

  val add_static_type : Typ.name -> aval -> unit model_monad

  val get_static_type : aval -> Typ.name option model_monad

  val deep_copy : ?depth_max:int -> aval -> aval model_monad

  val check_valid :
    ?must_be_valid_reason:Invalidation.must_be_valid_reason -> ValueOrigin.t -> unit model_monad

  val binop : Binop.t -> aval -> aval -> aval model_monad

  val binop_int : Binop.t -> aval -> IntLit.t -> aval model_monad

  val unop : Unop.t -> aval -> aval model_monad

  val read : Exp.t -> aval model_monad [@@warning "-unused-value-declaration"]

  val remove_allocation_attr_transitively : aval list -> unit model_monad

  val int : ?hist:ValueHistory.t -> int -> aval model_monad

  val string : string -> aval model_monad

  val string_concat : aval -> aval -> aval model_monad

  val access : access_mode -> aval -> Access.t -> aval model_monad

  val load_access : ?no_access:bool -> ?deref:bool -> aval -> Access.t -> aval model_monad

  val load : aval -> aval model_monad
  (** read the Dereference access from the value *)

  val and_dynamic_type_is : aval -> Typ.t -> unit model_monad

  val get_dynamic_type :
    ask_specialization:bool -> aval -> Formula.dynamic_type_data option model_monad

  val new_ : Exp.t -> aval model_monad

  val constructor :
       ?deref:bool
    -> ?field_of_string:(string -> Fieldname.t)
    -> Typ.Name.t
    -> (string * aval) list
    -> aval model_monad
  (** [constructor_dsl typ_name fields] builds a fresh object of type [typ_name] and initializes its
      fields using list [fields] *)

  val construct_dict :
       ?deref:bool
    -> ?field_of_string:(string -> Fieldname.t)
    -> Typ.name
    -> (string * (AbstractValue.t * ValueHistory.t)) list
    -> const_strings_only:bool
    -> aval model_monad

  val remove_hack_builder_attributes : aval -> unit model_monad
  [@@warning "-unused-value-declaration"]

  val fresh : ?more:string -> unit -> aval model_monad

  val fresh_nonneg : ?more:string -> unit -> aval model_monad

  val write_field : ref:aval -> Fieldname.t -> aval -> unit model_monad

  val store_field : ?deref:bool -> ref:aval -> Fieldname.t -> aval -> unit model_monad

  val store : ref:aval -> aval -> unit model_monad

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

  val null : aval model_monad

  (** {2 Tenv operations} *)

  val tenv_resolve_field_info : Typ.name -> Fieldname.t -> Struct.field_info option model_monad

  val tenv_resolve_fieldname :
    Typ.name -> string -> (Fieldname.t option * Tenv.unresolved_reason option) model_monad

  val tenv_type_is_defined : Typ.name -> bool model_monad
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
    val return_alloc_not_null :
      Attribute.allocator -> Exp.t option -> initialize:bool -> unit model_monad

    val free : Invalidation.t -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t -> unit model_monad

    val early_exit : model
  end
end

val unsafe_to_astate_transformer :
  unsat_info -> 'a model_monad -> CallEvent.t * model_data -> astate -> ('a * astate) sat_unsat_t
(** warning: the transformation will fail if the result of the computation is not a single abstract
    state with no error and it ignores the non-disjunctive state. You should think twice before
    using it... *)
