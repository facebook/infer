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
  (* polymorphic operations *)

  val ( let* ) : 'a model_monad -> ('a -> 'b model_monad) -> 'b model_monad

  val ret : 'a -> 'a model_monad

  val unreachable : 'a model_monad

  val list_fold :
    'a list -> init:'accum -> f:('accum -> 'a -> 'accum model_monad) -> 'accum model_monad

  val list_iter : 'a list -> f:('a -> unit model_monad) -> unit model_monad

  val list_filter_map : 'a list -> f:('a -> 'b option model_monad) -> 'b list model_monad

  val option_iter : 'a option -> f:('a -> unit model_monad) -> unit model_monad

  val assign_ret : aval -> unit model_monad
  (** assign the value to the return variable of the current function *)

  val dynamic_dispatch :
    cases:(Typ.name * 'a model_monad) list -> ?default:'a model_monad -> aval -> 'a model_monad

  val get_data : PulseModelsImport.model_data model_monad

  (* disjunctive reasonning *)

  val disjuncts : 'a model_monad list -> 'a model_monad

  val start_model : unit model_monad -> PulseModelsImport.model
  (** get a model from a disjunctive model_monad *)

  val lift_to_monad : PulseModelsImport.model -> unit model_monad
  (** beware that the model may modify the [PulseModelsImport.model_data.ret] field *)

  (* TODO (dpichardie) add an other function that lift the monad and return the
     value that is associated with [ret] *)

  (*****************************************************************)
  (* each PulseOperations functions you need should be copied here *)
  val allocation : Attribute.allocator -> aval -> unit model_monad

  val add_dynamic_type : Typ.t -> aval -> unit model_monad

  val add_static_type : Typ.name -> aval -> unit model_monad

  val get_dynamic_type : ask_specialization:bool -> aval -> Typ.t option model_monad

  val get_const_string : aval -> string option model_monad

  val tenv_resolve_fieldname : Typ.name -> Fieldname.t -> Struct.field_info option model_monad

  val and_eq_int : aval -> IntLit.t -> unit model_monad

  val eval_deref_access : access_mode -> aval -> Access.t -> aval model_monad

  val mk_fresh : model_desc:string -> aval model_monad

  val deep_copy : ?depth_max:int -> aval -> aval model_monad

  val aval_operand : aval -> PulseArithmetic.operand

  val prune_binop :
       negated:bool
    -> Binop.t
    -> PulseArithmetic.operand
    -> PulseArithmetic.operand
    -> unit model_monad

  val write_deref_field : ref:aval -> obj:aval -> Fieldname.t -> unit model_monad

  (* if necessary you can convert an operation outside of this module with the following operators *)
  val exec_command : (astate -> astate) -> unit model_monad

  val exec_operation : (astate -> 'a * astate) -> 'a model_monad
end
