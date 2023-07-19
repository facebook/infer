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

type 'a monad

type astate = AbductiveDomain.t

type aval = AbstractValue.t * ValueHistory.t

module Syntax : sig
  (* polymorphic operations *)

  val ( let* ) : 'a monad -> ('a -> 'b monad) -> 'b monad

  val ret : 'a -> 'a monad

  val list_iter : 'a list -> f:('a -> unit monad) -> unit monad

  val start_model : unit monad -> PulseModelsImport.model
  (** get a model from a monad *)

  val return_value : aval -> unit monad
  (** you can end a model with [ret ()] or the following function. The latter will properly assign
      [data.ret] *)

  (*****************************************************************)
  (* each PulseOperations functions you need should be copied here *)

  val add_dynamic_type : Typ.t -> aval -> unit monad

  val and_eq_int : aval -> IntLit.t -> unit monad

  val eval_deref_access : access_mode -> aval -> Access.t -> aval monad

  val mk_fresh : model_desc:string -> aval monad

  val write_deref_field : ref:aval -> obj:aval -> Fieldname.t -> unit monad

  (* if necessary you can convert an operation outside of this module with the following operator *)
  val exec_command : (astate -> astate) -> unit monad
end
