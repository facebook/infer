(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* Type of abstract semantic function of execution *)
type exec_fun =
     BufferOverrunUtils.ModelEnv.model_env
  -> ret:Ident.t * Typ.t
  -> BufferOverrunDomain.Mem.t
  -> BufferOverrunDomain.Mem.t

(* Type of checking function *)
type check_fun =
     BufferOverrunUtils.ModelEnv.model_env
  -> BufferOverrunDomain.Mem.t
  -> BufferOverrunProofObligations.ConditionSet.checked_t
  -> BufferOverrunProofObligations.ConditionSet.checked_t

type model = {exec: exec_fun; check: check_fun}

module Collection : sig
  val create_collection :
       BufferOverrunUtils.ModelEnv.model_env
    -> ret:Ident.t * Typ.t
    -> BufferOverrunDomain.Mem.t
    -> length:Itv.t
    -> BufferOverrunDomain.Mem.t
  (** Create a collection value with the [length] and assign it to [ret] *)

  val eval_collection_length : Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
  (** Evaluate length of Java collection *)
end

module Container : sig
  val eval_collection_length : Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
  (** Evaluate length of C++ container *)
end

module NSCollection : sig
  val eval_collection_length : Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
  (** Evaluate length of ObjC collection *)
end

module NSString : sig
  val get_length :
       BufferOverrunUtils.ModelEnv.model_env
    -> Exp.t
    -> BufferOverrunDomain.Mem.t
    -> BufferOverrunDomain.Val.t
  (** Get length of NSString string *)
end

module JavaString : sig
  val get_length :
       BufferOverrunUtils.ModelEnv.model_env
    -> Exp.t
    -> BufferOverrunDomain.Mem.t
    -> BufferOverrunDomain.Val.t
  (** Get length of Java string *)

  val constructor_from_char_ptr :
       BufferOverrunUtils.ModelEnv.model_env
    -> AbsLoc.PowLoc.t
    -> Exp.t
    -> BufferOverrunDomain.Mem.t
    -> BufferOverrunDomain.Mem.t
  (** Construct Java string from constant string *)
end

module Call : sig
  val dispatch : (Tenv.t, model, unit) ProcnameDispatcher.Call.dispatcher
end
