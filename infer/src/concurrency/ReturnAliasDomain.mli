(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** The access expression, rooted at a formal or a global, whose value a procedure returns, e.g.
    [this->p] for [T* get() { return p; }] and [&(this->x)] for [T& get() { return x; }]. Bottom
    when the procedure has not returned yet or only returns null, and top when it returns anything
    else. *)

include AbstractDomain.WithBottom

include AbstractDomain.WithTop with type t := t

val is_supported_procname : Procname.t -> bool
(** return aliases are only computed for C functions and C++ methods *)

val assign : FormalMap.t -> lhs:HilExp.AccessExpression.t -> rhs:HilExp.t -> t -> t
(** update the state for the assignment [lhs := rhs] if [lhs] is the return variable *)

val to_summary : Procdesc.t -> t -> HilExp.AccessExpression.t option
(** the alias to record in the summary of the procedure, if any; only pointers and references are
    recorded since only those can lead to further accesses or locks, and the accessors of
    [std::unique_ptr] and [std::shared_ptr] return [*this], the smart pointer itself *)

val subst :
     callee:Procname.t
  -> HilExp.t list
  -> HilExp.AccessExpression.t
  -> HilExp.AccessExpression.t option
(** [subst ~callee actuals alias] expresses [alias], taken from the summary of [callee], over the
    actuals of a call to [callee], if possible *)
