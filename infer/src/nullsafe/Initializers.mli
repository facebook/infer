(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functionality for evaluating typestates after class initialization. Nullsafe's notion of
    initialization augments Java's one in the following way:

    + A method is considered "initializer" if it is either marked as [@Initializer] or is in the
      list of known initializer methods (e.g. onCreate()).
    + For each constructor, Nullsafe assumes all initializer methods will be called after the
      constructor is called. (Nullsafe does not check if this assumption is indeed true).
    + Nullsafe uses assumption 2, in order to check if class fields were initialized or not. (a
      non-nullable field is considered not initialized if it is not initialized in at least one
      constructor). *)

val final_initializer_typestates_lazy :
     Tenv.t
  -> Procname.t
  -> Procdesc.t
  -> (bool -> Procname.t -> Procdesc.t -> 'a option -> 'b * 'c option)
  -> (Procname.t * 'c) list lazy_t
(** Typestates after the current constructor and all initializer procedures. *)

val final_constructor_typestates_lazy :
     Tenv.t
  -> Procname.t
  -> (bool -> Procname.t -> Procdesc.t -> 'a option -> 'b * 'c option)
  -> (Procname.t * 'c) list lazy_t
(** Typestates after all constructors. *)
