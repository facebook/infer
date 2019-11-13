(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to represent nullability of expressions inferred during
   flow-sensitive symbolic execution.
   NOTE: This is complementaty to {!InferredNullability.t}.
         {!InferredNullability} contains info about _formal_ nullability
         (what does the code say about nullability of a given type, according to
         explicit annotations and implicit agreements (e.g. models)).
         In contrast, InferredNullability represents what Nullsafe thinks about such and such
         expression according to its type inference rules.
   *)

type t [@@deriving compare]

val get_nullability : t -> Nullability.t

val create : TypeOrigin.t -> t

val is_nonnull_or_declared_nonnull : t -> bool

val is_nonnull : t -> bool

val descr_origin : t -> TypeErr.origin_descr
(** Human-readable description of the origin of a value.
  (How did nullsafe infer the nullability )
 *)

val get_origin : t -> TypeOrigin.t
(** The simple explanation of how was nullability inferred.  *)

val join : t -> t -> t
(** This is what happens with nullability when we join two flows in CFG,
    e.g.
    {[
      if(something) {
        a = e1;
      } else {
        a = e2;
      }
      // what is nullability of `a` at this point?
    ]}
  *)

val origin_is_fun_library : t -> bool

val to_string : t -> string
