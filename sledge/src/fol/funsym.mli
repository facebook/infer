(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Function Symbols *)

(** [Uninterp] functions symbols are treated as uninterpreted, while the
    others support some ad hoc simplification. For example, function symbols
    applied to literal values of the expected sorts are reduced to literal
    values. *)
type t =
  | Rem
      (** Remainder of division, satisfies [a = b * div a b + rem a b] and
          for integers [rem a b] has same sign as [a], and [|rem a b| < |b|] *)
  | BitAnd  (** Bitwise logical And *)
  | BitOr  (** Bitwise logical inclusive Or *)
  | BitXor  (** Bitwise logical eXclusive or *)
  | BitShl  (** Bitwise Shift left *)
  | BitLshr  (** Bitwise Logical shift right *)
  | BitAshr  (** Bitwise Arithmetic shift right *)
  | Signed of int
      (** [Signed n] interprets its argument as an [n]-bit signed integer.
          That is, it two's-complement--decodes the low [n] bits of the
          infinite two's-complement encoding of the argument. *)
  | Unsigned of int
      (** [Unsigned n] interprets its argument as an [n]-bit unsigned
          integer. That is, it unsigned-binary--decodes the low [n] bits of
          the infinite two's-complement encoding of the argument. *)
  | Uninterp of string  (** Uninterpreted function symbol *)
[@@deriving compare, equal, sexp]

val pp : t pp
val uninterp : string -> t

val eval :
     equal:('a -> 'a -> bool)
  -> get_z:('a -> Z.t option)
  -> ret_z:(Z.t -> 'a)
  -> get_q:('a -> Q.t option)
  -> ret_q:(Q.t -> 'a)
  -> t
  -> 'a array
  -> 'a option
(** [eval ~equal ~get_z ~ret_z ~get_q ~ret_q f xs] evaluates the application
    [f xs] using the provided embeddings of arguments into values. *)
