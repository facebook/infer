(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Manipulate possibly-reversed lists of instructions efficiently *)

type reversed

type not_reversed

type _ t

(** defined for convenience: we can write [Instrs.not_reversed_t] in other modules instead of
    [Instrs.not_reversed Instrs.t] *)
type not_reversed_t = not_reversed t

val empty : _ t

val singleton : Sil.instr -> _ t

val append_list : not_reversed t -> Sil.instr list -> not_reversed t

val prepend_list : not_reversed t -> Sil.instr list -> not_reversed t

val of_list : Sil.instr list -> not_reversed t

val filter_map : not_reversed t -> f:(Sil.instr -> Sil.instr option) -> not_reversed t

val map : not_reversed t -> f:(Sil.instr -> Sil.instr) -> not_reversed t
(** replace every instruction [instr] with [f instr]. Preserve physical equality. **)

val map_and_fold :
  not_reversed t -> f:('a -> Sil.instr -> 'a * Sil.instr) -> init:'a -> not_reversed t
(** replace every instruction [instr] with [snd (f context instr)]. The context is computed by
    folding [f] on [init] and previous instructions (before [instr]) in the collection. Preserve
    physical equality. **)

val concat_map_and_fold :
  not_reversed t -> f:('a -> Sil.instr -> 'a * Sil.instr array) -> init:'a -> not_reversed t
(** Like [map_and_fold] but replace every instruction [instr] with the list [snd (f context instr)]
    by threading an accumulator. Preserve physical equality. **)

val concat_map : not_reversed t -> f:(Sil.instr -> Sil.instr array) -> not_reversed t
[@@warning "-unused-value-declaration"]
(** replace every instruction [instr] with the list [f instr]. Preserve physical equality. **)

val reverse_order : not_reversed t -> reversed t

val is_empty : _ t -> bool

val count : _ t -> int

val exists : _ t -> f:(Sil.instr -> bool) -> bool

val for_all : _ t -> f:(Sil.instr -> bool) -> bool

val nth_exists : _ t -> int -> bool

val nth_exn : _ t -> int -> Sil.instr

val last : _ t -> Sil.instr option

val find_map : _ t -> f:(Sil.instr -> 'a option) -> 'a option

val pp : ?indent:bool -> ?print_types:bool -> Pp.env -> Format.formatter -> _ t -> unit

val fold : (_ t, Sil.instr, 'a) Container.fold

val foldi : _ t -> init:'a -> f:(int -> 'a -> Sil.instr -> 'a) -> 'a

val iter : (_ t, Sil.instr) Container.iter

val get_underlying_not_reversed : not_reversed t -> Sil.instr array

val instrs_get_normal_vars : not_reversed t -> Ident.t list
