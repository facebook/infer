(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type reversed

type not_reversed

type 'r t

type not_reversed_t = not_reversed t

val empty : not_reversed_t

val singleton : Sil.instr -> not_reversed_t

val append_list : not_reversed_t -> Sil.instr list -> not_reversed_t

val of_list : Sil.instr list -> not_reversed_t

val of_rev_list : Sil.instr list -> not_reversed_t

val filter_map : not_reversed_t -> f:(Sil.instr -> Sil.instr option) -> not_reversed_t

val map_changed :
     equal:(Sil.instr -> Sil.instr -> bool)
  -> not_reversed_t
  -> f:(Sil.instr -> Sil.instr)
  -> not_reversed_t

val reverse_order : not_reversed_t -> reversed t

val is_empty : _ t -> bool

val count : _ t -> int

val exists : _ t -> f:(Sil.instr -> bool) -> bool

val for_all : _ t -> f:(Sil.instr -> bool) -> bool

val nth_exists : _ t -> int -> bool

val nth_exn : _ t -> int -> Sil.instr

val last : _ t -> Sil.instr option

val find_map : _ t -> f:(Sil.instr -> 'a option) -> 'a option

val pp : Pp.env -> Format.formatter -> _ t -> unit

val fold : (_ t, Sil.instr, 'a) Container.fold

val iter : (_ t, Sil.instr) Container.iter
