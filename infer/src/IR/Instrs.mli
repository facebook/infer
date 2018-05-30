(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t

val empty : t

val single : Sil.instr -> t

val append_list : t -> Sil.instr list -> t

val prepend_one : Sil.instr -> t -> t

val reverse_order : t -> t

val is_empty : t -> bool

val count : t -> int

val exists : t -> f:(Sil.instr -> bool) -> bool

val for_all : t -> f:(Sil.instr -> bool) -> bool

val nth_exists : t -> int -> bool

val nth_exn : t -> int -> Sil.instr

val last : t -> Sil.instr option

val find_map : t -> f:(Sil.instr -> 'a option) -> 'a option

val pp : Pp.env -> Format.formatter -> t -> unit

val filter_map : t -> f:(Sil.instr -> Sil.instr option) -> t

val map_changed : equal:(Sil.instr -> Sil.instr -> bool) -> t -> f:(Sil.instr -> Sil.instr) -> t

val fold : (t, Sil.instr, 'a) Container.fold

val iter : (t, Sil.instr) Container.iter

val of_list : Sil.instr list -> t

val of_rev_list : Sil.instr list -> t
