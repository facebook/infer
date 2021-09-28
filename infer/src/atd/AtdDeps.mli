(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** functions necessary to include in atd generated files to enable ppx_compare *)

val equal_string : string -> string -> bool

val equal_list : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

val equal_option : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

val equal_int : int -> int -> bool

val equal_bool : bool -> bool -> bool
