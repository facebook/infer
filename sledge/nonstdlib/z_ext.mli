(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open NS0

include module type of struct
  include Z
end

val compare : t -> t -> int
val hash : t -> int
val hash_fold_t : t Hash.folder
val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t
val pp : t pp
val true_ : t
val false_ : t
val of_bool : bool -> t
val is_true : t -> bool
val is_false : t -> bool
