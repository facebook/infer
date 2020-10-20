(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open NS0

include module type of struct
  include Q
end

val of_z : Z.t -> t
val compare : t -> t -> int
val hash : t -> int
val hash_fold_t : t Hash.folder
val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t
val pp : t pp
val pow : t -> int -> t
