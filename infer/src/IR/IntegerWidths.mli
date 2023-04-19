(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Types *)

open! IStd

type t = {char_width: int; short_width: int; int_width: int; long_width: int; longlong_width: int}
[@@deriving compare, equal]

val java : t

val load : SourceFile.t -> t option

module SQLite : sig
  val serialize : t option -> Sqlite3.Data.t
end

val width_of_ikind : t -> Typ.ikind -> int

val range_of_ikind : t -> Typ.ikind -> Z.t * Z.t
