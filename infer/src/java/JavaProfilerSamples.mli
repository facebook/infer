(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module JNI : sig
  module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY : sig
    type t =
      | Boolean
      | Byte
      | Char
      | Short
      | Int
      | Long
      | Float
      | Double
      | Void
      (* FullyQualifiedClass is split between (package, class) *)
      | FullyQualifiedClass of (string * string)
      | Array of t
      | Method of (t list * t)
    [@@deriving compare]

    val equal : t -> t -> bool

    val parse_str : string -> t list

    val parse_method_str : string -> t list * t

    val to_java_type : t -> Typ.Procname.Java.java_type

    val pp : Format.formatter -> t -> unit
  end
end

val from_json_string : string -> Typ.Procname.t list
