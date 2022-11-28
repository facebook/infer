(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Typ.t

val boolean : t

val char : t [@@warning "-unused-value-declaration"]

val double : t

val float : t

val int : t

val long : t

val uint : t

val void : t

val void_star : t
(** [void*] type *)

module Name : sig
  type t = Typ.Name.t

  module Java : sig
    val java_io_serializable : t

    val java_lang_class : t

    val java_lang_cloneable : t

    val java_lang_object : t

    val java_lang_string : t

    val kotlin_coroutines_jvm_internal_restrictedsuspendlambda : t
  end

  module CSharp : sig
    val system_string : t

    val system_object : t
  end

  module Objc : sig
    val ns_enumerator : t

    val ns_object : t
  end
end

module Java : sig
  val byte : t

  val char : t

  val short : t

  val pointer_to_java_lang_class : t

  val pointer_to_java_lang_object : t

  val pointer_to_java_lang_string : t
end

module Objc : sig
  val pointer_to_nszone : t
end
