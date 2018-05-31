(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module ProfilerSample : Caml.Set.S with type elt = Typ.Procname.t

module JNI : sig
  val void_method_with_no_arguments : string

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

type labeled_profiler_sample = string * ProfilerSample.t [@@deriving compare]

val equal_labeled_profiler_sample : labeled_profiler_sample -> labeled_profiler_sample -> bool

val from_json_string : string -> use_signature:bool -> labeled_profiler_sample list

val from_json_file : string -> use_signature:bool -> labeled_profiler_sample list

val create_procname : classname:string -> methodname:string -> signature:string -> Typ.Procname.t
