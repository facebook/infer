(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** ****DO NOT USE DIRECTLY****

    This module is automatically [open]'d by the build system when compiling infer without Java
    support. The stubs implemented here do nothing. *)

module JMain : sig
  val from_arguments : sources:string list -> string -> unit

  val from_verbose_out : string -> unit
end

module JSourceFileInfo : sig
  val debug_on_file : string -> unit
end

module JSourceLocations : sig
  val debug_on_file : string -> unit
end
