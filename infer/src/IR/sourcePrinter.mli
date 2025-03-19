(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val add_info_before : sourcefile:SourceFile.t -> line:int -> info:string -> unit

val add_info_after : sourcefile:SourceFile.t -> line:int -> info:string -> unit

val write_all : unit -> unit
