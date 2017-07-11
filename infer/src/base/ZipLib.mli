(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val load : 'a Serialization.serializer -> string -> 'a option
(** [load serializer path] searches for the file at the given path in the zip libraries.
    If Config.infer_cache is set, already deserialized data will be saved there and [path]
    will be searched from the cache first. *)
