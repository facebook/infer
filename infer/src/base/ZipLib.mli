(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val load : 'a Serialization.serializer -> string -> 'a option
(** [load serializer path] searches for the file at the given path in the zip libraries.
    If Config.infer_cache is set, already deserialized data will be saved there and [path]
    will be searched from the cache first. *)
