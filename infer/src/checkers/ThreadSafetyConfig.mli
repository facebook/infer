(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

(** List of annotations that should be considered aliases of @ThreadSafe *)
module AnnotationAliases : sig

  val of_json : Yojson.Basic.json -> string list
end
