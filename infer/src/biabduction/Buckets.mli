(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Classify bugs into buckets *)

val classify_access :
     Localise.error_desc
  -> Localise.access option
  -> DecompiledExp.t option
  -> bool
  -> Localise.error_desc
(** Classify the bucket of an error desc using Location.access and nullable information *)
