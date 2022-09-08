(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of Stdlib.Result

val pp :
     ('ok_pp -> 'ok -> unit, unit) fmt
  -> 'ok_pp
  -> ('error_pp -> 'error -> unit, unit) fmt
  -> 'error_pp
  -> ('ok, 'error) t pp
