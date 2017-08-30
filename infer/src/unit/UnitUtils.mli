(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val assert_raises : ?msg:string -> exn -> (unit -> 'a) -> unit
(** OUnit2.assert_raises checks that a function raises some exception that's exactly the same as a
    reference exception, but some of our internal exceptions contain verbose and flaky data, eg
    backtraces. This will normalize such known exceptions by erasing their verbose data. Use this if
    you're suffering from OUnit2.assert_raises. *)
