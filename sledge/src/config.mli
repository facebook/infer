(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Configuration options *)

val run : (input:string -> output:string option -> 'a) -> 'a
(** [run main] parses command line options, performs some imperative
    initialization, and then executes [main] passing the configuration
    options. *)
