(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Reimplement C stubs in a simplified way to make it easier to build an Infer toplevel. This is
    because we don't care about the exact implementation of the C stubs as far as the toplevel
    goes. *)


let term_width () = 80
