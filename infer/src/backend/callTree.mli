(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** data-structure to represent call stacks in a compact tree form *)
type t =
  | Direct of Procname.t
  | Indirect of Procname.t * t list

(** print the list of call stacks in the tree *)
val pp : Format.formatter -> t -> unit
