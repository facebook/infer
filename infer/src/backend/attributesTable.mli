(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module to manage the table of attributes. *)


(** Save .attr file for the procedure into the attributes database. *)
val store_attributes : ProcAttributes.t -> unit

(** Load the attributes for the procedure from the attributes database. *)
val load_attributes : Procname.t -> ProcAttributes.t option
