(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A stats sample *)
type sample

val new_sample : time:int option -> sample
(** Create an empty sample with given creation timestamp. If time is not specified, corresponds to
    current timestamp. *)

val add_int : name:string -> value:int -> sample -> sample
(** Set a new integer field and its value to the sample. Overwrites if a field with this name was
    already set. *)

val add_normal : name:string -> value:string -> sample -> sample
(** Set a new string field and its value to the sample. Overwrites if a field with this name was
    already set. *)

val sample_to_json : sample -> Yojson.t
