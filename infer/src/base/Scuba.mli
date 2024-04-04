(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Low-level Scuba logging functionality. Provides functionality to log anything to any scuba
    table. (Note that Scuba is a schema-free storage, so it won't require any changes). Don't use
    this module directly for logging to tables with known structure. Use high-level functions that
    are aware of the table structure. *)

(** A scuba table *)
type table = InferEvents

(** A sample to be added to Scuba *)
type sample

val new_sample : time:int option -> sample
(** Create an empty sample with given creation timestamp. If time is not specified, corresponds to
    current timestamp. *)

val add_int : name:string -> value:int -> sample -> sample
(** Set a new integer field and its value to the sample. Overwrites if a field with this name was
    already set. *)

val add_normal : name:string -> value:string -> sample -> sample
(** Set a new string (normal in Scuba terminology) field and its value to the sample. Overwrites if
    a field with this name was already set. *)

val add_tagset : name:string -> value:string list -> sample -> sample
(** Set a new set of strings (tagset in Scuba terminology) field and its value to the sample.
    Overwrites if a field with this name was already set. *)

val log : table -> sample list -> unit
(** The main function. Log a collection of samples to the given table. *)

val sample_to_json : sample -> Yojson.t
