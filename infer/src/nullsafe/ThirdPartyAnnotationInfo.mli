(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** In-memory storage the information about nullability annotation of third-party methods. *)

type storage

val create_storage : unit -> storage

type file_parsing_error =
  {line_number: int; unparsable_method: string; parsing_error: ThirdPartyMethod.parsing_error}

val pp_parsing_error : Format.formatter -> file_parsing_error -> unit

val add_from_signature_file : storage -> lines:string list -> (unit, file_parsing_error) result
(** Parse the information from the signature file, and add it to the storage *)

val find_nullability_info :
  storage -> ThirdPartyMethod.unique_repr -> ThirdPartyMethod.nullability option
(** The main method. Do we have an information about the third-party method?
    If we do not, or it is not a third-party method, returns None.
    Otherwise returns the nullability information.
  *)
