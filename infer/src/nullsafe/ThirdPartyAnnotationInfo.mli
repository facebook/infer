(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** In-memory storage the information about nullability annotation of third-party methods. *)

type signature_info =
  { filename: string  (** File where the particular signature is stored *)
  ; line_number: int  (** Line number with this signature *)
  ; nullability: ThirdPartyMethod.nullability }

type storage

val create_storage : unit -> storage

type file_parsing_error =
  {line_number: int; unparsable_method: string; parsing_error: ThirdPartyMethod.parsing_error}

val pp_parsing_error : Format.formatter -> file_parsing_error -> unit

val add_from_signature_file :
  storage -> filename:string -> lines:string list -> (storage, file_parsing_error) result
(** Parse the information from the signature file, and add it to the storage *)

val find_nullability_info : storage -> ThirdPartyMethod.unique_repr -> signature_info option
(** The main method. Do we have an information about the third-party method? If we do not, or it is
    not a third-party method, returns None. Otherwise returns the nullability information. *)

val lookup_related_sig_file : storage -> package:string -> string option
(** If the package is third-party, return the relevant .sig file to add signatures for this package. *)

val lookup_related_sig_file_by_package : storage -> Procname.t -> string option
(** If the function is third-party (based on its package), return relevant .sig file to add the
    signature NOTE: this DOES NOT look if the function is is already *)
