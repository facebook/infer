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
  ; signature: ThirdPartyMethod.t }

(** The minimum information that is needed to _uniquely_ identify the method. That why we don't

    - include e.g. return type, access quilifiers, or whether the method is static (because Java
    - overload resolution rules ignore these things). In contrast, parameter types are essential,
    - because Java allows several methods with different types. *)
type unique_repr =
  { class_name: ThirdPartyMethod.fully_qualified_type
  ; method_name: ThirdPartyMethod.method_name
  ; param_types: ThirdPartyMethod.fully_qualified_type list }

val pp_unique_repr : Format.formatter -> unique_repr -> unit

val unique_repr_of_java_proc_name : Procname.Java.t -> unique_repr

type storage

val create_storage : unit -> storage

type file_parsing_error =
  {line_number: int; unparsable_method: string; parsing_error: ThirdPartyMethod.parsing_error}

val pp_parsing_error : Format.formatter -> file_parsing_error -> unit

val add_from_signature_file :
  storage -> filename:string -> lines:string list -> (storage, file_parsing_error) result
(** Parse the information from the signature file, and add it to the storage *)

val find_nullability_info : storage -> unique_repr -> signature_info option
(** The main method. Do we have an information about the third-party method? If we do not, or it is
    not a third-party method, returns None. Otherwise returns the nullability information. *)

val lookup_related_sig_file : storage -> package:string -> string option
(** If the package is third-party, return the relevant .sig file to add signatures for this package. *)

val lookup_related_sig_file_for_proc : storage -> Procname.Java.t -> string option
(** If the function is third-party (based on its package), return relevant .sig file *)

val is_third_party_proc : storage -> Procname.Java.t -> bool
(** Checks whether a required procname comes from third-party code based on available .sig files and
    config flags. NOTE: considering config flags is done for compatibility with the legacy behaviour
    and will be removed in the future *)

val is_third_party_typ : storage -> Typ.t -> bool
(** See [is_third_party_proc]. *)

val is_third_party_class_name : storage -> JavaClassName.t -> bool
(** See [is_third_party_proc]. *)
