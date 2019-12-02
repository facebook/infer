(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A singleton. Should be initialized once prior to start of nullsafe. *)

val initialize : absolute_path_to_repo:string option -> ThirdPartyAnnotationInfo.storage -> unit
(** Should be initialized exactly once before access. Path to repo is None if there is no repo on
    the disk. *)

val get_repo : unit -> ThirdPartyAnnotationInfo.storage
(** Can be accessed only when initialization was done *)

val get_user_friendly_third_party_sig_file_name : filename:string -> string
(** Given filename containing the repo signatures, render it in a way convenient for the user *)
