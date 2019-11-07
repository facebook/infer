(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type load_error

val pp_load_error : Format.formatter -> load_error -> unit

val load : path_to_repo_dir:string -> (ThirdPartyAnnotationInfo.storage, load_error) result
(** Given a path to a repo with 3rd annotation info, loads it from a disk to
    in-memory representation.
    After this is done, information can be requested via [ThirdPartyAnnotationInfo].
 *)
