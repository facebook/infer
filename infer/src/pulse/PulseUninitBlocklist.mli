(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_blocklisted_struct : Typ.name -> bool
(** Check if a struct name is in the blocklist for uninit checker. *)
