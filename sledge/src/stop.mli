(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Stop analysis when encountering issues *)

val on_unknown_call : 'a -> unit
val on_invalid_access : 'a -> unit
