(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Procnames for the builtin functions supported *)

include BUILTINS.S with type t = Typ.Procname.t

val is_declared : Typ.Procname.t -> bool
