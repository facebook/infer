(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Reachable of Procname.t | Extends of Typ.Name.t * Typ.Name.t

val to_string : t -> string
