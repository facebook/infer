(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_strict_mode_violation : Tenv.t -> Typ.Procname.t -> HilExp.t list -> bool
(** is the method a potential strict mode violation, given the actuals passed? *)
