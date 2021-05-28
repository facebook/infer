(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_model : Procname.t -> bool

val get_dispatch_closure_opt : (Exp.t * Typ.t) list -> (Procname.t * (Exp.t * Typ.t) list) option
