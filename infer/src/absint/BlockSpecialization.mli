(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type arg = Block of (Procname.t * CapturedVar.t list) | Var

val create_specialized_procdesc : Procname.t -> arg list -> Procname.t option
