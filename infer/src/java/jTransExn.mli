(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Sawja_pack

val create_exception_handlers :
     JContext.t
  -> Procdesc.Node.t list
  -> (int -> Procdesc.Node.t list)
  -> JBir.t
  -> int
  -> Procdesc.Node.t list
