(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open Sawja_pack

val create_exception_handlers :
  JContext.t -> Procdesc.Node.t list -> (int -> Procdesc.Node.t list) -> JBir.t -> int
  -> Procdesc.Node.t list
