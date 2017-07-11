(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type 'a t = {pdesc: Procdesc.t; tenv: Tenv.t; extras: 'a}

type no_extras

val empty_extras : no_extras

val make : Procdesc.t -> Tenv.t -> 'a -> 'a t

val make_empty_extras : Procdesc.t -> no_extras

val make_default : Procdesc.t -> Tenv.t -> no_extras t
