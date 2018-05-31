(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a t = {pdesc: Procdesc.t; tenv: Tenv.t; extras: 'a}

type no_extras

val empty_extras : no_extras

val make : Procdesc.t -> Tenv.t -> 'a -> 'a t

val make_default : Procdesc.t -> Tenv.t -> no_extras t
