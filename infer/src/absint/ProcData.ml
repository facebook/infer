(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a t = {pdesc: Procdesc.t; tenv: Tenv.t; extras: 'a}

type no_extras = unit

let empty_extras = ()

let make pdesc tenv extras = {pdesc; tenv; extras}

let make_default pdesc tenv = make pdesc tenv empty_extras
