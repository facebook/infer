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

type no_extras = unit

let empty_extras = ()

let make_empty_extras _ = ()

let make pdesc tenv extras = {pdesc; tenv; extras}

let make_default pdesc tenv = make pdesc tenv empty_extras
