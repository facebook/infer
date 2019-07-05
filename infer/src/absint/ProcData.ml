(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a t = {summary: Summary.t; tenv: Tenv.t; extras: 'a}

type no_extras = unit

let empty_extras = ()

let make summary tenv extras = {summary; tenv; extras}

let make_default summary tenv = make summary tenv empty_extras
