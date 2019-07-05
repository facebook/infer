(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a t = {summary: Summary.t; tenv: Tenv.t; extras: 'a}

type no_extras

val empty_extras : no_extras

val make : Summary.t -> Tenv.t -> 'a -> 'a t

val make_default : Summary.t -> Tenv.t -> no_extras t
