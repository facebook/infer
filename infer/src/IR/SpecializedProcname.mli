(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = {procname: Procname.t; specialization: Specialization.t option}
[@@deriving equal, compare, sexp]

module Set : PrettyPrintable.SexpPPSet with type elt = t

module Map : PrettyPrintable.PPMap with type key = t
