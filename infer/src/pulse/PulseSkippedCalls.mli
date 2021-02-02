(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include AbstractDomain.MapS with type key = Procname.t and type value = PulseTrace.t

val compare : t -> t -> int

val equal : t -> t -> bool

val yojson_of_t : t -> Yojson.Safe.t
