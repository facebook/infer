(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

include AbstractDomain.WithBottom

type summary = astate

val pp_summary : F.formatter -> summary -> unit

val integrate_summary : astate -> Typ.Procname.t -> Location.t -> summary -> astate
