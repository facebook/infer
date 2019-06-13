(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module ClassLoad : ExplicitTrace.Element with type t = string

module Event : ExplicitTrace.TraceElem with type elem_t = ClassLoad.t

include AbstractDomain.WithBottom

type summary = t

val pp_summary : F.formatter -> summary -> unit

val mem_typename : Typ.Name.t -> t -> bool

val add_typename : Location.t -> t -> Typ.Name.t -> t

val integrate_summary : Typ.Procname.t -> Location.t -> t -> summary -> t

val iter : (Event.t -> unit) -> t -> unit
