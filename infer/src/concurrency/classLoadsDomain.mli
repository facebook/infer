(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module ClassLoad = String

val get_java_class : Typ.Procname.t -> string option

module Event : ExplicitTrace.TraceElem with type elem_t = ClassLoad.t

include AbstractDomain.WithBottom

include PrettyPrintable.PPSet with type t = astate and type elt = Event.t

type summary = astate

val pp_summary : F.formatter -> summary -> unit

val integrate_summary : Typ.Procname.t -> Location.t -> astate -> summary -> astate
