(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include SinkTrace.S with type Sink.Kind.t = Pvar.t

module GlobalVar : PrettyPrintable.PrintableOrderedType with type t = Pvar.t

module GlobalVarSet : PrettyPrintable.PPSet with type elt = Pvar.t

val make_access : Pvar.t -> Location.t -> Sink.t

val trace_of_error : Location.t -> string -> sink_path -> Errlog.loc_trace_elem list
