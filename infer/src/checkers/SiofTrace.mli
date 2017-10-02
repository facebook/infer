(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

include SinkTrace.S with type Sink.Kind.t = Pvar.t

module GlobalVar : PrettyPrintable.PrintableOrderedType with type t = Pvar.t

module GlobalVarSet : PrettyPrintable.PPSet with type elt = Pvar.t

val make_access : Pvar.t -> Location.t -> Sink.t

val is_intraprocedural_access : Sink.t -> bool

val trace_of_error : Location.t -> string -> sink_path -> Errlog.loc_trace_elem list
