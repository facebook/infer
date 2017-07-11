(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module GlobalsAccesses : PrettyPrintable.PPSet with type elt = Pvar.t * Location.t

include SinkTrace.S with type Sink.Kind.t = GlobalsAccesses.t

val make_access : GlobalsAccesses.t -> Location.t -> Sink.t

val is_intraprocedural_access : Sink.t -> bool

val trace_of_error : Location.t -> string -> sink_path -> Errlog.loc_trace_elem list
