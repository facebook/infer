(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module PathContext = PulsePathContext

(** the trace represents the start of a cycle but isn't a cycle yet, it's a sequence of calls that
    end where ondemand returned no summary due to mutual recursion; these proto-cycles get bubbled
    the cyclic call stack until the cycle is closed again *)
type t = Trace.t

val mk : PathContext.t -> Location.t -> Procname.t -> t
(** a trace of length 1 *)

val get_inner_call : t -> Procname.t
(** the "initial", innermost call in the cycle *)

val pp : F.formatter -> t -> unit
