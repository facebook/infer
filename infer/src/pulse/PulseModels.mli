(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type exec_fun =
     Location.t
  -> ret:Ident.t * Typ.t
  -> actuals:(PulseDomain.AddrTracePair.t * Typ.t) list
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t list PulseOperations.access_result

type model = exec_fun

val dispatch : Typ.Procname.t -> CallFlags.t -> model option
