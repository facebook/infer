(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** returns true if [callee_pname] returns a tainted value *)
val returns_tainted : Procname.t -> ProcAttributes.t option -> bool

(** returns list of zero-indexed argument numbers of [callee_pname] that may be tainted *)
val accepts_sensitive_params : Procname.t -> ProcAttributes.t option -> int list

(** returns list of zero-indexed parameter numbers of [callee_pname] that should be
    considered tainted during symbolic execution *)
val tainted_params : Procname.t -> int list
