(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Preanalysis for eliminating dead local variables *)

(** Perform liveness analysis *)
val doit : ?f_translate_typ:(Tenv.t -> string -> unit) option -> Cfg.cfg -> Cg.t -> Tenv.t
  -> unit
