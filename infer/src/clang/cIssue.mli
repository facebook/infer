(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type mode = On | Off

type issue_desc = {
  name : string; (* issue name *)
  severity : Exceptions.err_kind;
  mode : mode;
  description : string; (* Description in the error message *)
  suggestion : string option; (* an optional suggestion or correction *)
  loc : Location.t; (* location in the code *)
}

val string_of_mode : mode -> string

val pp_issue : Format.formatter -> issue_desc -> unit

val should_run_check : mode -> bool
