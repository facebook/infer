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

type issue_desc =
  { id: string
  ; (* issue id *)
  description: string
  ; (* Description in the error message *)
  doc_url: string option
  ; mode: mode
  ; name: string option
  ; (* issue name, if no name is given name will be a readable version of id,
                           by removing underscores and capitalizing first letters of words *)
  loc: Location.t
  ; (* location in the code *)
  severity: Exceptions.err_kind
  ; suggestion: string option
  (* an optional suggestion or correction *) }

val string_of_mode : mode -> string

val pp_issue : Format.formatter -> issue_desc -> unit

val should_run_check : mode -> bool
