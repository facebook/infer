(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type procedures_translated =
  { procedures_translated_total: int
  ; procedures_translated_failed: int
  ; lang: string
  ; source_file: SourceFile.t }

type event =
  | UncaughtException of exn * int  (** exception, exitcode *)
  | ProceduresTranslatedSummary of procedures_translated  (** record of procedures translated *)

val get_log_identifier : unit -> string

val prepare : unit -> unit

val log : event -> unit

val dump : unit -> unit
