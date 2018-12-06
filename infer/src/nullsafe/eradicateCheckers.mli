(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Eradicate-based user-defined checkers. *)

val report_error :
     Tenv.t
  -> Typ.Procname.t
  -> Procdesc.t
  -> IssueType.t
  -> Location.t
  -> ?field_name:Typ.Fieldname.t option
  -> ?origin_loc:Location.t option
  -> ?exception_kind:(IssueType.t -> Localise.error_desc -> exn)
  -> ?severity:Exceptions.severity
  -> string
  -> unit
(** Report an error. *)
