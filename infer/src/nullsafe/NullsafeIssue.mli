(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Information about the nullsafe issue to be reported to the user / put into the result json *)

type t

val make :
     issue_type:IssueType.t
  -> description:string
  -> loc:Location.t
  -> severity:IssueType.severity
  -> (* If the issue is about a field (e.g. field not nullable etc.), here's this field *)
     field_name:Fieldname.t option
  -> t

val with_third_party_dependent_methods : (Procname.Java.t * AnnotatedSignature.t) list -> t -> t

val with_nullable_methods : TypeOrigin.method_call_origin list -> t -> t

val with_inconsistent_param_index : int option -> t -> t
(** Only for the "Inconsistent subclass param annotation" issue *)

val with_parameter_not_nullable_info : param_index:int -> proc_name:Procname.Java.t -> t -> t
(** Only for the "Paremeter not nullable" issue *)

val get_issue_type : t -> IssueType.t

val get_description : t -> string

val get_loc : t -> Location.t

val get_severity : t -> IssueType.severity

val get_nullsafe_extra : t -> Procname.Java.t -> Jsonbug_t.nullsafe_extra
