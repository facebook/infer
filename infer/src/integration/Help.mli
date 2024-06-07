(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val list_checkers : unit -> unit
(** print the list of all checkers *)

val list_categories : unit -> unit
(** print the list of all known issue categories *)

val list_issue_types : unit -> unit
(** print the list of all known issue types *)

val show_checkers : Checker.t list -> unit
(** show information about the given checkers *)

val show_issue_types : IssueType.t list -> unit
(** show information about the given issue_types *)

val write_website : website_root:string -> unit
(** generate files for the fbinfer.com website *)

val abs_url_of_issue_type : string -> string
(** given an issue type unique ID, return the URL relative to the root of the website, e.g.
    [abs_url_of_issue_type "NULL_DEREFERENCE"] is ["/docs/all-issue-types#null_dereference"] *)
