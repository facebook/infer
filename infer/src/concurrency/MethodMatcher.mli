(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** pattern matcher for Java methods *)
type t = Tenv.t -> Typ.Procname.t -> HilExp.t list -> bool

type record =
  { search_superclasses: bool
  ; method_prefix: bool
  ; actuals_pred: HilExp.t list -> bool
  ; classname: string
  ; methods: string list }

val default : record
(** record encapsulating the default arguments of [call_matches].  [classname=""] and [methods=[]].
    Useful for [with] expressions *)

val of_record : record -> t
(** make a matcher out of a record; optional values use defaults *)

val of_json : Yojson.Basic.json -> t
(** Parse a JSon object into a matcher.  The Json object must be a list of records, each
    corresponding to a single matcher.  Each record must have a ["classname"] field with a [string]
    value, and a ["methods"] field with a list of strings.  The record may also have boolean
    fields ["search_superclasses"] and ["method_prefix"].  If absent, the defaults are used.
    The resulting matcher matches if one of the matchers in the list does. *)

val of_list : t list -> t
(** Or combinator *)

val of_records : record list -> t
(** shorthand for [of_list (List.map ~f:of_record r)] *)
