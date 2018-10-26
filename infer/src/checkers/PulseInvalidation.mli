(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type cause =
  | CppDelete of AccessExpression.t
  | CppDestructor of Typ.Procname.t * AccessExpression.t
  | CFree of AccessExpression.t
  | StdVectorPushBack of AccessExpression.t
[@@deriving compare]

type t = {cause: cause; location: Location.t} [@@deriving compare]

val issue_type_of_cause : cause -> IssueType.t

include AbstractDomain.S with type astate = t
