(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module DeprecatedAPIUsage :
sig
  val checker :
    CLintersContext.context -> Ctl_parser_types.ast_node ->
    CTL.t * CIssue.issue_desc option
end
