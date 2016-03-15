(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val sources : PatternMatch.method_str list

val sinks : (PatternMatch.method_str * int list) list

val functions_with_tainted_params : (PatternMatch.method_str * int list) list
