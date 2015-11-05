(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


(* Module for warnings detected at translation time by the frontend *)


(* Checks for warnings on properties of class c *)
val check_for_property_errors : Cfg.cfg -> CContext.curr_class -> unit
