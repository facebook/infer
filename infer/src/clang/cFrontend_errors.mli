(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils


(* Module for warnings detected at translation time by the frontend *)

(* Run frontend checkers on a statement *)
val run_frontend_checkers_on_stmt : CTrans_utils.trans_state -> Clang_ast_t.stmt -> unit

(* Run frontend checkers on a declaration *)
val run_frontend_checkers_on_decl : Cfg.cfg -> Cg.t -> Clang_ast_t.decl -> unit

