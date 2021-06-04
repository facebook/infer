(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** for the Restart scheduler: raise when a worker tries to analyze a procedure already being
    analyzed by another process *)
exception ProcnameAlreadyLocked of {dependency_filename: string}

let is_not_restart_exception = function ProcnameAlreadyLocked _ -> false | _ -> true
