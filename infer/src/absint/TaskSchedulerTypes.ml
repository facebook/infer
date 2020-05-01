(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

exception ProcnameAlreadyLocked of Procname.t
(** for the Restart scheduler: raise when a worker tries to analyze a procedure already being
    analyzed by another process *)

type target = Procname of Procname.t | File of SourceFile.t
