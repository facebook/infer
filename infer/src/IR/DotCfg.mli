(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val emit_frontend_cfg : SourceFile.t -> Cfg.t -> unit
(** emit the given {!Cfg.t} in the "dot" format to a file determined by {!IBase.Config} values *)

val emit_proc_desc : SourceFile.t -> Procdesc.t -> string
(** emit the given {!Procdesc.t} in the "dot" format to a file in infer-out/captured/ and return the
    path to that file *)
