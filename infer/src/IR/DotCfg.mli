(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val emit : SourceFile.t -> Cfg.t -> unit
(** emit the given {!Cfg.t} in the "dot" format to a file determined by {!Config} values *)
