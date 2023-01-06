(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type error

val pp_error : Textual.SourceFile.t -> Format.formatter -> error -> unit

val run : Textual.Module.t -> TextualDecls.t -> error list
