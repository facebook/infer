(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val parse_string : ?filename:string -> string -> SemdiffDirectEngine.Rules.t

val parse_file : string -> SemdiffDirectEngine.Rules.t
