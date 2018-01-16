(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t

type compilation_data = {dir: string; command: string; args: string}

val filter_compilation_data : t -> f:(SourceFile.t -> bool) -> compilation_data list

val from_json_files : [< `Escaped of string | `Raw of string] list -> t
