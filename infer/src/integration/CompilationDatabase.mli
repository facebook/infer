(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

type compilation_data =
  { directory: string
  ; executable: string
  ; escaped_arguments: string list
        (** argument list, where each argument is already escaped for the shell. This is because in
            some cases the argument list contains arguments that are actually themselves a list of
            arguments, for instance because the compilation database only contains a "command"
            entry. *) }

val filter_compilation_data :
  t -> f:(SourceFile.t -> bool) -> (SourceFile.t * compilation_data) list

val from_json_files : [< `Escaped of string | `Raw of string] list -> t
