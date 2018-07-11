(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type source_files_filter = SourceFile.t -> bool

type procedures_filter = SourceFile.t -> Typ.Procname.t -> bool

val source_files_filter : source_files_filter Lazy.t
(** filter corresponding to `--source-files-filter` *)

val procedures_filter : procedures_filter Lazy.t
(** filter corresponding to `--procedures-filter` *)
