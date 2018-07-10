(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val mk_source_file_filter : filter:string option -> (SourceFile.t -> bool) Staged.t

val mk_procedure_name_filter :
  filter:string option -> (SourceFile.t -> Typ.Procname.t -> bool) Staged.t
