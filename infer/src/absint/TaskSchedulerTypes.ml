(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type target =
  | Procname of Procname.t
  | File of SourceFile.t
  | ProcUID of string
      (** matches primary key of [procedures] and [specs] tables; see [Database.ml] *)
