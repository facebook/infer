(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val class_name_location : SourceFile.t -> string -> Location.t
(** [class_name_location source class_name] searches in file [source] the declaration location for
    class name [class_name] *)
