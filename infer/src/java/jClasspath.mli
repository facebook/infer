(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

(** map entry for source files with potential basename collision within the same compiler call *)
type file_entry = Singleton of SourceFile.t | Duplicate of (string * SourceFile.t) list

type t = {classpath: string; sources: file_entry String.Map.t; classes: JBasics.ClassSet.t}

val load_from_verbose_output : string -> t
(** load the list of source files and the list of classes from the javac verbose file *)

val load_from_arguments : string -> t
(** load the list of source files and the list of classes from Config.generated_classes *)
