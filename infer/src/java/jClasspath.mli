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
type file_entry = Singleton of SourceFile.t | Duplicate of (string list * SourceFile.t) list

type t =
  { classpath_channel: Javalib.class_path
  ; sources: file_entry String.Map.t
  ; classes: JBasics.ClassSet.t }

type source =
  | FromVerboseOut of {verbose_out_file: string}
      (** load the list of source files and the list of classes from the javac verbose file *)
  | FromArguments of {path: string; sources: string list}
      (** load the list of source files and the list of classes from [Config.generated_classes] *)

val with_classpath : f:(t -> unit) -> source -> unit
(** load a class path, pass it to [f] and cleanup after [f] is done *)
