(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for parsing stack traces and using them to guide Infer analysis *)

type frame = {class_str: string; method_str: string; file_str: string; line_num: int option}

type t = {exception_name: string; frames: frame list}

val make : string -> frame list -> t

val make_frame : string -> string -> string -> int option -> frame

val frame_matches_location : frame -> Location.t -> bool

val of_string : string -> t

val of_json_file : string -> t
