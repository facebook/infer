(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
