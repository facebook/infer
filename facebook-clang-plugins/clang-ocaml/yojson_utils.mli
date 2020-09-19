(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val read_data_from_file : 'a Atdgen_runtime.Util.Json.reader -> string -> 'a

val write_data_to_file :
     ?pretty:bool
  -> ?compact_json:bool
  -> ?std_json:bool
  -> 'a Atdgen_runtime.Util.Json.writer
  -> string
  -> 'a
  -> unit

val ydump : ?compact_json:bool -> ?std_json:bool -> in_channel -> out_channel -> bool

val empty_string : string

val run_converter_tool :
  'a Atdgen_runtime.Util.Json.reader -> 'a Atdgen_runtime.Util.Json.writer -> unit
