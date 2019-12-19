(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type source_files_filter = SourceFile.t -> bool

type procedures_filter = SourceFile.t -> Procname.t -> bool

let filter_of_regexp_opt ~to_string r =
  match r with
  | None ->
      fun _ -> true
  | Some regexp ->
      fun x -> Str.string_match regexp (to_string x) 0


let ( &&& ) filter1 filter2 x1 x2 = filter1 x1 && filter2 x2

let mk_source_file_filter ~filter =
  let regexp_opt = Option.map ~f:Str.regexp filter in
  filter_of_regexp_opt ~to_string:SourceFile.to_string regexp_opt


let source_files_filter = lazy (mk_source_file_filter ~filter:Config.source_files_filter)

let mk_procedure_name_filter ~filter =
  let source_file_regexp, proc_name_regexp =
    match filter with
    | None ->
        (None, None)
    | Some filter_string -> (
      match String.lsplit2 ~on:':' filter_string with
      | Some (source_file_filter, proc_name_filter) ->
          (Some (Str.regexp source_file_filter), Some (Str.regexp proc_name_filter))
      | None ->
          (* if only one filter is supplied assume it's for procedure names and the source files are
             a wildcard *)
          (None, Some (Str.regexp filter_string)) )
  in
  let source_file_filter =
    filter_of_regexp_opt ~to_string:SourceFile.to_string source_file_regexp
  in
  let proc_name_filter = filter_of_regexp_opt ~to_string:Procname.to_string proc_name_regexp in
  source_file_filter &&& proc_name_filter


let procedures_filter = lazy (mk_procedure_name_filter ~filter:Config.procedures_filter)
