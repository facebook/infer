(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open Core

(* NOTE: All variants must be also added to `all_commands` below *)
type t = Analyze | Capture | Compile | Diff | Events | Explore | Report | ReportDiff | Run
[@@deriving compare]

let equal = [%compare.equal: t]

let command_to_string =
  [ (Analyze, "analyze")
  ; (Capture, "capture")
  ; (Compile, "compile")
  ; (Diff, "diff")
  ; (Events, "events")
  ; (Explore, "explore")
  ; (Report, "report")
  ; (ReportDiff, "reportdiff")
  ; (Run, "run") ]


let all_commands = List.map ~f:fst command_to_string

let to_string cmd = List.Assoc.find_exn ~equal command_to_string cmd

let of_string name =
  List.Assoc.find_exn ~equal:String.equal (List.Assoc.inverse command_to_string) name


let infer_exe_name = "infer"

let exe_name_of_command_string name = Printf.sprintf "%s-%s" infer_exe_name name

let to_exe_name cmd = to_string cmd |> exe_name_of_command_string

let of_exe_name exe_name =
  List.find_map command_to_string ~f:(function cmd, name ->
      if String.equal exe_name (exe_name_of_command_string name) then Some cmd else None )
