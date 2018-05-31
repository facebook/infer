(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* version macros like kCFCoreFoundationVersionNumber_iOS_9_0 are
   tied to specific float values, e.g. 1240.1.
   To be found in CoreFoundation/CFBase.h *)

type machine_readable_version = float

type human_readable_version = string

type t = machine_readable_version * human_readable_version

let version_numbers : t list =
  [ (478.23, "2.0")
  ; (478.26, "2.1")
  ; (478.29, "2.2")
  ; (478.47, "3.0")
  ; (478.52, "3.1")
  ; (478.61, "3.2")
  ; (550.32, "4.0")
  ; (550.38, "4.1")
  ; (550.52, "4.3")
  ; (675.00, "5.0")
  ; (690.10, "5.1")
  ; (793.00, "6.1")
  ; (847.20, "7.0")
  ; (847.24, "7.1")
  ; (1140.1, "8.0")
  ; (1141.14, "8.1")
  ; (1142.16, "8.2")
  ; (1144.17, "8.3")
  ; (1145.15, "8.4")
  ; (1240.1, "9.0")
  ; (1241.11, "9.1")
  ; (1242.13, "9.3")
  ; (1280.38, "9.4")
  ; (1348.0, "10.0")
  ; (1348.22, "10.2") ]


let sort_versions versions =
  let compare (version_float1, _) (version_float2, _) =
    Float.compare version_float1 version_float2
  in
  List.sort ~compare versions


let version_of number_s : human_readable_version option =
  let epsilon = 0.001 in
  let rec version_of_aux version_numbers number =
    match version_numbers with
    | (version_n, version_s) :: (next_version_n, next_version_s) :: rest ->
        if number -. version_n < epsilon && number -. version_n > ~-.epsilon then Some version_s
        else if number >= version_n +. epsilon && number <= next_version_n -. epsilon then
          Some next_version_s
        else version_of_aux ((next_version_n, next_version_s) :: rest) number
    | [(version_n, version_s)] ->
        if number >= version_n then Some version_s else None
    | [] ->
        None
  in
  let number_opt = try Some (float_of_string number_s) with Failure _ -> None in
  match number_opt with
  | None ->
      None
  | Some number ->
      version_of_aux (sort_versions version_numbers) number


let pp_diff_of_version_opt fmt (expected, actual) =
  let option_to_string opt = Option.value ~default:"" opt in
  Format.fprintf fmt "Expected: [%s] Found: [%s]" (option_to_string expected)
    (option_to_string actual)
