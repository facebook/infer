(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Core

[@@@warning "-32"]

(* easier to write Unix than Core_unix *)
module Unix = Core_unix

(* we don't care about the _unix distinction *)
module Filename = struct
  include Filename
  include Filename_unix
end

(* we don't care about the _unix distinction *)
module Sys = struct
  include Sys
  include Sys_unix
end

(* Compare police: generic compare mostly disabled. *)
let compare = No_polymorphic_compare.compare

let equal = No_polymorphic_compare.equal

let ( = ) = No_polymorphic_compare.( = )

let failwith _ : [`use_Logging_die_instead] = assert false

let failwithf _ : [`use_Logging_die_instead] = assert false

let invalid_arg _ : [`use_Logging_die_instead] = assert false

let invalid_argf _ : [`use_Logging_die_instead] = assert false

let exit = `In_general_prefer_using_Logging_exit_over_Pervasives_exit

[@@@warning "+32"]

module ANSITerminal : module type of ANSITerminal = struct
  include ANSITerminal

  (* more careful about when the channel is connected to a tty *)

  let print_string = if Unix.(isatty stdout) then print_string else fun _ -> Stdlib.print_string

  let prerr_string = if Unix.(isatty stderr) then prerr_string else fun _ -> Stdlib.prerr_string

  let printf styles fmt = Format.ksprintf (fun s -> print_string styles s) fmt

  let eprintf styles fmt = Format.ksprintf (fun s -> prerr_string styles s) fmt

  let sprintf = if Unix.(isatty stderr) then sprintf else fun _ -> Printf.sprintf
end
