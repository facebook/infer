(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import
open Core_kernel
open Llair_

let main ~input ~output =
  try
    let program = Frontend.translate input in
    Option.iter output ~f:(function
      | "-" ->
          Format.printf "%a@." Llair.fmt program
      | filename ->
          Out_channel.with_file filename ~f:(fun oc ->
              let ff = Format.formatter_of_out_channel oc in
              Format.fprintf ff "%a@." Llair.fmt program ) ) ;
    Format.printf "@\nRESULT: Success@."
  with exn ->
    let bt = Stdlib.Printexc.get_raw_backtrace () in
    ( match exn with
    | Frontend.Invalid_llvm msg ->
        Format.printf "@\nRESULT: Invalid input: %s@." msg
    | Unimplemented msg ->
        Format.printf "@\nRESULT: Unimplemented: %s@." msg
    | Failure msg ->
        Format.printf "@\nRESULT: Internal error: %s@." msg
    | _ ->
        Format.printf "@\nRESULT: Unknown error: %s@." (Stdlib.Printexc.to_string exn) ) ;
    Stdlib.Printexc.raise_with_backtrace exn bt

;;

main ~input:Stdlib.Sys.argv.(1) ~output:(Some "-")
