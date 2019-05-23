(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Sledge executable entry point *)

let main ~bound ~compile_only ~input ~output =
  try
    let program =
      if String.is_suffix input ~suffix:".llair" then
        In_channel.with_file input ~f:(fun ic ->
            (Marshal.from_channel ic : Llair.t) )
      else
        let program = Frontend.translate input in
        Trace.flush () ;
        Out_channel.with_file (input ^ ".llair") ~f:(fun oc ->
            Marshal.to_channel oc program [Marshal.Closures] ) ;
        program
    in
    Option.iter output ~f:(function
      | "-" -> Format.printf "%a@." Llair.pp program
      | filename ->
          Out_channel.with_file filename ~f:(fun oc ->
              let fs = Format.formatter_of_out_channel oc in
              Format.fprintf fs "%a@." Llair.pp program ) ) ;
    if not compile_only then (
      Control.exec_pgm ~bound program ;
      Trace.flush () ) ;
    Format.printf "@\nRESULT: Success@."
  with exn ->
    let bt = Caml.Printexc.get_raw_backtrace () in
    Trace.flush () ;
    ( match exn with
    | Frontend.Invalid_llvm msg ->
        Format.printf "@\nRESULT: Invalid input: %s@." msg
    | Unimplemented msg ->
        Format.printf "@\nRESULT: Unimplemented: %s@." msg
    | Failure msg -> Format.printf "@\nRESULT: Internal error: %s@." msg
    | _ ->
        Format.printf "@\nRESULT: Unknown error: %s@."
          (Caml.Printexc.to_string exn) ) ;
    Caml.Printexc.raise_with_backtrace exn bt

;;
Config.run main
