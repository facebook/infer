(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

let early_callback = ref (fun () -> ())

let late_callback = ref (fun () -> ())

let register callback_ref ~f ~description =
  let f_no_exn () =
    try f () with exn ->
      F.eprintf "%a: Error while running epilogue \"%s\":@ %a.@ Powering through...@." Pid.pp
        (Unix.getpid ()) description Exn.pp exn
  in
  let g = !callback_ref in
  callback_ref := fun () -> f_no_exn () ; g ()


let register_early ~f ~description = register early_callback ~f ~description

let register_late ~f ~description = register late_callback ~f ~description

let early () = !early_callback ()

let late () = !late_callback ()

let run () = early () ; late ()

(* Run the epilogues when we get SIGINT (Control-C). *)
let () =
  let run_epilogues_on_signal s =
    F.eprintf "*** %s: Caught %s, time to die@."
      (Filename.basename Sys.executable_name)
      (Signal.to_string s) ;
    run ()
  in
  Signal.Expert.handle Signal.int run_epilogues_on_signal


let reset () =
  (early_callback := fun () -> ()) ;
  late_callback := fun () -> ()


let register = register_early
