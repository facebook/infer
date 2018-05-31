(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

let late_callback = ref (fun () -> ())

let register_late f =
  let g = !late_callback in
  late_callback := fun () -> f () ; g ()


let late () = !late_callback ()

(* Run the epilogues when we get SIGINT (Control-C). We do not want to mask SIGINT unless at least
   one epilogue has been registered, so make this value lazy. *)
let activate_run_epilogues_on_signal =
  lazy
    (let run_epilogues_on_signal s =
       F.eprintf "*** %s: Caught %s, time to die@."
         (Filename.basename Sys.executable_name)
         (Signal.to_string s) ;
       (* Invoke the callback that runs at the end of uncaught_exception_handler *)
       late () ;
       (* Epilogues are registered with [at_exit] so exiting will make them run. *)
       Pervasives.exit 0
     in
     Signal.Expert.handle Signal.int run_epilogues_on_signal)


let register ~f desc =
  let f_no_exn () =
    if not !ProcessPoolState.in_child then
      try f () with exn ->
        F.eprintf "Error while running epilogue \"%s\":@ %a.@ Powering through...@." desc Exn.pp
          exn
  in
  (* We call `exit` in a bunch of places, so register the epilogues with [at_exit]. *)
  Pervasives.at_exit f_no_exn ;
  (* Register signal masking. *)
  Lazy.force activate_run_epilogues_on_signal
