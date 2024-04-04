(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
    try f ()
    with exn ->
      F.eprintf
        "%a: Error while running epilogue \"%s\":@\n  @[<v>%a@\n%s@]@\nPowering through...@." Pid.pp
        (Unix.getpid ()) description Exn.pp exn (Printexc.get_backtrace ())
  in
  let g = !callback_ref in
  callback_ref :=
    fun () ->
      f_no_exn () ;
      g ()


let register_early ~f ~description = register early_callback ~f ~description

let register_late ~f ~description = register late_callback ~f ~description

let early () = !early_callback ()

let late () = !late_callback ()

let run () =
  early () ;
  late ()


(** Raised when we are interrupted by SIGINT *)
exception Sigint

(* Raise a specific exception when we get SIGINT (Control-C). *)
let () = Caml.Sys.(set_signal sigint (Signal_handle (fun _ -> raise Sigint)))

let reset () =
  (early_callback := fun () -> ()) ;
  late_callback := fun () -> ()


let register = register_early
