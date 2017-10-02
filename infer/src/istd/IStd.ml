(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

include Core

module Unix_ = struct
  let improve f make_arg_sexps =
    try f ()
    with Unix.Unix_error (e, s, _) ->
      let buf = Buffer.create 100 in
      let fmt = Format.formatter_of_buffer buf in
      Format.pp_set_margin fmt 10000 ;
      Sexp.pp_hum fmt
        (Sexp.List
           (List.map (make_arg_sexps ()) ~f:(fun (name, value) -> Sexp.List [Sexp.Atom name; value]))) ;
      Format.pp_print_flush fmt () ;
      let arg_str = Buffer.contents buf in
      raise (Unix.Unix_error (e, s, arg_str))

  let create_process_redirect ~prog ~args ?(stdin= Unix.stdin) ?(stdout= Unix.stdout)
      ?(stderr= Unix.stderr) () =
    improve
      (fun () ->
        let prog_args = Array.of_list (prog :: args) in
        Caml.UnixLabels.create_process ~prog ~args:prog_args ~stdin ~stdout ~stderr |> Pid.of_int)
      (fun () ->
        [("prog", Sexp.Atom prog); ("args", Sexplib.Conv.sexp_of_list (fun a -> Sexp.Atom a) args)]
    )

  let fork_redirect_exec_wait ~prog ~args ?stdin ?stdout ?stderr () =
    Unix.waitpid (create_process_redirect ~prog ~args ?stdin ?stdout ?stderr ())
    |> Unix.Exit_or_signal.or_error |> ok_exn
end

module List_ = struct
  let rec fold_until ~init ~f l =
    match (l, init) with
    | _, `Stop init' | [], `Continue init'
     -> init'
    | h :: t, `Continue _
     -> fold_until ~init:(f init h) ~f t

  let merge_dedup l1 l2 ~compare =
    let rec loop acc l1 l2 =
      match (l1, l2) with
      | [], l2
       -> List.rev_append acc l2
      | l1, []
       -> List.rev_append acc l1
      | h1 :: t1, h2 :: t2
       -> let cmp = compare h1 h2 in
          if cmp = 0 then loop (h1 :: acc) t1 t2
          else if cmp < 0 then loop (h1 :: acc) t1 l2
          else loop (h2 :: acc) l1 t2
    in
    loop [] l1 l2
end

(* Use Caml.Set since they are serialized using Marshal, and Core.Std.Set includes the comparison
   function in its representation, which Marshal cannot (de)serialize. *)
module IntSet = Caml.Set.Make (Int)

(* Compare police: generic compare mostly disabled. *)
let compare = No_polymorphic_compare.compare

let equal = No_polymorphic_compare.equal

let ( = ) = No_polymorphic_compare.( = )

module PVariant = struct
  (* Equality for polymorphic variants *)
  let ( = ) (v1: [> ]) (v2: [> ]) = Polymorphic_compare.( = ) v1 v2
end

(** Reraise the exception after doing f. Always reraise immediately after catching the exception, otherwise the backtrace can be wrong *)
let reraise_after ~f exn =
  let backtrace = Caml.Printexc.get_raw_backtrace () in
  let () = f () in
  Caml.Printexc.raise_with_backtrace exn backtrace

(** Reraise the exception if f returns true. Always reraise immediately after catching the exception, otherwise the backtrace can be wrong *)
let reraise_if ~f exn =
  let backtrace = Caml.Printexc.get_raw_backtrace () in
  if f () then Caml.Printexc.raise_with_backtrace exn backtrace

let failwith _ : [`use_Logging_die_instead] = assert false

let failwithf _ : [`use_Logging_die_instead] = assert false

let invalid_arg _ : [`use_Logging_die_instead] = assert false

let invalid_argf _ : [`use_Logging_die_instead] = assert false

(** With Logging.exit you have more control of the code that invokes exit, for example when forking
    and running certain functions that may in turn invoke exit, and you want to handle the execution
    flow differently - like invoking certain callbacks before exiting, or not exiting at all. *)
let exit = `In_general_prefer_using_Logging_exit_over_Pervasives_exit

module ANSITerminal : module type of ANSITerminal = struct
  include ANSITerminal

  let print_string =
    if Unix.(isatty stdout) then print_string else fun _ -> Pervasives.print_string

  let prerr_string =
    if Unix.(isatty stderr) then prerr_string else fun _ -> Pervasives.prerr_string

  let printf styles fmt = Format.ksprintf (fun s -> print_string styles s) fmt

  let eprintf styles fmt = Format.ksprintf (fun s -> prerr_string styles s) fmt

  let sprintf = if Unix.(isatty stderr) then sprintf else fun _ -> Printf.sprintf
end
