(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

include Core.Std

module Unix_ = struct

  let improve f make_arg_sexps =
    try f () with
    | Unix.Unix_error (e, s, _) ->
        let buf = Buffer.create 100 in
        let fmt = Format.formatter_of_buffer buf in
        Format.pp_set_margin fmt 10000;
        Sexp.pp_hum fmt (
          Sexp.List (
            List.map (make_arg_sexps ())
              ~f:(fun (name, value) -> Sexp.List [Sexp.Atom name; value])));
        Format.pp_print_flush fmt ();
        let arg_str = Buffer.contents buf in
        raise (Unix.Unix_error (e, s, arg_str))

  let create_process_redirect
      ~prog ~args ?(stdin = Unix.stdin) ?(stdout = Unix.stdout) ?(stderr = Unix.stderr) () =
    improve
      (fun () ->
         let prog_args = Array.of_list (prog :: args) in
         Caml.UnixLabels.create_process ~prog ~args:prog_args ~stdin ~stdout ~stderr
         |> Pid.of_int)
      (fun () ->
         [("prog", Sexp.Atom prog);
          ("args", Sexplib.Conv.sexp_of_list (fun a -> Sexp.Atom a) args)])

  let fork_redirect_exec_wait ~prog ~args ?stdin ?stdout ?stderr () =
    Unix.waitpid (create_process_redirect ~prog ~args ?stdin ?stdout ?stderr ())
    |> Unix.Exit_or_signal.or_error |> ok_exn

  (* Unix.symlink has ambiguous function application when the optional argument is not provided, but
     the optional argument is not used in the implementation anyway. *)
  let symlink ~src ~dst = Unix.symlink ?to_dir:None ~src ~dst

end

module List_ = struct
  let rec fold_until ~init ~f l =
    match l, init with
    | _, `Stop init'
    | [], `Continue init' -> init'
    | h :: t, `Continue _ -> fold_until ~init:(f init h) ~f t
end

(* Use Caml.Set since they are serialized using Marshal, and Core.Std.Set includes the comparison
   function in its representation, which Marshal cannot (de)serialize. *)
module IntSet = Caml.Set.Make(Int)


(* Compare police: generic compare mostly disabled. *)
let compare = No_polymorphic_compare.compare
let equal = No_polymorphic_compare.equal
let (=) = No_polymorphic_compare.(=)

module PVariant = struct
  (* Equality for polymorphic variants *)
  let (=) (v1 : [> ]) (v2 : [> ]) = Polymorphic_compare.(=) v1 v2
end

let failwithf fmt =
  Format.kfprintf (fun _ -> failwith (Format.flush_str_formatter ()))
    Format.str_formatter fmt

let invalid_argf fmt =
  Format.kfprintf (fun _ -> invalid_arg (Format.flush_str_formatter ()))
    Format.str_formatter fmt
