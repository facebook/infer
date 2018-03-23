(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Global namespace opened in each source file by the build system *)

include Option.Monad_infix
module Vector = Vector
include Vector.Infix

module Z = struct
  include Z

  let sexp_of_t z = Sexp.Atom (Z.to_string z)

  let t_of_sexp = function
    | Sexp.Atom s -> Z.of_string s
    | _ -> assert false
end

let fst3 (x, _, _) = x

let snd3 (_, y, _) = y

let trd3 (_, _, z) = z

let ( >> ) f g x = g (f x)

let ( $ ) f g x = f x ; g x

let ( $> ) x f = f x ; x

let ( <$ ) f x = f x ; x

type ('a, 'b) fmt_str = ('a, Format.formatter, unit, 'b) format4

type 'a fmt = Format.formatter -> 'a -> unit

let option_fmt fmt pp ff = function
  | Some x -> Format.fprintf ff fmt pp x
  | None -> ()


let rec list_fmt sep pp ff = function
  | [] -> ()
  | [x] -> pp ff x
  | x :: xs -> Format.fprintf ff "%a%( %)%a" pp x sep (list_fmt sep pp) xs


let vector_fmt sep pp ff v = list_fmt sep pp ff (Vector.to_list v)

let warn fmt =
  let ff = Format.std_formatter in
  Format.pp_open_box ff 2 ;
  Format.pp_print_string ff "Warning: " ;
  Format.kfprintf
    (fun ff ->
      Format.pp_close_box ff () ;
      Format.pp_force_newline ff () )
    ff fmt


let raisef exn fmt =
  let bt = Caml.Printexc.get_raw_backtrace () in
  let ff = Format.str_formatter in
  Format.pp_open_box ff 2 ;
  Format.kfprintf
    (fun ff () ->
      Format.pp_close_box ff () ;
      let msg = Format.flush_str_formatter () in
      let exn = exn msg in
      Caml.Printexc.raise_with_backtrace exn bt )
    ff fmt


exception Unimplemented of string

let todo fmt = raisef (fun msg -> Unimplemented msg) fmt

let fail fmt = raisef (fun msg -> Failure msg) fmt

let assertf cnd fmt =
  if not cnd then fail fmt
  else Format.ikfprintf (fun _ () -> ()) Format.str_formatter fmt


let checkf cnd fmt =
  if not cnd then fail fmt
  else Format.ikfprintf (fun _ () -> true) Format.str_formatter fmt


type 'a or_error = ('a, exn * Caml.Printexc.raw_backtrace) Result.t

let or_error f x () =
  try Ok (f x) with exn -> Error (exn, Caml.Printexc.get_raw_backtrace ())
