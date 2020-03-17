(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace opened in each source file by the build system *)

include Stdio
include Import0

(** Tuple operations *)

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

(** Function combinators *)

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
let ( $ ) f g x = f x ; g x
let ( $> ) x f = f x ; x
let ( <$ ) f x = f x ; x

(** Failures *)

let fail = Trace.fail

exception Unimplemented of string

let todo fmt = Trace.raisef (fun msg -> Unimplemented msg) fmt

let warn fmt =
  let fs = Format.std_formatter in
  Format.pp_open_box fs 2 ;
  Format.pp_print_string fs "Warning: " ;
  Format.kfprintf
    (fun fs () ->
      Format.pp_close_box fs () ;
      Format.pp_force_newline fs () )
    fs fmt

(** Assertions *)

let assertf cnd fmt =
  if not cnd then fail fmt
  else Format.ikfprintf (fun _ () -> ()) Format.str_formatter fmt

let checkf cnd fmt =
  if not cnd then fail fmt
  else Format.ikfprintf (fun _ () -> true) Format.str_formatter fmt

let check f x =
  assert (f x ; true) ;
  x

let violates f x =
  assert (f x ; true) ;
  assert false

type 'a or_error = ('a, exn * Caml.Printexc.raw_backtrace) result

let or_error f x () =
  try Ok (f x) with exn -> Error (exn, Caml.Printexc.get_raw_backtrace ())

(** Extensions *)

module Invariant = struct
  include Base.Invariant

  let invariant here t sexp_of_t f =
    assert (
      ( try f ()
        with exn ->
          let bt = Caml.Printexc.get_raw_backtrace () in
          let exn =
            Base.Error.to_exn
              (Base.Error.create_s
                 (Base.Sexp.message "invariant failed"
                    [ ("", Sexplib.Conv.sexp_of_exn exn)
                    ; ("", Base.Source_code_position.sexp_of_t here)
                    ; ("", sexp_of_t t) ]))
          in
          Caml.Printexc.raise_with_backtrace exn bt ) ;
      true )
end

module Unit = Base.Unit

type unit = Unit.t [@@deriving compare, equal, hash, sexp]

module Bool = Base.Bool

type bool = Bool.t [@@deriving compare, equal, hash, sexp]

module Char = Base.Char

type char = Char.t [@@deriving compare, equal, hash, sexp]

module Int = Base.Int

type int = Int.t [@@deriving compare, equal, hash, sexp]

module Int64 = Base.Int64

type int64 = Int64.t [@@deriving compare, equal, hash, sexp]

module Z = struct
  let pp = Z.pp_print
  let hash = [%hash: Z.t]
  let hash_fold_t s z = Hash.fold_int s (hash z)
  let sexp_of_t z = Sexp.Atom (Z.to_string z)

  let t_of_sexp = function
    | Sexp.Atom s -> Z.of_string s
    | _ -> assert false

  (* the signed 1-bit integers are -1 and 0 *)
  let true_ = Z.minus_one
  let false_ = Z.zero
  let of_bool = function true -> true_ | false -> false_
  let is_true = Z.equal true_
  let is_false = Z.equal false_

  include Z
end

module Q = struct
  let pp = Q.pp_print
  let hash = Hashtbl.hash
  let hash_fold_t s q = Hash.fold_int s (hash q)
  let sexp_of_t q = Sexp.Atom (Q.to_string q)

  let t_of_sexp = function
    | Sexp.Atom s -> Q.of_string s
    | _ -> assert false

  let of_z = Q.of_bigint

  include Q
end

module String = struct
  module T = struct
    include Base.String

    let hash_fold_t = Hash.fold_string
    let hash = Hash.of_fold hash_fold_t
    let t_of_sexp = Sexplib.Conv.string_of_sexp
    let sexp_of_t = Sexplib.Conv.sexp_of_string
  end

  include T
  module Map = Map.Make (T)
end

type string = String.t [@@deriving compare, equal, hash, sexp]

module Option = Option

type 'a option = 'a Option.t [@@deriving compare, equal, hash, sexp]

include Option.Monad_infix
include Option.Monad_syntax
module Result = Base.Result

module Array = struct
  include Base.Array

  let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
end

module Vector = Vector
include Vector.Infix
module List = List

type 'a list = 'a List.t [@@deriving compare, equal, hash, sexp]

module Hash_queue = Core_kernel.Hash_queue
module Set = Set
module Hash_set = Base.Hash_set
module Map = Map
module Qset = Qset
module Hashtbl = Base.Hashtbl
