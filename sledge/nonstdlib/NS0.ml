(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace used when defining the rest of nonstdlib, which is
    extended in NS, the exposed interface of nonstdlib *)

(** Support for [@@deriving compare, equal, hash, sexp] on builtin types *)

include Ppx_compare_lib.Builtin
module Hash = Ppx_hash_lib.Std.Hash
include Hash.Builtin
module Sexp = Sexplib.Sexp
include Ppx_sexp_conv_lib.Conv

(** Iterators *)

module Iter = Iter
include Iter.Import

(** Specialize polymorphic comparison to int *)

module Int_compare = struct
  external ( = ) : int -> int -> bool = "%equal"
  external ( <> ) : int -> int -> bool = "%notequal"
  external ( < ) : int -> int -> bool = "%lessthan"
  external ( > ) : int -> int -> bool = "%greaterthan"
  external ( <= ) : int -> int -> bool = "%lessequal"
  external ( >= ) : int -> int -> bool = "%greaterequal"
  external compare : int -> int -> int = "%compare"
  external equal : int -> int -> bool = "%equal"

  let min x y = if x <= y then x else y
  let max x y = if x >= y then x else y
end

include Int_compare

(** Polymorphic comparison and hashing *)
module Poly = struct
  external ( = ) : 'a -> 'a -> bool = "%equal"
  external ( <> ) : 'a -> 'a -> bool = "%notequal"
  external ( < ) : 'a -> 'a -> bool = "%lessthan"
  external ( > ) : 'a -> 'a -> bool = "%greaterthan"
  external ( <= ) : 'a -> 'a -> bool = "%lessequal"
  external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
  external compare : 'a -> 'a -> int = "%compare"
  external equal : 'a -> 'a -> bool = "%equal"

  let min x y = if x <= y then x else y
  let max x y = if x >= y then x else y
  let hash = Stdlib.Hashtbl.hash
end

module Ord = Containers.Ord

(** Function combinators *)

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)

let ( $ ) f g x =
  f x ;
  g x

let ( $> ) x f =
  f x ;
  x

let ( <$ ) f x =
  f x ;
  x

let ( let@ ) x f = x @@ f

(** Tuple operations *)

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

(** Pretty-printing *)

type 'a pp = Format.formatter -> 'a -> unit
type ('a, 'b) fmt = ('a, Format.formatter, unit, 'b) format4

(** Monadic syntax *)

module type Applicative_syntax = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module type Monad_syntax = sig
  include Applicative_syntax

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

(** Data types *)

module Char = Containers.Char

(** Container utilities *)

let map_endo map t ~f =
  let change = ref false in
  let t' =
    map t ~f:(fun x ->
        let x' = f x in
        if x' != x then change := true ;
        x' )
  in
  if !change then t' else t

let fold_map_from_map map x s ~f =
  let s = ref s in
  let f y =
    let y', s' = f y !s in
    s := s' ;
    y'
  in
  let x' = map x ~f in
  (x', !s)

(** Containers *)

type 'a zero_one_many = Zero | One of 'a | Many
type ('a, 'b) zero_one_many2 = Zero2 | One2 of 'a * 'b | Many2

(* from upcoming Stdlib *)
module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b

  let left v = Left v
  let right v = Right v
end

module Pair = Containers.Pair
module Bijection = CCBijection [@@warning "-49"]

module FHeap = struct
  include Fheap

  let remove_top_exn h = snd (pop_exn h)
end

module HashQueue = Core_kernel.Hash_queue

(** Input / Output *)

module In_channel = Stdio.In_channel
module Out_channel = Stdio.Out_channel

(** Invariants *)

let register_sexp_of_exn exn sexp_of_exn =
  Sexplib.Conv.Exn_converter.add
    (Obj.Extension_constructor.of_val exn)
    sexp_of_exn

module Invariant = struct
  type position = Lexing.position =
    {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}
  [@@deriving sexp_of]

  exception Violation of exn * position * Sexp.t

  ;;
  register_sexp_of_exn
    (Violation (Not_found, Lexing.dummy_pos, Sexp.List []))
    (function
      | Violation (exn, pos, payload) ->
          Sexp.List
            [ Atom "Invariant.Violation"
            ; sexp_of_exn exn
            ; sexp_of_position pos
            ; payload ]
      | exn -> Sexp.Atom (Printexc.to_string exn) )

  let invariant here t sexp_of_t f =
    assert (
      ( try f ()
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          let exn = Violation (exn, here, sexp_of_t t) in
          Printexc.raise_with_backtrace exn bt ) ;
      true )

  module type S = sig
    type t

    val invariant : t -> unit
  end
end

(** Failures *)

exception Replay of exn * Sexp.t

;;
register_sexp_of_exn
  (Replay (Not_found, Sexp.List []))
  (function
    | Replay (exn, payload) ->
        Sexp.List [Atom "Replay"; sexp_of_exn exn; payload]
    | exn -> Sexp.Atom (Printexc.to_string exn) )

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
  assert (
    f x ;
    true ) ;
  x

let violates f x =
  assert (
    f x ;
    true ) ;
  assert false

(** Deprecated *)

module Hashtbl = struct end
