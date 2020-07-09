(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace intended to be opened in each source file *)

include NS0
module Monad = Monad

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
  assert (
    f x ;
    true ) ;
  x

let violates f x =
  assert (
    f x ;
    true ) ;
  assert false

(** Extensions *)

module Invariant = struct
  include Core.Invariant

  let invariant here t sexp_of_t f =
    assert (
      ( try f ()
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          let exn =
            Error.to_exn
              (Error.create_s
                 (Sexp.List
                    [ Atom "invariant failed"
                    ; sexp_of_exn exn
                    ; Source_code_position.sexp_of_t here
                    ; sexp_of_t t ]))
          in
          Printexc.raise_with_backtrace exn bt ) ;
      true )
end

(** Containers *)

module Option = Option
include Option.Import
module List = List

module Array = struct
  include Core.Array

  let hash_fold_t hash_fold_elt s a =
    Hash.Builtin.hash_fold_array_frozen hash_fold_elt s a

  module Import = struct
    type 'a array = 'a t [@@deriving compare, equal, hash, sexp]
  end

  let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
  let map_endo xs ~f = map_endo map xs ~f

  let fold_map_inplace a ~init ~f =
    let s = ref init in
    let f x =
      let s', x' = f !s x in
      s := s' ;
      x'
    in
    map_inplace a ~f ;
    !s
end

include Array.Import
module IArray = IArray
include IArray.Import
module Set = Set
module Map = Map
module Qset = Qset

(** Data types *)

module String = struct
  include (
    Core.String :
      sig
        include
          module type of Core.String with module Map := Core.String.Map
      end )

  module Map = Map.Make (Core.String)
end

module Q = struct
  let pp = Q.pp_print
  let hash = Hashtbl.hash
  let hash_fold_t s q = Int.hash_fold_t s (hash q)
  let sexp_of_t q = Sexp.Atom (Q.to_string q)

  let t_of_sexp = function
    | Sexp.Atom s -> Q.of_string s
    | _ -> assert false

  let of_z = Q.of_bigint

  include Q
end

module Z = struct
  let pp = Z.pp_print
  let hash = [%hash: Z.t]
  let hash_fold_t s z = Int.hash_fold_t s (hash z)
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

(** Utilities *)

module Timer = Timer
