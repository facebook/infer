(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
end

external ( = ) : int -> int -> bool = "%equal"
external ( <> ) : int -> int -> bool = "%notequal"
external ( < ) : int -> int -> bool = "%lessthan"
external ( > ) : int -> int -> bool = "%greaterthan"
external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"

let compare (a : int) b =
  let int_of_bool (b : bool) = (Obj.magic b : int) in
  int_of_bool (a > b) - int_of_bool (a < b)

external equal : int -> int -> bool = "%equal"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

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

(** Pretty-printer for argument type. *)
type 'a pp = Format.formatter -> 'a -> unit

(** Format strings. *)
type ('a, 'b) fmt = ('a, 'b) Trace.fmt

module Hash = Ppx_hash_lib.Std.Hash
module Sexp = Sexplib.Sexp

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

exception Duplicate

module Return = struct type 'r t = {return: 'a. 'r -> 'a} [@@unboxed] end

let with_return (type a) f =
  let module M = struct exception Return of a end in
  let return a = raise_notrace (M.Return a) in
  try f {Return.return} with M.Return a -> a
