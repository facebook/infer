(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace opened in each source file by the build system *)

include module type of Option.Monad_infix

module Z : sig
  include module type of Z

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t
end

module Vector = Vector

include module type of Vector.Infix

val fst3 : 'a * _ * _ -> 'a
(** First projection from a triple. *)

val snd3 : _ * 'a * _ -> 'a
(** Second projection from a triple. *)

val trd3 : _ * _ * 'a -> 'a
(** Third projection from a triple. *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition of functions: [(f >> g) x] is exactly equivalent to [g (f
    (x))]. Left associative. *)

val ( $ ) : ('a -> unit) -> ('a -> 'b) -> 'a -> 'b
(** Sequential composition of functions: [(f $ g) x] is exactly equivalent
    to [(f x) ; (g x)]. Left associative. *)

val ( $> ) : 'a -> ('a -> unit) -> 'a
(** Apply and ignore function: [x $> f] is exactly equivalent to [f x ; x].
    Left associative. *)

val ( <$ ) : ('a -> unit) -> 'a -> 'a
(** Reverse apply and ignore function: [f <$ x] is exactly equivalent to [f
    x ; x]. Left associative. *)

(** Format strings. *)
type ('a, 'b) fmt_str = ('a, Format.formatter, unit, 'b) format4

(** Formatting function for argument type. *)
type 'a fmt = Format.formatter -> 'a -> unit

val option_fmt :
  ('a_fmt -> 'a -> unit, unit) fmt_str -> 'a_fmt -> 'a option fmt
(** Format an option. *)

val list_fmt : (unit, unit) fmt_str -> 'a fmt -> 'a list fmt
(** Format a list. *)

val vector_fmt : (unit, unit) fmt_str -> 'a fmt -> 'a vector fmt
(** Format a vector. *)

exception Unimplemented of string

val warn : ('a, unit) fmt_str -> 'a
(** Issue a warning for a survivable problem. *)

val todo : ('a, unit -> _) fmt_str -> 'a
(** Raise an [Unimplemented] exception indicating that an input is valid but
    not handled by the current implementation. *)

val assertf : bool -> ('a, unit -> unit) fmt_str -> 'a
(** Raise an [Failure] exception if the bool argument is false, indicating
    that the expected condition was not satisfied. *)

val checkf : bool -> ('a, unit -> bool) fmt_str -> 'a
(** As [assertf] but returns the argument bool. *)

val fail : ('a, unit -> _) fmt_str -> 'a
(** Raise a [Failure] exception indicating a fatal error not covered by
    [assertf], [checkf], or [todo]. *)

type 'a or_error = ('a, exn * Caml.Printexc.raw_backtrace) Result.t

val or_error : ('a -> 'b) -> 'a -> unit -> 'b or_error
(** [or_error f x] runs [f x] and converts unhandled exceptions to errors. *)
