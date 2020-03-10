(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace opened in each source file by the build system *)

include module type of (
  Base :
    sig
      include
        (module type of Base
          (* prematurely deprecated, remove and use Stdlib instead *)
          with module Filename := Base.Filename
           and module Format := Base.Format
           and module Marshal := Base.Marshal
           and module Scanf := Base.Scanf
           and type ('ok, 'err) result := ('ok, 'err) Base.result
         [@warning "-3"])
    end )

(* undeprecate *)

external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"

include module type of Stdio
module Command = Core.Command
module Hash_queue = Core_kernel.Hash_queue

(** Tuple operations *)

val fst3 : 'a * _ * _ -> 'a
(** First projection from a triple. *)

val snd3 : _ * 'a * _ -> 'a
(** Second projection from a triple. *)

val trd3 : _ * _ * 'a -> 'a
(** Third projection from a triple. *)

(** Function combinators *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition of functions: [(f >> g) x] is exactly equivalent to
    [g (f (x))]. Left associative. *)

val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Reverse composition of functions: [(g << f) x] is exactly equivalent to
    [g (f (x))]. Left associative. *)

val ( $ ) : ('a -> unit) -> ('a -> 'b) -> 'a -> 'b
(** Sequential composition of functions: [(f $ g) x] is exactly equivalent
    to [(f x) ; (g x)]. Left associative. *)

val ( $> ) : 'a -> ('a -> unit) -> 'a
(** Reverse apply and ignore function: [x $> f] is exactly equivalent to
    [f x ; x]. Left associative. *)

val ( <$ ) : ('a -> unit) -> 'a -> 'a
(** Apply and ignore function: [f <$ x] is exactly equivalent to [f x ; x].
    Left associative. *)

(** Pretty-printing *)

(** Pretty-printer for argument type. *)
type 'a pp = Formatter.t -> 'a -> unit

(** Format strings. *)
type ('a, 'b) fmt = ('a, 'b) Trace.fmt

(** Failures *)

exception Unimplemented of string

val fail : ('a, unit -> _) fmt -> 'a
(** Emit a message at the current indentation level, and raise a [Failure]
    exception indicating a fatal error. *)

val todo : ('a, unit -> _) fmt -> 'a
(** Raise an [Unimplemented] exception indicating that an input is valid but
    not handled by the current implementation. *)

val warn : ('a, unit -> unit) fmt -> 'a
(** Issue a warning for a survivable problem. *)

(** Assertions *)

val assertf : bool -> ('a, unit -> unit) fmt -> 'a
(** Raise an [Failure] exception if the bool argument is false, indicating
    that the expected condition was not satisfied. *)

val checkf : bool -> ('a, unit -> bool) fmt -> 'a
(** As [assertf] but returns the argument bool. *)

val check : ('a -> unit) -> 'a -> 'a
(** Assert that function does not raise on argument, and return argument. *)

val violates : ('a -> unit) -> 'a -> _
(** Assert that function raises on argument. *)

type 'a or_error = ('a, exn * Caml.Printexc.raw_backtrace) result

val or_error : ('a -> 'b) -> 'a -> unit -> 'b or_error
(** [or_error f x] runs [f x] and converts unhandled exceptions to errors. *)

(** Extensions *)

module Invariant : module type of Base.Invariant

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

module Option : sig
  include module type of Base.Option

  val pp : ('a_pp -> 'a -> unit, unit) fmt -> 'a_pp -> 'a option pp
  (** Pretty-print an option. *)

  val cons : 'a t -> 'a list -> 'a list

  module Monad_syntax : Monad_syntax
end

include module type of Option.Monad_infix
include module type of Option.Monad_syntax with type 'a t = 'a option

module List : sig
  include module type of Base.List

  val pp :
       ?pre:(unit, unit) fmt
    -> ?suf:(unit, unit) fmt
    -> (unit, unit) fmt
    -> 'a pp
    -> 'a list pp
  (** Pretty-print a list. *)

  val pp_diff :
       compare:('a -> 'a -> int)
    -> (unit, unit) fmt
    -> 'a pp
    -> ('a list * 'a list) pp

  val pop_exn : 'a list -> 'a * 'a list

  val find_map_remove :
    'a list -> f:('a -> 'b option) -> ('b * 'a list) option

  val fold_option :
       'a t
    -> init:'accum
    -> f:('accum -> 'a -> 'accum option)
    -> 'accum option
  (** [fold_option t ~init ~f] is a short-circuiting version of [fold] that
      runs in the [Option] monad. If [f] returns [None], that value is
      returned without any additional invocations of [f]. *)

  val map_preserving_phys_equal : 'a t -> f:('a -> 'a) -> 'a t
  (** Like map, but preserves [phys_equal] if [f] preserves [phys_equal] of
      every element. *)

  val filter_map_preserving_phys_equal : 'a t -> f:('a -> 'a option) -> 'a t
  (** Like filter_map, but preserves [phys_equal] if [f] preserves
      [phys_equal] of every element. *)

  val rev_map_unzip : 'a t -> f:('a -> 'b * 'c) -> 'b list * 'c list
  (** [rev_map_unzip ~f xs] is [unzip (rev_map ~f xs)] but more efficient. *)

  val remove_exn : ?equal:('a -> 'a -> bool) -> 'a list -> 'a -> 'a list
  (** Returns the input list without the first element [equal] to the
      argument, or raise [Not_found] if no such element exists. [equal]
      defaults to physical equality. *)

  val remove : ?equal:('a -> 'a -> bool) -> 'a list -> 'a -> 'a list option
  val rev_init : int -> f:(int -> 'a) -> 'a list

  val symmetric_diff :
    compare:('a -> 'a -> int) -> 'a t -> 'a t -> ('a, 'a) Either.t t
end

module Map : sig
  include module type of Base.Map

  val pp : 'k pp -> 'v pp -> ('k, 'v, 'c) t pp

  val pp_diff :
       data_equal:('v -> 'v -> bool)
    -> 'k pp
    -> 'v pp
    -> ('v * 'v) pp
    -> (('k, 'v, 'c) t * ('k, 'v, 'c) t) pp

  val equal_m__t :
       (module Compare_m)
    -> ('v -> 'v -> bool)
    -> ('k, 'v, 'c) t
    -> ('k, 'v, 'c) t
    -> bool

  val find_and_remove : ('k, 'v, 'c) t -> 'k -> ('v * ('k, 'v, 'c) t) option

  val find_or_add :
       ('k, 'v, 'c) t
    -> 'k
    -> default:'v
    -> if_found:('v -> 'a)
    -> if_added:(('k, 'v, 'c) t -> 'a)
    -> 'a

  val map_preserving_phys_equal :
    ('k, 'v, 'c) t -> f:('v -> 'v) -> ('k, 'v, 'c) t
  (** Like map, but preserves [phys_equal] if [f] preserves [phys_equal] of
      every element. *)
end

module Result : sig
  include module type of Base.Result

  val pp : ('a_pp -> 'a -> unit, unit) fmt -> 'a_pp -> ('a, _) t pp
  (** Pretty-print a result. *)
end

module Vector : sig
  include module type of Vector

  val pp : (unit, unit) fmt -> 'a pp -> 'a t pp
  (** Pretty-print a vector. *)
end

include module type of Vector.Infix

module Set : sig
  include module type of Base.Set

  type ('e, 'c) tree

  val equal_m__t :
    (module Compare_m) -> ('elt, 'cmp) t -> ('elt, 'cmp) t -> bool

  val pp : 'e pp -> ('e, 'c) t pp
  val pp_diff : 'e pp -> (('e, 'c) t * ('e, 'c) t) pp
  val disjoint : ('e, 'c) t -> ('e, 'c) t -> bool
  val add_option : 'e option -> ('e, 'c) t -> ('e, 'c) t
  val add_list : 'e list -> ('e, 'c) t -> ('e, 'c) t
  val diff_inter : ('e, 'c) t -> ('e, 'c) t -> ('e, 'c) t * ('e, 'c) t

  val diff_inter_diff :
    ('e, 'c) t -> ('e, 'c) t -> ('e, 'c) t * ('e, 'c) t * ('e, 'c) t

  val of_vector : ('e, 'c) comparator -> 'e vector -> ('e, 'c) t
  val to_tree : ('e, 'c) t -> ('e, 'c) tree
end

module Qset : sig
  include module type of Qset

  val pp : (unit, unit) fmt -> ('a * Q.t) pp -> ('a, _) t pp
end

module Array : sig
  include module type of Base.Array

  val pp : (unit, unit) fmt -> 'a pp -> 'a array pp
end

module Q : sig
  include module type of struct include Q end

  val of_z : Z.t -> t
  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val pp : t pp
end

module Z : sig
  include module type of struct include Z end

  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val pp : t pp
  val true_ : t
  val false_ : t
  val of_bool : bool -> t
  val is_true : t -> bool
  val is_false : t -> bool
end
