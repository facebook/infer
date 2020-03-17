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
          with module Option := Base.Option
           and module List := Base.List
          (* prematurely deprecated, remove and use Stdlib instead *)
           and module Filename := Base.Filename
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
include module type of Import0

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
module Option = Option
include module type of Option.Monad_infix
include module type of Option.Monad_syntax with type 'a t = 'a option
module List = List

module Vector : sig
  include module type of Vector

  val pp : (unit, unit) fmt -> 'a pp -> 'a t pp
  (** Pretty-print a vector. *)
end

include module type of Vector.Infix

module type OrderedType = sig
  type t

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
end

exception Duplicate

module Set : sig
  module type S = sig
    type elt
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : (Sexp.t -> elt) -> Sexp.t -> t
    val pp : elt pp -> t pp
    val pp_diff : elt pp -> (t * t) pp

    (* initial constructors *)
    val empty : t
    val of_ : elt -> t
    val of_option : elt option -> t
    val of_list : elt list -> t
    val of_vector : elt vector -> t

    (* constructors *)
    val add : t -> elt -> t
    val add_option : elt option -> t -> t
    val add_list : elt list -> t -> t
    val remove : t -> elt -> t
    val filter : t -> f:(elt -> bool) -> t
    val union : t -> t -> t
    val union_list : t list -> t
    val diff : t -> t -> t
    val inter : t -> t -> t
    val diff_inter : t -> t -> t * t

    (* queries *)
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val is_subset : t -> of_:t -> bool
    val disjoint : t -> t -> bool
    val max_elt : t -> elt option

    (* traversals *)
    val fold : t -> init:'s -> f:('s -> elt -> 's) -> 's
  end

  module Make (Elt : OrderedType) : S with type elt = Elt.t
end

module Map : sig
  module type S = sig
    type key
    type +'a t

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
    val t_of_sexp : (Sexp.t -> key) -> (Sexp.t -> 'a) -> Sexp.t -> 'a t
    val pp : key pp -> 'a pp -> 'a t pp

    val pp_diff :
         data_equal:('a -> 'a -> bool)
      -> key pp
      -> 'a pp
      -> ('a * 'a) pp
      -> ('a t * 'a t) pp

    (* initial constructors *)
    val empty : 'a t

    (* constructors *)
    val set : 'a t -> key:key -> data:'a -> 'a t
    val add_exn : 'a t -> key:key -> data:'a -> 'a t
    val add_multi : 'a list t -> key:key -> data:'a -> 'a list t
    val remove : 'a t -> key -> 'a t
    val update : 'a t -> key -> f:('a option -> 'a) -> 'a t

    val merge :
         'a t
      -> 'b t
      -> f:
           (   key:key
            -> [`Both of 'a * 'b | `Left of 'a | `Right of 'b]
            -> 'c option)
      -> 'c t

    val merge_skewed :
      'a t -> 'a t -> combine:(key:key -> 'a -> 'a -> 'a) -> 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val filter_keys : 'a t -> f:(key -> bool) -> 'a t
    val filter_mapi : 'a t -> f:(key:key -> data:'a -> 'b option) -> 'b t

    (* queries *)
    val is_empty : 'b t -> bool
    val length : 'b t -> int
    val mem : 'a t -> key -> bool
    val find : 'a t -> key -> 'a option
    val find_and_remove : 'a t -> key -> ('a * 'a t) option
    val find_multi : 'a list t -> key -> 'a list
    val data : 'a t -> 'a list
    val to_alist : 'a t -> (key * 'a) list

    (* traversals *)
    val iter : 'a t -> f:('a -> unit) -> unit
    val iteri : 'a t -> f:(key:key -> data:'a -> unit) -> unit
    val for_alli : 'a t -> f:(key:key -> data:'a -> bool) -> bool
    val fold : 'a t -> init:'s -> f:(key:key -> data:'a -> 's -> 's) -> 's
  end

  module Make (Key : OrderedType) : S with type key = Key.t
end

module Qset : sig
  include module type of Qset

  val pp : (unit, unit) fmt -> ('a * Q.t) pp -> ('a, _) t pp
end

module Array : sig
  include module type of Base.Array

  val pp : (unit, unit) fmt -> 'a pp -> 'a array pp
end

module String : sig
  include module type of String

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t

  module Map : Map.S with type key = string
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
