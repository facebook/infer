(* This file is free software, part of iterator. See file "license" for more
   details. *)

(** {1 Simple and Efficient Iterators}

    Version of {!Iterator} with labels

    @since 0.5.5 *)

(** An iterator of values of type ['a]. If you give it a function
    ['a -> unit] it will be applied to every element of the iterator
    successively. *)
type +'a t = ('a -> unit) -> unit

type +'a iter = 'a t

(** {b NOTE} Type [('a, 'b) t2 = ('a -> 'b -> unit) -> unit] has been
    removed and subsumed by [('a * 'b) t]

    @since 1.0 *)

type 'a equal = 'a -> 'a -> bool
type 'a hash = 'a -> int

(** {2 Creation} *)

val from_iter : (('a -> unit) -> unit) -> 'a t
(** Build an iterator from a iter function *)

val from_labelled_iter : (f:('a -> unit) -> unit) -> 'a t
(** Build an iterator from a labelled iter function

    @since 1.2 *)

val from_fun : (unit -> 'a option) -> 'a t
(** Call the function repeatedly until it returns None. This iterator is
    transient, use {!persistent} if needed! *)

val empty : 'a t
(** Empty iterator. It contains no element. *)

val singleton : 'a -> 'a t
(** Singleton iterator, with exactly one element. *)

val doubleton : 'a -> 'a -> 'a t
(** Iterator with exactly two elements *)

val init : f:(int -> 'a) -> 'a t
(** [init f] is the infinite iterator [f 0; f 1; f 2; …].

    @since 0.9 *)

val cons : 'a -> 'a t -> 'a t
(** [cons x l] yields [x], then yields from [l]. Same as
    [append (singleton x) l] *)

val snoc : 'a t -> 'a -> 'a t
(** Same as {!cons} but yields the element after iterating on [l] *)

val return : 'a -> 'a t
(** Synonym to {!singleton} *)

val pure : 'a -> 'a t
(** Synonym to {!singleton} *)

val repeat : 'a -> 'a t
(** Infinite iterator of the same element. You may want to look at {!take}
    and the likes if you iterate on it. *)

val iterate : ('a -> 'a) -> 'a -> 'a t
(** [iterate f x] is the infinite iterator [x, f(x), f(f(x)), ...] *)

val forever : (unit -> 'b) -> 'b t
(** Iterator that calls the given function to produce elements. The iterator
    may be transient (depending on the function), and definitely is
    infinite. You may want to use {!take} and {!persistent}. *)

val cycle : 'a t -> 'a t
(** Cycle forever through the given iterator. Assume the given iterator can
    be traversed any amount of times (not transient). This yields an
    infinite iterator, you should use something like {!take} not to loop
    forever. *)

(** {2 Consumption} *)

val iter : f:('a -> unit) -> 'a t -> unit
(** Consume the iterator, passing all its arguments to the function.
    Basically [iter f seq] is just [seq f]. *)

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
(** Iterate on elements and their index in the iterator *)

val for_each : seq:'a t -> ('a -> unit) -> unit
(** Consume the iterator, passing all its arguments to the function.
    [for_each seq f] is the same as [iter f seq], i.e., [iter] with
    arguments reversed.

    @since 1.4 *)

val for_eachi : seq:'a t -> (int -> 'a -> unit) -> unit
(** Iterate on elements and their index in the iterator. [for_eachi seq f]
    is the same as [iteri f seq], i.e., [iteri] with arguments reversed.

    @since 1.4 *)

val fold : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
(** Fold over elements of the iterator, consuming it *)

val foldi : f:('a -> int -> 'b -> 'a) -> init:'a -> 'b t -> 'a
(** Fold over elements of the iterator and their index, consuming it *)

val fold_map : f:('acc -> 'a -> 'acc * 'b) -> init:'acc -> 'a t -> 'b t
(** [fold_map f acc l] is like {!map}, but it carries some state as in
    {!fold}. The state is not returned, it is just used to thread some
    information to the map function.

    @since 0.9 *)

val fold_filter_map :
  f:('acc -> 'a -> 'acc * 'b option) -> init:'acc -> 'a t -> 'b t
(** [fold_filter_map f acc l] is a {!fold_map}-like function, but the
    function can choose to skip an element by retuning [None].

    @since 0.9 *)

val map : f:('a -> 'b) -> 'a t -> 'b t
(** Map objects of the iterator into other elements, lazily *)

val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
(** Map objects, along with their index in the iterator *)

val map_by_2 : f:('a -> 'a -> 'a) -> 'a t -> 'a t
(** Map objects two by two. lazily. The last element is kept in the iterator
    if the count is odd.

    @since 0.7 *)

val for_all : f:('a -> bool) -> 'a t -> bool
(** Do all elements satisfy the predicate? *)

val exists : f:('a -> bool) -> 'a t -> bool
(** Exists there some element satisfying the predicate? *)

val mem : ?eq:('a -> 'a -> bool) -> x:'a -> 'a t -> bool
(** Is the value a member of the iterator?

    @param eq the equality predicate to use (default [(=)])
    @since 0.5 *)

val find : ('a -> 'b option) -> 'a t -> 'b option
(** Find the first element on which the function doesn't return [None]

    @since 0.5 *)

val find_map : f:('a -> 'b option) -> 'a t -> 'b option
(** Alias to {!find}

    @since 0.10 *)

val findi : f:(int -> 'a -> 'b option) -> 'a t -> 'b option
(** Indexed version of {!find}

    @since 0.9 *)

val find_mapi : f:(int -> 'a -> 'b option) -> 'a t -> 'b option
(** Alias to {!findi}

    @since 0.10 *)

val find_pred : f:('a -> bool) -> 'a t -> 'a option
(** [find_pred p l] finds the first element of [l] that satisfies [p], or
    returns [None] if no element satisfies [p]

    @since 0.9 *)

val find_pred_exn : f:('a -> bool) -> 'a t -> 'a
(** Unsafe version of {!find_pred}

    @raise Not_found if no such element is found
    @since 0.9 *)

val length : 'a t -> int
(** How long is the iterator? Forces the iterator. *)

val is_empty : 'a t -> bool
(** Is the iterator empty? Forces the iterator. *)

(** {2 Transformation} *)

val filter : f:('a -> bool) -> 'a t -> 'a t
(** Filter on elements of the iterator *)

val append : 'a t -> 'a t -> 'a t
(** Append two iterators. Iterating on the result is like iterating on the
    first, then on the second. *)

val append_l : 'a t list -> 'a t
(** Append iterators. Iterating on the result is like iterating on the each
    iterator of the list in order.

    @since 0.11 *)

val concat : 'a t t -> 'a t
(** Concatenate an iterator of iterators into one iterator. *)

val flatten : 'a t t -> 'a t
(** Alias for {!concat} *)

val flat_map : f:('a -> 'b t) -> 'a t -> 'b t
(** Alias to {!flatMap} with a more explicit name *)

val flat_map_l : f:('a -> 'b list) -> 'a t -> 'b t
(** Convenience function combining {!flat_map} and {!of_list}

    @since 0.9 *)

val seq_list : 'a t list -> 'a list t
(** [seq_list l] returns all the ways to pick one element in each
    sub-iterator in [l]. Assumes the sub-iterators can be iterated on
    several times.

    @since 0.11 *)

val seq_list_map : f:('a -> 'b t) -> 'a list -> 'b list t
(** [seq_list_map f l] maps [f] over every element of [l], then calls
    {!seq_list}

    @since 0.11 *)

val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
(** Map and only keep non-[None] elements Formerly [fmap] *)

val filter_mapi : f:(int -> 'a -> 'b option) -> 'a t -> 'b t
(** Map with indices, and only keep non-[None] elements

    @since 0.11 *)

val filter_count : f:('a -> bool) -> 'a t -> int
(** Count how many elements satisfy the given predicate

    @since 1.0 *)

val intersperse : x:'a -> 'a t -> 'a t
(** Insert the single element between every element of the iterator *)

val keep_some : 'a option t -> 'a t
(** [filter_some l] retains only elements of the form [Some x]. Same as
    [filter_map (fun x->x)]

    @since 1.0 *)

val keep_ok : ('a, _) Result.result t -> 'a t
(** [keep_ok l] retains only elements of the form [Ok x].

    @since 1.0 *)

val keep_error : (_, 'e) Result.result t -> 'e t
(** [keep_error l] retains only elements of the form [Error x].

    @since 1.0 *)

(** {2 Caching} *)

val persistent : 'a t -> 'a t
(** Iterate on the iterator, storing elements in an efficient internal
    structure.. The resulting iterator can be iterated on as many times as
    needed. {b Note}: calling persistent on an already persistent iterator
    will still make a new copy of the iterator! *)

val persistent_lazy : 'a t -> 'a t
(** Lazy version of {!persistent}. When calling [persistent_lazy s], a new
    iterator [s'] is immediately returned (without actually consuming [s])
    in constant time; the first time [s'] is iterated on, it also consumes
    [s] and caches its content into a inner data structure that will back
    [s'] for future iterations.

    {b warning}: on the first traversal of [s'], if the traversal is
    interrupted prematurely ({!take}, etc.) then [s'] will not be memorized,
    and the next call to [s'] will traverse [s] again. *)

(** {2 Misc} *)

val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** Sort the iterator. Eager, O(n) ram and O(n ln(n)) time. It iterates on
    elements of the argument iterator immediately, before it sorts them. *)

val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** Sort the iterator and remove duplicates. Eager, same as [sort] *)

val sorted : ?cmp:('a -> 'a -> int) -> 'a t -> bool
(** Checks whether the iterator is sorted. Eager, same as {!sort}.

    @since 0.9 *)

val group_succ_by : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
(** Group equal consecutive elements. Formerly synonym to [group].

    @since 0.6 *)

val group_by :
  ?hash:('a -> int) -> ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
(** Group equal elements, disregarding their order of appearance. The result
    iterator is traversable as many times as required. precondition: for any
    [x] and [y], if [eq x y] then [hash x=hash y] must hold.

    @since 0.6 *)

val count :
  ?hash:('a -> int) -> ?eq:('a -> 'a -> bool) -> 'a t -> ('a * int) t
(** Map each distinct element to its number of occurrences in the whole seq.
    Similar to [group_by seq |> map (fun l->List.hd l, List.length l)]

    @since 0.10 *)

val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** Remove consecutive duplicate elements. Basically this is like
    [fun seq -> map List.hd (group seq)]. *)

val product : 'a t -> 'b t -> ('a * 'b) t
(** Cartesian product of the iterators. When calling [product a b], the
    caller {b MUST} ensure that [b] can be traversed as many times as
    required (several times), possibly by calling {!persistent} on it
    beforehand. *)

val diagonal_l : 'a list -> ('a * 'a) t
(** All pairs of distinct positions of the list. [diagonal l] will return
    the iterator of all [List.nth i l, List.nth j l] if [i < j].

    @since 0.9 *)

val diagonal : 'a t -> ('a * 'a) t
(** All pairs of distinct positions of the iterator. Iterates only once on
    the iterator, which must be finite.

    @since 0.9 *)

val join : join_row:('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
(** [join ~join_row a b] combines every element of [a] with every element of
    [b] using [join_row]. If [join_row] returns None, then the two elements
    do not combine. Assume that [b] allows for multiple iterations. *)

val join_by :
     ?eq:'key equal
  -> ?hash:'key hash
  -> ('a -> 'key)
  -> ('b -> 'key)
  -> merge:('key -> 'a -> 'b -> 'c option)
  -> 'a t
  -> 'b t
  -> 'c t
(** [join key1 key2 ~merge] is a binary operation that takes two iterators
    [a] and [b], projects their elements resp. with [key1] and [key2], and
    combine values [(x,y)] from [(a,b)] with the same [key] using [merge].
    If [merge] returns [None], the combination of values is discarded.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must
    hold.

    @since 0.10 *)

val join_all_by :
     ?eq:'key equal
  -> ?hash:'key hash
  -> ('a -> 'key)
  -> ('b -> 'key)
  -> merge:('key -> 'a list -> 'b list -> 'c option)
  -> 'a t
  -> 'b t
  -> 'c t
(** [join_all_by key1 key2 ~merge] is a binary operation that takes two
    iterators [a] and [b], projects their elements resp. with [key1] and
    [key2], and, for each key [k] occurring in at least one of them:

    - compute the list [l1] of elements of [a] that map to [k]
    - compute the list [l2] of elements of [b] that map to [k]
    - call [merge k l1 l2]. If [merge] returns [None], the combination of
      values is discarded, otherwise it returns [Some c] and [c] is inserted
      in the result.

    @since 0.10 *)

val group_join_by :
     ?eq:'a equal
  -> ?hash:'a hash
  -> ('b -> 'a)
  -> 'a t
  -> 'b t
  -> ('a * 'b list) t
(** [group_join_by key2] associates to every element [x] of the first
    iterator, all the elements [y] of the second iterator such that
    [eq x (key y)]. Elements of the first iterators without corresponding
    values in the second one are mapped to [\[\]] precondition: for any [x]
    and [y], if [eq x y] then [hash x=hash y] must hold.

    @since 0.10 *)

val inter : ?eq:'a equal -> ?hash:'a hash -> 'a t -> 'a t -> 'a t
(** Intersection of two collections. Each element will occur at most once in
    the result. Eager. precondition: for any [x] and [y], if [eq x y] then
    [hash x=hash y] must hold.

    @since 0.10 *)

(*$=
  [2;4;5;6] (inter (1--6) (cons 2 (4--10)) |> sort |> to_list)
  [] (inter (0--5) (6--10) |> to_list)
*)

val union : ?eq:'a equal -> ?hash:'a hash -> 'a t -> 'a t -> 'a t
(** Union of two collections. Each element will occur at most once in the
    result. Eager. precondition: for any [x] and [y], if [eq x y] then
    [hash x=hash y] must hold.

    @since 0.10 *)

(*$=
  [2;4;5;6] (union (4--6) (cons 2 (4--5)) |> sort |> to_list)
*)

val diff : ?eq:'a equal -> ?hash:'a hash -> 'a t -> 'a t -> 'a t
(** Set difference. Eager.

    @since 0.10 *)

(*$=
  [1;2;8;9;10] (diff (1--10) (3--7) |> to_list)
*)

val subset : ?eq:'a equal -> ?hash:'a hash -> 'a t -> 'a t -> bool
(** [subset a b] returns [true] if all elements of [a] belong to [b]. Eager.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must
    hold.

    @since 0.10 *)

(*$T
  subset (2 -- 4) (1 -- 4)
  not (subset (1 -- 4) (2 -- 10))
*)

val unfoldr : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** [unfoldr f b] will apply [f] to [b]. If it yields [Some (x,b')] then [x]
    is returned and unfoldr recurses with [b']. *)

val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
(** Iterator of intermediate results *)

val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a option
(** Max element of the iterator, using the given comparison function.

    @return
      None if the iterator is empty, Some [m] where [m] is the maximal
      element otherwise *)

val max_exn : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
(** Unsafe version of {!max}

    @raise Not_found if the iterator is empty
    @since 0.10 *)

val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a option
(** Min element of the iterator, using the given comparison function. see
    {!max} for more details. *)

val min_exn : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
(** Unsafe version of {!min}

    @raise Not_found if the iterator is empty
    @since 0.10 *)

val sum : int t -> int
(** Sum of elements

    @since 0.11 *)

val sumf : float t -> float
(** Sum of elements, using Kahan summation

    @since 0.11 *)

val head : 'a t -> 'a option
(** First element, if any, otherwise [None]

    @since 0.5.1 *)

val head_exn : 'a t -> 'a
(** First element, if any, fails

    @raise Invalid_argument if the iterator is empty
    @since 0.5.1 *)

val take : int -> 'a t -> 'a t
(** Take at most [n] elements from the iterator. Works on infinite
    iterators. *)

val take_while : f:('a -> bool) -> 'a t -> 'a t
(** Take elements while they satisfy the predicate, then stops iterating.
    Will work on an infinite iterator [s] if the predicate is false for at
    least one element of [s]. *)

val fold_while :
  f:('a -> 'b -> 'a * [`Stop | `Continue]) -> init:'a -> 'b t -> 'a
(** Folds over elements of the iterator, stopping early if the accumulator
    returns [('a, `Stop)]

    @since 0.5.5 *)

val drop : int -> 'a t -> 'a t
(** Drop the [n] first elements of the iterator. Lazy. *)

val drop_while : f:('a -> bool) -> 'a t -> 'a t
(** Predicate version of {!drop} *)

val rev : 'a t -> 'a t
(** Reverse the iterator. O(n) memory and time, needs the iterator to be
    finite. The result is persistent and does not depend on the input being
    repeatable. *)

val zip_i : 'a t -> (int * 'a) t
(** Zip elements of the iterator with their index in the iterator.

    @since 1.0 Changed type to just give an iterator of pairs *)

val fold2 : f:('c -> 'a -> 'b -> 'c) -> init:'c -> ('a * 'b) t -> 'c
val iter2 : f:('a -> 'b -> unit) -> ('a * 'b) t -> unit
val map2 : f:('a -> 'b -> 'c) -> ('a * 'b) t -> 'c t

val map2_2 :
  f:('a -> 'b -> 'c) -> ('a -> 'b -> 'd) -> ('a * 'b) t -> ('c * 'd) t
(** [map2_2 f g seq2] maps each [x, y] of seq2 into [f x y, g x y] *)

(** {2 Data structures converters} *)

val to_list : 'a t -> 'a list
(** Convert the iterator into a list. Preserves order of elements. This
    function is tail-recursive, but consumes 2*n memory. If order doesn't
    matter to you, consider {!to_rev_list}. *)

val to_rev_list : 'a t -> 'a list
(** Get the list of the reversed iterator (more efficient than {!to_list}) *)

val of_list : 'a list -> 'a t

val on_list : ('a t -> 'b t) -> 'a list -> 'b list
(** [on_list f l] is equivalent to [to_list @@ f @@ of_list l].

    @since 0.5.2 *)

val pair_with_idx : 'a t -> (int * 'a) t
(** Similar to {!zip_i} but returns a normal iterator of tuples

    @since 0.11 *)

val to_opt : 'a t -> 'a option
(** Alias to {!head}

    @since 0.5.1 *)

val to_array : 'a t -> 'a array
(** Convert to an array. Currently not very efficient because an
    intermediate list is used. *)

val of_array : 'a array -> 'a t

val of_array_i : 'a array -> (int * 'a) t
(** Elements of the array, with their index *)

val array_slice : 'a array -> int -> int -> 'a t
(** [array_slice a i j] Iterator of elements whose indexes range from [i] to
    [j] *)

val of_opt : 'a option -> 'a t
(** Iterate on 0 or 1 values.

    @since 0.5.1 *)

val of_seq : 'a Seq.t -> 'a t
(** Iterator of elements of a {!Seq.t}.

    @since 1.5 *)

val to_seq_persistent : 'a t -> 'a Seq.t
(** Convert to a {!Seq}. Linear in memory and time (a copy is made in
    memory). This does not work on infinite iterators.

    @since 1.5 *)

val to_stack : 'a Stack.t -> 'a t -> unit
(** Push elements of the iterator on the stack *)

val of_stack : 'a Stack.t -> 'a t
(** Iterator of elements of the stack (same order as [Stack.iter]) *)

val to_queue : 'a Queue.t -> 'a t -> unit
(** Push elements of the iterator into the queue *)

val of_queue : 'a Queue.t -> 'a t
(** Iterator of elements contained in the queue, FIFO order *)

val hashtbl_add : ('a, 'b) Hashtbl.t -> ('a * 'b) t -> unit
(** Add elements of the iterator to the hashtable, with Hashtbl.add *)

val hashtbl_replace : ('a, 'b) Hashtbl.t -> ('a * 'b) t -> unit
(** Add elements of the iterator to the hashtable, with Hashtbl.replace
    (erases conflicting bindings) *)

val to_hashtbl : ('a * 'b) t -> ('a, 'b) Hashtbl.t
(** Build a hashtable from an iterator of key/value pairs *)

val of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) t
(** Iterator of key/value pairs from the hashtable *)

val hashtbl_keys : ('a, 'b) Hashtbl.t -> 'a t
val hashtbl_values : ('a, 'b) Hashtbl.t -> 'b t
val of_str : string -> char t
val to_str : char t -> string

val concat_str : string t -> string
(** Concatenate strings together, eagerly. Also see {!intersperse} to add a
    separator.

    @since 0.5 *)

(** Raised when the user tries to iterate several times on a transient
    iterator *)
exception OneShotSequence

val of_in_channel : in_channel -> char t
(** Iterates on characters of the input (can block when one iterates over
    the iterator). If you need to iterate several times on this iterator,
    use {!persistent}.

    @raise OneShotIterator when used more than once. *)

val to_buffer : char t -> Buffer.t -> unit
(** Copy content of the iterator into the buffer *)

val int_range : start:int -> stop:int -> int t
(** Iterator on integers in [start...stop] by steps 1. Also see {!(--)} for
    an infix version. *)

val int_range_dec : start:int -> stop:int -> int t
(** Iterator on decreasing integers in [stop...start] by steps -1. See
    {!(--^)} for an infix version *)

val int_range_by : step:int -> start:int -> stop:int -> int t
(** [int_range_by ~step ~start:i ~stop:j] is the range starting at [i],
    including [j], where the difference between successive elements is
    [step]. use a negative [step] for a decreasing iterator.

    @since 0.9
    @raise Invalid_argument if [step=0] *)

val bools : bool t
(** Iterates on [true] and [false]

    @since 0.9 *)

val of_set : (module Set.S with type elt = 'a and type t = 'b) -> 'b -> 'a t
(** Convert the given set to an iterator. The set module must be provided. *)

val to_set : (module Set.S with type elt = 'a and type t = 'b) -> 'a t -> 'b
(** Convert the iterator to a set, given the proper set module *)

type 'a gen = unit -> 'a option

val of_gen : 'a gen -> 'a t
(** Traverse eagerly the generator and build an iterator from it *)

val of_gen_once : 'a gen -> 'a t
(** One shot iterator using this generator. It must not be traversed twice.

    @since 1.5 *)

val to_gen : 'a t -> 'a gen
(** Make the iterator persistent (O(n)) and then iterate on it. Eager. *)

(** {3 Sets} *)

module Set : sig
  module type S = sig
    include Set.S

    val of_iter : elt iter -> t
    val to_iter : t -> elt iter
    val to_list : t -> elt list
    val of_list : elt list -> t

    val of_seq : elt iter -> t
    (** @deprecated use {!of_iter} instead *)

    val to_seq : t -> elt iter
    (** @deprecated use {!to_iter} instead *)
  end

  (** Create an enriched Set module from the given one *)
  module Adapt (X : Set.S) : S with type elt = X.elt and type t = X.t

  (** Functor to build an extended Set module from an ordered type *)
  module Make (X : Set.OrderedType) : S with type elt = X.t
end

(** {3 Maps} *)

module Map : sig
  module type S = sig
    include Map.S

    val to_iter : 'a t -> (key * 'a) iter
    val of_iter : (key * 'a) iter -> 'a t
    val keys : 'a t -> key iter
    val values : 'a t -> 'a iter
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t

    val to_seq : 'a t -> (key * 'a) iter
    (** @deprecated use {!to_iter} instead *)

    val of_seq : (key * 'a) iter -> 'a t
    (** @deprecated use {!of_iter} instead *)
  end

  (** Adapt a pre-existing Map module to make it iterator-aware *)
  module Adapt (M : Map.S) : S with type key = M.key and type 'a t = 'a M.t

  (** Create an enriched Map module, with iterator-aware functions *)
  module Make (V : Map.OrderedType) : S with type key = V.t
end

(** {2 Random iterators} *)

val random_int : int -> int t
(** Infinite iterator of random integers between 0 and the given higher
    bound (see Random.int) *)

val random_bool : bool t
(** Infinite iterator of random bool values *)

val random_float : float -> float t

val random_array : 'a array -> 'a t
(** Iterator of choices of an element in the array *)

val random_list : 'a list -> 'a t
(** Infinite iterator of random elements of the list. Basically the same as
    {!random_array}. *)

val shuffle : 'a t -> 'a t
(** [shuffle seq] returns a perfect shuffle of [seq]. Uses O(length seq)
    memory and time. Eager.

    @since 0.7 *)

val shuffle_buffer : n:int -> 'a t -> 'a t
(** [shuffle_buffer n seq] returns an iterator of element of [seq] in random
    order. The shuffling is not uniform. Uses O(n) memory.

    The first [n] elements of the iterator are consumed immediately. The
    rest is consumed lazily.

    @since 0.7 *)

(** {3 Sampling} *)

val sample : n:int -> 'a t -> 'a array
(** [sample n seq] returns k samples of [seq], with uniform probability. It
    will consume the iterator and use O(n) memory.

    It returns an array of size [min (length seq) n].

    @since 0.7 *)

(** {2 Infix functions} *)

module Infix : sig
  val ( -- ) : int -> int -> int t
  (** [a -- b] is the range of integers from [a] to [b], both included, in
      increasing order. It will therefore be empty if [a > b]. *)

  val ( --^ ) : int -> int -> int t
  (** [a --^ b] is the range of integers from [b] to [a], both included, in
      decreasing order (starts from [a]). It will therefore be empty if
      [a < b]. *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind (infix version of {!flat_map}

      @since 0.5 *)

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix version of {!map}

      @since 0.5 *)

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  (** Applicative operator (product+application)

      @since 0.5 *)

  val ( <+> ) : 'a t -> 'a t -> 'a t
  (** Concatenation of iterators

      @since 0.5 *)
end

include module type of Infix

(** {2 Pretty printing} *)

val pp_seq :
     ?sep:string
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit
(** Pretty print an iterator of ['a], using the given pretty printer to
    print each elements. An optional separator string can be provided. *)

val pp_buf :
  ?sep:string -> (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a t -> unit
(** Print into a buffer *)

val to_string : ?sep:string -> ('a -> string) -> 'a t -> string
(** Print into a string *)

(** {2 Basic IO}

    Very basic interface to manipulate files as iterator of chunks/lines.
    The iterators take care of opening and closing files properly; every
    time one iterates over an iterator, the file is opened/closed again.

    Example: copy a file ["a"] into file ["b"], removing blank lines:

    {[
      Iterator.(
        IO.lines_of "a" |> filter (fun l -> l <> "") |> IO.write_lines "b")
    ]}

    By chunks of [4096] bytes:

    {[
      Iterator.IO.(chunks_of ~size:4096 "a" |> write_to "b")
    ]}

    Read the lines of a file into a list:

    {[
      Iterator.IO.lines "a" |> Iterator.to_list
    ]}
    @since 0.5.1 *)

module IO : sig
  val lines_of : ?mode:int -> ?flags:open_flag list -> string -> string t
  (** [lines_of filename] reads all lines of the given file. It raises the
      same exception as would opening the file and read from it, except from
      [End_of_file] (which is caught). The file is {b always} properly
      closed. Every time the iterator is iterated on, the file is opened
      again, so different iterations might return different results

      @param mode default [0o644]
      @param flags default: [\[Open_rdonly\]] *)

  val chunks_of :
    ?mode:int -> ?flags:open_flag list -> ?size:int -> string -> string t
  (** Read chunks of the given [size] from the file. The last chunk might be
      smaller. Behaves like {!lines_of} regarding errors and options. Every
      time the iterator is iterated on, the file is opened again, so
      different iterations might return different results *)

  val write_to :
    ?mode:int -> ?flags:open_flag list -> string -> string t -> unit
  (** [write_to filename seq] writes all strings from [seq] into the given
      file. It takes care of opening and closing the file.

      @param mode default [0o644]
      @param flags
        used by [open_out_gen]. Default: [\[Open_creat;Open_wronly\]]. *)

  val write_bytes_to :
    ?mode:int -> ?flags:open_flag list -> string -> Bytes.t t -> unit
  (** @since 0.5.4 *)

  val write_lines :
    ?mode:int -> ?flags:open_flag list -> string -> string t -> unit
  (** Same as {!write_to}, but intercales ['\n'] between each string *)

  val write_bytes_lines :
    ?mode:int -> ?flags:open_flag list -> string -> Bytes.t t -> unit
  (** @since 0.5.4 *)
end
