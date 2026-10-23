(**************************************************************************)
(* ParMap: a simple library to perform Map computations on a multi-core   *)
(*                                                                        *)
(*  Author(s):  Marco Danelutto, Roberto Di Cosmo                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 2 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the LICENSE file for more information.                   *)
(**************************************************************************)

(** Module [Parmap]: efficient parallel map, fold and mapfold on lists and
    arrays on multicores.

    All the primitives allow to control the granularity of the parallelism
    via an optional parameter [chunksize]: if [chunksize] is omitted, the
    input sequence is split evenly among the available cores; if [chunksize]
    is specified, the input data is split in chunks of size [chunksize] and
    dispatched to the available cores using an on demand strategy that
    ensures automatic load balancing.

    A specific primitive {!array_float_parmap} is provided for fast operations on float arrays.
    *)

(** {1 Configuring available cores} *)

(** {2 Setting and getting the default value for ncores } *)

val set_default_ncores : int -> unit

val get_default_ncores : unit -> int

(** {2 Getting ncores being used during parallel execution } *)

val get_ncores : unit -> int

(** {2 Enabling/disabling processes core pinning } *)

val disable_core_pinning: unit -> unit
  (** [disable_core_pinning ()] will prevent forked out processes
      from being pinned to a specific core.
      WARNING: this may have a negative impact on performance,
      but might be necessary on systems where several parmap computations
      are running concurrently. *)

val enable_core_pinning: unit -> unit
(** [enable_core_pinning ()] turns on core pinning (it is on by default). *)

(** {2 Setting and getting an explicity mapping from processes to cores } *)

val set_core_mapping: int array -> unit
  (** [set_core_mapping m] installs the array [m] as the mapping to be used to pin
      processes to cores. Process [i] will be pinned to core [m.(i mod Array.length m)].
      *)

(** Getting the current worker rank.

    The master process has rank -1. Other processes
    have the rank at which they were forked out (a worker's rank is in [0..ncores-1])  *)
val get_rank : unit -> int

(** {1 Parallel map and folds} *)

type 'a sequence = L of 'a list | A of 'a array;;

(** {2 Generic operations} *)

(** The {!parmapfold}, {!parfold} and {!parmap} generic functions, for efficiency reasons,
    convert the input data into an array internally, so we provide the ['a sequence] type
    to allow passing an array directly as input.
    If you want to perform a parallel map operation on an array, use {!array_parmap} or {!array_float_parmap} instead.
    *)

(** The optional [init] (resp. [finalize]) function is called once by each child process just after creation
    (resp. just before exit).
    [init] and [finalize] both default to doing nothing.
    [init i] takes the child rank [i] as parameter (first forked child has rank 0, next 1, etc.).
*)

(** {3 Parallel mapfold} *)

val parmapfold : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ('a -> 'b) -> 'a sequence -> ('b-> 'c -> 'c) -> 'c -> ('c->'c->'c) -> 'c
  (** [parmapfold ~ncores:n f (L l) op b concat ] computes [List.fold_right op (List.map f l) b]
      by forking [n] processes on a multicore machine.
      You need to provide the extra [concat] operator to combine the partial results of the
      fold computed on each core. If 'b = 'c, then [concat] may be simply [op].
      The order of computation in parallel changes w.r.t. sequential execution, so this
      function is only correct if [op] and [concat] are associative and commutative.
      If the optional [chunksize] parameter is specified,
      the processes compute the result in an on-demand fashion
      on blocks of size [chunksize].
      [parmapfold ~ncores:n f (A a) op b concat ] computes [Array.fold_right op (Array.map f a) b]
      *)

(** {3 Parallel fold} *)

val parfold: ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ('a -> 'b -> 'b) -> 'a sequence -> 'b -> ('b->'b->'b) -> 'b
  (** [parfold ~ncores:n op (L l) b concat] computes [List.fold_right op l b]
      by forking [n] processes on a multicore machine.
      You need to provide the extra [concat] operator to combine the partial results of the
      fold computed on each core. If 'b = 'c, then [concat] may be simply [op].
      The order of computation in parallel changes w.r.t. sequential execution, so this
      function is only correct if [op] and [concat] are associative and commutative.
      If the optional [chunksize] parameter is specified,
      the processes compute the result in an on-demand fashion
      on blocks of size [chunksize]. 
      [parfold ~ncores:n op (A a) b concat] similarly computes [Array.fold_right op a b].
      *)

(** {3 Parallel map} *)

val parmap : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ?keeporder:bool-> ('a -> 'b) -> 'a sequence -> 'b list
  (** [parmap  ~ncores:n f (L l) ] computes [List.map f l]
      by forking [n] processes on a multicore machine.
      [parmap  ~ncores:n f (A a) ] computes [Array.map f a]
      by forking [n] processes on a multicore machine.
      If the optional [chunksize] parameter is specified,
      the processes compute the result in an on-demand fashion
      on blocks of size [chunksize]; this provides automatic
      load balancing for unbalanced computations, preserving
      the order of the results if [keeporder] is set to true. *)

(** {3 Parallel iteration} *)

val pariter : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ('a -> unit) -> 'a sequence -> unit
  (** [pariter  ~ncores:n f (L l) ] computes [List.iter f l]
      by forking [n] processes on a multicore machine.
      [parmap  ~ncores:n f (A a) ] computes [Array.iter f a]
      by forking [n] processes on a multicore machine.
      If the optional [chunksize] parameter is specified,
      the processes perform the computation in an on-demand fashion
      on blocks of size [chunksize]; this provides automatic
      load balancing for unbalanced computations. *)

(** {3 Parallel mapfold, indexed} *)

val parmapifold : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> (int -> 'a -> 'b) -> 'a sequence -> ('b-> 'c -> 'c) -> 'c -> ('c->'c->'c) -> 'c
  (** Like parmapfold, but the map function gets as an extra argument
      the index of the mapped element *)

(** {3 Parallel map, indexed} *)

val parmapi : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ?keeporder:bool -> (int -> 'a -> 'b) -> 'a sequence -> 'b list
  (** Like parmap, but the map function gets as an extra argument
      the index of the mapped element *)

(** {3 Parallel iteration, indexed} *)

val pariteri : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> (int -> 'a -> unit) -> 'a sequence -> unit
  (** Like pariter, but the iterated function gets as an extra argument
      the index of the sequence element *)

(** {3 Parallel map on arrays} *)

val array_parmap : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ?keeporder:bool -> ('a -> 'b) -> 'a array -> 'b array
  (** [array_parmap  ~ncores:n f a ] computes [Array.map f a]
      by forking [n] processes on a multicore machine.
      If the optional [chunksize] parameter is specified,
      the processes compute the result in an on-demand fashion
      on blochs of size [chunksize]; this provides automatic
      load balancing for unbalanced computations, preserving
      the order of the results if [keeporder] is set to true. *)

(** {3 Parallel map on arrays, indexed} *)

val array_parmapi : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ?keeporder:bool -> (int -> 'a -> 'b) -> 'a array -> 'b array
  (** Like array_parmap, but the map function gets as an extra argument
      the index of the mapped element *)

(** {2 Float array operations} *)

exception WrongArraySize

type buf

val init_shared_buffer : float array -> buf
  (** [init_shared_buffer a] creates a new memory mapped shared buffer big enough to hold a float array of the size of [a].
      This buffer can be reused in a series of calls to [array_float_parmap], avoiding the cost of reallocating it each time. *)

(** {3 Parallel map on float arrays } *)
                                        
val array_float_parmap : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ?result: float array -> ?sharedbuffer: buf -> ('a -> float) -> 'a array -> float array
  (** [array_float_parmap  ~ncores:n f a ] computes [Array.map f a] by forking
      [n] processes on a multicore machine, and preallocating the resulting
      array as shared memory, which allows significantly more efficient
      computation than calling the generic array_parmap function.  If the
      optional [chunksize] parameter is specified, the processes compute the
      result in an on-demand fashion on blochs of size [chunksize]; this
      provides automatic load balancing for unbalanced computations, *and* the
      order of the result is guaranteed to be preserved.

      In case you already have at hand an array where to store the result, you
      can squeeze out some more cpu cycles by passing it as optional parameter
      [result]: this will avoid the creation of a result array, which can be
      costly for very large data sets. Raises [WrongArraySize] if [result] is too
      small to hold the data.

      It is possible to share the same preallocated shared memory space across
      calls, by initialising the space calling [init_shared_buffer a] and
      passing the result as the optional [sharedbuffer] parameter to each
      subsequent call to [array_float_parmap].  Raises WrongArraySize if
      [sharedbuffer] is too small to hold the input data. *)

(** {3 Parallel map on float arrays, indexed } *)

val array_float_parmapi : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ?result: float array -> ?sharedbuffer: buf -> (int -> 'a -> float) -> 'a array -> float array
  (** Like array_float_parmap, but the map function gets as an extra argument
      the index of the mapped element *)

(** {1 Debugging and Helpers} *)

val debugging : bool -> unit
(** Enable or disable debugging code in the library; default: false *)

val redirect : ?path:string -> id:int -> unit
  (** Helper function that redirects stdout and stderr to files
      located in the directory [path], carrying names of the shape
      stdout.NNN and stderr.NNN where NNN is the [id] of the used core.
      Useful when writing initialisation functions to be passed as
      [init] argument to the parallel combinators.
      The default value for [path] is /tmp/.parmap.PPPP with PPPP the
      process id of the main program.
   *)
