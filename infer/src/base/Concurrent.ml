(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Queue = struct
  type 'a t = {mutex: Error_checking_mutex.t; non_empty: Condition.t; queue: 'a Queue.t}

  let create ?capacity () =
    { mutex= Error_checking_mutex.create ()
    ; non_empty= Condition.create ()
    ; queue= Queue.create ?capacity () }


  let enqueue v t =
    Error_checking_mutex.critical_section t.mutex ~f:(fun () ->
        Queue.enqueue t.queue v ;
        Condition.signal t.non_empty )


  let dequeue t =
    let rec dequeue_loop () =
      match Queue.dequeue t.queue with
      | Some v ->
          v
      | None ->
          Condition.wait t.non_empty t.mutex ;
          dequeue_loop ()
    in
    Error_checking_mutex.critical_section t.mutex ~f:dequeue_loop


  let dequeue_opt t =
    Error_checking_mutex.critical_section t.mutex ~f:(fun () -> Queue.dequeue t.queue)


  let wait_until_non_empty t =
    let rec wait_until_non_empty_loop () =
      if Queue.is_empty t.queue then (
        Condition.wait t.non_empty t.mutex ;
        wait_until_non_empty_loop () )
    in
    Error_checking_mutex.critical_section t.mutex ~f:wait_until_non_empty_loop
end

module type Map = sig
  type key

  type 'a t

  val empty : unit -> 'a t

  val clear : 'a t -> unit

  val add : 'a t -> key -> 'a -> unit

  val filter : 'a t -> (key -> 'a -> bool) -> unit

  val find_opt : 'a t -> key -> 'a option

  val remove : 'a t -> key -> unit
end

module MakeMap (M : Stdlib.Map.S) : Map with type key = M.key = struct
  type key = M.key

  type 'a t = Error_checking_mutex.t * 'a M.t Atomic.t

  let empty () = (Error_checking_mutex.create (), Atomic.make M.empty)

  let update_in_mutex (mutex, a) ~f =
    Error_checking_mutex.critical_section mutex ~f:(fun () -> Atomic.get a |> f |> Atomic.set a)


  let clear (t : 'a t) = update_in_mutex t ~f:(fun _ -> M.empty)

  let filter (t : 'a t) f = update_in_mutex t ~f:(M.filter f)

  let add t k v = update_in_mutex t ~f:(M.add k v)

  let find_opt (_, atomic_map) key = Atomic.get atomic_map |> M.find_opt key

  let remove t key = update_in_mutex t ~f:(M.remove key)
end

module type Hashtbl = sig
  module Hash : Stdlib.Hashtbl.S

  type key

  type 'a t

  val create : int -> 'a t

  val clear : 'a t -> unit

  val find_opt : 'a t -> key -> 'a option

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val length : 'a t -> int

  val remove : 'a t -> key -> unit

  val replace : 'a t -> key -> 'a -> unit

  val with_hashtable : ('a Hash.t -> 'b) -> 'a t -> 'b

  val wrap_hashtable : 'a Hash.t -> 'a t
end

module MakeHashtbl (H : Stdlib.Hashtbl.S) : Hashtbl with type key = H.key with module Hash = H =
struct
  module Hash = H

  type key = H.key

  type 'a t = {mutex: Error_checking_mutex.t; hash: 'a H.t}

  let create size = {mutex= Error_checking_mutex.create (); hash= H.create size}

  let in_mutex {mutex; hash} ~f = Error_checking_mutex.critical_section mutex ~f:(fun () -> f hash)

  let clear t = in_mutex t ~f:H.clear

  let find_opt t key = in_mutex t ~f:(fun h -> H.find_opt h key)

  let fold f t init = in_mutex t ~f:(fun h -> H.fold f h init)

  let iter f t = in_mutex t ~f:(fun h -> H.iter f h)

  let length t = in_mutex t ~f:H.length

  let remove t key = in_mutex t ~f:(fun h -> H.remove h key)

  let replace t k v = in_mutex t ~f:(fun h -> H.replace h k v)

  let with_hashtable f t = in_mutex t ~f

  let wrap_hashtable hash = {mutex= Error_checking_mutex.create (); hash}
end
