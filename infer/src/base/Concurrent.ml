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

module type CacheS = sig
  module HQ : Hash_queue.S

  type 'a t

  val create : name:string -> 'a t

  val lookup : 'a t -> HQ.key -> 'a option

  val add : 'a t -> HQ.key -> 'a -> unit

  val remove : 'a t -> HQ.key -> unit

  val clear : 'a t -> unit

  val set_lru_mode : 'a t -> lru_limit:int option -> unit

  val with_hashqueue : ('a HQ.t -> unit) -> 'a t -> unit
end

module MakeCache (Key : sig
  type t [@@deriving compare, equal, hash, show, sexp]
end) : CacheS with type HQ.key = Key.t = struct
  module HQ = Hash_queue.Make (Key)

  type 'a t =
    {mutex: Error_checking_mutex.t; name: string; hq: 'a HQ.t; mutable lru_limit: int option}

  let create ~name = {name; mutex= Error_checking_mutex.create (); hq= HQ.create (); lru_limit= None}

  let in_mutex {mutex; hq} ~f = Error_checking_mutex.critical_section mutex ~f:(fun () -> f hq)

  let add t k v =
    in_mutex t ~f:(fun hq ->
        HQ.remove hq k |> ignore ;
        HQ.enqueue_front_exn hq k v ;
        match t.lru_limit with
        | None ->
            ()
        | Some limit ->
            let n = HQ.length hq - limit in
            if n > 0 then HQ.drop_back ~n hq )


  let lookup t k =
    in_mutex t ~f:(fun hq ->
        let result_opt = HQ.lookup_and_move_to_front hq k in
        if Option.is_some result_opt then Stats.add_cache_hit ~name:t.name
        else Stats.add_cache_miss ~name:t.name ;
        result_opt )


  let clear t = in_mutex t ~f:HQ.clear

  let remove t key = in_mutex t ~f:(fun h -> HQ.remove h key |> ignore)

  let set_lru_mode t ~lru_limit =
    in_mutex t ~f:(fun hq ->
        t.lru_limit <- lru_limit ;
        HQ.clear hq )


  let with_hashqueue f t = in_mutex t ~f
end
