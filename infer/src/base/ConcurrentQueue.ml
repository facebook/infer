(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

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
