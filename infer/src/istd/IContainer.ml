(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a singleton_or_more = Empty | Singleton of 'a | More

let singleton_or_more ~fold t =
  With_return.with_return (fun {return} ->
      fold t ~init:Empty ~f:(fun acc item ->
          match acc with Empty -> Singleton item | _ -> return More ) )


let mem_nth ~fold t index =
  With_return.with_return (fun {return} ->
      let _ : int =
        fold t ~init:index ~f:(fun index _ -> if index <= 0 then return true else index - 1)
      in
      false )


let forto excl ~init ~f =
  let rec aux excl ~f acc i = if i >= excl then acc else aux excl ~f (f acc i) (i + 1) in
  aux excl ~f init 0


let rev_map_to_list ~fold t ~f = fold t ~init:[] ~f:(fun acc item -> f item :: acc)

let rev_filter_map_to_list ~fold t ~f =
  fold t ~init:[] ~f:(fun acc item -> IList.opt_cons (f item) acc)
