(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

(** Hash tables *)

include HashTable_intf

module Make (Key : HashedType) = struct
  include CCHashtbl.Make [@inlined] (Key)

  let create ?(size = 0) () = create size
  let set tbl ~key ~data = replace tbl key data

  let add_multi tbl ~key ~data =
    update tbl ~k:key ~f:(fun _ -> function
      | None -> Some [data] | Some datas -> Some (data :: datas) )

  let update tbl key ~f = update tbl ~k:key ~f:(fun _ dat -> f dat)
  let find_exn = find
  let find = find_opt

  let find_or_add tbl key ~default =
    let found = ref None in
    update tbl key ~f:(function
      | None ->
          let v = default () in
          found := Some v ;
          Some v
      | Some v ->
          found := Some v ;
          Some v ) ;
    Option.get_exn !found

  let iteri tbl ~f = iter (fun key data -> f ~key ~data) tbl
  let fold tbl s ~f = fold (fun key data acc -> f ~key ~data acc) tbl s
end
