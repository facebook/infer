(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'c for_child_info = {child_slot: int; child_id: 'c; is_first_update: bool}

type ('a, 'b, 'c) t =
  { remaining_tasks: unit -> int
  ; is_empty: unit -> bool
  ; finished: result:'b option -> 'a -> unit
  ; next: 'c for_child_info -> ('a * (unit -> unit)) option }

let chain (gen1 : ('a, 'b, 'c) t) (gen2 : ('a, 'b, 'c) t) : ('a, 'b, 'c) t =
  let remaining_tasks () = gen1.remaining_tasks () + gen2.remaining_tasks () in
  let gen1_returned_empty = ref false in
  let gen1_is_empty () =
    gen1_returned_empty := !gen1_returned_empty || gen1.is_empty () ;
    !gen1_returned_empty
  in
  let is_empty () = gen1_is_empty () && gen2.is_empty () in
  let finished ~result work_item =
    if gen1_is_empty () then gen2.finished ~result work_item else gen1.finished ~result work_item
  in
  let next for_child_info =
    if gen1_is_empty () then gen2.next for_child_info else gen1.next for_child_info
  in
  {remaining_tasks; is_empty; finished; next}


let of_list ~finish (lst : 'a list) : ('a, _, _) t =
  let content = ref lst in
  let length = ref (List.length lst) in
  let remaining_tasks () = !length in
  let is_empty () = List.is_empty !content in
  let finished ~result work_item =
    match finish result work_item with
    | None ->
        decr length
    | Some task ->
        content := task :: !content
  in
  let next _for_child_info =
    match !content with
    | [] ->
        None
    | x :: xs ->
        content := xs ;
        Some (x, Fn.id)
  in
  {remaining_tasks; is_empty; finished; next}


let finish_always_none result _ = match result with Some _ -> assert false | None -> None
