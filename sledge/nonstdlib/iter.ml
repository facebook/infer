(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include IterLabels

module Import = struct
  type 'a iter = 'a t
end

let pop seq =
  match head seq with Some x -> Some (x, drop 1 seq) | None -> None

let contains_dup (type elt) seq ~cmp =
  let module S = CCSet.Make (struct
    type t = elt

    let compare = cmp
  end) in
  let exception Found_dup in
  try
    fold ~init:S.empty seq ~f:(fun elts x ->
        let elts' = S.add x elts in
        if elts' == elts then raise_notrace Found_dup else elts' )
    |> ignore ;
    false
  with Found_dup -> true

let fold_opt seq ~init ~f =
  let state = ref init in
  let exception Stop in
  try
    seq (fun x ->
        match f !state x with
        | Some s -> state := s
        | None -> raise_notrace Stop ) ;
    Some !state
  with Stop -> None

let fold_until (type res) seq ~init ~f ~finish =
  let state = ref init in
  let exception Stop of res in
  try
    seq (fun x ->
        match f !state x with
        | `Continue s -> state := s
        | `Stop r -> raise_notrace (Stop r) ) ;
    finish !state
  with Stop r -> r
