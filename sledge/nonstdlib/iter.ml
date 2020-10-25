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

let find_map seq ~f = find_map ~f seq
let find seq ~f = find (CCOpt.if_ f) seq
let find_exn seq ~f = CCOpt.get_exn (find ~f seq)

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

let fold_result (type s e) seq ~init ~f =
  let state = ref init in
  let exception Stop of (s, e) result in
  try
    seq (fun x ->
        match f !state x with
        | Ok s -> state := s
        | Error _ as e -> raise_notrace (Stop e) ) ;
    Ok !state
  with Stop e -> e
