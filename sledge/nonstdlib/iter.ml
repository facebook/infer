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

let mem elt seq ~eq = mem ~eq ~x:elt seq
let map seq ~f = map ~f seq
let flat_map seq ~f = flat_map ~f seq
let filter seq ~f = filter ~f seq
let sort seq ~cmp = sort ~cmp seq
let sort_uniq seq ~cmp = sort_uniq ~cmp seq
let sorted seq ~cmp = sorted ~cmp seq
let group_succ_by seq ~eq = group_succ_by ~eq seq
let group_by seq ~hash ~eq = group_by ~hash ~eq seq
let count seq ~hash ~eq = count ~hash ~eq seq
let uniq seq ~eq = uniq ~eq seq
let join_by ~eq ~hash k1 k2 ~merge = join_by ~eq ~hash k1 k2 ~merge
let join_all_by ~eq ~hash k1 k2 ~merge = join_all_by ~eq ~hash k1 k2 ~merge
let group_join_by ~eq ~hash = group_join_by ~eq ~hash
let inter ~eq ~hash = inter ~eq ~hash
let union ~eq ~hash = union ~eq ~hash
let diff ~eq ~hash = diff ~eq ~hash
let subset ~eq ~hash = subset ~eq ~hash
let max seq ~lt = max ~lt seq
let max_exn seq ~lt = max_exn ~lt seq
let min seq ~lt = min ~lt seq
let min_exn seq ~lt = min_exn ~lt seq

let pop seq =
  match head seq with Some x -> Some (x, drop 1 seq) | None -> None

let find_map seq ~f = find_map ~f seq
let find seq ~f = find (CCOpt.if_ f) seq
let find_exn seq ~f = CCOpt.get_exn (find ~f seq)
let fold seq init ~f = fold ~f:(fun s x -> f x s) ~init seq

let contains_dup (type elt) seq ~cmp =
  let module S = CCSet.Make (struct
    type t = elt

    let compare = cmp
  end) in
  let exception Found_dup in
  try
    fold seq S.empty ~f:(fun x elts ->
        let elts' = S.add x elts in
        if elts' == elts then raise_notrace Found_dup else elts' )
    |> ignore ;
    false
  with Found_dup -> true

let fold_opt seq s ~f =
  let state = ref s in
  let exception Stop in
  try
    seq (fun x ->
        match f x !state with
        | Some s -> state := s
        | None -> raise_notrace Stop ) ;
    Some !state
  with Stop -> None

let folding_map seq s ~f =
  fold_map
    ~f:(fun s x ->
      let y, s = f x s in
      (s, y) )
    ~init:s seq

let fold_map seq s ~f =
  let r = ref s in
  let seq' =
    persistent (fun yield ->
        seq (fun x ->
            let y, s = f x !r in
            r := s ;
            yield y ) )
  in
  (!r, seq')

let fold_until (type res) seq s ~f ~finish =
  let state = ref s in
  let exception Stop of res in
  try
    seq (fun x ->
        match f x !state with
        | `Continue s -> state := s
        | `Stop r -> raise_notrace (Stop r) ) ;
    finish !state
  with Stop r -> r

let fold_result (type s e) seq s ~f =
  let state = ref s in
  let exception Stop of (s, e) result in
  try
    seq (fun x ->
        match f x !state with
        | Ok s -> state := s
        | Error _ as e -> raise_notrace (Stop e) ) ;
    Ok !state
  with Stop e -> e
