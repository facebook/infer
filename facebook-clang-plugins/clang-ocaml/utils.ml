(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module H = Hashtbl

(* misc *)
(* maps '-' to the standard input *)
let open_in name = if name = "-" then stdin else Stdlib.open_in name

(* maps '-' to the standard output *)
let open_out name = if name = "-" then stdout else Stdlib.open_out name

let make_cached f =
  let h = H.create 10 in
  function
  | x -> (
    try H.find h x
    with Not_found ->
      let y = f x in
      H.add h x y ;
      y )


(* missing string API *)

let string_starts_with s1 s2 =
  try
    let n = String.length s2 in
    String.sub s1 0 n = s2
  with Invalid_argument _ -> false


let string_ends_with s1 s2 =
  try
    let n = String.length s2 in
    String.sub s1 (String.length s1 - n) n = s2
  with Invalid_argument _ -> false


let string_split c s =
  let len = String.length s in
  let rec aux acc pos =
    if pos >= len then "" :: acc
    else
      try
        let next = String.index_from s pos c in
        aux (String.sub s pos (next - pos) :: acc) (next + 1)
      with Not_found -> String.sub s pos (String.length s - pos) :: acc
  in
  List.rev (aux [] 0)


let string_join c l = String.concat (String.make 1 c) l

(* lists *)

let rec list_starts_with l1 l2 =
  match (l1, l2) with
  | _, [] ->
      true
  | x1 :: q1, x2 :: q2 when x1 = x2 ->
      list_starts_with q1 q2
  | _ ->
      false


let list_ends_with l1 l2 = list_starts_with (List.rev l1) (List.rev l2)

(* missing stream API *)

let line_stream_of_channel channel =
  Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)


let stream_concat streams =
  let current_stream = ref None in
  let rec next i =
    try
      let stream =
        match !current_stream with
        | Some stream ->
            stream
        | None ->
            let stream = Stream.next streams in
            current_stream := Some stream ;
            stream
      in
      try Some (Stream.next stream)
      with Stream.Failure ->
        current_stream := None ;
        next i
    with Stream.Failure -> None
  in
  Stream.from next


let stream_append s1 s2 = stream_concat (Stream.of_list [s1; s2])

let stream_map f stream =
  let rec next _ = try Some (f (Stream.next stream)) with Stream.Failure -> None in
  Stream.from next


let stream_filter p stream =
  let rec next i =
    try
      let value = Stream.next stream in
      if p value then Some value else next i
    with Stream.Failure -> None
  in
  Stream.from next


let stream_fold f init stream =
  let result = ref init in
  Stream.iter (fun x -> result := f x !result) stream ;
  !result


let stream_to_list s = List.rev (stream_fold (fun x l -> x :: l) [] s)

(* simplistic unit testing *)

let string_counters = Hashtbl.create 10

let assert_true s b =
  ( try
      let i = Hashtbl.find string_counters s in
      Hashtbl.replace string_counters s (i + 1)
    with Not_found -> Hashtbl.add string_counters s 1 ) ;
  if not b then (
    Printf.fprintf stderr "%s (%d)\n" s (Hashtbl.find string_counters s) ;
    exit 1 )
  else ()


let assert_false s b = assert_true s (not b)

let assert_equal s x y = assert_true s (x = y)

(* union-find data structure *)

module DisjointSet = struct
  type 'a bucket = {mutable parent: 'a; mutable rank: int}

  type 'a t = ('a, 'a bucket) Hashtbl.t

  let create () = Hashtbl.create 10

  let bucket t x =
    try Hashtbl.find t x
    with Not_found ->
      let b = {parent= x; rank= 0} in
      Hashtbl.add t x b ;
      b


  let rec find_bucket t x =
    let b = bucket t x in
    if b.parent = x then b
    else
      let b0 = find_bucket t b.parent in
      b.parent <- b0.parent ;
      b0


  let find t x = (find_bucket t x).parent

  let union t x y =
    let bx = find_bucket t x and by = find_bucket t y in
    if bx.parent <> by.parent then
      if bx.rank < by.rank then bx.parent <- by.parent
      else (
        by.parent <- bx.parent ;
        if bx.rank = by.rank then bx.rank <- bx.rank + 1 )


  let iter t f = Hashtbl.iter (fun x b -> f x (if x = b.parent then x else find t b.parent)) t
end

(* Helper for command line parsing with Arg *)

let fix_arg_spec l usage_msg =
  let result = ref [] in
  let usage () =
    Arg.usage !result usage_msg ;
    exit 0
  in
  let extra =
    [ ("-h", Arg.Unit usage, " Display this list of options.")
    ; ("-help", Arg.Unit usage, " ")
    ; ("--help", Arg.Unit usage, " ") ]
  in
  result := Arg.align (l @ extra) ;
  !result
