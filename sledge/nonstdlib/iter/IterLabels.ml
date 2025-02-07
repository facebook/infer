(* This file is free software, part of iter. See file "license" for more
   details. *)

(** {1 Simple and Efficient Iterators} *)

module Array = Stdlib.Array
module Hashtbl = Stdlib.Hashtbl
module List = Stdlib.List
module Result = Stdlib.Result
module String = Stdlib.String

(** Iter abstract iterator type *)
type 'a t = ('a -> unit) -> unit

module Import = struct
  type 'a iter = 'a t
end

include Import

(*$inject
  let pp_ilist = Q.Print.(list int)
*)

type 'a equal = 'a -> 'a -> bool
type 'a hash = 'a -> int

(** Build an iterator from a iter function *)
let from_iter f = f

let from_labelled_iter iter f = iter ~f

let rec from_fun f k =
  match f () with
  | None -> ()
  | Some x ->
      k x ;
      from_fun f k

let[@inline] empty _ = ()

(*$R
  let seq = empty in
  OUnit.assert_bool "empty" (is_empty seq);
  OUnit.assert_bool "empty"
    (try iter (fun _ -> raise Exit) seq; true with Exit -> false);
*)

let[@inline] return x k = k x
let singleton = return
let pure = return

let[@inline] doubleton x y k =
  k x ;
  k y

let[@inline] cons x l k =
  k x ;
  l k

let[@inline] snoc l x k =
  l k ;
  k x

let[@inline] repeat x k =
  while true do
    k x
  done

(*$R
  let seq = repeat "hello" in
  OUnit.assert_equal ["hello"; "hello"; "hello"]
    (seq |> take 3 |> to_list);
*)

let init ~f yield =
  let rec aux i =
    yield (f i) ;
    aux (i + 1)
  in
  aux 0

(*$=
  [0;1;2;3;4] (init (fun x->x) |> take 5 |> to_list)
*)

let rec iterate x ~f k =
  k x ;
  iterate ~f (f x) k

let rec forever ~f k =
  k (f ()) ;
  forever ~f k

let cycle s k =
  while true do
    s k
  done

let[@inline] iter seq ~f = seq f

let iteri seq ~f =
  let r = ref 0 in
  seq (fun x ->
      f !r x ;
      incr r )

let for_each seq f = iter ~f seq
let for_eachi seq f = iteri ~f seq

let fold seq init ~f =
  let r = ref init in
  seq (fun elt -> r := f elt !r) ;
  !r

(*$R
  let n = (1 -- 10)
    |> fold (+) 0 in
  OUnit.assert_equal 55 n;
*)

let foldi seq init ~f =
  let i = ref 0 in
  let r = ref init in
  seq (fun elt ->
      r := f !i elt !r ;
      incr i ) ;
  !r

(*$R
  let l = ["hello"; "world"]
    |> of_list
    |> foldi (fun acc i x -> (i,x) :: acc) [] in
  OUnit.assert_equal [1, "world"; 0, "hello"] l;
*)

let folding_map seq init ~f yield =
  let r = ref init in
  seq (fun x ->
      let y, acc' = f x !r in
      r := acc' ;
      yield y )

(*$= & ~printer:Q.Print.(list int)
  [0;1;3;5] (0--3 |> fold_map (fun prev x -> x,prev+x) 0 |> to_list)
*)

let fold_filter_map seq init ~f yield =
  let r = ref init in
  seq (fun x ->
      let y, acc' = f x !r in
      r := acc' ;
      match y with None -> () | Some y' -> yield y' )

let[@inline] map seq ~f k = seq (fun x -> k (f x))

let[@inline] mapi seq ~f k =
  let i = ref 0 in
  seq (fun x ->
      k (f !i x) ;
      incr i )

let map_by_2 seq ~f k =
  let r = ref None in
  let f y = match !r with None -> r := Some y | Some x -> k (f x y) in
  seq f ;
  match !r with None -> () | Some x -> k x

let[@inline] filter seq ~f k = seq (fun x -> if f x then k x)

let[@inline] append s1 s2 k =
  s1 k ;
  s2 k

let[@inline] append_l l k = List.iter (fun sub -> sub k) l
let[@inline] concat s k = s (fun s' -> s' k)

(*$R
  let s1 = (1 -- 5) in
  let s2 = (6 -- 10) in
  let l = [1;2;3;4;5;6;7;8;9;10] in
  OUnit.assert_equal l (to_list (append s1 s2));
*)

(*$R
  (1 -- 1000)
    |> map (fun i -> i -- (i+1))
    |> concat
    |> length
    |> OUnit.assert_equal 2000
*)

let flatten = concat
let[@inline] flat_map seq ~f k = seq (fun x -> f x k)

(*$R
  (1 -- 1000)
    |> flat_map (fun i -> i -- (i+1))
    |> length
    |> OUnit.assert_equal 2000
*)

let[@inline] flat_map_l seq ~f k = seq (fun x -> List.iter k (f x))

let[@unroll 2] rec seq_list_map l ~f k =
  match l with
  | [] -> k []
  | x :: tail ->
      f x (fun x' -> seq_list_map ~f tail (fun tail' -> k (x' :: tail')))

let[@inline] seq_list l = seq_list_map ~f:(fun x -> x) l

(*$= & ~printer:Q.Print.(list @@ list int)
  [[1;2];[1;3]] (seq_list [singleton 1; doubleton 2 3] |> to_list)
  [] (seq_list [singleton 1; empty; doubleton 2 3] |> to_list)
  [[1;2;4];[1;3;4]] (seq_list [singleton 1; doubleton 2 3; singleton 4] |> to_list)
*)

let[@inline] filter_map seq ~f k =
  seq (fun x -> match f x with None -> () | Some y -> k y)

let filter_mapi seq ~f k =
  let i = ref 0 in
  seq (fun x ->
      let j = !i in
      incr i ;
      match f j x with None -> () | Some y -> k y )

let filter_count seq ~f =
  let i = ref 0 in
  seq (fun x -> if f x then incr i) ;
  !i

(*$Q
  Q.(list int) (fun l -> \
    let seq = of_list l and f x = x mod 2 = 0 in \
    filter_count f seq = (filter f seq |> length))
*)

let intersperse elem seq k =
  let first = ref true in
  seq (fun x ->
      if !first then first := false else k elem ;
      k x )

(*$R
  (1 -- 100)
    |> (fun seq -> intersperse 0 seq)
    |> take 10
    |> to_list
    |> OUnit.assert_equal [1;0;2;0;3;0;4;0;5;0]
*)

let keep_some seq k = seq (function Some x -> k x | None -> ())

let keep_ok seq k =
  seq (function Result.Ok x -> k x | Result.Error _ -> ())

let keep_error seq k =
  seq (function Result.Error x -> k x | Result.Ok _ -> ())

(** Mutable unrolled list to serve as intermediate storage *)
module MList = struct
  type 'a node = Nil | Cons of 'a array * int ref * 'a node ref

  (* build and call callback on every element *)
  let of_iter_with seq k =
    let start = ref Nil in
    let chunk_size = ref 8 in
    (* fill the list. prev: tail-reference from previous node *)
    let prev, cur = (ref start, ref Nil) in
    seq (fun x ->
        k x ;
        (* callback *)
        match !cur with
        | Nil ->
            let n = !chunk_size in
            if n < 4096 then chunk_size := 2 * !chunk_size ;
            cur := Cons (Array.make n x, ref 1, ref Nil)
        | Cons (a, n, next) ->
            assert (!n < Array.length a) ;
            a.(!n) <- x ;
            incr n ;
            if !n = Array.length a then (
              !prev := !cur ;
              prev := next ;
              cur := Nil ) ) ;
    !prev := !cur ;
    !start

  let of_iter seq = of_iter_with seq (fun _ -> ())

  let rec iter f l =
    match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
        for i = 0 to !n - 1 do
          f a.(i)
        done ;
        iter f !tl

  let iteri f l =
    let rec iteri i f l =
      match l with
      | Nil -> ()
      | Cons (a, n, tl) ->
          for j = 0 to !n - 1 do
            f (i + j) a.(j)
          done ;
          iteri (i + !n) f !tl
    in
    iteri 0 f l

  let rec iter_rev f l =
    match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
        iter_rev f !tl ;
        for i = !n - 1 downto 0 do
          f a.(i)
        done

  let length l =
    let rec len acc l =
      match l with Nil -> acc | Cons (_, n, tl) -> len (acc + !n) !tl
    in
    len 0 l

  (** Get element by index *)
  let rec get l i =
    match l with
    | Nil -> raise (Invalid_argument "MList.get")
    | Cons (a, n, _) when i < !n -> a.(i)
    | Cons (_, n, tl) -> get !tl (i - !n)

  let to_iter l k = iter k l

  let _to_next arg l =
    let cur = ref l in
    let i = ref 0 in
    (* offset in cons *)
    let rec get_next _ =
      match !cur with
      | Nil -> None
      | Cons (_, n, tl) when !i = !n ->
          cur := !tl ;
          i := 0 ;
          get_next arg
      | Cons (a, _, _) ->
          let x = a.(!i) in
          incr i ;
          Some x
    in
    get_next

  let to_gen l = _to_next () l

  let to_seq l =
    let rec make (l, i) () =
      match l with
      | Nil -> Seq.Nil
      | Cons (_, n, tl) when i = !n -> make (!tl, 0) ()
      | Cons (a, _, _) -> Seq.Cons (a.(i), make (l, i + 1))
    in
    make (l, 0)
end

let persistent seq =
  let l = MList.of_iter seq in
  MList.to_iter l

(*$R
  let printer = pp_ilist in
  let iter = of_gen_once (let i=ref (-1) in fun() -> incr i; if !i < 5 then Some !i else None) in
  (* consume iter into a persistent version of itself *)
  let iter' = persistent iter in
  OUnit.assert_raises OneShotSequence (fun () -> iter |> to_list);
  OUnit.assert_equal ~printer [0;1;2;3;4] (iter' |> to_list);
  OUnit.assert_equal ~printer [0;1;2;3;4] (iter' |> to_list);
  OUnit.assert_equal ~printer [0;1;2;3;4] (iter' |> to_seq_persistent |> of_seq |> to_list);
*)

(*$R
  let printer = pp_ilist in
  let iter = (0 -- 10_000) in
  let iter' = persistent iter in
  OUnit.assert_equal 10_001 (length iter');
  OUnit.assert_equal 10_001 (length iter');
  OUnit.assert_equal ~printer [0;1;2;3] (iter' |> take 4 |> to_list);
*)

type 'a lazy_state = LazySuspend | LazyCached of 'a t

let persistent_lazy (seq : 'a t) =
  let r = ref LazySuspend in
  fun k ->
    match !r with
    | LazyCached seq' -> seq' k
    | LazySuspend ->
        (* here if this traversal is interruted, no caching occurs *)
        let seq' = MList.of_iter_with seq k in
        r := LazyCached (MList.to_iter seq')

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

let sort seq ~cmp =
  (* use an intermediate list, then sort the list *)
  let l = fold ~f:(fun x l -> x :: l) seq [] in
  let l = List.fast_sort cmp l in
  fun k -> List.iter k l

(*$R
  (1 -- 100)
    |> sort ~cmp:(fun i j -> j - i)
    |> take 4
    |> to_list
    |> OUnit.assert_equal [100;99;98;97]
*)

exception Exit_sorted

let sorted seq ~cmp =
  let prev = ref None in
  try
    seq (fun x ->
        match !prev with
        | Some y when cmp y x > 0 -> raise_notrace Exit_sorted
        | _ -> prev := Some x ) ;
    true
  with Exit_sorted -> false

(*$T
  of_list [1;2;3;4] |> sorted
  not (of_list [1;2;3;0;4] |> sorted)
  sorted empty
*)

let group_succ_by seq ~eq k =
  let cur = ref [] in
  seq (fun x ->
      match !cur with
      | [] -> cur := [x]
      | y :: _ as l when eq x y ->
          cur := x :: l (* [x] belongs to the group *)
      | _ :: _ as l ->
          k l ;
          (* yield group, and start another one *)
          cur := [x] ) ;
  (* last list *)
  match !cur with [] -> () | _ :: _ as l -> k l

(*$R
  [1;2;3;3;2;2;3;4]
    |> of_list |> group_succ_by ?eq:None |> to_list
    |> OUnit.assert_equal [[1];[2];[3;3];[2;2];[3];[4]]
*)

let group_by (type k) seq ~hash ~eq =
  let module Tbl = Hashtbl.Make (struct
    type t = k

    let equal = eq
    let hash = hash
  end) in
  (* compute group table *)
  let tbl =
    lazy
      (let tbl = Tbl.create 32 in
       seq (fun x ->
           let l = try Tbl.find tbl x with Not_found -> [] in
           Tbl.replace tbl x (x :: l) ) ;
       tbl )
  in
  fun yield -> Tbl.iter (fun _ l -> yield l) (Lazy.force tbl)

(*$R
  [1;2;3;3;2;2;3;4]
    |> of_list |> group_by ?eq:None ?hash:None |> sort ?cmp:None |> to_list
    |> OUnit.assert_equal [[1];[2;2;2];[3;3;3];[4]]
*)

let count (type k) seq ~hash ~eq =
  let module Tbl = Hashtbl.Make (struct
    type t = k

    let equal = eq
    let hash = hash
  end) in
  (* compute group table *)
  let tbl =
    lazy
      (let tbl = Tbl.create 32 in
       seq (fun x ->
           let n = try Tbl.find tbl x with Not_found -> 0 in
           Tbl.replace tbl x (n + 1) ) ;
       tbl )
  in
  fun yield -> Tbl.iter (fun x n -> yield (x, n)) (Lazy.force tbl)

(*$R
  [1;2;3;3;2;2;3;4]
    |> of_list |> count ?eq:None ?hash:None |> sort ?cmp:None |> to_list
    |> OUnit.assert_equal [1,1;2,3;3,3;4,1]
*)

let uniq seq ~eq k =
  let has_prev = ref false and prev = ref (Obj.magic 0) in
  (* avoid option type, costly *)
  seq (fun x ->
      if !has_prev && eq !prev x then () (* duplicate *)
      else (
        has_prev := true ;
        prev := x ;
        k x ) )

(*$R
  [1;2;2;3;4;4;4;3;3]
    |> of_list |> uniq ?eq:None |> to_list
    |> OUnit.assert_equal [1;2;3;4;3]
*)

let sort_uniq (type elt) seq ~cmp =
  let module S = Stdlib.Set.Make (struct
    type t = elt

    let compare = cmp
  end) in
  let set = fold ~f:(fun x acc -> S.add x acc) seq S.empty in
  fun k -> S.iter k set

(*$R
  [42;1;2;3;4;5;4;3;2;1]
    |> of_list
    |> sort_uniq ?cmp:None
    |> to_list
    |> OUnit.assert_equal [1;2;3;4;5;42]
*)

exception Found_dup

let contains_dup (type elt) seq ~cmp =
  let module S = Stdlib.Set.Make (struct
    type t = elt

    let compare = cmp
  end) in
  try
    fold seq S.empty ~f:(fun x elts ->
        let elts' = S.add x elts in
        if elts' == elts then raise_notrace Found_dup else elts' )
    |> ignore ;
    false
  with Found_dup -> true

let[@inline] product outer inner k =
  outer (fun x -> inner (fun y -> k (x, y)))

(*$R
  let a = 0 -- 2 in
  let b = of_list ["a";"b";"c"] in
  let s = product a b |> map (fun (x,y) -> y,x)
    |> to_list |> List.sort compare in
  OUnit.assert_equal ["a",0; "a", 1; "a", 2;
                      "b",0; "b", 1; "b", 2;
                      "c",0; "c", 1; "c", 2;] s
*)

let rec diagonal_l l yield =
  match l with
  | [] -> ()
  | x :: tail ->
      List.iter (fun y -> yield (x, y)) tail ;
      diagonal_l tail yield

(*$=
  [0,1; 0,2; 1,2] (diagonal_l [0;1;2] |> to_list)
  *)

let diagonal seq =
  let l = ref [] in
  seq (fun x -> l := x :: !l) ;
  diagonal_l (List.rev !l)

(*$=
  [0,1; 0,2; 1,2] (of_list [0;1;2] |> diagonal |> to_list)
  *)

let join s1 s2 ~f:join_row k =
  s1 (fun a ->
      s2 (fun b -> match join_row a b with None -> () | Some c -> k c) )

(*$R
  let s1 = (1 -- 3) in
  let s2 = of_list ["1"; "2"] in
  let join_row i j =
    if string_of_int i = j then Some (string_of_int i ^ " = " ^ j) else None
  in
  let s = join ~join_row s1 s2 in
  OUnit.assert_equal ["1 = 1"; "2 = 2"] (to_list s);
*)

let join_by (type a) c1 c2 ~key1:f1 ~key2:f2 ~eq ~hash ~f:merge =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c1 (fun x ->
      let key = f1 x in
      Tbl.add tbl key x ) ;
  let res = ref [] in
  c2 (fun y ->
      let key = f2 y in
      let xs = Tbl.find_all tbl key in
      List.iter
        (fun x ->
          match merge key x y with None -> () | Some z -> res := z :: !res
          )
        xs ) ;
  fun yield -> List.iter yield !res

type ('a, 'b) join_all_cell =
  {mutable ja_left: 'a list; mutable ja_right: 'b list}

let join_all_by (type a) c1 c2 ~key1:f1 ~key2:f2 ~eq ~hash ~f:merge =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  (* build the map [key -> cell] *)
  c1 (fun x ->
      let key = f1 x in
      try
        let c = Tbl.find tbl key in
        c.ja_left <- x :: c.ja_left
      with Not_found -> Tbl.add tbl key {ja_left= [x]; ja_right= []} ) ;
  c2 (fun y ->
      let key = f2 y in
      try
        let c = Tbl.find tbl key in
        c.ja_right <- y :: c.ja_right
      with Not_found -> Tbl.add tbl key {ja_left= []; ja_right= [y]} ) ;
  let res = ref [] in
  Tbl.iter
    (fun key cell ->
      match merge key cell.ja_left cell.ja_right with
      | None -> ()
      | Some z -> res := z :: !res )
    tbl ;
  fun yield -> List.iter yield !res

let group_join_by (type a) c1 c2 ~eq ~hash ~f =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c1 (fun x -> Tbl.replace tbl x []) ;
  c2 (fun y ->
      (* project [y] into some element of [c1] *)
      let key = f y in
      try
        let l = Tbl.find tbl key in
        Tbl.replace tbl key (y :: l)
      with Not_found -> () ) ;
  fun yield -> Tbl.iter (fun k l -> yield (k, l)) tbl

(*$=
  ['a', ["abc"; "attic"]; \
   'b', ["barbary"; "boom"; "bop"]; \
   'c', []] \
  (group_join_by (fun s->s.[0]) \
    (of_str "abc") \
    (of_list ["abc"; "boom"; "attic"; "deleted"; "barbary"; "bop"]) \
  |> map (fun (c,l)->c,List.sort Stdlib.compare l) \
  |> sort |> to_list)
*)

let union (type a) ~eq ~hash c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c1 (fun x -> Tbl.replace tbl x ()) ;
  c2 (fun x -> Tbl.replace tbl x ()) ;
  fun yield -> Tbl.iter (fun x _ -> yield x) tbl

type inter_status = Inter_left | Inter_both

let inter (type a) ~eq ~hash c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c1 (fun x -> Tbl.replace tbl x Inter_left) ;
  c2 (fun x ->
      try
        match Tbl.find tbl x with
        | Inter_left -> Tbl.replace tbl x Inter_both
        (* save *)
        | Inter_both -> ()
      with Not_found -> () ) ;
  fun yield -> Tbl.iter (fun x res -> if res = Inter_both then yield x) tbl

let diff (type a) ~eq ~hash c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c2 (fun x -> Tbl.replace tbl x ()) ;
  fun yield -> c1 (fun x -> if not (Tbl.mem tbl x) then yield x)

exception Subset_exit

let subset (type a) ~eq ~hash c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c2 (fun x -> Tbl.replace tbl x ()) ;
  try
    c1 (fun x -> if not (Tbl.mem tbl x) then raise_notrace Subset_exit) ;
    true
  with Subset_exit -> false

let rec unfoldr b ~f k =
  match f b with
  | None -> ()
  | Some (x, b') ->
      k x ;
      unfoldr ~f b' k

(*$R
  let f x = if x < 5 then Some (string_of_int x,x+1) else None in
  unfoldr f 0
    |> to_list
    |> OUnit.assert_equal ["0"; "1"; "2"; "3"; "4"]
*)

let scan seq acc ~f k =
  k acc ;
  let acc = ref acc in
  seq (fun elt ->
      let acc' = f elt !acc in
      k acc' ;
      acc := acc' )

(*$R
  (1 -- 5)
    |> scan (+) 0
    |> to_list
    |> OUnit.assert_equal ~printer:pp_ilist [0;1;3;6;10;15]
*)

let max seq ~lt =
  let ret = ref None in
  seq (fun x ->
      match !ret with
      | None -> ret := Some x
      | Some y -> if lt y x then ret := Some x ) ;
  !ret

let max_exn seq ~lt =
  match max ~lt seq with Some x -> x | None -> raise_notrace Not_found

let min seq ~lt =
  let ret = ref None in
  seq (fun x ->
      match !ret with
      | None -> ret := Some x
      | Some y -> if lt x y then ret := Some x ) ;
  !ret

let min_exn seq ~lt =
  match min ~lt seq with Some x -> x | None -> raise Not_found

(*$= & ~printer:string_of_int
  100 (0 -- 100 |> max_exn ?lt:None)
  0 (0 -- 100 |> min_exn ?lt:None)
*)

let[@inline] sum seq =
  let n = ref 0 in
  seq (fun x -> n := !n + x) ;
  !n

(*$T
  (of_list [1;2;3] |> sum) = 6
*)

(* https://en.wikipedia.org/wiki/Kahan_summation_algorithm *)
let sumf seq : float =
  let sum = ref 0. in
  let c = ref 0. in
  (* error compensation *)
  seq (fun x ->
      let y = x -. !c in
      let t = !sum +. y in
      c := t -. !sum -. y ;
      sum := t ) ;
  !sum

(*$R
  let seq = of_list [10000.0; 3.14159; 2.71828] in
  assert_equal ~printer:string_of_float 10005.85987 (sumf seq)
*)

exception ExitHead

let head seq =
  let r = ref None in
  try
    seq (fun x ->
        r := Some x ;
        raise_notrace ExitHead ) ;
    None
  with ExitHead -> !r

let head_exn seq =
  match head seq with None -> invalid_arg "Iter.head_exn" | Some x -> x

exception ExitTake

let take n seq k =
  let count = ref 0 in
  try
    seq (fun x ->
        if !count = n then raise_notrace ExitTake ;
        incr count ;
        k x )
  with ExitTake -> ()

(*$R
  let l = to_list (take 0 (of_list [1])) in
  OUnit.assert_equal ~printer:pp_ilist [] l;
  let l = to_list (take 5 (of_list [1;2;3;4;5;6;7;8;9;10])) in
  OUnit.assert_equal ~printer:pp_ilist [1;2;3;4;5] l;
*)

exception ExitTakeWhile

let take_while seq ~f k =
  try seq (fun x -> if f x then k x else raise_notrace ExitTakeWhile)
  with ExitTakeWhile -> ()

exception ExitFoldWhile

let fold_while seq s ~f =
  let state = ref s in
  let consume x =
    let cont, acc = f x !state in
    state := acc ;
    match cont with `Stop -> raise_notrace ExitFoldWhile | `Continue -> ()
  in
  try
    seq consume ;
    !state
  with ExitFoldWhile -> !state

(*$R
  let n = of_list [true;true;false;true]
    |> fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 in
  OUnit.assert_equal 2 n;
*)

exception Stop_fold_opt

let fold_opt seq s ~f =
  let state = ref s in
  try
    seq (fun x ->
        match f x !state with
        | Some s -> state := s
        | None -> raise_notrace Stop_fold_opt ) ;
    Some !state
  with Stop_fold_opt -> None

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

let drop n seq k =
  let count = ref 0 in
  seq (fun x -> if !count >= n then k x else incr count)

(*$R
  (1 -- 5) |> drop 2 |> to_list |> OUnit.assert_equal [3;4;5]
*)

let drop_while seq ~f k =
  let drop = ref true in
  seq (fun x ->
      if !drop then
        if f x then ()
        else (
          drop := false ;
          k x )
      else k x )

let pop seq =
  match head seq with Some x -> Some (x, drop 1 seq) | None -> None

let pop_exn seq = (head_exn seq, drop 1 seq)

let reduce seq ~f =
  match pop seq with Some (x, seq) -> Some (fold seq x ~f) | None -> None

let reduce_exn seq ~f =
  let x, seq = pop_exn seq in
  fold seq x ~f

let rev seq =
  let l = MList.of_iter seq in
  fun k -> MList.iter_rev k l

(*$R
  (1 -- 5) |> rev |> to_list |> OUnit.assert_equal [5;4;3;2;1]
*)

exception ExitForall

let for_all seq ~f =
  try
    seq (fun x -> if not (f x) then raise_notrace ExitForall) ;
    true
  with ExitForall -> false

(*$R
  OUnit.assert_bool "true" (for_all (fun x -> x < 10) (1--9));
  OUnit.assert_bool "false" (not (for_all (fun x -> x < 10) (2--11)));
  OUnit.assert_bool "true" (for_all (fun _ -> false) empty);
  OUnit.assert_bool "nested"
    (for_all
      (fun seq -> not (for_all (fun x -> x < 8) seq))
      (1 -- 10 >|= fun x -> x--20));
*)

exception ExitExists

(** Exists there some element satisfying the predicate? *)
let exists seq ~f =
  try
    seq (fun x -> if f x then raise_notrace ExitExists) ;
    false
  with ExitExists -> true

(*$R
  (1 -- 100)
    |> exists (fun x -> x = 59)
    |> OUnit.assert_bool "exists";
  (1 -- 100)
    |> exists (fun x -> x < 0)
    |> (fun x -> not x)
    |> OUnit.assert_bool "not exists";
*)

let mem x seq ~eq = exists ~f:(eq x) seq

exception ExitFind

let find_map seq ~f =
  let r = ref None in
  ( try
      seq (fun x ->
          match f x with
          | None -> ()
          | Some _ as res ->
              r := res ;
              raise_notrace ExitFind )
    with ExitFind -> () ) ;
  !r

let find_mapi seq ~f =
  let i = ref 0 in
  let r = ref None in
  ( try
      seq (fun x ->
          match f !i x with
          | None -> incr i
          | Some _ as res ->
              r := res ;
              raise_notrace ExitFind )
    with ExitFind -> () ) ;
  !r

let find seq ~f = find_map ~f:(fun x -> if f x then Some x else None) seq

let findi seq ~f =
  find_mapi ~f:(fun i x -> if f i x then Some x else None) seq

let find_exn seq ~f =
  match find ~f seq with Some x -> x | None -> raise Not_found

let[@inline] length seq =
  let r = ref 0 in
  seq (fun _ -> incr r) ;
  !r

(*$R
  (1 -- 1000) |> length |> OUnit.assert_equal 1000
*)

exception ExitIsEmpty

let is_empty seq =
  try
    seq (fun _ -> raise_notrace ExitIsEmpty) ;
    true
  with ExitIsEmpty -> false

(** {2 Transform an iterator} *)

let[@inline] zip_i seq k =
  let r = ref 0 in
  seq (fun x ->
      let n = !r in
      incr r ;
      k (n, x) )

let fold2 seq2 acc ~f =
  let acc = ref acc in
  seq2 (fun (x, y) -> acc := f x y !acc) ;
  !acc

let[@inline] iter2 seq2 ~f = seq2 (fun (x, y) -> f x y)
let[@inline] map2 seq2 ~f k = seq2 (fun (x, y) -> k (f x y))
let[@inline] map2_2 seq2 ~f ~g k = seq2 (fun (x, y) -> k (f x y, g x y))

(** {2 Basic data structures converters} *)

let to_list seq = List.rev (fold ~f:(fun x y -> x :: y) seq [])
let[@inline] to_rev_list seq = fold ~f:(fun x y -> x :: y) seq []
let[@inline] of_list l k = List.iter k l
let on_list l ~f = to_list (f (of_list l))

let pair_with_idx seq k =
  let r = ref 0 in
  seq (fun x ->
      let n = !r in
      incr r ;
      k (n, x) )

let to_opt = head
let[@inline] of_opt o k = match o with None -> () | Some x -> k x

let to_array seq =
  let l = MList.of_iter seq in
  let n = MList.length l in
  if n = 0 then [||]
  else
    let a = Array.make n (MList.get l 0) in
    MList.iteri (fun i x -> a.(i) <- x) l ;
    a

let[@inline] of_array a k = Array.iter k a

let[@inline] of_array_i a k =
  for i = 0 to Array.length a - 1 do
    k (i, Array.unsafe_get a i)
  done

let array_slice a i j k =
  assert (i >= 0 && j < Array.length a) ;
  for idx = i to j do
    k a.(idx) (* iterate on sub-array *)
  done

let rec of_seq l k =
  match l () with
  | Seq.Nil -> ()
  | Seq.Cons (x, tl) ->
      k x ;
      of_seq tl k

let to_seq_persistent seq =
  let l = MList.of_iter seq in
  MList.to_seq l

let[@inline] to_stack s seq = iter ~f:(fun x -> Stack.push x s) seq
let[@inline] of_stack s k = Stack.iter k s
let[@inline] to_queue q seq = seq (fun x -> Queue.push x q)
let[@inline] of_queue q k = Queue.iter k q
let[@inline] hashtbl_add h seq = seq (fun (k, v) -> Hashtbl.add h k v)

(*$R
  let h = (1 -- 5)
    |> zip_i
    |> to_hashtbl in
  (0 -- 4)
    |> iter (fun i -> OUnit.assert_equal (i+1) (Hashtbl.find h i));
  OUnit.assert_equal [0;1;2;3;4] (hashtbl_keys h |> sort ?cmp:None |> to_list);
*)

let hashtbl_replace h seq = seq (fun (k, v) -> Hashtbl.replace h k v)

let to_hashtbl seq =
  let h = Hashtbl.create 3 in
  hashtbl_replace h seq ;
  h

let[@inline] of_hashtbl h k = Hashtbl.iter (fun a b -> k (a, b)) h
let hashtbl_keys h k = Hashtbl.iter (fun a _ -> k a) h
let hashtbl_values h k = Hashtbl.iter (fun _ b -> k b) h
let[@inline] of_str s k = String.iter k s

let to_str seq =
  let b = Buffer.create 64 in
  iter ~f:(fun c -> Buffer.add_char b c) seq ;
  Buffer.contents b

let concat_str seq =
  let b = Buffer.create 64 in
  iter ~f:(Buffer.add_string b) seq ;
  Buffer.contents b

exception OneShotSequence

let of_in_channel ic =
  let first = ref true in
  fun k ->
    if not !first then raise OneShotSequence
    else (
      first := false ;
      try
        while true do
          let c = input_char ic in
          k c
        done
      with End_of_file -> () )

let to_buffer seq buf = seq (fun c -> Buffer.add_char buf c)

(*$R
  let b = Buffer.create 4 in
  let upp = function 'a'..'z' as c -> Char.chr (Char.code c - Char.code 'a' + Char.code 'A') | c -> c in
  "hello world"
    |> of_str |> rev |> map upp
    |> (fun seq -> to_buffer seq b);
  OUnit.assert_equal "DLROW OLLEH" (Buffer.contents b);
*)

(** Iterator on integers in [start...stop] by steps 1 *)
let int_range ~start ~stop k =
  for i = start to stop do
    k i
  done

(*$R
  OUnit.assert_equal ~printer:pp_ilist [1;2;3;4] (to_list (1--4));
  OUnit.assert_equal ~printer:pp_ilist [10;9;8;7;6] (to_list (10 --^ 6));
  OUnit.assert_equal ~printer:pp_ilist [] (to_list (10--4));
  OUnit.assert_equal ~printer:pp_ilist [] (to_list (10 --^ 60));
*)

let int_range_dec ~start ~stop k =
  for i = start downto stop do
    k i
  done

let int_range_by ~step ~start:i ~stop:j yield =
  if step = 0 then invalid_arg "int_range_by" ;
  for k = 0 to (j - i) / step do
    yield ((k * step) + i)
  done

(*$= & ~printer:Q.Print.(list int)
  [1;2;3;4] (int_range_by ~step:1 1 4 |> to_list)
  [4;3;2;1] (int_range_by ~step:~-1 4 1 |> to_list)
  [6;4;2] (int_range_by 6 1 ~step:~-2 |> to_list)
  [] (int_range_by ~step:1 4 1 |> to_list)
*)

(*$Q
  Q.(pair small_int small_int) (fun (i,j) -> \
    let i = Stdlib.min i j and j = Stdlib.max i j in \
    (i--j |> to_list) = (int_range_by ~step:1 i j |> to_list))
  Q.(pair small_int small_int) (fun (i,j) -> \
    let i = Stdlib.min i j and j = Stdlib.max i j in \
    (i--j |> to_rev_list) = (int_range_by ~step:~-1 j i |> to_list))
*)

let bools k =
  k false ;
  k true

module type Iterable = sig
  type elt
  type t

  val iter : t -> f:(elt -> unit) -> unit
end

let of_set (type s v) m set =
  let module S = (val m : Iterable with type t = s and type elt = v) in
  fun k -> S.iter ~f:k set

module type Addable = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
end

let to_set (type s v) m seq =
  let module S = (val m : Addable with type t = s and type elt = v) in
  fold ~f:(fun x set -> S.add x set) seq S.empty

type 'a gen = unit -> 'a option

(* consume the generator to build a MList *)
let rec of_gen1_ g k =
  match g () with
  | None -> ()
  | Some x ->
      k x ;
      of_gen1_ g k

let of_gen_once g =
  let first = ref true in
  fun k ->
    if !first then first := false else raise OneShotSequence ;
    of_gen1_ g k

let of_gen g =
  let l = MList.of_iter (of_gen1_ g) in
  MList.to_iter l

let to_gen seq =
  let l = MList.of_iter seq in
  MList.to_gen l

(** {2 Functorial conversions between sets and iterators} *)

module Set = struct
  module type S = sig
    type elt
    type t

    val of_iter : elt iter -> t
    val to_iter : t -> elt iter
    val to_list : t -> elt list
    val of_list : elt list -> t

    val of_seq : elt iter -> t
    (** @deprecated use {!of_iter} instead *)

    val to_seq : t -> elt iter
    (** @deprecated use {!to_iter} instead *)
  end

  (** Create an enriched Set module from the given one *)
  module Adapt (X : sig
    type elt
    type t

    val empty : t
    val add : elt -> t -> t
    val iter : (elt -> unit) -> t -> unit
    val elements : t -> elt list
  end) : S with type elt := X.elt and type t := X.t = struct
    let to_iter_ set k = X.iter k set
    let of_iter_ seq = fold ~f:(fun x set -> X.add x set) seq X.empty
    let to_iter = to_iter_
    let of_iter = of_iter_
    let to_seq = to_iter_
    let of_seq = of_iter_
    let of_list l = List.fold_left (fun set x -> X.add x set) X.empty l
    let to_list = X.elements
  end
end

(** {2 Conversion between maps and iterators.} *)

module Map = struct
  module type S = sig
    type key
    type +!'a t

    val to_iter : 'a t -> (key * 'a) iter
    val of_iter : (key * 'a) iter -> 'a t
    val keys : 'a t -> key iter
    val values : 'a t -> 'a iter
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t

    val to_seq : 'a t -> (key * 'a) iter
    (** @deprecated use {!to_iter} instead *)

    val of_seq : (key * 'a) iter -> 'a t
    (** @deprecated use {!of_iter} instead *)
  end

  (** Adapt a pre-existing Map module to make it iterator-aware *)
  module Adapt (M : sig
    type key
    type +!'a t

    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val bindings : 'a t -> (key * 'a) list
  end) : S with type key := M.key and type 'a t := 'a M.t = struct
    let to_iter m = from_iter (fun k -> M.iter (fun x y -> k (x, y)) m)
    let of_iter seq = fold ~f:(fun (k, v) m -> M.add k v m) seq M.empty
    let keys m = from_iter (fun k -> M.iter (fun x _ -> k x) m)
    let values m = from_iter (fun k -> M.iter (fun _ y -> k y) m)
    let of_list l = of_iter (of_list l)
    let to_list = M.bindings
    let to_seq = to_iter
    let of_seq = of_iter
  end
end

(** {2 Infinite iterators of random values} *)

let random_int bound = forever ~f:(fun () -> Random.int bound)
let random_bool = forever ~f:Random.bool
let random_float bound = forever ~f:(fun () -> Random.float bound)

let random_array a k =
  assert (Array.length a > 0) ;
  while true do
    let i = Random.int (Array.length a) in
    k a.(i)
  done

let random_list l = random_array (Array.of_list l)

(* See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let shuffle_array a =
  for k = Array.length a - 1 downto 0 + 1 do
    let l = Random.int (k + 1) in
    let tmp = a.(l) in
    a.(l) <- a.(k) ;
    a.(k) <- tmp
  done

let shuffle seq =
  let a = to_array seq in
  shuffle_array a ;
  of_array a

let shuffle_buffer n seq k =
  let seq_front = take n seq in
  let a = to_array seq_front in
  let l = Array.length a in
  if l < n then (
    shuffle_array a ;
    of_array a k )
  else
    let seq = drop n seq in
    let f x =
      let i = Random.int n in
      let y = a.(i) in
      a.(i) <- x ;
      k y
    in
    seq f

(** {2 Sampling} *)

(** See https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_R *)
let sample k seq =
  match head seq with
  | None -> [||]
  | Some x ->
      let a = Array.make k x in
      let i = ref (-1) in
      let f x =
        incr i ;
        if !i < k then a.(!i) <- x
        else
          let j = Random.int !i in
          if j < k then a.(j) <- x else ()
      in
      seq f ;
      if !i < k then Array.sub a 0 (!i + 1) else a

(*$inject
  let array_for_all f a =
    try
    for i=0 to Array.length a-1 do
      if not (f a.(i)) then raise Exit
     done; true
  with Exit -> false
*)

(*$QR
  Q.(pair (list int) (1 -- 20)) (fun (l, n) ->
    let seq = of_list l in
    let a = sample n seq in
    (array_for_all (fun x -> exists ((=) x) seq) a)
    && (Array.length a = Stdlib.min (length seq) n) )
*)

(** {2 Infix functions} *)

module Infix = struct
  let[@inline] ( -- ) i j = int_range ~start:i ~stop:j
  let[@inline] ( --^ ) i j = int_range_dec ~start:i ~stop:j
  let[@inline] ( >>= ) x f = flat_map ~f x
  let[@inline] ( >|= ) x f = map ~f x
  let[@inline] ( <*> ) funs args k = funs (fun f -> args (fun x -> k (f x)))
  let ( <+> ) = append
end

include Infix

(** {2 Pretty printing of iterators} *)

(** Pretty print an ['a iter], using the given pretty printer to print each
    elements. An optional separator string can be provided. *)
let pp_seq ?(sep = ", ") pp_elt formatter seq =
  let first = ref true in
  seq (fun x ->
      if !first then first := false
      else (
        Format.pp_print_string formatter sep ;
        Format.pp_print_cut formatter () ) ;
      pp_elt formatter x )

let pp_buf ?(sep = ", ") pp_elt buf seq =
  let first = ref true in
  seq (fun x ->
      if !first then first := false else Buffer.add_string buf sep ;
      pp_elt buf x )

let to_string ?sep pp_elt seq =
  let buf = Buffer.create 25 in
  pp_buf ?sep (fun buf x -> Buffer.add_string buf (pp_elt x)) buf seq ;
  Buffer.contents buf

(** {2 Basic IO} *)

module IO = struct
  let lines_of ?(mode = 0o644) ?(flags = [Open_rdonly]) filename k =
    let ic = open_in_gen flags mode filename in
    try
      while true do
        let line = input_line ic in
        k line
      done
    with
    | End_of_file -> close_in ic
    | e ->
        close_in_noerr ic ;
        raise e

  let chunks_of ?(mode = 0o644) ?(flags = []) ?(size = 1024) filename k =
    let ic = open_in_gen flags mode filename in
    try
      let buf = Bytes.create size in
      let n = ref 0 in
      let stop = ref false in
      while not !stop do
        n := 0 ;
        (* try to read [size] chars. If [input] returns [0] it means the end
           of file, so we stop, but first we yield the current chunk *)
        while !n < size && not !stop do
          let n' = input ic buf !n (size - !n) in
          if n' = 0 then stop := true else n := !n + n'
        done ;
        if !n > 0 then k (Bytes.sub_string buf 0 !n)
      done ;
      close_in ic
    with e ->
      close_in_noerr ic ;
      raise e

  let write_bytes_to ?(mode = 0o644) ?(flags = [Open_creat; Open_wronly])
      filename seq =
    let oc = open_out_gen flags mode filename in
    try
      seq (fun s -> output oc s 0 (Bytes.length s)) ;
      close_out oc
    with e ->
      close_out oc ;
      raise e

  let write_to ?mode ?flags filename seq =
    write_bytes_to ?mode ?flags filename (map ~f:Bytes.unsafe_of_string seq)

  let write_bytes_lines ?mode ?flags filename seq =
    let ret = Bytes.unsafe_of_string "\n" in
    write_bytes_to ?mode ?flags filename (snoc (intersperse ret seq) ret)

  let write_lines ?mode ?flags filename seq =
    write_bytes_lines ?mode ?flags filename
      (map ~f:Bytes.unsafe_of_string seq)
end

(* regression tests *)

(*$R
  let s = (take 10 (repeat 1)) in
  OUnit.assert_bool "not empty" (not (is_empty s));
*)
