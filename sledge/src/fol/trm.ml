(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms *)

let pp_boxed fs fmt =
  Format.pp_open_box fs 2 ;
  Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt

(** Variable terms, represented as a subtype of general terms *)
module rec Var : sig
  include Var_intf.VAR with type t = private Trm.t

  val of_ : Trm.t -> t
  val of_trm : Trm.t -> t option
end = struct
  module T = struct
    type t = Trm.t [@@deriving compare, equal, sexp]

    let invariant x =
      let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
      match x with
      | Var _ -> ()
      | _ -> fail "non-var: %a" Sexp.pp_hum (sexp_of_t x) ()

    let make ~id ~name = Trm._Var id name |> check invariant
    let id = function Trm.Var v -> v.id | x -> violates invariant x
    let name = function Trm.Var v -> v.name | x -> violates invariant x
  end

  include Var0.Make (T)

  let of_ v = v |> check T.invariant
  let of_trm = function Trm.Var _ as v -> Some v | _ -> None
end

and Arith0 :
  (Arithmetic.REPRESENTATION with type var := Var.t with type trm := Trm.t) =
  Arithmetic.Representation
    (Var)
    (struct
      include Trm

      type trm = t [@@deriving compare, equal, sexp]
    end)

and Arith :
  (Arithmetic.S
    with type var := Var.t
    with type trm := Trm.t
    with type t = Arith0.t) = struct
  include Arith0

  include Make (struct
    let get_arith (e : Trm.t) =
      match e with
      | Z z -> Some (Arith.const (Q.of_z z))
      | Q q -> Some (Arith.const q)
      | Arith a -> Some a
      | _ -> None
  end)
end

(** Terms, built from variables and applications of function symbols from
    various theories. Denote functions from structures to values. *)
and Trm : sig
  type t = private
    (* variables *)
    | Var of {id: int; name: string}
    (* arithmetic *)
    | Z of Z.t
    | Q of Q.t
    | Arith of Arith.t
    (* sequences (of flexible size) *)
    | Splat of t
    | Sized of {seq: t; siz: t}
    | Extract of {seq: t; off: t; len: t}
    | Concat of t array
    (* records (with fixed indices) *)
    | Select of {idx: int; rcd: t}
    | Update of {idx: int; rcd: t; elt: t}
    | Record of t array
    | Ancestor of int
    (* uninterpreted *)
    | Apply of Funsym.t * t array
  [@@deriving compare, equal, sexp]

  val ppx : Var.strength -> t pp
  val pp : t pp
  val _Var : int -> string -> t
  val _Z : Z.t -> t
  val _Q : Q.t -> t
  val _Arith : Arith.t -> t
  val _Splat : t -> t
  val _Sized : t -> t -> t
  val _Extract : t -> t -> t -> t
  val _Concat : t array -> t
  val _Select : int -> t -> t
  val _Update : int -> t -> t -> t
  val _Record : t array -> t
  val _Ancestor : int -> t
  val _Apply : Funsym.t -> t array -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val seq_size_exn : t -> t
  val seq_size : t -> t option
  val get_z : t -> Z.t option
  val get_q : t -> Q.t option
  val vars : t -> Var.t iter
end = struct
  type t =
    | Var of {id: int; name: string}
    | Z of Z.t
    | Q of Q.t
    | Arith of Arith.t
    | Splat of t
    | Sized of {seq: t; siz: t}
    | Extract of {seq: t; off: t; len: t}
    | Concat of t array
    | Select of {idx: int; rcd: t}
    | Update of {idx: int; rcd: t; elt: t}
    | Record of t array
    | Ancestor of int
    | Apply of Funsym.t * t array
  [@@deriving compare, equal, sexp]

  let compare x y =
    if x == y then 0
    else
      match (x, y) with
      | Var {id= i; name= _}, Var {id= j; name= _} when i > 0 && j > 0 ->
          Int.compare i j
      | _ -> compare x y

  let equal x y =
    x == y
    ||
    match (x, y) with
    | Var {id= i; name= _}, Var {id= j; name= _} when i > 0 && j > 0 ->
        Int.equal i j
    | _ -> equal x y

  let rec ppx strength fs trm =
    let rec pp fs trm =
      let pf fmt = pp_boxed fs fmt in
      match trm with
      | Var _ as v -> Var.ppx strength fs (Var.of_ v)
      | Z z -> Trace.pp_styled `Magenta "%a" fs Z.pp z
      | Q q -> Trace.pp_styled `Magenta "%a" fs Q.pp q
      | Arith a -> Arith.ppx strength fs a
      | Splat x -> pf "%a^" pp x
      | Sized {seq; siz} -> pf "@<1>⟨%a,%a@<1>⟩" pp siz pp seq
      | Extract {seq; off; len} -> pf "%a[%a,%a)" pp seq pp off pp len
      | Concat [||] -> pf "@<2>⟨⟩"
      | Concat xs -> pf "(%a)" (Array.pp "@,^" pp) xs
      | Select {idx; rcd} -> pf "%a[%i]" pp rcd idx
      | Update {idx; rcd; elt} ->
          pf "[%a@ @[| %i → %a@]]" pp rcd idx pp elt
      | Record xs -> pf "{%a}" (ppx_record strength) xs
      | Ancestor i -> pf "(ancestor %i)" i
      | Apply (f, [||]) -> pf "%a" Funsym.pp f
      | Apply
          ( ( (Rem | BitAnd | BitOr | BitXor | BitShl | BitLshr | BitAshr)
            as f )
          , [|x; y|] ) ->
          pf "(%a@ %a@ %a)" pp x Funsym.pp f pp y
      | Apply (f, es) ->
          pf "%a(%a)" Funsym.pp f (Array.pp ",@ " (ppx strength)) es
    in
    pp fs trm

  and ppx_record strength fs elts =
    [%Trace.fprintf
      fs "%a"
        (fun fs elts ->
          let exception Not_a_string in
          match
            String.init (Array.length elts) ~f:(fun i ->
                match elts.(i) with
                | Z c -> Char.of_int_exn (Z.to_int c)
                | _ -> raise_notrace Not_a_string )
          with
          | s -> Format.fprintf fs "%S" s
          | exception (Not_a_string | Z.Overflow | Failure _) ->
              Format.fprintf fs "@[<h>%a@]"
                (Array.pp ",@ " (ppx strength))
                elts )
        elts]

  let pp = ppx (fun _ -> None)

  let invariant e =
    let@ () = Invariant.invariant [%here] e [%sexp_of: t] in
    match e with
    | Q q -> assert (not (Z.equal Z.one (Q.den q)))
    | Arith a -> (
      match Arith.classify a with
      | Compound -> ()
      | Trm _ | Const _ -> assert false )
    | _ -> ()

  (** Destruct *)

  let get_z = function Z z -> Some z | _ -> None
  let get_q = function Q q -> Some q | Z z -> Some (Q.of_z z) | _ -> None

  (** Construct *)

  let _Var id name = Var {id; name} |> check invariant

  (* statically allocated since they are tested with == *)
  let zero = Z Z.zero |> check invariant
  let one = Z Z.one |> check invariant

  let _Z z =
    (if Z.equal Z.zero z then zero else if Z.equal Z.one z then one else Z z)
    |> check invariant

  let _Q q =
    (if Z.equal Z.one (Q.den q) then _Z (Q.num q) else Q q)
    |> check invariant

  let _Arith a =
    ( match Arith.classify a with
    | Trm e -> e
    | Const q -> _Q q
    | Compound -> Arith a )
    |> check invariant

  let add x y = _Arith Arith.(add (trm x) (trm y))
  let sub x y = _Arith Arith.(sub (trm x) (trm y))
  let _Splat x = Splat x |> check invariant

  let seq_size_exn =
    let invalid = Invalid_argument "seq_size_exn" in
    let rec seq_size_exn = function
      | Sized {siz= n} | Extract {len= n} -> n
      | Concat a0U ->
          Array.fold ~f:(fun aJ a0I -> add a0I (seq_size_exn aJ)) a0U zero
      | _ -> raise invalid
    in
    seq_size_exn

  let seq_size e =
    try Some (seq_size_exn e) with Invalid_argument _ -> None

  let _Sized seq siz =
    ( match seq_size seq with
    (* ⟨n,α⟩ ==> α when n ≡ |α| *)
    | Some n when equal siz n -> seq
    | _ -> Sized {seq; siz} )
    |> check invariant

  let partial_compare x y =
    match sub x y with
    | Z z -> Some (Int.sign (Z.sign z))
    | Q q -> Some (Int.sign (Q.sign q))
    | _ -> None

  let partial_ge x y =
    match partial_compare x y with Some (Pos | Zero) -> true | _ -> false

  let empty_seq = Concat [||]

  let rec _Extract seq off len =
    [%trace]
      ~call:(fun {pf} -> pf "%a" pp (Extract {seq; off; len}))
      ~retn:(fun {pf} -> pf "%a" pp)
    @@ fun () ->
    (* _[_,0) ==> ⟨⟩ *)
    ( if equal len zero then empty_seq
    else
      let o_l = add off len in
      match seq with
      (* α[m,k)[o,l) ==> α[m+o,l) when k ≥ o+l *)
      | Extract {seq= a; off= m; len= k} when partial_ge k o_l ->
          _Extract a (add m off) len
      (* ⟨n,E^⟩[o,l) ==> ⟨l,E^⟩ when n ≥ o+l *)
      | Sized {siz= n; seq= Splat _ as e} when partial_ge n o_l ->
          _Sized e len
      (* ⟨n,a⟩[0,n) ==> ⟨n,a⟩ *)
      | Sized {siz= n} when equal off zero && equal n len -> seq
      (* For (α₀^α₁)[o,l) there are 3 cases:
       *
       * ⟨...⟩^⟨...⟩
       *  [,)
       * o < o+l ≤ |α₀| : (α₀^α₁)[o,l) ==> α₀[o,l) ^ α₁[0,0)
       *
       * ⟨...⟩^⟨...⟩
       *   [  ,  )
       * o ≤ |α₀| < o+l : (α₀^α₁)[o,l) ==> α₀[o,|α₀|-o) ^ α₁[0,l-(|α₀|-o))
       *
       * ⟨...⟩^⟨...⟩
       *        [,)
       * |α₀| ≤ o : (α₀^α₁)[o,l) ==> α₀[o,0) ^ α₁[o-|α₀|,l)
       *
       * So in general:
       *
       * (α₀^α₁)[o,l) ==> α₀[o,l₀) ^ α₁[o₁,l-l₀)
       * where l₀ = max 0 (min l |α₀|-o)
       *       o₁ = max 0 o-|α₀|
       *)
      | Concat na1N -> (
        match len with
        | Z l ->
            Array.fold_map_until na1N (l, off)
              ~f:(fun naI (l, oI) ->
                if Z.equal Z.zero l then
                  `Continue (_Extract naI oI zero, (l, oI))
                else
                  let nI = seq_size_exn naI in
                  let oI_nI = sub oI nI in
                  match oI_nI with
                  | Z z ->
                      let oJ = if Z.sign z <= 0 then zero else oI_nI in
                      let lI = Z.(max zero (min l (neg z))) in
                      let l = Z.(l - lI) in
                      `Continue (_Extract naI oI (_Z lI), (l, oJ))
                  | _ -> `Stop (Extract {seq; off; len}) )
              ~finish:(fun (e1N, _) -> _Concat e1N)
        | _ -> Extract {seq; off; len} )
      (* α[o,l) *)
      | _ -> Extract {seq; off; len} )
    |> check invariant

  and _Concat xs =
    [%trace]
      ~call:(fun {pf} -> pf "%a" pp (Concat xs))
      ~retn:(fun {pf} -> pf "%a" pp)
    @@ fun () ->
    (* (α^(β^γ)^δ) ==> (α^β^γ^δ) *)
    let flatten xs =
      if Array.exists ~f:(function Concat _ -> true | _ -> false) xs then
        Array.flat_map ~f:(function Concat s -> s | e -> [|e|]) xs
      else xs
    in
    let simp_adjacent e f =
      match (e, f) with
      (* ⟨n,a⟩[o,k)^⟨n,a⟩[o+k,l) ==> ⟨n,a⟩[o,k+l) when n ≥ o+k+l *)
      | ( Extract {seq= Sized {siz= n} as na; off= o; len= k}
        , Extract {seq= na'; off= o_k; len= l} )
        when equal na na' && equal o_k (add o k) && partial_ge n (add o_k l)
        ->
          Some (_Extract na o (add k l))
      (* ⟨m,E^⟩^⟨n,E^⟩ ==> ⟨m+n,E^⟩ *)
      | Sized {siz= m; seq= Splat _ as a}, Sized {siz= n; seq= a'}
        when equal a a' ->
          Some (_Sized a (add m n))
      | _ -> None
    in
    let xs = flatten xs in
    let xs = Array.reduce_adjacent ~f:simp_adjacent xs in
    (if Array.length xs = 1 then xs.(0) else Concat xs) |> check invariant

  let _Select idx rcd = Select {idx; rcd} |> check invariant
  let _Update idx rcd elt = Update {idx; rcd; elt} |> check invariant
  let _Record es = Record es |> check invariant
  let _Ancestor i = Ancestor i |> check invariant

  let _Apply f es =
    ( match Funsym.eval ~equal ~get_z ~ret_z:_Z ~get_q ~ret_q:_Q f es with
    | Some c -> c
    | None -> Apply (f, es) )
    |> check invariant

  (** Traverse *)

  let rec iter_vars e ~f =
    match e with
    | Var _ as v -> f (Var.of_ v)
    | Z _ | Q _ | Ancestor _ -> ()
    | Splat x | Select {rcd= x} -> iter_vars ~f x
    | Sized {seq= x; siz= y} | Update {rcd= x; elt= y} ->
        iter_vars ~f x ;
        iter_vars ~f y
    | Extract {seq= x; off= y; len= z} ->
        iter_vars ~f x ;
        iter_vars ~f y ;
        iter_vars ~f z
    | Concat xs | Record xs | Apply (_, xs) ->
        Array.iter ~f:(iter_vars ~f) xs
    | Arith a -> Iter.iter ~f:(iter_vars ~f) (Arith.trms a)

  let vars e = Iter.from_labelled_iter (iter_vars e)
end

module T = struct
  type t = Trm.t [@@deriving compare, sexp]
end

module Map = struct
  include Map.Make (T)
  include Provide_of_sexp (T)
end

type arith = Arith.t

include Trm

let pp_diff fs (x, y) = Format.fprintf fs "-- %a ++ %a" pp x pp y

(** Construct *)

(* variables *)

let var v = (v : Var.t :> t)

(* arithmetic *)

let zero = _Z Z.zero
let one = _Z Z.one
let integer z = _Z z
let rational q = _Q q
let neg x = _Arith Arith.(neg (trm x))
let add = Trm.add
let sub = Trm.sub
let mulq q x = _Arith Arith.(mulc q (trm x))
let mul x y = _Arith (Arith.mul x y)
let div x y = _Arith (Arith.div x y)
let pow x i = _Arith (Arith.pow x i)
let arith = _Arith

(* sequences *)

let splat = _Splat
let sized ~seq ~siz = _Sized seq siz
let extract ~seq ~off ~len = _Extract seq off len
let concat elts = _Concat elts

(* records *)

let select ~rcd ~idx = _Select idx rcd
let update ~rcd ~idx ~elt = _Update idx rcd elt
let record elts = _Record elts
let ancestor i = _Ancestor i

(* uninterpreted *)

let apply sym args = _Apply sym args

(** Transform *)

let rec map_vars e ~f =
  match e with
  | Var _ as v -> (f (Var.of_ v) : Var.t :> t)
  | Z _ | Q _ -> e
  | Arith a -> map1 (Arith.map ~f:(map_vars ~f)) e _Arith a
  | Splat x -> map1 (map_vars ~f) e _Splat x
  | Sized {seq; siz} -> map2 (map_vars ~f) e _Sized seq siz
  | Extract {seq; off; len} -> map3 (map_vars ~f) e _Extract seq off len
  | Concat xs -> mapN (map_vars ~f) e _Concat xs
  | Select {idx; rcd} -> map1 (map_vars ~f) e (_Select idx) rcd
  | Update {idx; rcd; elt} -> map2 (map_vars ~f) e (_Update idx) rcd elt
  | Record xs -> mapN (map_vars ~f) e _Record xs
  | Ancestor _ -> e
  | Apply (g, xs) -> mapN (map_vars ~f) e (_Apply g) xs

let map e ~f =
  match e with
  | Var _ | Z _ | Q _ -> e
  | Arith a ->
      let a' = Arith.map ~f a in
      if a == a' then e else _Arith a'
  | Splat x -> map1 f e _Splat x
  | Sized {seq; siz} -> map2 f e _Sized seq siz
  | Extract {seq; off; len} -> map3 f e _Extract seq off len
  | Concat xs -> mapN f e _Concat xs
  | Select {idx; rcd} -> map1 f e (_Select idx) rcd
  | Update {idx; rcd; elt} -> map2 f e (_Update idx) rcd elt
  | Record xs -> mapN f e _Record xs
  | Ancestor _ -> e
  | Apply (g, xs) -> mapN f e (_Apply g) xs

(** Traverse *)

let iter_subtrms e ~f =
  match e with
  | Var _ | Z _ | Q _ | Ancestor _ -> ()
  | Arith a -> Iter.iter ~f (Arith.trms a)
  | Splat x | Select {rcd= x} -> f x
  | Sized {seq= x; siz= y} | Update {rcd= x; elt= y} ->
      f x ;
      f y
  | Extract {seq= x; off= y; len= z} ->
      f x ;
      f y ;
      f z
  | Concat xs | Record xs | Apply (_, xs) -> Array.iter ~f xs

let subtrms e = Iter.from_labelled_iter (iter_subtrms e)

(** Query *)

let fv e = Var.Set.of_iter (vars e)
let rec height e = 1 + Iter.fold ~f:(fun x -> max (height x)) (subtrms e) 0
