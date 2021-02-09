(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms *)

(** Representation of Arithmetic terms *)
module rec Arith0 :
  (Arithmetic.REPRESENTATION
    with type var := Trm.Var1.t
    with type trm := Trm.t) =
  Arithmetic.Representation (Trm.Var1) (Trm)

(** Arithmetic terms *)
and Arith : (Arithmetic.S with type trm := Trm.t with type t = Arith0.t) =
struct
  include Arith0

  include Make (struct
    let to_trm = Trm._Arith

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
    (* uninterpreted *)
    | Apply of Funsym.t * t array
  [@@deriving compare, equal, sexp]

  (** Variable terms, represented as a subtype of general terms *)
  module Var1 : sig
    type trm := t

    include Var_intf.VAR with type t = private trm

    val of_ : trm -> t
    val of_trm : trm -> t option
  end

  val ppx : Var1.strength -> t pp
  val pp : t pp

  include Invariant.S with type t := t

  val _Var : int -> string -> t
  val _Z : Z.t -> t
  val _Q : Q.t -> t
  val _Arith : Arith.t -> t
  val _Splat : t -> t
  val _Sized : seq:t -> siz:t -> t
  val _Extract : seq:t -> off:t -> len:t -> t
  val _Concat : t array -> t
  val _Apply : Funsym.t -> t array -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val seq_size_exn : t -> t
  val seq_size : t -> t option
  val get_z : t -> Z.t option
  val get_q : t -> Q.t option
  val vars : t -> Var1.t iter
end = struct
  type t =
    | Var of {id: int; name: string [@ignore]}
    | Z of Z.t
    | Q of Q.t
    | Arith of Arith.t
    | Splat of t
    | Sized of {seq: t; siz: t}
    | Extract of {seq: t; off: t; len: t}
    | Concat of t array
    | Apply of Funsym.t * t array
  [@@deriving compare, equal, sexp]

  (* nul-terminated string value represented by a concatenation *)
  let string_of_concat xs =
    let exception Not_a_string in
    try
      let len_1 = Array.length xs - 1 in
      ( match xs.(len_1) with
      | Sized {siz= Z o; seq= Z c} when Z.equal Z.one o && Z.equal Z.zero c
        ->
          ()
      | _ -> raise_notrace Not_a_string ) ;
      Some
        (String.init len_1 ~f:(fun i ->
             match xs.(i) with
             | Sized {siz= Z o; seq= Z c} when Z.equal Z.one o ->
                 Char.of_int_exn (Z.to_int c)
             | _ -> raise_notrace Not_a_string ))
    with _ -> None

  let rec ppx strength fs trm =
    let rec pp fs trm =
      let pp_boxed fs fmt =
        Format.pp_open_box fs 2 ;
        Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
      in
      let pf fmt = pp_boxed fs fmt in
      match trm with
      | Var {id; name} -> (
          if id < 0 then Trace.pp_styled `Bold "%%%s!%i" fs name (-id)
          else
            match strength trm with
            | None -> Format.fprintf fs "%%%s_%i" name id
            | Some `Universal -> Trace.pp_styled `Bold "%%%s_%i" fs name id
            | Some `Existential ->
                Trace.pp_styled `Cyan "%%%s_%i" fs name id
            | Some `Anonymous -> Trace.pp_styled `Cyan "_" fs )
      | Z z -> Trace.pp_styled `Magenta "%a" fs Z.pp z
      | Q q -> Trace.pp_styled `Magenta "%a" fs Q.pp q
      | Arith a -> Arith.ppx (ppx strength) fs a
      | Splat x -> pf "%a^" pp x
      | Sized {seq; siz} -> pf "@<1>⟨%a,%a@<1>⟩" pp siz pp seq
      | Extract {seq; off; len} -> pf "%a[%a,%a)" pp seq pp off pp len
      | Concat [||] -> pf "@<2>⟨⟩"
      | Concat xs -> (
        match string_of_concat xs with
        | Some s -> pf "%S" s
        | None -> pf "(%a)" (Array.pp "@,^" pp) xs )
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

  let pp = ppx (fun _ -> None)

  module Var1 = struct
    module T = struct
      type nonrec t = t [@@deriving compare, equal, sexp]

      let invariant x =
        let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
        match x with
        | Var _ -> ()
        | _ -> fail "non-var: %a" Sexp.pp_hum (sexp_of_t x) ()

      let make ~id ~name = Var {id; name} |> check invariant
      let id = function Var v -> v.id | x -> violates invariant x
      let name = function Var v -> v.name | x -> violates invariant x
    end

    include Var0.Make (T)

    let of_ v = v |> check T.invariant
    let of_trm = function Var _ as v -> Some v | _ -> None
  end

  let invariant e =
    let@ () = Invariant.invariant [%here] e [%sexp_of: t] in
    match e with
    | Q q -> assert (not (Z.equal Z.one (Q.den q)))
    | Arith a -> (
      match Arith.classify a with
      | Trm _ | Const _ -> assert false
      | _ -> () )
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
    | _ -> Arith a )
    |> check invariant

  let add x y = _Arith Arith.(add (trm x) (trm y))
  let sub x y = _Arith Arith.(sub (trm x) (trm y))

  let _Splat x =
    (* 0^ ==> 0 *)
    (if x == zero then x else Splat x) |> check invariant

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

  let _Sized ~seq ~siz =
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

  let rec _Extract ~seq ~off ~len =
    [%trace]
      ~call:(fun {pf} -> pf "@ %a" pp (Extract {seq; off; len}))
      ~retn:(fun {pf} -> pf "%a" pp)
    @@ fun () ->
    (* _[_,0) ==> ⟨⟩ *)
    ( if equal len zero then empty_seq
    else
      let o_l = add off len in
      match seq with
      (* α[m,k)[o,l) ==> α[m+o,l) when k ≥ o+l *)
      | Extract {seq= a; off= m; len= k} when partial_ge k o_l ->
          _Extract ~seq:a ~off:(add m off) ~len
      (* ⟨n,0⟩[o,l) ==> ⟨l,0⟩ when n ≥ o+l *)
      | Sized {siz= n; seq} when seq == zero && partial_ge n o_l ->
          _Sized ~seq ~siz:len
      (* ⟨n,E^⟩[o,l) ==> ⟨l,E^⟩ when n ≥ o+l *)
      | Sized {siz= n; seq= Splat _ as e} when partial_ge n o_l ->
          _Sized ~seq:e ~siz:len
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
                  `Continue (_Extract ~seq:naI ~off:oI ~len:zero, (l, oI))
                else
                  let nI = seq_size_exn naI in
                  let oI_nI = sub oI nI in
                  match oI_nI with
                  | Z z ->
                      let oJ = if Z.sign z <= 0 then zero else oI_nI in
                      let lI = Z.(max zero (min l (neg z))) in
                      let l = Z.(l - lI) in
                      `Continue
                        (_Extract ~seq:naI ~off:oI ~len:(_Z lI), (l, oJ))
                  | _ -> `Stop (Extract {seq; off; len}) )
              ~finish:(fun (e1N, _) -> _Concat e1N)
        | _ -> Extract {seq; off; len} )
      (* α[o,l) *)
      | _ -> Extract {seq; off; len} )
    |> check invariant

  and _Concat xs =
    [%trace]
      ~call:(fun {pf} -> pf "@ %a" pp (Concat xs))
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
          Some (_Extract ~seq:na ~off:o ~len:(add k l))
      (* ⟨m,0⟩^⟨n,0⟩ ==> ⟨m+n,0⟩ *)
      | Sized {siz= m; seq= a}, Sized {siz= n; seq= a'}
        when a == zero && a' == zero ->
          Some (_Sized ~seq:a ~siz:(add m n))
      (* ⟨m,E^⟩^⟨n,E^⟩ ==> ⟨m+n,E^⟩ *)
      | Sized {siz= m; seq= Splat _ as a}, Sized {siz= n; seq= a'}
        when equal a a' ->
          Some (_Sized ~seq:a ~siz:(add m n))
      | _ -> None
    in
    let xs = flatten xs in
    let xs = Array.reduce_adjacent ~f:simp_adjacent xs in
    (if Array.length xs = 1 then xs.(0) else Concat xs) |> check invariant

  let _Apply f es =
    ( match Funsym.eval ~equal ~get_z ~ret_z:_Z ~get_q ~ret_q:_Q f es with
    | Some c -> c
    | None -> Apply (f, es) )
    |> check invariant

  (** Traverse *)

  let rec iter_vars e ~f =
    match e with
    | Var _ as v -> f (Var1.of_ v)
    | Z _ | Q _ -> ()
    | Splat x -> iter_vars ~f x
    | Sized {seq= x; siz= y} ->
        iter_vars ~f x ;
        iter_vars ~f y
    | Extract {seq= x; off= y; len= z} ->
        iter_vars ~f x ;
        iter_vars ~f y ;
        iter_vars ~f z
    | Concat xs | Apply (_, xs) -> Array.iter ~f:(iter_vars ~f) xs
    | Arith a -> Iter.iter ~f:(iter_vars ~f) (Arith.trms a)

  let vars e = Iter.from_labelled_iter (iter_vars e)
end

include Trm
module Var = Var1

module Set = struct
  include Set.Make (Trm)
  include Provide_of_sexp (Trm)
  include Provide_pp (Trm)

  let of_vars : Var.Set.t -> t =
   fun vs ->
    of_iter
      (Iter.map ~f:(fun v -> (v : Var.t :> Trm.t)) (Var.Set.to_iter vs))
end

module Map = struct
  include Map.Make (Trm)
  include Provide_of_sexp (Trm)
end

type arith = Arith.t

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
let sized = _Sized
let extract = _Extract
let concat elts = _Concat elts

(* uninterpreted *)

let apply sym args = _Apply sym args

(** Traverse *)

let trms = function
  | Var _ | Z _ | Q _ -> Iter.empty
  | Arith a -> Arith.trms a
  | Splat x -> Iter.(cons x empty)
  | Sized {seq; siz} -> Iter.(cons seq (cons siz empty))
  | Extract {seq; off; len} -> Iter.(cons seq (cons off (cons len empty)))
  | Concat xs | Apply (_, xs) -> Iter.of_array xs

let is_atomic = function
  | Var _ | Z _ | Q _ | Concat [||] | Apply (_, [||]) -> true
  | Arith _ | Splat _ | Sized _ | Extract _ | Concat _ | Apply _ -> false

let rec atoms e =
  if is_atomic e then Iter.return e else Iter.flat_map ~f:atoms (trms e)

(** Query *)

let fv e = Var.Set.of_iter (vars e)
let rec height e = 1 + Iter.fold ~f:(fun x -> max (height x)) (trms e) 0

(** Transform *)

let rec map_vars e ~f =
  match e with
  | Var _ as v -> (f (Var.of_ v) : Var.t :> t)
  | Z _ | Q _ -> e
  | Arith a -> map1 (Arith.map ~f:(map_vars ~f)) e _Arith a
  | Splat x -> map1 (map_vars ~f) e _Splat x
  | Sized {seq; siz} ->
      map2 (map_vars ~f) e (fun seq siz -> _Sized ~seq ~siz) seq siz
  | Extract {seq; off; len} ->
      map3 (map_vars ~f) e
        (fun seq off len -> _Extract ~seq ~off ~len)
        seq off len
  | Concat xs -> mapN (map_vars ~f) e _Concat xs
  | Apply (g, xs) -> mapN (map_vars ~f) e (_Apply g) xs

let map e ~f =
  match e with
  | Var _ | Z _ | Q _ -> e
  | Arith a -> map1 (Arith.map ~f) e _Arith a
  | Splat x -> map1 f e _Splat x
  | Sized {seq; siz} -> map2 f e (fun seq siz -> _Sized ~seq ~siz) seq siz
  | Extract {seq; off; len} ->
      map3 f e (fun seq off len -> _Extract ~seq ~off ~len) seq off len
  | Concat xs -> mapN f e _Concat xs
  | Apply (g, xs) -> mapN f e (_Apply g) xs

let fold_map e = fold_map_from_map map e
