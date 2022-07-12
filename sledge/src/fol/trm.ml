(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms *)

(* Define term type using polymorphic arithmetic type, with derived compare,
   equal, and sexp_of functions *)
module Trm1 = struct
  type compare [@@deriving compare, equal, sexp]

  type arith = (t, compare) Arithmetic.t

  and sized = {seq: t; siz: t}

  and t =
    (* variables *)
    | Var of {id: int; name: string [@ignore]}
    (* arithmetic *)
    | Z of Z.t
    | Q of Q.t
    | Arith of arith
    (* sequences (of flexible size) *)
    | Splat of t
    | Extract of {seq: t; siz: t; off: t; len: t}
    | Concat of sized array
    (* uninterpreted *)
    | Apply of Funsym.t * t array
  [@@deriving compare, equal, sexp]

  let compare a b =
    if a == b then 0
    else
      match (a, b) with
      | Var x, Var y -> Int.compare x.id y.id
      | _ -> compare a b
end

(* Add comparer, needed to instantiate arithmetic and containers *)
module Trm2 = struct
  include Comparer.Counterfeit (Trm1)
  include Trm1
end

(* Specialize arithmetic type and define operations using comparer *)
module Arith0 = Arithmetic.Make (Trm2)

(* Add ppx, defined recursively with Arith0.ppx *)
module Trm3 = struct
  include Trm2

  (* nul-terminated string value represented by a concatenation *)
  let string_of_concat xs =
    let exception Not_a_string in
    try
      let len_1 = Array.length xs - 1 in
      ( match xs.(len_1) with
      | {siz= Z o; seq= Z c} when Z.equal Z.one o && Z.equal Z.zero c -> ()
      | _ -> raise_notrace Not_a_string ) ;
      Some
        (String.init len_1 ~f:(fun i ->
             match xs.(i) with
             | {siz= Z o; seq= Z c} when Z.equal Z.one o ->
                 Char.of_int_exn (Z.to_int c)
             | _ -> raise_notrace Not_a_string ) )
    with Not_a_string | Z.Overflow | Invalid_argument _ -> None

  let rec ppx strength fs trm =
    let rec pp fs trm =
      let pp_boxed fs fmt =
        Format.pp_open_box fs 2 ;
        Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
      in
      let pf fmt = pp_boxed fs fmt in
      match trm with
      | Var {id; name} -> (
          if id < 0 then
            Dbg.pp_styled `Bold "%%%s!%i" fs name (id + Int.max_int)
          else
            match strength trm with
            | None -> Format.fprintf fs "%%%s_%i" name id
            | Some `Universal -> Dbg.pp_styled `Bold "%%%s_%i" fs name id
            | Some `Existential -> Dbg.pp_styled `Cyan "%%%s_%i" fs name id
            | Some `Anonymous -> Dbg.pp_styled `Cyan "_" fs )
      | Z z -> Dbg.pp_styled `Magenta "%a" fs Z.pp z
      | Q q -> Dbg.pp_styled `Magenta "%a" fs Q.pp q
      | Arith a -> Arith0.ppx (ppx strength) fs a
      | Splat x -> pf "%a^" pp x
      | Extract {seq; siz; off; len} ->
          pf "%a[%a,%a)" pp_sized {seq; siz} pp off pp len
      | Concat [||] -> pf "@<2>⟨⟩"
      | Concat xs -> (
        match string_of_concat xs with
        | Some s -> pf "%S" s
        | None -> pf "(%a)" (Array.pp "@,^" pp_sized) xs )
      | Apply (f, [||]) -> pf "%a" Funsym.pp f
      | Apply
          ( ( (Rem | BitAnd | BitOr | BitXor | BitShl | BitLshr | BitAshr)
            as f )
          , [|x; y|] ) ->
          pf "(%a@ %a@ %a)" pp x Funsym.pp f pp y
      | Apply (f, es) ->
          pf "%a(%a)" Funsym.pp f (Array.pp ",@ " (ppx strength)) es
    and pp_sized fs {seq; siz} =
      Format.fprintf fs "@[<2>@<1>⟨%a,%a@<1>⟩@]" pp siz pp seq
    in
    pp fs trm

  let pp = ppx (fun _ -> None)
  let pp_diff fs (x, y) = Format.fprintf fs "-- %a ++ %a" pp x pp y
end

(* Define containers over terms *)
module Trm4 = struct
  include Trm3

  module Set = struct
    include Set.Make (Trm3)
    include Provide_of_sexp (Trm3)
    include Provide_pp (Trm3)
  end

  module Map = struct
    include Map.Make (Trm3)
    include Provide_of_sexp (Trm3)
  end
end

(* Define variables as a subtype of terms *)
module Var = struct
  open Trm4

  module V = struct
    type nonrec t = t [@@deriving compare, equal, sexp]
    type strength = t -> [`Universal | `Existential | `Anonymous] option

    let pp = pp
    let ppx = ppx

    let invariant x =
      let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
      match x with
      | Var _ -> ()
      | _ -> fail "non-var: %a" Sexp.pp_hum (sexp_of_t x) ()

    let make ~id ~name = Var {id; name} |> check invariant
    let id = function Var v -> v.id | x -> violates invariant x
    let name = function Var v -> v.name | x -> violates invariant x

    module Set = struct
      include Set

      let ppx strength vs = pp_full (ppx strength) vs

      let pp_xs fs xs =
        if not (is_empty xs) then
          Format.fprintf fs "@<2>∃ @[%a@] .@;<1 2>" pp xs
    end

    module Map = Map

    let fresh name ~wrt =
      let max =
        match Set.max_elt wrt with None -> 0 | Some m -> max 0 (id m)
      in
      let x' = make ~id:(max + 1) ~name in
      (x', Set.add x' wrt)

    let freshen v ~wrt = fresh (name v) ~wrt

    let identified ~name ~id =
      assert (id > 0) ;
      make ~id:(id - Int.max_int) ~name

    let of_ v = v |> check invariant
    let of_trm = function Var _ as v -> Some v | _ -> None
  end

  include V
  module Subst = Subst.Make (V)
end

(* Add definitions needed for arithmetic embedding into terms *)
module Trm = struct
  include Trm4

  (** Invariant *)

  let invariant e =
    let@ () = Invariant.invariant [%here] e [%sexp_of: t] in
    match e with
    | Q q -> assert (not (Z.equal Z.one (Q.den q)))
    | Arith a -> (
      match Arith0.classify a with
      | Trm _ | Const _ -> assert false
      | _ -> () )
    | _ -> ()

  (** Traverse *)

  let rec iter_vars e ~f =
    match e with
    | Var _ as v -> f (Var.of_ v)
    | Z _ | Q _ -> ()
    | Arith a -> Iter.iter ~f:(iter_vars ~f) (Arith0.trms a)
    | Splat x -> iter_vars ~f x
    | Extract {seq; siz; off; len} ->
        iter_vars ~f seq ;
        iter_vars ~f siz ;
        iter_vars ~f off ;
        iter_vars ~f len
    | Concat xs ->
        Array.iter
          ~f:(fun {seq; siz} ->
            iter_vars ~f seq ;
            iter_vars ~f siz )
          xs
    | Apply (_, xs) -> Array.iter ~f:(iter_vars ~f) xs

  let vars e = Iter.from_labelled_iter (iter_vars e)

  (** Construct *)

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
    ( match Arith0.classify a with
    | Trm e -> e
    | Const q -> _Q q
    | _ -> Arith a )
    |> check invariant
end

include Trm

(* Instantiate arithmetic with embedding into terms, yielding full
   Arithmetic interface *)
module Arith =
  Arith0.Embed (Var) (Trm)
    (struct
      let to_trm = _Arith

      let get_arith e =
        match e with
        | Z z -> Some (Arith0.const (Q.of_z z))
        | Q q -> Some (Arith0.const q)
        | Arith a -> Some a
        | _ -> None
    end)

(* Full Trm definition, using full arithmetic interface *)

(** Invariant *)

let seq_size_exn =
  let invalid = Invalid_argument "seq_size_exn" in
  let add x y = _Arith Arith.(add (trm x) (trm y)) in
  function
  | Extract {len} -> len
  | Concat a0U -> Array.fold ~f:(fun aJ a0I -> add a0I aJ.siz) a0U zero
  | _ -> raise invalid

let seq_size e = try Some (seq_size_exn e) with Invalid_argument _ -> None

let rec invariant e =
  let@ () = Invariant.invariant [%here] e [%sexp_of: t] in
  match e with
  | Extract {seq; siz; off= _; len= _} ->
      assert (Option.for_all ~f:(equal siz) (seq_size seq))
  | Concat na1N ->
      Array.iter na1N ~f:(fun {seq; siz} ->
          invariant seq ;
          assert (Option.for_all ~f:(equal siz) (seq_size seq)) )
  | _ -> Trm.invariant e

(** Destruct *)

let get_z = function Z z -> Some z | _ -> None
let get_q = function Q q -> Some q | Z z -> Some (Q.of_z z) | _ -> None

(** Traverse *)

let trms = function
  | Var _ | Z _ | Q _ -> Iter.empty
  | Arith a -> Arith.trms a
  | Splat x -> Iter.(cons x empty)
  | Extract {seq; siz; off; len} ->
      Iter.(cons seq (cons siz (cons off (cons len empty))))
  | Concat xs ->
      Array.fold_right xs Iter.empty ~f:(fun {seq; siz} i ->
          Iter.cons seq (Iter.cons siz i) )
  | Apply (_, xs) -> Iter.of_array xs

(** Classification *)

let is_atomic = function
  | Var _ | Z _ | Q _ | Concat [||] | Apply (_, [||]) -> true
  | Arith _ | Splat _ | Extract _ | Concat _ | Apply _ -> false

let rec atoms e =
  if is_atomic e then Iter.return e else Iter.flat_map ~f:atoms (trms e)

type kind = InterpApp | NonInterpAtom | InterpAtom | UninterpApp
[@@deriving compare, equal, sexp_of]

let classify e =
  [%dbg]
    ~call:(fun {pf} -> pf "%a" pp e)
    ~retn:(fun {pf} k -> pf "%a" Sexp.pp (sexp_of_kind k))
  @@ fun () ->
  match e with
  | Var _ -> NonInterpAtom
  | Z _ | Q _ -> InterpAtom
  | Arith a ->
      if Arith.non_interpreted a then UninterpApp
      else (
        assert (
          match Arith.classify a with
          | Trm _ | Const _ -> violates invariant e
          | Interpreted -> true
          | Uninterpreted -> false ) ;
        InterpApp )
  | Concat [||] -> InterpAtom
  | Splat _ | Extract _ | Concat _ -> InterpApp
  | Apply (_, [||]) -> NonInterpAtom
  | Apply _ -> UninterpApp

let is_interp_app e = match classify e with InterpApp -> true | _ -> false

let is_interpreted e =
  match classify e with
  | InterpAtom | InterpApp -> true
  | NonInterpAtom | UninterpApp -> false

let non_interpreted e =
  match classify e with
  | InterpAtom | InterpApp -> false
  | NonInterpAtom | UninterpApp -> true

let rec solvables e =
  match classify e with
  | InterpAtom -> Iter.empty
  | InterpApp -> solvable_trms e
  | NonInterpAtom | UninterpApp -> Iter.return e

and solvable_trms e = Iter.flat_map ~f:solvables (trms e)

let rec transitive_solvables e =
  Iter.flat_map
    ~f:(fun b -> Iter.snoc (transitive_solvable_trms b) b)
    (solvables e)

and transitive_solvable_trms e =
  Iter.flat_map ~f:transitive_solvables (solvable_trms e)

(** Construct *)

(** Check that constructor functions are non-expansive wrt solvables, that
    is, transitive_solvables(args) ⊇ solvable_trms(result). *)
let solvables_contained_in_ args result =
  let new_solvables =
    Set.diff
      (Set.of_iter (solvable_trms result))
      (Set.of_iter (Iter.flat_map ~f:transitive_solvables args))
  in
  assert (
    Set.is_empty new_solvables
    || fail "new solvables %a in %a not in %a" Set.pp new_solvables pp
         result (List.pp "@ " pp) (Iter.to_list args) () )

let solvables_contained_in args result =
  solvables_contained_in_ (Iter.of_array args) result

(* variables *)

let var v = (v : Var.t :> t)

(* arithmetic *)

let integer z = _Z z
let rational q = _Q q
let neg x = _Arith Arith.(neg (trm x))
let add x y = _Arith Arith.(add (trm x) (trm y))
let sub x y = _Arith Arith.(sub (trm x) (trm y))
let mulq q x = _Arith Arith.(mulc q (trm x))
let mul x y = _Arith (Arith.mul x y)
let div x y = _Arith (Arith.div x y)
let pow x i = _Arith (Arith.pow x i)
let arith = _Arith

(* sequences *)

let splat x =
  (* 0^ ==> 0 *)
  (if x == zero then x else Splat x)
  |> check (solvables_contained_in [|x|])
  |> check invariant

let partial_compare x y =
  match sub x y with
  | Z z -> Some (Int.sign (Z.sign z))
  | Q q -> Some (Int.sign (Q.sign q))
  | _ -> None

let partial_ge x y =
  match partial_compare x y with Some (Pos | Zero) -> true | _ -> false

let partial_min x y =
  match partial_compare x y with
  | Some Neg -> Some x
  | Some (Pos | Zero) -> Some y
  | None -> None

let empty_seq = Concat [||]

let rec extract ~seq ~siz ~off ~len =
  [%dbg]
    ~call:(fun {pf} ->
      pf "@ %a" pp (Extract {seq; siz; off; len}) ;
      assert (Option.for_all ~f:(equal siz) (seq_size seq)) )
    ~retn:(fun {pf} e ->
      pf "%a" pp e ;
      solvables_contained_in [|seq; off; len|] e ;
      invariant e )
  @@ fun () ->
  if
    (* _[_,0) ==> ⟨⟩ *)
    equal len zero
    (* ⟨n,_⟩[o,_) ==> ⟨⟩ when o ≥ n *)
    || partial_ge off siz
  then empty_seq
  else if
    (* ⟨_,0⟩[_,_) ==> 0 *)
    seq == Trm.zero
    (* ⟨n,a⟩[0,n) ==> a *)
    || (off == zero && equal siz len)
  then seq
  else
    match seq with
    (* ⟨n,E^⟩[_,_) ==> E^ *)
    | Splat _ -> seq
    (*
     * For ⟨n,a⟩[m,k)[o,l) there are 3 cases:
     * 
     * ⟨.......⟩
     *  [  ,  )  ie: m ≤ m+o ≤ m+o+l ≤ m+k  in particular l ≤ k-o
     *   [ , )   so: ⟨n,a⟩[m,k)[o,l) ==> ⟨n,a⟩[m+o,l)
     * 
     *  [ , )    ie: m ≤ m+o ≤ m+k ≤ m+o+l  in particular k-o ≤ l
     *   [ , )   so: ⟨n,a⟩[m,k)[o,l) ==> ⟨n,a⟩[m+o,k-o)
     * 
     *  [,)      ie: m ≤ m+k ≤ m+o ≤ m+o+l  in particular k ≤ o
     *      [,)  so: ⟨n,a⟩[m,k)[o,l) ==> ⟨⟩
     *
     * So in general:
     *
     * ⟨n,a⟩[m,k)[o,l) ==> ⟨n,a⟩[m+o, max 0 (min k-o l))
     *)
    | Extract {seq= a; siz= n; off= m; len= k} -> (
      (* Note that:
       *     if partial_ge off k (* k ≤ o *) then empty_seq else
       * is covered above by the [partial_ge off siz] case since
       * [siz = seq_size (Extract {len= k; _}) = k]. *)
      match partial_min (sub k off) len with
      | Some min -> extract ~seq:a ~siz:n ~off:(add m off) ~len:min
      | None -> Extract {seq; siz; off; len} )
    (*
     * For (α₀^α₁)[o,l) there are 3 cases:
     *
     * ⟨...⟩^⟨...⟩  ie: o < o+l ≤ |α₀|
     *  [,)         so: (α₀^α₁)[o,l) ==> α₀[o,l) ^ α₁[0,0)
     *
     * ⟨...⟩^⟨...⟩  ie: o ≤ |α₀| < o+l
     *   [  ,  )    so: (α₀^α₁)[o,l) ==> α₀[o,|α₀|-o) ^ α₁[0,l-(|α₀|-o))
     *
     * ⟨...⟩^⟨...⟩  ie: |α₀| ≤ o
     *        [,)   so: (α₀^α₁)[o,l) ==> α₀[o,0) ^ α₁[o-|α₀|,l)
     *
     * So in general:
     *
     * (α₀^α₁)[o,l) ==> α₀[o,l₀) ^ α₁[o₁,l-l₀)
     * where l₀ = max 0 (min l |α₀|-o)
     *       o₁ = max 0 o-|α₀|
     *)
    | Concat na1N ->
        let n = Array.length na1N - 1 in
        let rec loop i oI lIN =
          let {siz= nI; seq= aI} = na1N.(i) in
          let j = i + 1 in
          if i = n then
            [{seq= extract ~seq:aI ~siz:nI ~off:oI ~len:lIN; siz= lIN}]
          else
            let oI_nI = sub oI nI in
            match (get_z oI_nI, get_z lIN) with
            | Some z, _ when Z.sign z >= 0 (* oᵢ ≥ |αᵢ| *) ->
                let oJ = oI_nI in
                let lJN = lIN in
                loop j oJ lJN
            | Some z, Some lIN ->
                let lI = Z.(max zero (min lIN (neg z))) in
                let oJ = zero in
                let lJN = Z.(lIN - lI) in
                let len = _Z lI in
                {seq= extract ~seq:aI ~siz:nI ~off:oI ~len; siz= len}
                :: loop j oJ (_Z lJN)
            | _ ->
                let naIN = Array.sub ~pos:i na1N in
                let seq = concat naIN in
                let siz = add oI lIN in
                [{seq= Extract {seq; siz; off= oI; len= lIN}; siz= lIN}]
        in
        concat (Array.of_list (loop 0 off len))
    (* ⟨n,a⟩[o,l) *)
    | _ -> Extract {seq; siz; off; len}

and concat xs =
  [%dbg]
    ~call:(fun {pf} -> pf "@ %a" pp (Concat xs))
    ~retn:(fun {pf} c ->
      pf "%a" pp c ;
      solvables_contained_in_ (trms (Concat xs)) c ;
      invariant c )
  @@ fun () ->
  (* (α^(β^γ)^δ) ==> (α^β^γ^δ) *)
  let flatten xs =
    if Array.for_all ~f:(function {seq= Concat _} -> false | _ -> true) xs
    then xs
    else
      Array.flat_map ~f:(function {seq= Concat s} -> s | na -> [|na|]) xs
  in
  let simp_adjacent {siz= m; seq= a} {siz= n; seq= b} =
    match (a, b) with
    (* ⟨n,a⟩[o,k)^⟨n,a⟩[o+k,l) ==> ⟨n,a⟩[o,k+l) when n ≥ o+k+l *)
    | ( Extract {seq= a; siz= n; off= o; len= k}
      , Extract {seq= a'; siz= n'; off= o_k; len= l} )
      when equal a a'
           && equal n n'
           && equal o_k (add o k)
           && partial_ge n (add o_k l) ->
        let kl = add k l in
        Some {seq= extract ~seq:a ~siz:n ~off:o ~len:kl; siz= kl}
    (* ⟨m,0⟩^⟨n,0⟩ ==> ⟨m+n,0⟩ *)
    | _ when a == zero && b == zero -> Some {siz= add m n; seq= a}
    (* ⟨m,E^⟩^⟨n,E^⟩ ==> ⟨m+n,E^⟩ *)
    | Splat _, _ when equal a b -> Some {seq= a; siz= add m n}
    | _ -> None
  in
  let xs = flatten xs in
  let xs = Array.reduce_adjacent ~f:simp_adjacent xs in
  match xs with
  (* ⟨_,a⟩ ==> a *)
  | [|{seq= a}|] -> a
  | [||] -> empty_seq
  | _ -> Concat xs

(* uninterpreted *)

let apply f es =
  ( match Funsym.eval ~equal ~get_z ~ret_z:_Z ~get_q ~ret_q:_Q f es with
  | Some c -> c
  | None -> Apply (f, es) )
  |> check (solvables_contained_in es)
  |> check invariant

(** Query *)

let fv e = Var.Set.of_iter (vars e)
let rec height e = 1 + Iter.fold ~f:(fun x -> max (height x)) (trms e) 0

(** Transform *)

let map_sized ({seq; siz} as na) ~f =
  map2 f na (fun seq siz -> {seq; siz}) seq siz

let rec map_vars e ~f =
  match e with
  | Var _ as v -> (f (Var.of_ v) : Var.t :> t)
  | Z _ | Q _ -> e
  | Arith a -> map1 (Arith.map ~f:(map_vars ~f)) e _Arith a
  | Splat x -> map1 (map_vars ~f) e splat x
  | Extract {seq; siz; off; len} ->
      map4 (map_vars ~f) e
        (fun seq siz off len -> extract ~seq ~siz ~off ~len)
        seq siz off len
  | Concat xs -> mapN (map_sized ~f:(map_vars ~f)) e concat xs
  | Apply (g, xs) -> mapN (map_vars ~f) e (apply g) xs

let map e ~f =
  match e with
  | Var _ | Z _ | Q _ -> e
  | Arith a -> map1 (Arith.map ~f) e _Arith a
  | Splat x -> map1 f e splat x
  | Extract {seq; siz; off; len} ->
      map4 f e
        (fun seq siz off len -> extract ~seq ~siz ~off ~len)
        seq siz off len
  | Concat xs -> mapN (map_sized ~f) e concat xs
  | Apply (g, xs) -> mapN f e (apply g) xs

let fold_map e = fold_map_from_map map e

let rec map_solvables e ~f =
  match classify e with
  | InterpAtom -> e
  | NonInterpAtom | UninterpApp -> f e
  | InterpApp -> map_solvable_trms ~f e

and map_solvable_trms e ~f = map ~f:(map_solvables ~f) e
