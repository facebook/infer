(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ses

let pp_boxed fs fmt =
  Format.pp_open_box fs 2 ;
  Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt

(*
 * Terms
 *)

(** Variable terms, represented as a subtype of general terms *)
module rec Var : sig
  include Var_intf.VAR with type t = private Trm.trm

  val of_ : Trm.trm -> t
end = struct
  module T = struct
    type t = Trm.trm [@@deriving compare, equal, sexp]

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
end

and Arith0 :
  (Arithmetic.REPRESENTATION
    with type var := Var.t
    with type trm := Trm.trm) =
  Arithmetic.Representation (Trm)

and Arith :
  (Arithmetic.S
    with type var := Var.t
    with type trm := Trm.trm
    with type t = Arith0.t) = struct
  include Arith0

  include Make (struct
    let get_arith (e : Trm.trm) =
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
  type var = Var.t

  type trm = private
    (* variables *)
    | Var of {id: int; name: string}
    (* arithmetic *)
    | Z of Z.t
    | Q of Q.t
    | Arith of Arith.t
    (* sequences (of flexible size) *)
    | Splat of trm
    | Sized of {seq: trm; siz: trm}
    | Extract of {seq: trm; off: trm; len: trm}
    | Concat of trm array
    (* records (with fixed indices) *)
    | Select of {idx: int; rcd: trm}
    | Update of {idx: int; rcd: trm; elt: trm}
    | Record of trm array
    | Ancestor of int
    (* uninterpreted *)
    | Apply of Funsym.t * trm array
  [@@deriving compare, equal, sexp]

  val ppx : Var.t Var.strength -> trm pp
  val _Var : int -> string -> trm
  val _Z : Z.t -> trm
  val _Q : Q.t -> trm
  val _Arith : Arith.t -> trm
  val _Splat : trm -> trm
  val _Sized : trm -> trm -> trm
  val _Extract : trm -> trm -> trm -> trm
  val _Concat : trm array -> trm
  val _Select : int -> trm -> trm
  val _Update : int -> trm -> trm -> trm
  val _Record : trm array -> trm
  val _Ancestor : int -> trm
  val _Apply : Funsym.t -> trm array -> trm
end = struct
  type var = Var.t

  type trm =
    | Var of {id: int; name: string}
    | Z of Z.t
    | Q of Q.t
    | Arith of Arith.t
    | Splat of trm
    | Sized of {seq: trm; siz: trm}
    | Extract of {seq: trm; off: trm; len: trm}
    | Concat of trm array
    | Select of {idx: int; rcd: trm}
    | Update of {idx: int; rcd: trm; elt: trm}
    | Record of trm array
    | Ancestor of int
    | Apply of Funsym.t * trm array
  [@@deriving compare, equal, sexp]

  let compare_trm x y =
    if x == y then 0
    else
      match (x, y) with
      | Var {id= i; name= _}, Var {id= j; name= _} when i > 0 && j > 0 ->
          Int.compare i j
      | _ -> compare_trm x y

  let equal_trm x y =
    x == y
    ||
    match (x, y) with
    | Var {id= i; name= _}, Var {id= j; name= _} when i > 0 && j > 0 ->
        Int.equal i j
    | _ -> equal_trm x y

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
                | _ -> raise Not_a_string )
          with
          | s -> Format.fprintf fs "%S" s
          | exception (Not_a_string | Z.Overflow | Failure _) ->
              Format.fprintf fs "@[<h>%a@]"
                (Array.pp ",@ " (ppx strength))
                elts )
        elts]

  let invariant e =
    let@ () = Invariant.invariant [%here] e [%sexp_of: trm] in
    match e with
    | Q q -> assert (not (Z.equal Z.one (Q.den q)))
    | Arith a -> (
      match Arith.classify a with
      | Compound -> ()
      | Trm _ | Const _ -> assert false )
    | _ -> ()

  (* destructors *)

  let get_z = function Z z -> Some z | _ -> None
  let get_q = function Q q -> Some q | Z z -> Some (Q.of_z z) | _ -> None

  (* constructors *)

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

  let _Splat x = Splat x |> check invariant
  let _Sized seq siz = Sized {seq; siz} |> check invariant
  let _Extract seq off len = Extract {seq; off; len} |> check invariant
  let _Concat es = Concat es |> check invariant
  let _Select idx rcd = Select {idx; rcd} |> check invariant
  let _Update idx rcd elt = Update {idx; rcd; elt} |> check invariant
  let _Record es = Record es |> check invariant
  let _Ancestor i = Ancestor i |> check invariant

  let _Apply f es =
    ( match
        Funsym.eval ~equal:equal_trm ~get_z ~ret_z:_Z ~get_q ~ret_q:_Q f es
      with
    | Some c -> c
    | None -> Apply (f, es) )
    |> check invariant
end

open Trm

let zero = _Z Z.zero
let one = _Z Z.one

(*
 * Formulas
 *)

include Propositional.Make (struct
  include Trm

  let zero = zero

  let eval_eq d e =
    match (d, e) with
    | Z y, Z z -> Some (Z.equal y z)
    | Q q, Q r -> Some (Q.equal q r)
    | _ -> None

  let eval_eq0 = function
    | Z z -> Some (Z.equal Z.zero z)
    | Q q -> Some (Q.equal Q.zero q)
    | _ -> None

  let eval_pos = function
    | Z z -> Some (Z.gt z Z.zero)
    | Q q -> Some (Q.gt q Q.zero)
    | _ -> None
end)

open Fml

(*
 * Conditional terms
 *)

(** Conditional terms, denoting functions from structures to values, taking
    the form of trees with internal nodes labeled with formulas and leaves
    labeled with terms. *)
type cnd = [`Ite of fml * cnd * cnd | `Trm of trm]
[@@deriving compare, equal, sexp]

(*
 * Expressions
 *)

(** Expressions, which are partitioned into terms, conditional terms, and
    formulas. *)
type exp = [cnd | `Fml of fml] [@@deriving compare, equal, sexp]

(*
 * Representation operations
 *)

(** pp *)

let ppx_f strength fs fml =
  let pp_t = Trm.ppx strength in
  let rec pp fs fml =
    let pf fmt = pp_boxed fs fmt in
    let pp_join sep pos neg =
      pf "(%a%t%a)" (Fmls.pp ~sep pp) pos
        (fun ppf ->
          if (not (Fmls.is_empty pos)) && not (Fmls.is_empty neg) then
            Format.fprintf ppf sep )
        (Fmls.pp ~sep (fun fs fml -> pp fs (_Not fml)))
        neg
    in
    match (fml : fml) with
    | Tt -> pf "tt"
    | Not Tt -> pf "ff"
    | Eq (x, y) -> pf "(%a@ = %a)" pp_t x pp_t y
    | Not (Eq (x, y)) -> pf "(%a@ @<2>≠ %a)" pp_t x pp_t y
    | Eq0 x -> pf "(0 = %a)" pp_t x
    | Not (Eq0 x) -> pf "(0 @<2>≠ %a)" pp_t x
    | Pos x -> pf "(0 < %a)" pp_t x
    | Not (Pos x) -> pf "(0 @<2>≥ %a)" pp_t x
    | Not x -> pf "@<1>¬%a" pp x
    | And {pos; neg} -> pp_join "@ @<2>∧ " pos neg
    | Or {pos; neg} -> pp_join "@ @<2>∨ " pos neg
    | Iff (x, y) -> pf "(%a@ <=> %a)" pp x pp y
    | Cond {cnd; pos; neg} ->
        pf "@[<hv 1>(%a@ ? %a@ : %a)@]" pp cnd pp pos pp neg
    | Lit (p, xs) -> pf "%a(%a)" Predsym.pp p (Array.pp ",@ " pp_t) xs
  in
  pp fs fml

let pp_f = ppx_f (fun _ -> None)

let ppx_c strength fs ct =
  let pp_t = Trm.ppx strength in
  let pp_f = ppx_f strength in
  let rec pp fs ct =
    let pf fmt = pp_boxed fs fmt in
    match ct with
    | `Ite (cnd, thn, els) -> pf "(%a@ ? %a@ : %a)" pp_f cnd pp thn pp els
    | `Trm t -> pp_t fs t
  in
  pp fs ct

let ppx strength fs = function
  | #cnd as c -> ppx_c strength fs c
  | `Fml f -> ppx_f strength fs f

let pp = ppx (fun _ -> None)

(** fold_vars *)

let fold_pos_neg ~pos ~neg s ~f =
  let f_not p s = f (_Not p) s in
  Fmls.fold ~f:f_not neg (Fmls.fold ~f pos s)

let rec fold_vars_t e s ~f =
  match e with
  | Z _ | Q _ | Ancestor _ -> s
  | Var _ as v -> f (Var.of_ v) s
  | Splat x | Select {rcd= x} -> fold_vars_t ~f x s
  | Sized {seq= x; siz= y} | Update {rcd= x; elt= y} ->
      fold_vars_t ~f x (fold_vars_t ~f y s)
  | Extract {seq= x; off= y; len= z} ->
      fold_vars_t ~f x (fold_vars_t ~f y (fold_vars_t ~f z s))
  | Concat xs | Record xs | Apply (_, xs) ->
      Array.fold ~f:(fold_vars_t ~f) xs s
  | Arith a -> Iter.fold ~f:(fold_vars_t ~f) (Arith.iter a) s

let rec fold_vars_f p s ~f =
  match (p : fml) with
  | Tt -> s
  | Eq (x, y) -> fold_vars_t ~f x (fold_vars_t ~f y s)
  | Eq0 x | Pos x -> fold_vars_t ~f x s
  | Not x -> fold_vars_f ~f x s
  | And {pos; neg} | Or {pos; neg} ->
      fold_pos_neg ~f:(fold_vars_f ~f) ~pos ~neg s
  | Iff (x, y) -> fold_vars_f ~f x (fold_vars_f ~f y s)
  | Cond {cnd; pos; neg} ->
      fold_vars_f ~f cnd (fold_vars_f ~f pos (fold_vars_f ~f neg s))
  | Lit (_, xs) -> Array.fold ~f:(fold_vars_t ~f) xs s

let rec fold_vars_c c s ~f =
  match c with
  | `Ite (cnd, thn, els) ->
      fold_vars_f ~f cnd (fold_vars_c ~f thn (fold_vars_c ~f els s))
  | `Trm t -> fold_vars_t ~f t s

let fold_vars e s ~f =
  match e with
  | `Fml p -> fold_vars_f ~f p s
  | #cnd as c -> fold_vars_c ~f c s

(** map *)

let map1 f e cons x =
  let x' = f x in
  if x == x' then e else cons x'

let map2 f e cons x y =
  let x' = f x in
  let y' = f y in
  if x == x' && y == y' then e else cons x' y'

let map3 f e cons x y z =
  let x' = f x in
  let y' = f y in
  let z' = f z in
  if x == x' && y == y' && z == z' then e else cons x' y' z'

let mapN f e cons xs =
  let xs' = Array.map_endo ~f xs in
  if xs' == xs then e else cons xs'

let map_pos_neg f e cons ~pos ~neg =
  let pos' = Fmls.map ~f pos in
  let neg' = Fmls.map ~f neg in
  if pos' == pos && neg' == neg then e else cons ~pos:pos' ~neg:neg'

(** map_trms *)

let rec map_trms_f ~f b =
  match b with
  | Tt -> b
  | Eq (x, y) -> map2 f b _Eq x y
  | Eq0 x -> map1 f b _Eq0 x
  | Pos x -> map1 f b _Pos x
  | Not x -> map1 (map_trms_f ~f) b _Not x
  | And {pos; neg} -> map_pos_neg (map_trms_f ~f) b _And ~pos ~neg
  | Or {pos; neg} -> map_pos_neg (map_trms_f ~f) b _Or ~pos ~neg
  | Iff (x, y) -> map2 (map_trms_f ~f) b _Iff x y
  | Cond {cnd; pos; neg} -> map3 (map_trms_f ~f) b _Cond cnd pos neg
  | Lit (p, xs) -> mapN f b (_Lit p) xs

(** map_vars *)

let rec map_vars_t ~f e =
  match e with
  | Var _ as v -> (f (Var.of_ v) : var :> trm)
  | Z _ | Q _ -> e
  | Arith a ->
      let a' = Arith.map ~f:(map_vars_t ~f) a in
      if a == a' then e else _Arith a'
  | Splat x -> map1 (map_vars_t ~f) e _Splat x
  | Sized {seq; siz} -> map2 (map_vars_t ~f) e _Sized seq siz
  | Extract {seq; off; len} -> map3 (map_vars_t ~f) e _Extract seq off len
  | Concat xs -> mapN (map_vars_t ~f) e _Concat xs
  | Select {idx; rcd} -> map1 (map_vars_t ~f) e (_Select idx) rcd
  | Update {idx; rcd; elt} -> map2 (map_vars_t ~f) e (_Update idx) rcd elt
  | Record xs -> mapN (map_vars_t ~f) e _Record xs
  | Ancestor _ -> e
  | Apply (g, xs) -> mapN (map_vars_t ~f) e (_Apply g) xs

let map_vars_f ~f = map_trms_f ~f:(map_vars_t ~f)

let rec map_vars_c ~f c =
  match c with
  | `Ite (cnd, thn, els) ->
      let cnd' = map_vars_f ~f cnd in
      let thn' = map_vars_c ~f thn in
      let els' = map_vars_c ~f els in
      if cnd' == cnd && thn' == thn && els' == els then c
      else `Ite (cnd', thn', els')
  | `Trm t ->
      let t' = map_vars_t ~f t in
      if t' == t then c else `Trm t'

let map_vars ~f = function
  | `Fml p -> `Fml (map_vars_f ~f p)
  | #cnd as c -> (map_vars_c ~f c :> exp)

(*
 * Core construction functions
 *
 * Support functions for constructing expressions as if terms and formulas
 * could be freely mixed, instead of being strictly partitioned into terms
 * and formulas stratified below conditional terms and then expressions.
 *)

(** Map a unary function on terms over the leaves of a conditional term,
    rebuilding the tree of conditionals with the supplied ite construction
    function. *)
let rec map_cnd : (fml -> 'a -> 'a -> 'a) -> (trm -> 'a) -> cnd -> 'a =
 fun f_ite f_trm -> function
  | `Trm trm -> f_trm trm
  | `Ite (cnd, thn, els) ->
      let thn' = map_cnd f_ite f_trm thn in
      let els' = map_cnd f_ite f_trm els in
      f_ite cnd thn' els'

(** Embed a formula into a conditional term (associating true with 1 and
    false with 0), identity on conditional terms. *)
let embed_into_cnd : exp -> cnd = function
  | #cnd as c -> c
  (* p ==> (p ? 1 : 0) *)
  | `Fml fml -> `Ite (fml, `Trm one, `Trm zero)

(** Project out a formula that is embedded into a conditional term.

    - [project_out_fml] is left inverse to [embed_into_cnd] in the sense
      that [project_out_fml (embed_into_cnd (`Fml f)) = Some f]. *)
let project_out_fml : cnd -> fml option = function
  (* (p ? 1 : 0) ==> p *)
  | `Ite (cnd, `Trm one', `Trm zero') when one == one' && zero == zero' ->
      Some cnd
  | _ -> None

(** Embed a conditional term into a formula (associating 0 with false and
    non-0 with true, lifted over the tree mapping conditional terms to
    conditional formulas), identity on formulas.

    - [embed_into_fml] is left inverse to [embed_into_cnd] in the sense that
      [embed_into_fml ((embed_into_cnd (`Fml f)) :> exp) = f].
    - [embed_into_fml] is not right inverse to [embed_into_cnd] since
      [embed_into_fml] can only preserve one bit of information from its
      argument. So in general
      [(embed_into_cnd (`Fml (embed_into_fml x)) :> exp)] is not equivalent
      to [x].
    - The weaker condition that
      [0 ≠ (embed_into_cnd (`Fml (embed_into_fml x)) :> exp)] iff
      [0 ≠ x] holds. *)
let embed_into_fml : exp -> fml = function
  | `Fml fml -> fml
  | #cnd as c -> map_cnd _Cond (fun e -> _Not (_Eq0 e)) c

(** Construct a conditional term, or formula if possible precisely. *)
let ite : fml -> exp -> exp -> exp =
 fun cnd thn els ->
  match (thn, els) with
  | `Fml pos, `Fml neg -> `Fml (_Cond cnd pos neg)
  | _ -> (
      let c = `Ite (cnd, embed_into_cnd thn, embed_into_cnd els) in
      match project_out_fml c with Some f -> `Fml f | None -> c )

(** Map a unary function on terms over an expression. *)
let ap1 : (trm -> exp) -> exp -> exp =
 fun f x -> map_cnd ite f (embed_into_cnd x)

let ap1t : (trm -> trm) -> exp -> exp = fun f -> ap1 (fun x -> `Trm (f x))

let ap1f : (trm -> fml) -> exp -> fml =
 fun f x -> map_cnd _Cond f (embed_into_cnd x)

(** Map a binary function on terms over conditional terms. This yields a
    conditional tree with the structure from the first argument where each
    leaf has been replaced by a conditional tree with the structure from the
    second argument where each leaf has been replaced by the application of
    the argument binary function to the corresponding leaves from the first
    and second argument. *)
let map2_cnd :
    (fml -> 'a -> 'a -> 'a) -> (trm -> trm -> 'a) -> cnd -> cnd -> 'a =
 fun f_ite f_trm x y ->
  map_cnd f_ite (fun x' -> map_cnd f_ite (fun y' -> f_trm x' y') y) x

(** Map a binary function on terms over expressions. *)
let ap2 : (trm -> trm -> exp) -> exp -> exp -> exp =
 fun f x y -> map2_cnd ite f (embed_into_cnd x) (embed_into_cnd y)

let ap2t : (trm -> trm -> trm) -> exp -> exp -> exp =
 fun f -> ap2 (fun x y -> `Trm (f x y))

let ap2f : (trm -> trm -> fml) -> exp -> exp -> fml =
 fun f x y -> map2_cnd _Cond f (embed_into_cnd x) (embed_into_cnd y)

(** Map a ternary function on terms over conditional terms. *)
let map3_cnd :
       (fml -> 'a -> 'a -> 'a)
    -> (trm -> trm -> trm -> 'a)
    -> cnd
    -> cnd
    -> cnd
    -> 'a =
 fun f_ite f_trm x y z ->
  map_cnd f_ite
    (fun x' ->
      map_cnd f_ite (fun y' -> map_cnd f_ite (fun z' -> f_trm x' y' z') z) y
      )
    x

(** Map a ternary function on terms over expressions. *)
let ap3 : (trm -> trm -> trm -> exp) -> exp -> exp -> exp -> exp =
 fun f x y z ->
  map3_cnd ite f (embed_into_cnd x) (embed_into_cnd y) (embed_into_cnd z)

let ap3t : (trm -> trm -> trm -> trm) -> exp -> exp -> exp -> exp =
 fun f -> ap3 (fun x y z -> `Trm (f x y z))

(** Reverse-map an nary function on terms over conditional terms. *)
let rev_mapN_cnd :
    (fml -> 'a -> 'a -> 'a) -> (trm list -> 'a) -> cnd list -> 'a =
 fun f_ite f_trms rev_xs ->
  let rec loop xs' = function
    | x :: xs -> map_cnd f_ite (fun x' -> loop (x' :: xs') xs) x
    | [] -> f_trms xs'
  in
  loop [] rev_xs

(** Map an nary function on terms over expressions. *)
let apNt : (trm array -> trm) -> exp array -> exp =
 fun f xs ->
  rev_mapN_cnd ite
    (fun xs -> `Trm (f (Array.of_list xs)))
    (Array.to_list_rev_map ~f:embed_into_cnd xs)

let apNf : (trm array -> fml) -> exp array -> fml =
 fun f xs ->
  rev_mapN_cnd _Cond
    (fun xs -> f (Array.of_list xs))
    (Array.to_list_rev_map ~f:embed_into_cnd xs)

(*
 * Terms: exposed interface
 *)

module Term = struct
  (* Exposed terms are represented as expressions, which allow formulas to
     appear at toplevel, although semantically these are redundant with
     their [inject]ion into [trm] proper. This redundancy of representation
     is allowed in order to avoid churning formulas back and forth between
     [fml] and [cnd] via [inject] and [project] in cases where formulas only
     transiently pass through term contexts. The construction functions will
     convert such a formula [p] into [(p ? 1 : 0)] as soon as it is used as
     a subterm, so this redundancy is only lazily delaying normalization by
     one step. *)
  module T = struct
    type t = exp [@@deriving compare, equal, sexp]
  end

  include T
  module Map = Map.Make (T)

  let ppx = ppx
  let pp = pp

  (* variables *)

  let var v = `Trm (v : var :> trm)

  (* arithmetic *)

  let zero = `Trm zero
  let one = `Trm one
  let integer z = `Trm (_Z z)
  let rational q = `Trm (_Q q)
  let neg = ap1t @@ fun x -> _Arith Arith.(neg (trm x))
  let add = ap2t @@ fun x y -> _Arith Arith.(add (trm x) (trm y))
  let sub = ap2t @@ fun x y -> _Arith Arith.(sub (trm x) (trm y))
  let mulq q = ap1t @@ fun x -> _Arith Arith.(mulc q (trm x))
  let mul = ap2t @@ fun x y -> _Arith (Arith.mul x y)
  let div = ap2t @@ fun x y -> _Arith (Arith.div x y)
  let pow x i = (ap1t @@ fun x -> _Arith (Arith.pow x i)) x

  (* sequences *)

  let splat = ap1t _Splat
  let sized ~seq ~siz = ap2t _Sized seq siz
  let extract ~seq ~off ~len = ap3t _Extract seq off len
  let concat elts = apNt _Concat elts

  (* records *)

  let select ~rcd ~idx = ap1t (_Select idx) rcd
  let update ~rcd ~idx ~elt = ap2t (_Update idx) rcd elt
  let record elts = apNt _Record elts
  let ancestor i = `Trm (_Ancestor i)

  (* uninterpreted *)

  let apply sym args = apNt (_Apply sym) args

  (* if-then-else *)

  let ite ~cnd ~thn ~els = ite cnd thn els

  (** Destruct *)

  let d_int = function `Trm (Z z) -> Some z | _ -> None

  let get_const = function
    | `Trm (Z z) -> Some (Q.of_z z)
    | `Trm (Q q) -> Some q
    | _ -> None

  (** Access *)

  let split_const = function
    | `Trm (Z z) -> (zero, Q.of_z z)
    | `Trm (Q q) -> (zero, q)
    | `Trm (Arith a) ->
        let a_c, c = Arith.split_const a in
        (`Trm (_Arith a_c), c)
    | e -> (e, Q.zero)

  (** Traverse *)

  let fold_vars = fold_vars

  (** Transform *)

  let map_vars = map_vars

  let fold_map_vars e s0 ~f =
    let s = ref s0 in
    let f x =
      let x', s' = f x !s in
      s := s' ;
      x'
    in
    let e' = map_vars ~f e in
    (e', !s)

  let rename s e = map_vars ~f:(Var.Subst.apply s) e

  (** Query *)

  let fv e = fold_vars ~f:Var.Set.add e Var.Set.empty
end

(*
 * Formulas: exposed interface
 *)

module Formula = struct
  type t = fml [@@deriving compare, equal, sexp]

  let inject f = `Fml f
  let project = function `Fml f -> Some f | #cnd as c -> project_out_fml c
  let ppx = ppx_f
  let pp = pp_f

  (* constants *)

  let tt = mk_Tt ()
  let ff = _Not tt

  (* comparisons *)

  let eq = ap2f _Eq
  let dq a b = _Not (eq a b)
  let eq0 = ap1f _Eq0
  let dq0 a = _Not (eq0 a)
  let pos = ap1f _Pos

  (* a > b iff a-b > 0 iff 0 < a-b *)
  let gt a b = if b == Term.zero then pos a else pos (Term.sub a b)

  (* a ≥ b iff 0 ≥ b-a iff ¬(0 < b-a) *)
  let ge a b =
    if a == Term.zero then _Not (pos b) else _Not (pos (Term.sub b a))

  let lt a b = gt b a
  let le a b = ge b a

  (* uninterpreted *)

  let lit p es = apNf (_Lit p) es

  (* connectives *)

  let and_ = and_
  let andN = function [] -> tt | b :: bs -> List.fold ~f:and_ bs b
  let or_ = or_
  let orN = function [] -> ff | b :: bs -> List.fold ~f:or_ bs b
  let iff = _Iff
  let xor p q = _Not (_Iff p q)
  let cond ~cnd ~pos ~neg = _Cond cnd pos neg
  let not_ = _Not

  (** Query *)

  let fv e = fold_vars_f ~f:Var.Set.add e Var.Set.empty

  (** Traverse *)

  let fold_vars = fold_vars_f

  (** Transform *)

  let map_vars = map_vars_f

  let rec map_terms ~f b =
    let lift_map1 : (exp -> exp) -> t -> (trm -> t) -> trm -> t =
     fun f b cons x -> map1 f b (ap1f cons) (`Trm x)
    in
    let lift_map2 :
        (exp -> exp) -> t -> (trm -> trm -> t) -> trm -> trm -> t =
     fun f b cons x y -> map2 f b (ap2f cons) (`Trm x) (`Trm y)
    in
    let lift_mapN : (exp -> exp) -> t -> (trm array -> t) -> trm array -> t
        =
     fun f b cons xs ->
      mapN f b (apNf cons) (Array.map ~f:(fun x -> `Trm x) xs)
    in
    match b with
    | Tt -> b
    | Eq (x, y) -> lift_map2 f b _Eq x y
    | Eq0 x -> lift_map1 f b _Eq0 x
    | Pos x -> lift_map1 f b _Pos x
    | Not x -> map1 (map_terms ~f) b _Not x
    | And {pos; neg} -> map_pos_neg (map_terms ~f) b _And ~pos ~neg
    | Or {pos; neg} -> map_pos_neg (map_terms ~f) b _Or ~pos ~neg
    | Iff (x, y) -> map2 (map_terms ~f) b _Iff x y
    | Cond {cnd; pos; neg} -> map3 (map_terms ~f) b _Cond cnd pos neg
    | Lit (p, xs) -> lift_mapN f b (_Lit p) xs

  let fold_map_vars e s0 ~f =
    let s = ref s0 in
    let f x =
      let x', s' = f x !s in
      s := s' ;
      x'
    in
    let e' = map_vars ~f e in
    (e', !s)

  let rename s e = map_vars ~f:(Var.Subst.apply s) e

  let fold_dnf :
         meet1:('literal -> 'conjunction -> 'conjunction)
      -> join1:('conjunction -> 'disjunction -> 'disjunction)
      -> top:'conjunction
      -> bot:'disjunction
      -> 'formula
      -> 'disjunction =
   fun ~meet1 ~join1 ~top ~bot fml ->
    let rec add_conjunct fml (cjn, splits) =
      match fml with
      | Tt | Eq _ | Eq0 _ | Pos _ | Iff _ | Lit _ | Not _ ->
          (meet1 fml cjn, splits)
      | And {pos; neg} ->
          fold_pos_neg ~f:add_conjunct ~pos ~neg (cjn, splits)
      | Or {pos; neg} -> (cjn, (pos, neg) :: splits)
      | Cond {cnd; pos; neg} ->
          add_conjunct
            (or_ (and_ cnd pos) (and_ (not_ cnd) neg))
            (cjn, splits)
    in
    let rec add_disjunct (cjn, splits) fml djn =
      let cjn, splits = add_conjunct fml (cjn, splits) in
      match splits with
      | (pos, neg) :: splits ->
          fold_pos_neg ~f:(add_disjunct (cjn, splits)) ~pos ~neg djn
      | [] -> join1 cjn djn
    in
    add_disjunct (top, []) fml bot
end

(*
 * Convert to Ses
 *)

let v_to_ses : var -> Ses.Var.t =
 fun v -> Ses.Var.identified ~id:(Var.id v) ~name:(Var.name v)

let vs_to_ses : Var.Set.t -> Ses.Var.Set.t =
 fun vs ->
  Var.Set.fold vs Ses.Var.Set.empty ~f:(fun v ->
      Ses.Var.Set.add (v_to_ses v) )

let rec arith_to_ses poly =
  Arith.fold_monomials poly Ses.Term.zero ~f:(fun mono coeff e ->
      Ses.Term.add e
        (Ses.Term.mulq coeff
           (Arith.fold_factors mono Ses.Term.one ~f:(fun trm pow f ->
                let rec exp b i =
                  assert (i > 0) ;
                  if i = 1 then b else Ses.Term.mul b (exp f (i - 1))
                in
                Ses.Term.mul f (exp (t_to_ses trm) pow) ))) )

and t_to_ses : trm -> Ses.Term.t = function
  | Var {name; id} -> Ses.Term.var (Ses.Var.identified ~name ~id)
  | Z z -> Ses.Term.integer z
  | Q q -> Ses.Term.rational q
  | Arith a -> arith_to_ses a
  | Splat x -> Ses.Term.splat (t_to_ses x)
  | Sized {seq; siz} ->
      Ses.Term.sized ~seq:(t_to_ses seq) ~siz:(t_to_ses siz)
  | Extract {seq; off; len} ->
      Ses.Term.extract ~seq:(t_to_ses seq) ~off:(t_to_ses off)
        ~len:(t_to_ses len)
  | Concat es -> Ses.Term.concat (Array.map ~f:t_to_ses es)
  | Select {idx; rcd} -> Ses.Term.select ~rcd:(t_to_ses rcd) ~idx
  | Update {idx; rcd; elt} ->
      Ses.Term.update ~rcd:(t_to_ses rcd) ~idx ~elt:(t_to_ses elt)
  | Record es ->
      Ses.Term.record (IArray.of_array (Array.map ~f:t_to_ses es))
  | Ancestor i -> Ses.Term.rec_record i
  | Apply (Rem, [|x; y|]) -> Ses.Term.rem (t_to_ses x) (t_to_ses y)
  | Apply (BitAnd, [|x; y|]) -> Ses.Term.and_ (t_to_ses x) (t_to_ses y)
  | Apply (BitOr, [|x; y|]) -> Ses.Term.or_ (t_to_ses x) (t_to_ses y)
  | Apply (BitXor, [|x; y|]) -> Ses.Term.dq (t_to_ses x) (t_to_ses y)
  | Apply (BitShl, [|x; y|]) -> Ses.Term.shl (t_to_ses x) (t_to_ses y)
  | Apply (BitLshr, [|x; y|]) -> Ses.Term.lshr (t_to_ses x) (t_to_ses y)
  | Apply (BitAshr, [|x; y|]) -> Ses.Term.ashr (t_to_ses x) (t_to_ses y)
  | Apply (Signed n, [|x|]) -> Ses.Term.signed n (t_to_ses x)
  | Apply (Unsigned n, [|x|]) -> Ses.Term.unsigned n (t_to_ses x)
  | Apply (sym, xs) ->
      Ses.Term.apply sym (IArray.of_array (Array.map ~f:t_to_ses xs))

let rec f_to_ses : fml -> Ses.Term.t = function
  | Tt -> Ses.Term.true_
  | Not Tt -> Ses.Term.false_
  | Eq (x, y) -> Ses.Term.eq (t_to_ses x) (t_to_ses y)
  | Eq0 x -> Ses.Term.eq Ses.Term.zero (t_to_ses x)
  | Pos x -> Ses.Term.lt Ses.Term.zero (t_to_ses x)
  | Not p -> Ses.Term.not_ (f_to_ses p)
  | And {pos; neg} ->
      fold_pos_neg
        ~f:(fun f p -> Ses.Term.and_ p (f_to_ses f))
        ~pos ~neg Ses.Term.true_
  | Or {pos; neg} ->
      fold_pos_neg
        ~f:(fun f p -> Ses.Term.or_ p (f_to_ses f))
        ~pos ~neg Ses.Term.false_
  | Iff (p, q) -> Ses.Term.eq (f_to_ses p) (f_to_ses q)
  | Cond {cnd; pos; neg} ->
      Ses.Term.conditional ~cnd:(f_to_ses cnd) ~thn:(f_to_ses pos)
        ~els:(f_to_ses neg)
  | Lit (sym, args) ->
      Ses.Term.poslit sym (IArray.of_array (Array.map ~f:t_to_ses args))

let rec to_ses : exp -> Ses.Term.t = function
  | `Ite (cnd, thn, els) ->
      Ses.Term.conditional ~cnd:(f_to_ses cnd)
        ~thn:(to_ses (thn :> exp))
        ~els:(to_ses (els :> exp))
  | `Trm t -> t_to_ses t
  | `Fml f -> f_to_ses f

(*
 * Convert from Ses
 *)

let v_of_ses : Ses.Var.t -> var =
 fun v -> Var.identified ~id:(Ses.Var.id v) ~name:(Ses.Var.name v)

let vs_of_ses : Ses.Var.Set.t -> Var.Set.t =
 fun vs ->
  Ses.Var.Set.fold ~f:(fun v -> Var.Set.add (v_of_ses v)) vs Var.Set.empty

let uap1 f = ap1t (fun x -> _Apply f [|x|])
let uap2 f = ap2t (fun x y -> _Apply f [|x; y|])
let litN p = apNf (_Lit p)

let rec uap_tt f a = uap1 f (of_ses a)
and uap_ttt f a b = uap2 f (of_ses a) (of_ses b)

and ap2 mk_f mk_t a b =
  match (of_ses a, of_ses b) with
  | `Fml p, `Fml q -> `Fml (mk_f p q)
  | x, y -> mk_t x y

and ap2_f mk_f mk_t a b = ap2 mk_f (fun x y -> `Fml (mk_t x y)) a b

and apN mk_f mk_t mk_unit es =
  match
    Ses.Term.Set.fold es (None, None) ~f:(fun e (fs, ts) ->
        match of_ses e with
        | `Fml f ->
            (Some (match fs with None -> f | Some g -> mk_f f g), ts)
        | t -> (fs, Some (match ts with None -> t | Some u -> mk_t t u)) )
  with
  | Some f, Some t -> mk_t t (Formula.inject f)
  | Some f, None -> `Fml f
  | None, Some t -> t
  | None, None -> `Fml mk_unit

and of_ses : Ses.Term.t -> exp =
 fun t ->
  let open Term in
  let open Formula in
  match t with
  | Var {id; name} -> var (Var.identified ~id ~name)
  | Integer {data} -> integer data
  | Rational {data} -> rational data
  | Ap1 (Signed {bits}, e) -> uap_tt (Signed bits) e
  | Ap1 (Unsigned {bits}, e) -> uap_tt (Unsigned bits) e
  | Ap2 (Eq, d, e) -> ap2_f iff eq d e
  | Ap2 (Dq, d, e) -> ap2_f xor dq d e
  | Ap2 (Lt, d, e) -> ap2_f (fun p q -> and_ (not_ p) q) lt d e
  | Ap2 (Le, d, e) -> ap2_f (fun p q -> or_ (not_ p) q) le d e
  | PosLit (p, es) ->
      `Fml (litN p (IArray.to_array (IArray.map ~f:of_ses es)))
  | NegLit (p, es) ->
      `Fml (not_ (litN p (IArray.to_array (IArray.map ~f:of_ses es))))
  | Add sum -> (
    match Ses.Term.Qset.pop_min_elt sum with
    | None -> zero
    | Some (e, q, sum) ->
        let mul e q =
          if Q.equal Q.one q then of_ses e
          else
            match of_ses e with
            | `Trm (Z z) -> rational (Q.mul q (Q.of_z z))
            | `Trm (Q r) -> rational (Q.mul q r)
            | t -> mulq q t
        in
        Ses.Term.Qset.fold ~f:(fun e q -> add (mul e q)) sum (mul e q) )
  | Mul prod -> (
    match Ses.Term.Qset.pop_min_elt prod with
    | None -> one
    | Some (e, q, prod) ->
        let rec expn e n =
          let p = Z.pred n in
          if Z.sign p = 0 then e else mul e (expn e p)
        in
        let exp e q =
          let n = Q.num q in
          let sn = Z.sign n in
          if sn = 0 then of_ses e
          else if sn > 0 then expn (of_ses e) n
          else div one (expn (of_ses e) (Z.neg n))
        in
        Ses.Term.Qset.fold ~f:(fun e q -> mul (exp e q)) prod (exp e q) )
  | Ap2 (Div, d, e) -> div (of_ses d) (of_ses e)
  | Ap2 (Rem, d, e) -> uap_ttt Rem d e
  | And es -> apN and_ (uap2 BitAnd) tt es
  | Or es -> apN or_ (uap2 BitOr) ff es
  | Ap2 (Xor, d, e) -> ap2 xor (uap2 BitXor) d e
  | Ap2 (Shl, d, e) -> uap_ttt BitShl d e
  | Ap2 (Lshr, d, e) -> uap_ttt BitLshr d e
  | Ap2 (Ashr, d, e) -> uap_ttt BitAshr d e
  | Ap3 (Conditional, cnd, thn, els) -> (
      let cnd = embed_into_fml (of_ses cnd) in
      match (of_ses thn, of_ses els) with
      | `Fml pos, `Fml neg -> `Fml (cond ~cnd ~pos ~neg)
      | thn, els -> ite ~cnd ~thn ~els )
  | Ap1 (Splat, byt) -> splat (of_ses byt)
  | Ap3 (Extract, seq, off, len) ->
      extract ~seq:(of_ses seq) ~off:(of_ses off) ~len:(of_ses len)
  | Ap2 (Sized, siz, seq) -> sized ~seq:(of_ses seq) ~siz:(of_ses siz)
  | ApN (Concat, args) ->
      concat (Array.map ~f:of_ses (IArray.to_array args))
  | Ap1 (Select idx, rcd) -> select ~rcd:(of_ses rcd) ~idx
  | Ap2 (Update idx, rcd, elt) ->
      update ~rcd:(of_ses rcd) ~idx ~elt:(of_ses elt)
  | ApN (Record, elts) ->
      record (Array.map ~f:of_ses (IArray.to_array elts))
  | RecRecord i -> ancestor i
  | Apply (sym, args) ->
      apply sym (Array.map ~f:of_ses (IArray.to_array args))

let f_of_ses e = embed_into_fml (of_ses e)

let v_map_ses : (var -> var) -> Ses.Var.t -> Ses.Var.t =
 fun f x ->
  let v = v_of_ses x in
  let v' = f v in
  if v' == v then x else v_to_ses v'

let ses_map : (Ses.Term.t -> Ses.Term.t) -> exp -> exp =
 fun f x -> of_ses (f (to_ses x))

let f_ses_map : (Ses.Term.t -> Ses.Term.t) -> fml -> fml =
 fun f x -> f_of_ses (f (f_to_ses x))

(*
 * Contexts
 *)

module Context = struct
  type t = Ses.Equality.t [@@deriving sexp]

  let invariant = Ses.Equality.invariant

  (* Query *)

  let fold_vars x s ~f =
    Ses.Equality.fold_vars ~f:(fun v -> f (v_of_ses v)) x s

  let fv e = fold_vars ~f:Var.Set.add e Var.Set.empty
  let is_empty x = Ses.Equality.is_true x
  let is_unsat x = Ses.Equality.is_false x
  let implies x b = Ses.Equality.implies x (f_to_ses b)

  let refutes x b =
    Ses.Term.is_false (Ses.Equality.normalize x (f_to_ses b))

  let normalize x e = ses_map (Ses.Equality.normalize x) e

  (* Classes *)

  let class_of x e = List.map ~f:of_ses (Ses.Equality.class_of x (to_ses e))

  let classes x =
    Ses.Term.Map.fold (Ses.Equality.classes x) Term.Map.empty
      ~f:(fun ~key:rep ~data:cls clss ->
        let rep' = of_ses rep in
        let cls' = List.map ~f:of_ses cls in
        Term.Map.add ~key:rep' ~data:cls' clss )

  let diff_classes r s =
    Term.Map.filter_mapi (classes r) ~f:(fun ~key:rep ~data:cls ->
        match
          List.filter cls ~f:(fun exp ->
              not (implies s (Formula.eq rep exp)) )
        with
        | [] -> None
        | cls -> Some cls )

  (* Pretty printing *)

  let pp_raw = Ses.Equality.pp
  let ppx_cls x = List.pp "@ = " (Term.ppx x)

  let ppx_classes x fs clss =
    List.pp "@ @<2>∧ "
      (fun fs (rep, cls) ->
        Format.fprintf fs "@[%a@ = %a@]" (Term.ppx x) rep (ppx_cls x)
          (List.sort ~cmp:Term.compare cls) )
      fs
      (Iter.to_list (Term.Map.to_iter clss))

  let pp fs r = ppx_classes (fun _ -> None) fs (classes r)

  let ppx var_strength fs clss noneqs =
    let first = Term.Map.is_empty clss in
    if not first then Format.fprintf fs "  " ;
    ppx_classes var_strength fs clss ;
    List.pp
      ~pre:(if first then "@[  " else "@ @[@<2>∧ ")
      "@ @<2>∧ "
      (Formula.ppx var_strength)
      fs noneqs ~suf:"@]" ;
    first && List.is_empty noneqs

  let ppx_diff var_strength fs parent_ctx fml ctx =
    let fml' = f_ses_map (Ses.Equality.normalize ctx) fml in
    ppx var_strength fs
      (diff_classes ctx parent_ctx)
      (if Formula.(equal tt fml') then [] else [fml'])

  (* Construct *)

  let empty = Ses.Equality.true_

  let add vs f x =
    let vs', x' = Ses.Equality.and_term (vs_to_ses vs) (f_to_ses f) x in
    (vs_of_ses vs', x')

  let union vs x y =
    let vs', z = Ses.Equality.and_ (vs_to_ses vs) x y in
    (vs_of_ses vs', z)

  let inter vs x y =
    let vs', z = Ses.Equality.or_ (vs_to_ses vs) x y in
    (vs_of_ses vs', z)

  let interN vs xs =
    let vs', z = Ses.Equality.orN (vs_to_ses vs) xs in
    (vs_of_ses vs', z)

  let dnf f =
    let meet1 a (vs, p, x) =
      let vs, x = add vs a x in
      (vs, Formula.and_ p a, x)
    in
    let join1 = Iter.cons in
    let top = (Var.Set.empty, Formula.tt, empty) in
    let bot = Iter.empty in
    Formula.fold_dnf ~meet1 ~join1 ~top ~bot f

  let rename x sub = Ses.Equality.rename x (v_map_ses (Var.Subst.apply sub))

  (* Substs *)

  module Subst = struct
    type t = Ses.Equality.Subst.t [@@deriving sexp]

    let pp = Ses.Equality.Subst.pp
    let is_empty = Ses.Equality.Subst.is_empty

    let fold s z ~f =
      Ses.Equality.Subst.fold s z ~f:(fun ~key ~data ->
          f ~key:(of_ses key) ~data:(of_ses data) )

    let subst s = ses_map (Ses.Equality.Subst.subst s)

    let partition_valid vs s =
      let t, ks, u = Ses.Equality.Subst.partition_valid (vs_to_ses vs) s in
      (t, vs_of_ses ks, u)
  end

  let apply_subst vs s x =
    let vs', x' = Ses.Equality.apply_subst (vs_to_ses vs) s x in
    (vs_of_ses vs', x')

  let solve_for_vars vss x =
    Ses.Equality.solve_for_vars (List.map ~f:vs_to_ses vss) x

  let elim vs x = Ses.Equality.elim (vs_to_ses vs) x

  (* Replay debugging *)

  type call =
    | Add of Var.Set.t * fml * t
    | Union of Var.Set.t * t * t
    | Inter of Var.Set.t * t * t
    | InterN of Var.Set.t * t list
    | Rename of t * Var.Subst.t
    | Is_unsat of t
    | Implies of t * fml
    | Refutes of t * fml
    | Normalize of t * exp
    | Apply_subst of Var.Set.t * Subst.t * t
    | Solve_for_vars of Var.Set.t list * t
    | Elim of Var.Set.t * t
  [@@deriving sexp]

  let replay c =
    match call_of_sexp (Sexp.of_string c) with
    | Add (us, e, r) -> add us e r |> ignore
    | Union (us, r, s) -> union us r s |> ignore
    | Inter (us, r, s) -> inter us r s |> ignore
    | InterN (us, rs) -> interN us rs |> ignore
    | Rename (r, s) -> rename r s |> ignore
    | Is_unsat r -> is_unsat r |> ignore
    | Implies (r, f) -> implies r f |> ignore
    | Refutes (r, f) -> refutes r f |> ignore
    | Normalize (r, e) -> normalize r e |> ignore
    | Apply_subst (us, s, r) -> apply_subst us s r |> ignore
    | Solve_for_vars (vss, r) -> solve_for_vars vss r |> ignore
    | Elim (ks, r) -> elim ks r |> ignore

  (* Debug wrappers *)

  let report ~name ~elapsed ~aggregate ~count =
    Format.eprintf "%15s time: %12.3f ms  %12.3f ms  %12d calls@." name
      elapsed aggregate count

  let dump_threshold = ref 1000.

  let wrap tmr f call =
    let f () =
      Timer.start tmr ;
      let r = f () in
      Timer.stop_report tmr (fun ~name ~elapsed ~aggregate ~count ->
          report ~name ~elapsed ~aggregate ~count ;
          if Float.(elapsed > !dump_threshold) then (
            dump_threshold := 2. *. !dump_threshold ;
            Format.eprintf "@\n%a@\n@." Sexp.pp_hum (sexp_of_call (call ()))
            ) ) ;
      r
    in
    if not [%debug] then f ()
    else
      try f ()
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        let sexp = sexp_of_call (call ()) in
        raise (Replay (exn, bt, sexp))

  let add_tmr = Timer.create "add" ~at_exit:report
  let union_tmr = Timer.create "union" ~at_exit:report
  let inter_tmr = Timer.create "inter" ~at_exit:report
  let interN_tmr = Timer.create "interN" ~at_exit:report
  let rename_tmr = Timer.create "rename" ~at_exit:report
  let is_unsat_tmr = Timer.create "is_unsat" ~at_exit:report
  let implies_tmr = Timer.create "implies" ~at_exit:report
  let refutes_tmr = Timer.create "refutes" ~at_exit:report
  let normalize_tmr = Timer.create "normalize" ~at_exit:report
  let apply_subst_tmr = Timer.create "apply_subst" ~at_exit:report
  let solve_for_vars_tmr = Timer.create "solve_for_vars" ~at_exit:report
  let elim_tmr = Timer.create "elim" ~at_exit:report

  let add us e r =
    wrap add_tmr (fun () -> add us e r) (fun () -> Add (us, e, r))

  let union us r s =
    wrap union_tmr (fun () -> union us r s) (fun () -> Union (us, r, s))

  let inter us r s =
    wrap inter_tmr (fun () -> inter us r s) (fun () -> Inter (us, r, s))

  let interN us rs =
    wrap interN_tmr (fun () -> interN us rs) (fun () -> InterN (us, rs))

  let rename r s =
    wrap rename_tmr (fun () -> rename r s) (fun () -> Rename (r, s))

  let is_unsat r =
    wrap is_unsat_tmr (fun () -> is_unsat r) (fun () -> Is_unsat r)

  let implies r f =
    wrap implies_tmr (fun () -> implies r f) (fun () -> Implies (r, f))

  let refutes r f =
    wrap refutes_tmr (fun () -> refutes r f) (fun () -> Refutes (r, f))

  let normalize r e =
    wrap normalize_tmr (fun () -> normalize r e) (fun () -> Normalize (r, e))

  let apply_subst us s r =
    wrap apply_subst_tmr
      (fun () -> apply_subst us s r)
      (fun () -> Apply_subst (us, s, r))

  let solve_for_vars vss r =
    wrap solve_for_vars_tmr
      (fun () -> solve_for_vars vss r)
      (fun () -> Solve_for_vars (vss, r))

  let elim ks r =
    wrap elim_tmr (fun () -> elim ks r) (fun () -> Elim (ks, r))
end
