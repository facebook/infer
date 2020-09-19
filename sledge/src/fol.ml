(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let pp_boxed fs fmt =
  Format.pp_open_box fs 2 ;
  Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt

(*
 * (Uninterpreted) Function Symbols
 *)

module Funsym = struct
  type t =
    | Float of string
    | Label of {parent: string; name: string}
    | Mul
    | Div
    | Rem
    | EmptyRecord
    | RecRecord of int
    | BitAnd
    | BitOr
    | BitXor
    | BitShl
    | BitLshr
    | BitAshr
    | Signed of int
    | Unsigned of int
    | Convert of {src: Llair.Typ.t; dst: Llair.Typ.t}
  [@@deriving compare, equal, sexp]

  let pp fs f =
    let pf fmt = pp_boxed fs fmt in
    match f with
    | Float s -> pf "%s" s
    | Label {name} -> pf "%s" name
    | Mul -> pf "@<1>×"
    | Div -> pf "/"
    | Rem -> pf "%%"
    | EmptyRecord -> pf "{}"
    | RecRecord i -> pf "(rec_record %i)" i
    | BitAnd -> pf "&&"
    | BitOr -> pf "||"
    | BitXor -> pf "xor"
    | BitShl -> pf "shl"
    | BitLshr -> pf "lshr"
    | BitAshr -> pf "ashr"
    | Signed n -> pf "(s%i)" n
    | Unsigned n -> pf "(u%i)" n
    | Convert {src; dst} -> pf "(%a)(%a)" Llair.Typ.pp dst Llair.Typ.pp src
end

(*
 * Terms
 *)

(** Terms, denoting functions from structures to values, built from
    variables and applications of function symbols from various theories. *)
type trm =
  | Var of {id: int; name: string}
  | Z of Z.t
  | Q of Q.t
  | Neg of trm
  | Add of trm * trm
  | Sub of trm * trm
  | Mulq of Q.t * trm
  | Splat of trm
  | Sized of {seq: trm; siz: trm}
  | Extract of {seq: trm; off: trm; len: trm}
  | Concat of trm array
  | Select of {rcd: trm; idx: trm}
  | Update of {rcd: trm; idx: trm; elt: trm}
  | Tuple of trm array
  | Project of {ary: int; idx: int; tup: trm}
  | Apply of Funsym.t * trm
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

let zero = Z Z.zero
let one = Z Z.one
let _Neg x = Neg x

let _Add x y =
  match (x, y) with
  | _, Q q when Q.sign q = 0 -> x
  | Q q, _ when Q.sign q = 0 -> y
  | _ -> Add (x, y)

let _Sub x y = Sub (x, y)

let _Mulq q x =
  if Q.equal Q.one q then x else if Q.sign q = 0 then zero else Mulq (q, x)

let _Splat x = Splat x
let _Sized seq siz = Sized {seq; siz}
let _Extract seq off len = Extract {seq; off; len}
let _Concat es = Concat es
let _Select rcd idx = Select {rcd; idx}
let _Update rcd idx elt = Update {rcd; idx; elt}
let _Tuple es = Tuple es
let _Project ary idx tup = Project {ary; idx; tup}
let _Apply f a = Apply (f, a)

(*
 * (Uninterpreted) Predicate Symbols
 *)

module Predsym = struct
  type t = Ordered [@@deriving compare, equal, hash, sexp]

  let pp fs p =
    let pf fmt = pp_boxed fs fmt in
    match p with Ordered -> pf "ordered"
end

(*
 * Formulas
 *)

(** Formulas, denoting sets of structures, built from propositional
    variables, applications of predicate symbols from various theories, and
    first-order logic connectives. *)
module Fml : sig
  type fml = private
    | Tt
    | Ff
    | Eq of trm * trm
    | Dq of trm * trm
    | Eq0 of trm  (** [Eq0(x)] iff x = 0 *)
    | Dq0 of trm  (** [Dq0(x)] iff x ≠ 0 *)
    | Gt0 of trm  (** [Gt0(x)] iff x > 0 *)
    | Ge0 of trm  (** [Ge0(x)] iff x ≥ 0 *)
    | Lt0 of trm  (** [Lt0(x)] iff x < 0 *)
    | Le0 of trm  (** [Le0(x)] iff x ≤ 0 *)
    | And of fml * fml
    | Or of fml * fml
    | Iff of fml * fml
    | Xor of fml * fml
    | Cond of {cnd: fml; pos: fml; neg: fml}
    | UPosLit of Predsym.t * trm
    | UNegLit of Predsym.t * trm
  [@@deriving compare, equal, sexp]

  val _Tt : fml
  val _Ff : fml
  val _Eq : trm -> trm -> fml
  val _Dq : trm -> trm -> fml
  val _Eq0 : trm -> fml
  val _Dq0 : trm -> fml
  val _Gt0 : trm -> fml
  val _Ge0 : trm -> fml
  val _Lt0 : trm -> fml
  val _Le0 : trm -> fml
  val _Not : fml -> fml
  val _And : fml -> fml -> fml
  val _Or : fml -> fml -> fml
  val _Iff : fml -> fml -> fml
  val _Xor : fml -> fml -> fml
  val _Cond : fml -> fml -> fml -> fml
  val _UPosLit : Predsym.t -> trm -> fml
  val _UNegLit : Predsym.t -> trm -> fml
end = struct
  type fml =
    | Tt
    | Ff
    | Eq of trm * trm
    | Dq of trm * trm
    | Eq0 of trm
    | Dq0 of trm
    | Gt0 of trm
    | Ge0 of trm
    | Lt0 of trm
    | Le0 of trm
    | And of fml * fml
    | Or of fml * fml
    | Iff of fml * fml
    | Xor of fml * fml
    | Cond of {cnd: fml; pos: fml; neg: fml}
    | UPosLit of Predsym.t * trm
    | UNegLit of Predsym.t * trm
  [@@deriving compare, equal, sexp]

  let sort_fml x y = if compare_fml x y <= 0 then (x, y) else (y, x)

  (** Some normalization is necessary for [embed_into_fml] (defined below)
      to be left inverse to [embed_into_cnd]. Essentially
      [0 ≠ (p ? 1 : 0)] needs to normalize to [p], by way of
      [0 ≠ (p ? 1 : 0)] ==> [(p ? 0 ≠ 1 : 0 ≠ 0)] ==> [(p ? tt : ff)]
      ==> [p]. *)

  let _Tt = Tt
  let _Ff = Ff

  (** classification of terms as either semantically equal or disequal, or
      if semantic relationship is unknown, as either syntactically less than
      or greater than *)
  type compare_semantic_syntactic = SemEq | SemDq | SynLt | SynGt

  let compare_semantic_syntactic d e =
    match (d, e) with
    | Z y, Z z -> if Z.equal y z then SemEq else SemDq
    | Q q, Q r -> if Q.equal q r then SemEq else SemDq
    | Z z, Q q | Q q, Z z -> if Q.equal (Q.of_z z) q then SemEq else SemDq
    | _ ->
        let ord = compare_trm d e in
        if ord < 0 then SynLt else if ord = 0 then SemEq else SynGt

  let _Eq0 x =
    match compare_semantic_syntactic zero x with
    (* 0 = 0 ==> tt *)
    | SemEq -> Tt
    (* 0 = N ==> ff for N ≢ 0 *)
    | SemDq -> Ff
    | SynLt | SynGt -> Eq0 x

  let _Dq0 x =
    match compare_semantic_syntactic zero x with
    (* 0 ≠ 0 ==> ff *)
    | SemEq -> Ff
    (* 0 ≠ N ==> tt for N ≢ 0 *)
    | SemDq -> Tt
    | SynLt | SynGt -> Dq0 x

  let _Eq x y =
    if x == zero then _Eq0 y
    else if y == zero then _Eq0 x
    else
      match compare_semantic_syntactic x y with
      | SemEq -> Tt
      | SemDq -> Ff
      | SynLt -> Eq (x, y)
      | SynGt -> Eq (y, x)

  let _Dq x y =
    if x == zero then _Dq0 y
    else if y == zero then _Dq0 x
    else
      match compare_semantic_syntactic x y with
      | SemEq -> Ff
      | SemDq -> Tt
      | SynLt -> Dq (x, y)
      | SynGt -> Dq (y, x)

  let _Gt0 = function
    | Z z -> if Z.gt z Z.zero then Tt else Ff
    | Q q -> if Q.gt q Q.zero then Tt else Ff
    | x -> Gt0 x

  let _Ge0 = function
    | Z z -> if Z.geq z Z.zero then Tt else Ff
    | Q q -> if Q.geq q Q.zero then Tt else Ff
    | x -> Ge0 x

  let _Lt0 = function
    | Z z -> if Z.lt z Z.zero then Tt else Ff
    | Q q -> if Q.lt q Q.zero then Tt else Ff
    | x -> Lt0 x

  let _Le0 = function
    | Z z -> if Z.leq z Z.zero then Tt else Ff
    | Q q -> if Q.leq q Q.zero then Tt else Ff
    | x -> Le0 x

  let _UPosLit p x = UPosLit (p, x)
  let _UNegLit p x = UNegLit (p, x)

  let is_negative = function
    | Ff | Dq _ | Dq0 _ | Lt0 _ | Le0 _ | Or _ | Xor _ | UNegLit _ -> true
    | Tt | Eq _ | Eq0 _ | Gt0 _ | Ge0 _ | And _ | Iff _ | UPosLit _ | Cond _
      ->
        false

  type equal_or_opposite = Equal | Opposite | Unknown

  let rec equal_or_opposite p q =
    match (p, q) with
    | Tt, Ff | Ff, Tt -> Opposite
    | Eq (a, b), Dq (a', b') | Dq (a, b), Eq (a', b') ->
        if equal_trm a a' && equal_trm b b' then Opposite else Unknown
    | Eq0 a, Dq0 a'
     |Dq0 a, Eq0 a'
     |Gt0 a, Le0 a'
     |Ge0 a, Lt0 a'
     |Lt0 a, Ge0 a'
     |Le0 a, Gt0 a' ->
        if equal_trm a a' then Opposite else Unknown
    | And (a, b), Or (a', b') | Or (a', b'), And (a, b) -> (
      match equal_or_opposite a a' with
      | Opposite -> (
        match equal_or_opposite b b' with
        | Opposite -> Opposite
        | _ -> Unknown )
      | _ -> Unknown )
    | Iff (p, q), Xor (p', q') | Xor (p, q), Iff (p', q') ->
        if equal_fml p p' && equal_fml q q' then Opposite else Unknown
    | Cond {cnd= c; pos= p; neg= n}, Cond {cnd= c'; pos= p'; neg= n'} ->
        if equal_fml c c' then
          match equal_or_opposite p p' with
          | Opposite -> (
            match equal_or_opposite n n' with
            | Opposite -> Opposite
            | _ -> Unknown )
          | Equal -> if equal_fml n n' then Equal else Unknown
          | Unknown -> Unknown
        else Unknown
    | UPosLit (p, x), UNegLit (p', x') | UNegLit (p, x), UPosLit (p', x') ->
        if Predsym.equal p p' && equal_trm x x' then Opposite else Unknown
    | _ -> if equal_fml p q then Equal else Unknown

  let _And p q =
    match (p, q) with
    | Tt, p | p, Tt -> p
    | Ff, _ | _, Ff -> Ff
    | _ -> (
      match equal_or_opposite p q with
      | Equal -> p
      | Opposite -> Ff
      | Unknown ->
          let p, q = sort_fml p q in
          And (p, q) )

  let _Or p q =
    match (p, q) with
    | Ff, p | p, Ff -> p
    | Tt, _ | _, Tt -> Tt
    | _ -> (
      match equal_or_opposite p q with
      | Equal -> p
      | Opposite -> Tt
      | Unknown ->
          let p, q = sort_fml p q in
          Or (p, q) )

  let rec _Iff p q =
    match (p, q) with
    | Tt, p | p, Tt -> p
    | Ff, p | p, Ff -> _Not p
    | _ -> (
      match equal_or_opposite p q with
      | Equal -> Tt
      | Opposite -> Ff
      | Unknown ->
          let p, q = sort_fml p q in
          Iff (p, q) )

  and _Xor p q =
    match (p, q) with
    | Tt, p | p, Tt -> _Not p
    | Ff, p | p, Ff -> p
    | _ -> (
      match equal_or_opposite p q with
      | Equal -> Ff
      | Opposite -> Tt
      | Unknown ->
          let p, q = sort_fml p q in
          Xor (p, q) )

  and _Not = function
    | Tt -> _Ff
    | Ff -> _Tt
    | Eq (x, y) -> _Dq x y
    | Dq (x, y) -> _Eq x y
    | Eq0 x -> _Dq0 x
    | Dq0 x -> _Eq0 x
    | Gt0 x -> _Le0 x
    | Ge0 x -> _Lt0 x
    | Lt0 x -> _Ge0 x
    | Le0 x -> _Gt0 x
    | And (x, y) -> _Or (_Not x) (_Not y)
    | Or (x, y) -> _And (_Not x) (_Not y)
    | Iff (x, y) -> _Xor x y
    | Xor (x, y) -> _Iff x y
    | Cond {cnd; pos; neg} -> _Cond cnd (_Not pos) (_Not neg)
    | UPosLit (p, x) -> _UNegLit p x
    | UNegLit (p, x) -> _UPosLit p x

  and _Cond cnd pos neg =
    match (cnd, pos, neg) with
    (* (tt ? p : n) ==> p *)
    | Tt, _, _ -> pos
    (* (ff ? p : n) ==> n *)
    | Ff, _, _ -> neg
    (* (c ? tt : ff) ==> c *)
    | _, Tt, Ff -> cnd
    (* (c ? ff : tt) ==> ¬c *)
    | _, Ff, Tt -> _Not cnd
    (* (c ? p : ff) ==> c ∧ p *)
    | _, _, Ff -> _And cnd pos
    (* (c ? ff : n) ==> ¬c ∧ n *)
    | _, Ff, _ -> _And (_Not cnd) neg
    (* (c ? tt : n) ==> c ∨ n *)
    | _, Tt, _ -> _Or cnd neg
    (* (c ? p : tt) ==> ¬c ∨ p *)
    | _, _, Tt -> _Or (_Not cnd) pos
    | _ -> (
      match equal_or_opposite pos neg with
      (* (c ? p : p) ==> c *)
      | Equal -> cnd
      (* (c ? p : ¬p) ==> c <=> p *)
      | Opposite -> _Iff cnd pos
      (* (c ? p : n) <=> (¬c ? n : p) *)
      | Unknown when is_negative cnd ->
          Cond {cnd= _Not cnd; pos= neg; neg= pos}
      (* (c ? p : n) *)
      | _ -> Cond {cnd; pos; neg} )
end

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
 * Variables
 *)

(** Variable terms *)
module Var : sig
  type t = private trm [@@deriving compare, equal, sexp]
  type strength = t -> [`Universal | `Existential | `Anonymous] option

  val ppx : strength -> t pp
  val pp : t pp

  module Map : Map.S with type key := t

  module Set : sig
    include NS.Set.S with type elt := t

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val ppx : strength -> t pp
    val pp : t pp
    val pp_xs : t pp
  end

  val of_ : trm -> t
  val of_exp : exp -> t option
  val program : name:string -> global:bool -> t
  val fresh : string -> wrt:Set.t -> t * Set.t

  val identified : name:string -> id:int -> t
  (** Variable with the given [id]. Variables are compared by [id] alone,
      [name] is used only for printing. The only way to ensure [identified]
      variables do not clash with [fresh] variables is to pass the
      [identified] variables to [fresh] in [wrt]:
      [Var.fresh name ~wrt:(Var.Set.of_ (Var.identified ~name ~id))]. *)

  val id : t -> int
  val name : t -> string

  module Subst : sig
    type var := t
    type t [@@deriving compare, equal, sexp]
    type x = {sub: t; dom: Set.t; rng: Set.t}

    val pp : t pp
    val empty : t
    val freshen : Set.t -> wrt:Set.t -> x * Set.t
    val invert : t -> t
    val restrict : t -> Set.t -> x
    val is_empty : t -> bool
    val domain : t -> Set.t
    val range : t -> Set.t
    val fold : t -> init:'a -> f:(var -> var -> 'a -> 'a) -> 'a
    val apply : t -> var -> var
  end
end = struct
  module T = struct
    type t = trm [@@deriving compare, equal, sexp]
    type strength = t -> [`Universal | `Existential | `Anonymous] option

    let invariant (x : t) =
      let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
      match x with
      | Var _ -> ()
      | _ -> fail "non-var: %a" Sexp.pp_hum (sexp_of_trm x) ()

    let ppx strength fs v =
      let pf fmt = pp_boxed fs fmt in
      match (v : trm) with
      | Var {name; id= -1} -> Trace.pp_styled `Bold "%@%s" fs name
      | Var {name; id= 0} -> Trace.pp_styled `Bold "%%%s" fs name
      | Var {name; id} -> (
        match strength v with
        | None -> pf "%%%s_%d" name id
        | Some `Universal -> Trace.pp_styled `Bold "%%%s_%d" fs name id
        | Some `Existential -> Trace.pp_styled `Cyan "%%%s_%d" fs name id
        | Some `Anonymous -> Trace.pp_styled `Cyan "_" fs )
      | x -> violates invariant x

    let pp = ppx (fun _ -> None)
  end

  include T

  module Map = struct
    include Map.Make (T)
    include Provide_of_sexp (T)
  end

  module Set = struct
    include Set.Make (T)
    include Provide_of_sexp (T)

    let ppx strength vs = pp (T.ppx strength) vs
    let pp vs = pp T.pp vs

    let pp_xs fs xs =
      if not (is_empty xs) then
        Format.fprintf fs "@<2>∃ @[%a@] .@;<1 2>" pp xs
  end

  (* access *)

  let id = function Var v -> v.id | x -> violates invariant x
  let name = function Var v -> v.name | x -> violates invariant x

  (* construct *)

  let of_ = function Var _ as v -> v | _ -> invalid_arg "Var.of_"

  let of_exp = function
    | `Trm (Var _ as v) -> Some (v |> check invariant)
    | _ -> None

  let program ~name ~global = Var {name; id= (if global then -1 else 0)}

  let fresh name ~wrt =
    let max = match Set.max_elt wrt with None -> 0 | Some max -> id max in
    let x' = Var {name; id= max + 1} in
    (x', Set.add wrt x')

  let identified ~name ~id = Var {name; id}

  (*
   * Renamings
   *)

  (** Variable renaming substitutions *)
  module Subst = struct
    type t = trm Map.t [@@deriving compare, equal, sexp_of]
    type x = {sub: t; dom: Set.t; rng: Set.t}

    let t_of_sexp = Map.t_of_sexp t_of_sexp
    let pp = Map.pp pp pp

    let invariant s =
      let@ () = Invariant.invariant [%here] s [%sexp_of: t] in
      let domain, range =
        Map.fold s ~init:(Set.empty, Set.empty)
          ~f:(fun ~key ~data (domain, range) ->
            (* substs are injective *)
            assert (not (Set.mem range data)) ;
            (Set.add domain key, Set.add range data) )
      in
      assert (Set.disjoint domain range)

    let empty = Map.empty
    let is_empty = Map.is_empty

    let freshen vs ~wrt =
      let dom = Set.inter wrt vs in
      ( if Set.is_empty dom then
        ({sub= empty; dom= Set.empty; rng= Set.empty}, wrt)
      else
        let wrt = Set.union wrt vs in
        let sub, rng, wrt =
          Set.fold dom ~init:(empty, Set.empty, wrt)
            ~f:(fun (sub, rng, wrt) x ->
              let x', wrt = fresh (name x) ~wrt in
              let sub = Map.add_exn sub ~key:x ~data:x' in
              let rng = Set.add rng x' in
              (sub, rng, wrt) )
        in
        ({sub; dom; rng}, wrt) )
      |> check (fun ({sub; _}, _) -> invariant sub)

    let fold sub ~init ~f =
      Map.fold sub ~init ~f:(fun ~key ~data s -> f key data s)

    let domain sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key ~data:_ domain ->
          Set.add domain key )

    let range sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key:_ ~data range ->
          Set.add range data )

    let invert sub =
      Map.fold sub ~init:empty ~f:(fun ~key ~data sub' ->
          Map.add_exn sub' ~key:data ~data:key )
      |> check invariant

    let restrict sub vs =
      Map.fold sub ~init:{sub; dom= Set.empty; rng= Set.empty}
        ~f:(fun ~key ~data z ->
          if Set.mem vs key then
            {z with dom= Set.add z.dom key; rng= Set.add z.rng data}
          else (
            assert (
              (* all substs are injective, so the current mapping is the
                 only one that can cause [data] to be in [rng] *)
              (not (Set.mem (range (Map.remove sub key)) data))
              || violates invariant sub ) ;
            {z with sub= Map.remove z.sub key} ) )
      |> check (fun {sub; dom; rng} ->
             assert (Set.equal dom (domain sub)) ;
             assert (Set.equal rng (range sub)) )

    let apply sub v = Map.find sub v |> Option.value ~default:v
  end
end

type var = Var.t

(*
 * Representation operations
 *)

(** pp *)

let encoded_record r =
  let exception Not_a_record in
  let rec encoded_record_ i = function
    | Apply (EmptyRecord, Tuple [||]) when Z.equal i Z.zero -> []
    | Update {rcd= Apply (EmptyRecord, Tuple [||]); idx= Z j; elt}
      when Z.equal i j ->
        [elt]
    | Update {rcd; idx= Z j; elt} when Z.equal i j ->
        elt :: encoded_record_ (Z.succ i) rcd
    | _ -> raise Not_a_record
  in
  match encoded_record_ Z.zero r with
  | es -> Some es
  | exception Not_a_record -> None

let rec ppx_t strength fs trm =
  let rec pp fs trm =
    let pf fmt = pp_boxed fs fmt in
    match trm with
    | Var _ as v -> Var.ppx strength fs (Var.of_ v)
    | Z z -> Trace.pp_styled `Magenta "%a" fs Z.pp z
    | Q q -> Trace.pp_styled `Magenta "%a" fs Q.pp q
    | Neg x -> pf "(- %a)" pp x
    | Add (x, y) -> pf "(%a@ + %a)" pp x pp y
    | Sub (x, y) -> pf "(%a@ - %a)" pp x pp y
    | Mulq (q, x) -> pf "(%a@ @<2>× %a)" Q.pp q pp x
    | Splat x -> pf "%a^" pp x
    | Sized {seq; siz} -> pf "@<1>⟨%a,%a@<1>⟩" pp siz pp seq
    | Extract {seq; off; len} -> pf "%a[%a,%a)" pp seq pp off pp len
    | Concat [||] -> pf "@<2>⟨⟩"
    | Concat xs -> pf "(%a)" (Array.pp "@,^" pp) xs
    | Select {rcd; idx} -> pf "%a[%a]" pp rcd pp idx
    | Update {rcd; idx; elt} -> (
      match encoded_record trm with
      | None -> pf "[%a@ @[| %a → %a@]]" pp rcd pp idx pp elt
      | Some elts -> pf "{%a}" (pp_record strength) elts )
    | Tuple xs -> pf "(%a)" (Array.pp ",@ " (ppx_t strength)) xs
    | Project {ary; idx; tup} -> pf "proj(%i,%i)(%a)" ary idx pp tup
    | Apply (f, Tuple [||]) -> pf "%a" Funsym.pp f
    | Apply
        ( ( ( Mul | Div | Rem | BitAnd | BitOr | BitXor | BitShl | BitLshr
            | BitAshr ) as f )
        , Tuple [|x; y|] ) ->
        pf "(%a@ %a@ %a)" pp x Funsym.pp f pp y
    | Apply (f, a) -> pf "%a@ %a" Funsym.pp f pp a
  in
  pp fs trm

and pp_record strength fs elts =
  [%Trace.fprintf
    fs "%a"
      (fun fs elts ->
        let exception Not_a_string in
        match
          String.of_char_list
            (List.map elts ~f:(function
              | Z c -> Char.of_int_exn (Z.to_int c)
              | _ -> raise Not_a_string ))
        with
        | s -> Format.fprintf fs "%S" s
        | exception (Not_a_string | Z.Overflow | Failure _) ->
            Format.fprintf fs "@[<h>%a@]"
              (List.pp ",@ " (ppx_t strength))
              elts )
      elts]

let pp_t = ppx_t (fun _ -> None)

let ppx_f strength fs fml =
  let pp_t = ppx_t strength in
  let rec pp fs fml =
    let pf fmt = pp_boxed fs fmt in
    match (fml : fml) with
    | Tt -> pf "tt"
    | Ff -> pf "ff"
    | Eq (x, y) -> pf "(%a@ = %a)" pp_t x pp_t y
    | Dq (x, y) -> pf "(%a@ @<2>≠ %a)" pp_t x pp_t y
    | Eq0 x -> pf "(0 = %a)" pp_t x
    | Dq0 x -> pf "(0 @<2>≠ %a)" pp_t x
    | Gt0 x -> pf "(0 < %a)" pp_t x
    | Ge0 x -> pf "(0 @<2>≤ %a)" pp_t x
    | Lt0 x -> pf "(0 > %a)" pp_t x
    | Le0 x -> pf "(0 @<2>≥ %a)" pp_t x
    | And (x, y) -> pf "(%a@ @<2>∧ %a)" pp x pp y
    | Or (x, y) -> pf "(%a@ @<2>∨ %a)" pp x pp y
    | Iff (x, y) -> pf "(%a@ <=> %a)" pp x pp y
    | Xor (x, y) -> pf "(%a@ xor %a)" pp x pp y
    | Cond {cnd; pos; neg} -> pf "(%a@ ? %a@ : %a)" pp cnd pp pos pp neg
    | UPosLit (p, x) -> pf "%a(%a)" Predsym.pp p pp_t x
    | UNegLit (p, x) -> pf "@<1>¬%a(%a)" Predsym.pp p pp_t x
  in
  pp fs fml

let pp_f = ppx_f (fun _ -> None)

let ppx_c strength fs ct =
  let pp_t = ppx_t strength in
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

let rec fold_vars_t e ~init ~f =
  match e with
  | Var _ as v -> f init (Var.of_ v)
  | Z _ | Q _ -> init
  | Neg x
   |Mulq (_, x)
   |Splat x
   |Project {ary= _; idx= _; tup= x}
   |Apply (_, x) ->
      fold_vars_t ~f x ~init
  | Add (x, y)
   |Sub (x, y)
   |Sized {seq= x; siz= y}
   |Select {rcd= x; idx= y} ->
      fold_vars_t ~f x ~init:(fold_vars_t ~f y ~init)
  | Update {rcd= x; idx= y; elt= z} | Extract {seq= x; off= y; len= z} ->
      fold_vars_t ~f x
        ~init:(fold_vars_t ~f y ~init:(fold_vars_t ~f z ~init))
  | Concat xs | Tuple xs ->
      Array.fold ~f:(fun init -> fold_vars_t ~f ~init) xs ~init

let rec fold_vars_f ~init p ~f =
  match (p : fml) with
  | Tt | Ff -> init
  | Eq (x, y) | Dq (x, y) -> fold_vars_t ~f x ~init:(fold_vars_t ~f y ~init)
  | Eq0 x | Dq0 x | Gt0 x | Ge0 x | Lt0 x | Le0 x -> fold_vars_t ~f x ~init
  | And (x, y) | Or (x, y) | Iff (x, y) | Xor (x, y) ->
      fold_vars_f ~f x ~init:(fold_vars_f ~f y ~init)
  | Cond {cnd; pos; neg} ->
      fold_vars_f ~f cnd
        ~init:(fold_vars_f ~f pos ~init:(fold_vars_f ~f neg ~init))
  | UPosLit (_, x) | UNegLit (_, x) -> fold_vars_t ~f x ~init

let rec fold_vars_c ~init ~f = function
  | `Ite (cnd, thn, els) ->
      fold_vars_f ~f cnd
        ~init:(fold_vars_c ~f thn ~init:(fold_vars_c ~f els ~init))
  | `Trm t -> fold_vars_t ~f t ~init

let fold_vars ~init e ~f =
  match e with
  | `Fml p -> fold_vars_f ~f ~init p
  | #cnd as c -> fold_vars_c ~f ~init c

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

(** map_trms *)

let rec map_trms_f ~f b =
  match b with
  | Tt | Ff -> b
  | Eq (x, y) -> map2 f b _Eq x y
  | Dq (x, y) -> map2 f b _Dq x y
  | Eq0 x -> map1 f b _Eq0 x
  | Dq0 x -> map1 f b _Dq0 x
  | Gt0 x -> map1 f b _Gt0 x
  | Ge0 x -> map1 f b _Ge0 x
  | Lt0 x -> map1 f b _Lt0 x
  | Le0 x -> map1 f b _Le0 x
  | And (x, y) -> map2 (map_trms_f ~f) b _And x y
  | Or (x, y) -> map2 (map_trms_f ~f) b _Or x y
  | Iff (x, y) -> map2 (map_trms_f ~f) b _Iff x y
  | Xor (x, y) -> map2 (map_trms_f ~f) b _Xor x y
  | Cond {cnd; pos; neg} -> map3 (map_trms_f ~f) b _Cond cnd pos neg
  | UPosLit (p, x) -> map1 f b (_UPosLit p) x
  | UNegLit (p, x) -> map1 f b (_UNegLit p) x

(** map_vars *)

let rec map_vars_t ~f e =
  match e with
  | Var _ as v -> (f (Var.of_ v) : var :> trm)
  | Z _ | Q _ -> e
  | Neg x -> map1 (map_vars_t ~f) e _Neg x
  | Add (x, y) -> map2 (map_vars_t ~f) e _Add x y
  | Sub (x, y) -> map2 (map_vars_t ~f) e _Sub x y
  | Mulq (q, x) -> map1 (map_vars_t ~f) e (_Mulq q) x
  | Splat x -> map1 (map_vars_t ~f) e _Splat x
  | Sized {seq; siz} -> map2 (map_vars_t ~f) e _Sized seq siz
  | Extract {seq; off; len} -> map3 (map_vars_t ~f) e _Extract seq off len
  | Concat xs -> mapN (map_vars_t ~f) e _Concat xs
  | Select {rcd; idx} -> map2 (map_vars_t ~f) e _Select rcd idx
  | Update {rcd; idx; elt} -> map3 (map_vars_t ~f) e _Update rcd idx elt
  | Tuple xs -> mapN (map_vars_t ~f) e _Tuple xs
  | Project {ary; idx; tup} -> map1 (map_vars_t ~f) e (_Project ary idx) tup
  | Apply (g, x) -> map1 (map_vars_t ~f) e (_Apply g) x

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
  | #cnd as c -> map_cnd _Cond _Dq0 c

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
let apNt : (trm list -> trm) -> exp array -> exp =
 fun f xs ->
  rev_mapN_cnd ite
    (fun xs -> `Trm (f xs))
    (Array.fold ~f:(fun xs x -> embed_into_cnd x :: xs) ~init:[] xs)

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

  (* constants *)

  let zero = `Trm zero
  let one = `Trm one

  let integer z =
    if Z.equal Z.zero z then zero
    else if Z.equal Z.one z then one
    else `Trm (Z z)

  let rational q = `Trm (Q q)

  (* arithmetic *)

  let neg = ap1t _Neg
  let add = ap2t _Add
  let sub = ap2t _Sub
  let mulq q = ap1t (_Mulq q)

  let mul =
    ap2 (fun x y ->
        match x with
        | Z z -> mulq (Q.of_z z) (`Trm y)
        | Q q -> mulq q (`Trm y)
        | _ -> (
          match y with
          | Z z -> mulq (Q.of_z z) (`Trm x)
          | Q q -> mulq q (`Trm x)
          | _ ->
              ap2t
                (fun x y -> Apply (Mul, Tuple [|x; y|]))
                (`Trm x) (`Trm y) ) )

  (* sequences *)

  let splat = ap1t _Splat
  let sized ~seq ~siz = ap2t _Sized seq siz
  let extract ~seq ~off ~len = ap3t _Extract seq off len
  let concat elts = apNt (fun es -> _Concat (Array.of_list es)) elts

  (* records *)

  let select ~rcd ~idx = ap2t _Select rcd idx
  let update ~rcd ~idx ~elt = ap3t _Update rcd idx elt

  (* tuples *)

  let tuple elts = apNt (fun es -> _Tuple (Array.of_list es)) elts
  let project ~ary ~idx tup = ap1t (_Project ary idx) tup

  (* if-then-else *)

  let ite ~cnd ~thn ~els = ite cnd thn els

  (** Destruct *)

  let d_int = function `Trm (Z z) -> Some z | _ -> None

  (** Access *)

  let const_of x =
    let rec const_of t =
      let neg = Option.map ~f:Q.neg in
      let add = Option.map2 ~f:Q.add in
      match t with
      | Z z -> Some (Q.of_z z)
      | Q q -> Some q
      | Neg x -> neg (const_of x)
      | Add (x, y) -> add (const_of x) (const_of y)
      | Sub (x, y) -> add (const_of x) (neg (const_of y))
      | _ -> None
    in
    match x with `Trm t -> const_of t | _ -> None

  (** Traverse *)

  let fold_vars = fold_vars

  (** Transform *)

  let map_vars = map_vars

  let fold_map_vars e ~init ~f =
    let s = ref init in
    let f x =
      let s', x' = f !s x in
      s := s' ;
      x'
    in
    let e' = map_vars ~f e in
    (!s, e')

  let rename s e = map_vars ~f:(Var.Subst.apply s) e

  (** Query *)

  let fv e = fold_vars e ~f:Var.Set.add ~init:Var.Set.empty
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

  let tt = _Tt
  let ff = _Ff

  (* comparisons *)

  let eq = ap2f _Eq
  let dq = ap2f _Dq
  let eq0 = ap1f _Eq0
  let dq0 = ap1f _Dq0
  let gt0 = ap1f _Gt0
  let ge0 = ap1f _Ge0
  let lt0 = ap1f _Lt0
  let le0 = ap1f _Le0

  let gt a b =
    if a == Term.zero then lt0 b
    else if b == Term.zero then gt0 a
    else gt0 (Term.sub a b)

  let ge a b =
    if a == Term.zero then le0 b
    else if b == Term.zero then ge0 a
    else ge0 (Term.sub a b)

  let lt a b = gt b a
  let le a b = ge b a

  (* connectives *)

  let and_ = _And
  let andN = function [] -> tt | b :: bs -> List.fold ~init:b ~f:and_ bs
  let or_ = _Or
  let orN = function [] -> ff | b :: bs -> List.fold ~init:b ~f:or_ bs
  let iff = _Iff
  let xor = _Xor
  let cond ~cnd ~pos ~neg = _Cond cnd pos neg
  let not_ = _Not

  (** Query *)

  let fv e = fold_vars_f e ~f:Var.Set.add ~init:Var.Set.empty

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
    match b with
    | Tt | Ff -> b
    | Eq (x, y) -> lift_map2 f b _Eq x y
    | Dq (x, y) -> lift_map2 f b _Dq x y
    | Eq0 x -> lift_map1 f b _Eq0 x
    | Dq0 x -> lift_map1 f b _Dq0 x
    | Gt0 x -> lift_map1 f b _Gt0 x
    | Ge0 x -> lift_map1 f b _Ge0 x
    | Lt0 x -> lift_map1 f b _Lt0 x
    | Le0 x -> lift_map1 f b _Le0 x
    | And (x, y) -> map2 (map_terms ~f) b _And x y
    | Or (x, y) -> map2 (map_terms ~f) b _Or x y
    | Iff (x, y) -> map2 (map_terms ~f) b _Iff x y
    | Xor (x, y) -> map2 (map_terms ~f) b _Xor x y
    | Cond {cnd; pos; neg} -> map3 (map_terms ~f) b _Cond cnd pos neg
    | UPosLit (p, x) -> lift_map1 f b (_UPosLit p) x
    | UNegLit (p, x) -> lift_map1 f b (_UNegLit p) x

  let fold_map_vars ~init e ~f =
    let s = ref init in
    let f x =
      let s', x' = f !s x in
      s := s' ;
      x'
    in
    let e' = map_vars ~f e in
    (!s, e')

  let rename s e = map_vars ~f:(Var.Subst.apply s) e

  let fold_dnf :
         meet1:('literal -> 'conjunction -> 'conjunction)
      -> join1:('conjunction -> 'disjunction -> 'disjunction)
      -> top:'conjunction
      -> bot:'disjunction
      -> 'formula
      -> 'disjunction =
   fun ~meet1 ~join1 ~top ~bot fml ->
    let rec add_conjunct (cjn, splits) fml =
      match fml with
      | Tt | Ff | Eq _ | Dq _ | Eq0 _ | Dq0 _ | Gt0 _ | Ge0 _ | Lt0 _
       |Le0 _ | Iff _ | Xor _ | UPosLit _ | UNegLit _ ->
          (meet1 fml cjn, splits)
      | And (p, q) -> add_conjunct (add_conjunct (cjn, splits) p) q
      | Or (p, q) -> (cjn, [p; q] :: splits)
      | Cond {cnd; pos; neg} ->
          (cjn, [and_ cnd pos; and_ (not_ cnd) neg] :: splits)
    in
    let rec add_disjunct (cjn, splits) djn fml =
      let cjn, splits = add_conjunct (cjn, splits) fml in
      match splits with
      | split :: splits ->
          List.fold ~f:(add_disjunct (cjn, splits)) ~init:djn split
      | [] -> join1 cjn djn
    in
    add_disjunct (top, []) bot fml
end

(*
 * Convert to Ses
 *)

let v_to_ses : var -> Ses.Var.t =
 fun v -> Ses.Var.identified ~id:(Var.id v) ~name:(Var.name v)

let vs_to_ses : Var.Set.t -> Ses.Var.Set.t =
 fun vs ->
  Var.Set.fold vs ~init:Ses.Var.Set.empty ~f:(fun vs v ->
      Ses.Var.Set.add vs (v_to_ses v) )

let to_int e =
  match Ses.Term.d_int e with
  | Some z -> (
    match Z.to_int z with
    | i -> i
    | exception Z.Overflow -> fail "non-int: %a" Ses.Term.pp e () )
  | None -> fail "non-Z: %a" Ses.Term.pp e ()

let rec t_to_ses : trm -> Ses.Term.t = function
  | Var {name; id} -> Ses.Term.var (Ses.Var.identified ~name ~id)
  | Z z -> Ses.Term.integer z
  | Q q -> Ses.Term.rational q
  | Neg x -> Ses.Term.neg (t_to_ses x)
  | Add (x, y) -> Ses.Term.add (t_to_ses x) (t_to_ses y)
  | Sub (x, y) -> Ses.Term.sub (t_to_ses x) (t_to_ses y)
  | Mulq (q, x) -> Ses.Term.mulq q (t_to_ses x)
  | Splat x -> Ses.Term.splat (t_to_ses x)
  | Sized {seq; siz} ->
      Ses.Term.sized ~seq:(t_to_ses seq) ~siz:(t_to_ses siz)
  | Extract {seq; off; len} ->
      Ses.Term.extract ~seq:(t_to_ses seq) ~off:(t_to_ses off)
        ~len:(t_to_ses len)
  | Concat es -> Ses.Term.concat (Array.map ~f:t_to_ses es)
  | Select {rcd; idx} ->
      Ses.Term.select ~rcd:(t_to_ses rcd) ~idx:(to_int (t_to_ses idx))
  | Update {rcd; idx; elt} ->
      Ses.Term.update ~rcd:(t_to_ses rcd)
        ~idx:(to_int (t_to_ses idx))
        ~elt:(t_to_ses elt)
  | Apply (Float s, Tuple [||]) -> Ses.Term.float s
  | Apply (Label {parent; name}, Tuple [||]) -> Ses.Term.label ~parent ~name
  | Apply (Mul, Tuple [|x; y|]) -> Ses.Term.mul (t_to_ses x) (t_to_ses y)
  | Apply (Div, Tuple [|x; y|]) -> Ses.Term.div (t_to_ses x) (t_to_ses y)
  | Apply (Rem, Tuple [|x; y|]) -> Ses.Term.rem (t_to_ses x) (t_to_ses y)
  | Apply (EmptyRecord, Tuple [||]) ->
      Ses.Term.record (IArray.of_array [||])
  | Apply (RecRecord i, Tuple [||]) -> Ses.Term.rec_record i
  | Apply (BitAnd, Tuple [|x; y|]) ->
      Ses.Term.and_ (t_to_ses x) (t_to_ses y)
  | Apply (BitOr, Tuple [|x; y|]) -> Ses.Term.or_ (t_to_ses x) (t_to_ses y)
  | Apply (BitXor, Tuple [|x; y|]) -> Ses.Term.dq (t_to_ses x) (t_to_ses y)
  | Apply (BitShl, Tuple [|x; y|]) -> Ses.Term.shl (t_to_ses x) (t_to_ses y)
  | Apply (BitLshr, Tuple [|x; y|]) ->
      Ses.Term.lshr (t_to_ses x) (t_to_ses y)
  | Apply (BitAshr, Tuple [|x; y|]) ->
      Ses.Term.ashr (t_to_ses x) (t_to_ses y)
  | Apply (Signed n, Tuple [|x|]) -> Ses.Term.signed n (t_to_ses x)
  | Apply (Unsigned n, Tuple [|x|]) -> Ses.Term.unsigned n (t_to_ses x)
  | Apply (Convert {src; dst}, Tuple [|x|]) ->
      Ses.Term.convert src ~to_:dst (t_to_ses x)
  | (Apply _ | Tuple _ | Project _) as t ->
      fail "cannot translate to Ses: %a" pp_t t ()

let rec f_to_ses : fml -> Ses.Term.t = function
  | Tt -> Ses.Term.true_
  | Ff -> Ses.Term.false_
  | Eq (x, y) -> Ses.Term.eq (t_to_ses x) (t_to_ses y)
  | Dq (x, y) -> Ses.Term.dq (t_to_ses x) (t_to_ses y)
  | Eq0 x -> Ses.Term.eq Ses.Term.zero (t_to_ses x)
  | Dq0 x -> Ses.Term.dq Ses.Term.zero (t_to_ses x)
  | Gt0 x -> Ses.Term.lt Ses.Term.zero (t_to_ses x)
  | Ge0 x -> Ses.Term.le Ses.Term.zero (t_to_ses x)
  | Lt0 x -> Ses.Term.lt (t_to_ses x) Ses.Term.zero
  | Le0 x -> Ses.Term.le (t_to_ses x) Ses.Term.zero
  | And (p, q) -> Ses.Term.and_ (f_to_ses p) (f_to_ses q)
  | Or (p, q) -> Ses.Term.or_ (f_to_ses p) (f_to_ses q)
  | Iff (p, q) -> Ses.Term.eq (f_to_ses p) (f_to_ses q)
  | Xor (p, q) -> Ses.Term.dq (f_to_ses p) (f_to_ses q)
  | Cond {cnd; pos; neg} ->
      Ses.Term.conditional ~cnd:(f_to_ses cnd) ~thn:(f_to_ses pos)
        ~els:(f_to_ses neg)
  | UPosLit (Ordered, Tuple [|x; y|]) ->
      Ses.Term.ord (t_to_ses x) (t_to_ses y)
  | UNegLit (Ordered, Tuple [|x; y|]) ->
      Ses.Term.uno (t_to_ses x) (t_to_ses y)
  | (UPosLit (Ordered, _) | UNegLit (Ordered, _)) as f ->
      fail "cannot translate to Ses: %a" pp_f f ()

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
  Ses.Var.Set.fold vs ~init:Var.Set.empty ~f:(fun vs v ->
      Var.Set.add vs (v_of_ses v) )

let uap0 f = `Trm (Apply (f, Tuple [||]))
let uap1 f = ap1t (fun x -> Apply (f, Tuple [|x|]))
let uap2 f = ap2t (fun x y -> Apply (f, Tuple [|x; y|]))
let upos2 p = ap2f (fun x y -> _UPosLit p (Tuple [|x; y|]))
let uneg2 p = ap2f (fun x y -> _UNegLit p (Tuple [|x; y|]))

let rec uap_tt f a = uap1 f (of_ses a)
and uap_ttt f a b = uap2 f (of_ses a) (of_ses b)
and ap_ttf f a b = `Fml (f (of_ses a) (of_ses b))

and ap2 mk_f mk_t a b =
  match (of_ses a, of_ses b) with
  | `Fml p, `Fml q -> `Fml (mk_f p q)
  | x, y -> mk_t x y

and ap2_f mk_f mk_t a b = ap2 mk_f (fun x y -> `Fml (mk_t x y)) a b

and apN mk_f mk_t mk_unit es =
  match
    Ses.Term.Set.fold ~init:(None, None) es ~f:(fun (fs, ts) e ->
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
  | Float {data} -> uap0 (Float data)
  | Label {parent; name} -> uap0 (Label {parent; name})
  | Ap1 (Signed {bits}, e) -> uap_tt (Signed bits) e
  | Ap1 (Unsigned {bits}, e) -> uap_tt (Unsigned bits) e
  | Ap1 (Convert {src; dst}, e) -> uap_tt (Convert {src; dst}) e
  | Ap2 (Eq, d, e) -> ap2_f iff eq d e
  | Ap2 (Dq, d, e) -> ap2_f xor dq d e
  | Ap2 (Lt, d, e) -> ap2_f (fun p q -> and_ (not_ p) q) lt d e
  | Ap2 (Le, d, e) -> ap2_f (fun p q -> or_ (not_ p) q) le d e
  | Ap2 (Ord, d, e) -> ap_ttf (upos2 Ordered) d e
  | Ap2 (Uno, d, e) -> ap_ttf (uneg2 Ordered) d e
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
        Ses.Term.Qset.fold sum ~init:(mul e q) ~f:(fun e q s ->
            add (mul e q) s ) )
  | Mul prod -> (
    match Ses.Term.Qset.pop_min_elt prod with
    | None -> one
    | Some (e, q, prod) ->
        let rec expn e n =
          let p = Z.pred n in
          if Z.sign p = 0 then e else uap2 Mul e (expn e p)
        in
        let exp e q =
          let n = Q.num q in
          let sn = Z.sign n in
          if sn = 0 then of_ses e
          else if sn > 0 then expn (of_ses e) n
          else uap2 Div one (expn (of_ses e) (Z.neg n))
        in
        Ses.Term.Qset.fold prod ~init:(exp e q) ~f:(fun e q s ->
            uap2 Mul (exp e q) s ) )
  | Ap2 (Div, d, e) -> uap_ttt Div d e
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
  | Ap1 (Select idx, rcd) ->
      select ~rcd:(of_ses rcd) ~idx:(integer (Z.of_int idx))
  | Ap2 (Update idx, rcd, elt) ->
      update ~rcd:(of_ses rcd)
        ~idx:(integer (Z.of_int idx))
        ~elt:(of_ses elt)
  | ApN (Record, elts) ->
      let init = uap0 EmptyRecord in
      IArray.foldi ~init elts ~f:(fun i rcd e ->
          update ~rcd ~idx:(integer (Z.of_int i)) ~elt:(of_ses e) )
  | RecRecord i -> uap0 (RecRecord i)

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

  let fold_vars ~init x ~f =
    Ses.Equality.fold_vars x ~init ~f:(fun s v -> f s (v_of_ses v))

  let fv e = fold_vars e ~f:Var.Set.add ~init:Var.Set.empty
  let is_empty x = Ses.Equality.is_true x
  let is_unsat x = Ses.Equality.is_false x
  let implies x b = Ses.Equality.implies x (f_to_ses b)

  let refutes x b =
    Ses.Term.is_false (Ses.Equality.normalize x (f_to_ses b))

  let normalize x e = ses_map (Ses.Equality.normalize x) e

  (* Classes *)

  let class_of x e = List.map ~f:of_ses (Ses.Equality.class_of x (to_ses e))

  let classes_of_ses clss =
    Ses.Term.Map.fold clss ~init:Term.Map.empty
      ~f:(fun ~key:rep ~data:cls clss ->
        let rep' = of_ses rep in
        let cls' = List.map ~f:of_ses cls in
        Term.Map.set ~key:rep' ~data:cls' clss )

  let classes x = classes_of_ses (Ses.Equality.classes x)

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
          (List.sort ~compare:Term.compare cls) )
      fs (Term.Map.to_alist clss)

  let pp fs r = ppx_classes (fun _ -> None) fs (classes r)

  let ppx_diff var_strength fs parent_ctx fml ctx =
    let clss = diff_classes ctx parent_ctx in
    let first = Term.Map.is_empty clss in
    if not first then Format.fprintf fs "  " ;
    ppx_classes var_strength fs clss ;
    let fml =
      let normalizef x e = f_ses_map (Ses.Equality.normalize x) e in
      let fml' = normalizef ctx fml in
      if Formula.(equal tt fml') then [] else [fml']
    in
    List.pp
      ~pre:(if first then "@[  " else "@ @[@<2>∧ ")
      "@ @<2>∧ "
      (Formula.ppx var_strength)
      fs fml ~suf:"@]" ;
    first && List.is_empty fml

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

    let fold s ~init ~f =
      Ses.Equality.Subst.fold s ~init ~f:(fun ~key ~data ->
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

(*
 * Convert from Llair
 *)

module Term_of_Llair = struct
  let rec uap_te f a = uap1 f (exp a)
  and uap_tte f a b = uap2 f (exp a) (exp b)

  and usap_ttt : 'a. (exp -> exp -> 'a) -> _ -> _ -> _ -> 'a =
   fun f typ a b ->
    let bits = Llair.Typ.bit_size_of typ in
    f (uap1 (Unsigned bits) (exp a)) (uap1 (Unsigned bits) (exp b))

  and usap_ttf (f : exp -> exp -> fml) typ a b = `Fml (usap_ttt f typ a b)
  and ap_ttt : 'a. (exp -> exp -> 'a) -> _ -> _ -> 'a =
   fun f a b -> f (exp a) (exp b)
  and ap_ttf (f : exp -> exp -> fml) a b = `Fml (ap_ttt f a b)

  and ap_fff (f : fml -> fml -> fml) a b =
    `Fml (f (embed_into_fml (exp a)) (embed_into_fml (exp b)))

  and ap_ffff (f : fml -> fml -> fml -> fml) a b c =
    `Fml
      (f
         (embed_into_fml (exp a))
         (embed_into_fml (exp b))
         (embed_into_fml (exp c)))

  and exp : Llair.Exp.t -> exp =
   fun e ->
    let open Term in
    let open Formula in
    match e with
    | Reg {name; global; typ= _} -> var (Var.program ~name ~global)
    | Label {parent; name} -> uap0 (Label {parent; name})
    | Integer {typ= _; data} -> integer data
    | Float {data; typ= _} -> (
      match Q.of_float (Float.of_string data) with
      | q when Q.is_real q -> rational q
      | _ | (exception Invalid_argument _) -> uap0 (Float data) )
    | Ap1 (Signed {bits}, _, e) ->
        let a = exp e in
        if bits = 1 then
          match Formula.project a with
          | Some fml -> Formula.inject fml
          | _ -> uap1 (Signed bits) a
        else uap1 (Signed bits) a
    | Ap1 (Unsigned {bits}, _, e) ->
        let a = exp e in
        if bits = 1 then
          match Formula.project a with
          | Some fml -> Formula.inject fml
          | _ -> uap1 (Unsigned bits) a
        else uap1 (Unsigned bits) a
    | Ap1 (Convert {src}, dst, e) -> uap_te (Convert {src; dst}) e
    | Ap2 (Eq, Integer {bits= 1; _}, p, q) -> ap_fff iff p q
    | Ap2 (Dq, Integer {bits= 1; _}, p, q) -> ap_fff xor p q
    | Ap2 ((Gt | Ugt), Integer {bits= 1; _}, p, q)
     |Ap2 ((Lt | Ult), Integer {bits= 1; _}, q, p) ->
        ap_fff (fun p q -> and_ p (not_ q)) p q
    | Ap2 ((Ge | Uge), Integer {bits= 1; _}, p, q)
     |Ap2 ((Le | Ule), Integer {bits= 1; _}, q, p) ->
        ap_fff (fun p q -> or_ p (not_ q)) p q
    | Ap2 (Eq, _, d, e) -> ap_ttf eq d e
    | Ap2 (Dq, _, d, e) -> ap_ttf dq d e
    | Ap2 (Gt, _, d, e) -> ap_ttf gt d e
    | Ap2 (Lt, _, d, e) -> ap_ttf lt d e
    | Ap2 (Ge, _, d, e) -> ap_ttf ge d e
    | Ap2 (Le, _, d, e) -> ap_ttf le d e
    | Ap2 (Ugt, typ, d, e) -> usap_ttf gt typ d e
    | Ap2 (Ult, typ, d, e) -> usap_ttf lt typ d e
    | Ap2 (Uge, typ, d, e) -> usap_ttf ge typ d e
    | Ap2 (Ule, typ, d, e) -> usap_ttf le typ d e
    | Ap2 (Ord, _, d, e) -> ap_ttf (upos2 Ordered) d e
    | Ap2 (Uno, _, d, e) -> ap_ttf (uneg2 Ordered) d e
    | Ap2 (Add, Integer {bits= 1; _}, p, q) -> ap_fff xor p q
    | Ap2 (Sub, Integer {bits= 1; _}, p, q) -> ap_fff xor p q
    | Ap2 (Mul, Integer {bits= 1; _}, p, q) -> ap_fff and_ p q
    | Ap2 (Add, _, d, e) -> ap_ttt add d e
    | Ap2 (Sub, _, d, e) -> ap_ttt sub d e
    | Ap2 (Mul, _, d, e) -> ap_ttt mul d e
    | Ap2 (Div, _, d, e) -> uap_tte Div d e
    | Ap2 (Rem, _, d, e) -> uap_tte Rem d e
    | Ap2 (Udiv, typ, d, e) -> usap_ttt (uap2 Div) typ d e
    | Ap2 (Urem, typ, d, e) -> usap_ttt (uap2 Rem) typ d e
    | Ap2 (And, Integer {bits= 1; _}, p, q) -> ap_fff and_ p q
    | Ap2 (Or, Integer {bits= 1; _}, p, q) -> ap_fff or_ p q
    | Ap2 (Xor, Integer {bits= 1; _}, p, q) -> ap_fff xor p q
    | Ap2 (And, _, d, e) -> ap_ttt (uap2 BitAnd) d e
    | Ap2 (Or, _, d, e) -> ap_ttt (uap2 BitOr) d e
    | Ap2 (Xor, _, d, e) -> ap_ttt (uap2 BitXor) d e
    | Ap2 (Shl, _, d, e) -> ap_ttt (uap2 BitShl) d e
    | Ap2 (Lshr, _, d, e) -> ap_ttt (uap2 BitLshr) d e
    | Ap2 (Ashr, _, d, e) -> ap_ttt (uap2 BitAshr) d e
    | Ap3 (Conditional, Integer {bits= 1; _}, cnd, pos, neg) ->
        ap_ffff _Cond cnd pos neg
    | Ap3 (Conditional, _, cnd, thn, els) ->
        ite ~cnd:(embed_into_fml (exp cnd)) ~thn:(exp thn) ~els:(exp els)
    | Ap1 (Select idx, _, rcd) ->
        select ~rcd:(exp rcd) ~idx:(integer (Z.of_int idx))
    | Ap2 (Update idx, _, rcd, elt) ->
        update ~rcd:(exp rcd) ~idx:(integer (Z.of_int idx)) ~elt:(exp elt)
    | ApN (Record, _, elts) ->
        let init = uap0 EmptyRecord in
        IArray.foldi ~init elts ~f:(fun i rcd e ->
            update ~rcd ~idx:(integer (Z.of_int i)) ~elt:(exp e) )
    | RecRecord (i, _) -> uap0 (RecRecord i)
    | Ap1 (Splat, _, byt) -> splat (exp byt)
end

module Formula_of_Llair = struct
  let exp e = embed_into_fml (Term_of_Llair.exp e)
end

module Var_of_Llair = struct
  let reg r =
    match
      Var.of_exp (Term_of_Llair.exp (r : Llair.Reg.t :> Llair.Exp.t))
    with
    | Some v -> v
    | _ -> violates Llair.Reg.invariant r

  let regs =
    Llair.Reg.Set.fold ~init:Var.Set.empty ~f:(fun s r ->
        Var.Set.add s (reg r) )
end
