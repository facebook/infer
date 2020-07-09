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
  | Mul of trm * trm
  | Div of trm * trm
  | Rem of trm * trm
  | Splat of trm
  | Sized of {seq: trm; siz: trm}
  | Extract of {seq: trm; off: trm; len: trm}
  | Concat of trm iarray
  | Select of trm * trm
  | Update of trm * trm * trm
  | Record of trm iarray
  | RecRecord of int
  | Label of {parent: string; name: string}
  | Float of string
  | BAnd of trm * trm
  | BOr of trm * trm
  | BXor of trm * trm
  | BShl of trm * trm
  | BLshr of trm * trm
  | BAshr of trm * trm
  | Signed of int * trm
  | Unsigned of int * trm
  | Convert of {src: Llair.Typ.t; dst: Llair.Typ.t; arg: trm}
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

let _Neg x = Neg x
let _Add x y = Add (x, y)
let _Sub x y = Sub (x, y)
let _Mulq q x = Mulq (q, x)
let _Mul x y = Mul (x, y)
let _Div x y = Div (x, y)
let _Rem x y = Rem (x, y)
let _Splat x = Splat x
let _Sized seq siz = Sized {seq; siz}
let _Extract seq off len = Extract {seq; off; len}
let _Concat es = Concat es
let _Select r i = Select (r, i)
let _Update r i x = Update (r, i, x)
let _Record es = Record es
let _RecRecord n = RecRecord n
let _BAnd x y = BAnd (x, y)
let _BOr x y = BOr (x, y)
let _BXor x y = BXor (x, y)
let _BShl x y = BShl (x, y)
let _BLshr x y = BLshr (x, y)
let _BAshr x y = BAshr (x, y)
let _Signed n x = Signed (n, x)
let _Unsigned n x = Unsigned (n, x)
let _Convert src dst arg = Convert {src; dst; arg}

(*
 * Formulas
 *)

(** Formulas, denoting sets of structures, built from propositional
    variables, applications of predicate symbols from various theories, and
    first-order logic connectives. *)
type fml =
  | Tt
  | Ff
  | Eq of trm * trm
  | Dq of trm * trm
  | Lt of trm * trm
  | Le of trm * trm
  | Ord of trm * trm
  | Uno of trm * trm
  | Not of fml
  | And of fml * fml
  | Or of fml * fml
  | Iff of fml * fml
  | Xor of fml * fml
  | Imp of fml * fml
  | Cond of {cnd: fml; pos: fml; neg: fml}
[@@deriving compare, equal, sexp]

let _Eq x y = Eq (x, y)
let _Dq x y = Dq (x, y)
let _Lt x y = Lt (x, y)
let _Le x y = Le (x, y)
let _Ord x y = Ord (x, y)
let _Uno x y = Uno (x, y)
let _Not p = Not p
let _And p q = And (p, q)
let _Or p q = Or (p, q)
let _Iff p q = Iff (p, q)
let _Xor p q = Xor (p, q)
let _Imp p q = Imp (p, q)
let _Cond cnd pos neg = Cond {cnd; pos; neg}

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

let rec ppx_t strength fs trm =
  let rec pp fs trm =
    let pf fmt = pp_boxed fs fmt in
    match (trm : trm) with
    | Var _ as v -> Var.ppx strength fs (Var.of_ v)
    | Z z -> Trace.pp_styled `Magenta "%a" fs Z.pp z
    | Q q -> Trace.pp_styled `Magenta "%a" fs Q.pp q
    | Neg x -> pf "(- %a)" pp x
    | Add (x, y) -> pf "(%a@ + %a)" pp x pp y
    | Sub (x, y) -> pf "(%a@ - %a)" pp x pp y
    | Mulq (q, x) -> pf "(%a@ @<2>× %a)" Q.pp q pp x
    | Mul (x, y) -> pf "(%a@ @<2>× %a)" pp x pp y
    | Div (x, y) -> pf "(%a@ / %a)" pp x pp y
    | Rem (x, y) -> pf "(%a@ %% %a)" pp x pp y
    | Splat x -> pf "%a^" pp x
    | Sized {seq; siz} -> pf "@<1>⟨%a,%a@<1>⟩" pp siz pp seq
    | Extract {seq; off; len} -> pf "%a[%a,%a)" pp seq pp off pp len
    | Concat xs when IArray.is_empty xs -> pf "@<2>⟨⟩"
    | Concat xs -> pf "(%a)" (IArray.pp "@,^" pp) xs
    | Select (rcd, idx) -> pf "%a[%a]" pp rcd pp idx
    | Update (rcd, idx, elt) ->
        pf "[%a@ @[| %a → %a@]]" pp rcd pp idx pp elt
    | Record xs -> pf "{%a}" (pp_record strength) xs
    | RecRecord i -> pf "(rec_record %i)" i
    | Label {name} -> pf "%s" name
    | Float s -> pf "%s" s
    | BAnd (x, y) -> pf "(%a@ && %a)" pp x pp y
    | BOr (x, y) -> pf "(%a@ || %a)" pp x pp y
    | BXor (x, y) -> pf "(%a@ xor %a)" pp x pp y
    | BShl (x, y) -> pf "(%a@ shl %a)" pp x pp y
    | BLshr (x, y) -> pf "(%a@ lshr %a)" pp x pp y
    | BAshr (x, y) -> pf "(%a@ ashr %a)" pp x pp y
    | Signed (n, x) -> pf "((s%i)@ %a)" n pp x
    | Unsigned (n, x) -> pf "((u%i)@ %a)" n pp x
    | Convert {src; dst; arg} ->
        pf "((%a)(%a)@ %a)" Llair.Typ.pp dst Llair.Typ.pp src pp arg
  in
  pp fs trm

and pp_record strength fs elts =
  [%Trace.fprintf
    fs "%a"
      (fun fs elts ->
        match
          String.init (IArray.length elts) ~f:(fun i ->
              match IArray.get elts i with
              | Z z -> Char.of_int_exn (Z.to_int z)
              | _ -> raise (Invalid_argument "not a string") )
        with
        | s -> Format.fprintf fs "@[<h>%s@]" (String.escaped s)
        | exception _ ->
            Format.fprintf fs "@[<h>%a@]"
              (IArray.pp ",@ " (ppx_t strength))
              elts )
      elts]

let ppx_f strength fs fml =
  let pp_t = ppx_t strength in
  let rec pp fs fml =
    let pf fmt = pp_boxed fs fmt in
    match (fml : fml) with
    | Tt -> pf "tt"
    | Ff -> pf "ff"
    | Eq (x, y) -> pf "(%a@ = %a)" pp_t x pp_t y
    | Dq (x, y) -> pf "(%a@ @<2>≠ %a)" pp_t x pp_t y
    | Lt (x, y) -> pf "(%a@ < %a)" pp_t x pp_t y
    | Le (x, y) -> pf "(%a@ @<2>≤ %a)" pp_t x pp_t y
    | Ord (x, y) -> pf "(%a@ ord %a)" pp_t x pp_t y
    | Uno (x, y) -> pf "(%a@ uno %a)" pp_t x pp_t y
    | Not x -> pf "¬%a" pp x
    | And (x, y) -> pf "(%a@ @<2>∧ %a)" pp x pp y
    | Or (x, y) -> pf "(%a@ @<2>∨ %a)" pp x pp y
    | Iff (x, y) -> pf "(%a@ <=> %a)" pp x pp y
    | Xor (x, y) -> pf "(%a@ xor %a)" pp x pp y
    | Imp (x, y) -> pf "(%a@ => %a)" pp x pp y
    | Cond {cnd; pos; neg} -> pf "(%a@ ? %a@ : %a)" pp cnd pp pos pp neg
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
  match (e : trm) with
  | Var _ as v -> f init (Var.of_ v)
  | Z _ | Q _ | RecRecord _ | Label _ | Float _ -> init
  | Neg x
   |Mulq (_, x)
   |Splat x
   |Signed (_, x)
   |Unsigned (_, x)
   |Convert {src= _; dst= _; arg= x} ->
      fold_vars_t ~f x ~init
  | Add (x, y)
   |Sub (x, y)
   |Mul (x, y)
   |Div (x, y)
   |Rem (x, y)
   |Sized {seq= x; siz= y}
   |Select (x, y)
   |BAnd (x, y)
   |BOr (x, y)
   |BXor (x, y)
   |BShl (x, y)
   |BLshr (x, y)
   |BAshr (x, y) ->
      fold_vars_t ~f x ~init:(fold_vars_t ~f y ~init)
  | Update (x, y, z) | Extract {seq= x; off= y; len= z} ->
      fold_vars_t ~f x
        ~init:(fold_vars_t ~f y ~init:(fold_vars_t ~f z ~init))
  | Concat xs | Record xs ->
      IArray.fold ~f:(fun init -> fold_vars_t ~f ~init) xs ~init

let rec fold_vars_f ~init p ~f =
  match (p : fml) with
  | Tt | Ff -> init
  | Eq (x, y) | Dq (x, y) | Lt (x, y) | Le (x, y) | Ord (x, y) | Uno (x, y)
    ->
      fold_vars_t ~f x ~init:(fold_vars_t ~f y ~init)
  | Not x -> fold_vars_f ~f x ~init
  | And (x, y) | Or (x, y) | Iff (x, y) | Xor (x, y) | Imp (x, y) ->
      fold_vars_f ~f x ~init:(fold_vars_f ~f y ~init)
  | Cond {cnd; pos; neg} ->
      fold_vars_f ~f cnd
        ~init:(fold_vars_f ~f pos ~init:(fold_vars_f ~f neg ~init))

let rec fold_vars_c ~init ~f = function
  | `Ite (cnd, thn, els) ->
      fold_vars_f ~f cnd
        ~init:(fold_vars_c ~f thn ~init:(fold_vars_c ~f els ~init))
  | `Trm t -> fold_vars_t ~f t ~init

let fold_vars ~init e ~f =
  match e with
  | `Fml p -> fold_vars_f ~f ~init p
  | #cnd as c -> fold_vars_c ~f ~init c

(** map_vars *)

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
  let xs' = IArray.map_endo ~f xs in
  if xs' == xs then e else cons xs'

let rec map_vars_t ~f e =
  match e with
  | Var _ as v -> (f (Var.of_ v) : var :> trm)
  | Z _ | Q _ -> e
  | Neg x -> map1 (map_vars_t ~f) e _Neg x
  | Add (x, y) -> map2 (map_vars_t ~f) e _Add x y
  | Sub (x, y) -> map2 (map_vars_t ~f) e _Sub x y
  | Mulq (q, x) -> map1 (map_vars_t ~f) e (_Mulq q) x
  | Mul (x, y) -> map2 (map_vars_t ~f) e _Mul x y
  | Div (x, y) -> map2 (map_vars_t ~f) e _Div x y
  | Rem (x, y) -> map2 (map_vars_t ~f) e _Rem x y
  | Splat x -> map1 (map_vars_t ~f) e _Splat x
  | Sized {seq; siz} -> map2 (map_vars_t ~f) e _Sized seq siz
  | Extract {seq; off; len} -> map3 (map_vars_t ~f) e _Extract seq off len
  | Concat xs -> mapN (map_vars_t ~f) e _Concat xs
  | Select (r, i) -> map2 (map_vars_t ~f) e _Select r i
  | Update (r, i, e) -> map3 (map_vars_t ~f) e _Update r i e
  | Record xs -> mapN (map_vars_t ~f) e _Record xs
  | RecRecord _ | Label _ | Float _ -> e
  | BAnd (x, y) -> map2 (map_vars_t ~f) e _BAnd x y
  | BOr (x, y) -> map2 (map_vars_t ~f) e _BOr x y
  | BXor (x, y) -> map2 (map_vars_t ~f) e _BXor x y
  | BShl (x, y) -> map2 (map_vars_t ~f) e _BShl x y
  | BLshr (x, y) -> map2 (map_vars_t ~f) e _BLshr x y
  | BAshr (x, y) -> map2 (map_vars_t ~f) e _BAshr x y
  | Signed (n, x) -> map1 (map_vars_t ~f) e (_Signed n) x
  | Unsigned (n, x) -> map1 (map_vars_t ~f) e (_Unsigned n) x
  | Convert {src; dst; arg} -> map1 (map_vars_t ~f) e (_Convert src dst) arg

let rec map_vars_f ~f e =
  match e with
  | Tt | Ff -> e
  | Eq (x, y) -> map2 (map_vars_t ~f) e _Eq x y
  | Dq (x, y) -> map2 (map_vars_t ~f) e _Dq x y
  | Lt (x, y) -> map2 (map_vars_t ~f) e _Lt x y
  | Le (x, y) -> map2 (map_vars_t ~f) e _Le x y
  | Ord (x, y) -> map2 (map_vars_t ~f) e _Ord x y
  | Uno (x, y) -> map2 (map_vars_t ~f) e _Uno x y
  | Not x -> map1 (map_vars_f ~f) e _Not x
  | And (x, y) -> map2 (map_vars_f ~f) e _And x y
  | Or (x, y) -> map2 (map_vars_f ~f) e _Or x y
  | Iff (x, y) -> map2 (map_vars_f ~f) e _Iff x y
  | Xor (x, y) -> map2 (map_vars_f ~f) e _Xor x y
  | Imp (x, y) -> map2 (map_vars_f ~f) e _Imp x y
  | Cond {cnd; pos; neg} -> map3 (map_vars_f ~f) e _Cond cnd pos neg

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

let zero = Z Z.zero
let one = Z Z.one

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
  | #cnd as c ->
      (* Some normalization is necessary for [embed_into_fml] to be left
         inverse to [embed_into_cnd]. Essentially [0 ≠ (p ? 1 : 0)] needs to
         normalize to [p], by way of [0 ≠ (p ? 1 : 0)] ==> [(p ? 0 ≠ 1 : 0 ≠
         0)] ==> [(p ? tt : ff)] ==> [p]. *)
      let dq0 : trm -> fml = function
        (* 0 ≠ 0 ==> ff *)
        | Z _ as z when z == zero -> Ff
        (* 0 ≠ N ==> tt for N≠0 *)
        | Z _ -> Tt
        | t -> Dq (zero, t)
      in
      let cond : fml -> fml -> fml -> fml =
       fun cnd pos neg ->
        match (pos, neg) with
        (* (p ? tt : ff) ==> p *)
        | Tt, Ff -> cnd
        | _ -> Cond {cnd; pos; neg}
      in
      map_cnd cond dq0 c

(** Construct a conditional term, or formula if possible precisely. *)
let ite : fml -> exp -> exp -> exp =
 fun cnd thn els ->
  match (thn, els) with
  | `Fml pos, `Fml neg -> `Fml (Cond {cnd; pos; neg})
  | _ -> (
      let c = `Ite (cnd, embed_into_cnd thn, embed_into_cnd els) in
      match project_out_fml c with Some f -> `Fml f | None -> c )

(** Map a unary function on terms over an expression. *)
let ap1 : (trm -> exp) -> exp -> exp =
 fun f x -> map_cnd ite f (embed_into_cnd x)

let ap1t : (trm -> trm) -> exp -> exp = fun f -> ap1 (fun x -> `Trm (f x))

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
 * Formulas: exposed interface
 *)

module Formula = struct
  type t = fml [@@deriving compare, equal, sexp]

  let inject f = `Fml f
  let project = function `Fml f -> Some f | #cnd as c -> project_out_fml c
  let ppx = ppx_f
  let pp = pp_f

  (* constants *)

  let tt = Tt
  let ff = Ff

  (* comparisons *)

  let eq = ap2f _Eq
  let dq = ap2f _Dq
  let lt = ap2f _Lt
  let le = ap2f _Le
  let ord = ap2f _Ord
  let uno = ap2f _Uno

  (* connectives *)

  let not_ = _Not
  let and_ = _And
  let or_ = _Or
  let iff = _Iff
  let xor = _Xor
  let imp = _Imp
  let nimp x y = not_ (imp x y)
  let cond ~cnd ~pos ~neg = _Cond cnd pos neg

  (** Query *)

  let fv e = fold_vars_f e ~f:Var.Set.add ~init:Var.Set.empty
  let is_true = function Tt -> true | _ -> false
  let is_false = function Ff -> true | _ -> false

  (** Traverse *)

  let fold_vars = fold_vars_f

  (** Transform *)

  let map_vars = map_vars_f

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

  let disjuncts p =
    let rec disjuncts_ p ds =
      match p with
      | Or (a, b) -> disjuncts_ a (disjuncts_ b ds)
      | Cond {cnd; pos; neg} ->
          disjuncts_ (And (cnd, pos)) (disjuncts_ (And (Not cnd, neg)) ds)
      | d -> d :: ds
    in
    disjuncts_ p []
end

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

  let float s =
    match Q.of_float (Float.of_string s) with
    | q when Q.is_real q -> rational q
    | _ | (exception Invalid_argument _) -> `Trm (Float s)

  let label ~parent ~name = `Trm (Label {parent; name})

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
          | _ -> ap2t _Mul (`Trm x) (`Trm y) ) )

  let div = ap2t _Div
  let rem = ap2t _Rem

  (* sequences *)

  let splat = ap1t _Splat
  let sized ~seq ~siz = ap2t _Sized seq siz
  let extract ~seq ~off ~len = ap3t _Extract seq off len
  let concat elts = apNt (fun es -> _Concat (IArray.of_list es)) elts

  (* records *)

  let select ~rcd ~idx = ap2t _Select rcd idx
  let update ~rcd ~idx ~elt = ap3t _Update rcd idx elt
  let record elts = apNt (fun es -> _Record (IArray.of_list es)) elts
  let rec_record i = `Trm (_RecRecord i)

  (* bitwise *)

  let band = ap2t _BAnd
  let bor = ap2t _BOr
  let bxor = ap2t _BXor
  let bshl = ap2t _BShl
  let blshr = ap2t _BLshr
  let bashr = ap2t _BAshr

  (* type conversions *)

  let signed bits = function
    | `Fml _ as fml when bits = 1 -> fml
    | arg -> ap1t (_Signed bits) arg

  let unsigned bits = function
    | `Fml _ as fml when bits = 1 -> fml
    | arg -> ap1t (_Unsigned bits) arg

  let convert src ~to_:dst arg = ap1t (_Convert src dst) arg

  (* if-then-else *)

  let ite ~cnd ~thn ~els = ite cnd thn els

  (** Destruct *)

  let d_int = function `Trm (Z z) -> Some z | _ -> None

  (** Access *)

  let const_of x =
    let rec const_of t =
      let add = Option.map2 ~f:Q.add in
      let neg = Option.map ~f:Q.neg in
      match t with
      | Z z -> Some (Q.of_z z)
      | Q q -> Some q
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
  | Mul (x, y) -> Ses.Term.mul (t_to_ses x) (t_to_ses y)
  | Div (x, y) -> Ses.Term.div (t_to_ses x) (t_to_ses y)
  | Rem (x, y) -> Ses.Term.rem (t_to_ses x) (t_to_ses y)
  | Select (r, i) ->
      Ses.Term.select ~rcd:(t_to_ses r) ~idx:(to_int (t_to_ses i))
  | Update (r, i, e) ->
      Ses.Term.update ~rcd:(t_to_ses r)
        ~idx:(to_int (t_to_ses i))
        ~elt:(t_to_ses e)
  | Record es -> Ses.Term.record (IArray.map ~f:t_to_ses es)
  | RecRecord i -> Ses.Term.rec_record i
  | Splat x -> Ses.Term.splat (t_to_ses x)
  | Sized {seq; siz} ->
      Ses.Term.sized ~seq:(t_to_ses seq) ~siz:(t_to_ses siz)
  | Extract {seq; off; len} ->
      Ses.Term.extract ~seq:(t_to_ses seq) ~off:(t_to_ses off)
        ~len:(t_to_ses len)
  | Concat es ->
      Ses.Term.concat (IArray.to_array (IArray.map ~f:t_to_ses es))
  | BAnd (x, y) -> Ses.Term.and_ (t_to_ses x) (t_to_ses y)
  | BOr (x, y) -> Ses.Term.or_ (t_to_ses x) (t_to_ses y)
  | BXor (x, y) -> Ses.Term.dq (t_to_ses x) (t_to_ses y)
  | BShl (x, y) -> Ses.Term.shl (t_to_ses x) (t_to_ses y)
  | BLshr (x, y) -> Ses.Term.lshr (t_to_ses x) (t_to_ses y)
  | BAshr (x, y) -> Ses.Term.ashr (t_to_ses x) (t_to_ses y)
  | Label {parent; name} -> Ses.Term.label ~parent ~name
  | Float s -> Ses.Term.float s
  | Signed (n, x) -> Ses.Term.signed n (t_to_ses x)
  | Unsigned (n, x) -> Ses.Term.unsigned n (t_to_ses x)
  | Convert {src; dst; arg} -> Ses.Term.convert src ~to_:dst (t_to_ses arg)

let rec f_to_ses : fml -> Ses.Term.t = function
  | Tt -> Ses.Term.true_
  | Ff -> Ses.Term.false_
  | Eq (x, y) -> Ses.Term.eq (t_to_ses x) (t_to_ses y)
  | Dq (x, y) -> Ses.Term.dq (t_to_ses x) (t_to_ses y)
  | Lt (x, y) -> Ses.Term.lt (t_to_ses x) (t_to_ses y)
  | Le (x, y) -> Ses.Term.le (t_to_ses x) (t_to_ses y)
  | Ord (x, y) -> Ses.Term.ord (t_to_ses x) (t_to_ses y)
  | Uno (x, y) -> Ses.Term.uno (t_to_ses x) (t_to_ses y)
  | Not p -> Ses.Term.not_ (f_to_ses p)
  | And (p, q) -> Ses.Term.and_ (f_to_ses p) (f_to_ses q)
  | Or (p, q) -> Ses.Term.or_ (f_to_ses p) (f_to_ses q)
  | Iff (p, q) -> Ses.Term.eq (f_to_ses p) (f_to_ses q)
  | Xor (p, q) -> Ses.Term.dq (f_to_ses p) (f_to_ses q)
  | Imp (p, q) -> Ses.Term.le (f_to_ses p) (f_to_ses q)
  | Cond {cnd; pos; neg} ->
      Ses.Term.conditional ~cnd:(f_to_ses cnd) ~thn:(f_to_ses pos)
        ~els:(f_to_ses neg)

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

let rec ap_tt f a = f (of_ses a)
and ap_ttt f a b = f (of_ses a) (of_ses b)
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
  | Float {data} -> float data
  | Label {parent; name} -> label ~parent ~name
  | Ap1 (Signed {bits}, e) -> ap_tt (signed bits) e
  | Ap1 (Unsigned {bits}, e) -> ap_tt (unsigned bits) e
  | Ap1 (Convert {src; dst}, e) -> ap_tt (convert src ~to_:dst) e
  | Ap2 (Eq, d, e) -> ap2_f iff eq d e
  | Ap2 (Dq, d, e) -> ap2_f xor dq d e
  | Ap2 (Lt, d, e) -> ap2_f (Fn.flip nimp) lt d e
  | Ap2 (Le, d, e) -> ap2_f imp le d e
  | Ap2 (Ord, d, e) -> ap_ttf ord d e
  | Ap2 (Uno, d, e) -> ap_ttf uno d e
  | Add sum -> (
    match Ses.Term.Qset.pop_min_elt sum with
    | None -> zero
    | Some (e, q, sum) ->
        let mul e q = mulq q (of_ses e) in
        Ses.Term.Qset.fold sum ~init:(mul e q) ~f:(fun e q s ->
            add (mul e q) s ) )
  | Mul prod -> (
    match Ses.Term.Qset.pop_min_elt prod with
    | None -> one
    | Some (e, q, prod) ->
        let rec expn e n =
          if Z.sign n = 0 then one else mul e (expn e (Z.pred n))
        in
        let exp e q =
          let n = Q.num q in
          if Z.sign n >= 0 then expn (of_ses e) n
          else div one (expn (of_ses e) (Z.neg n))
        in
        Ses.Term.Qset.fold prod ~init:(exp e q) ~f:(fun e q s ->
            mul (exp e q) s ) )
  | Ap2 (Div, d, e) -> ap_ttt div d e
  | Ap2 (Rem, d, e) -> ap_ttt rem d e
  | And es -> apN and_ band tt es
  | Or es -> apN or_ bor ff es
  | Ap2 (Xor, d, e) -> ap2 xor bxor d e
  | Ap2 (Shl, d, e) -> ap_ttt bshl d e
  | Ap2 (Lshr, d, e) -> ap_ttt blshr d e
  | Ap2 (Ashr, d, e) -> ap_ttt bashr d e
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
      concat (IArray.to_array (IArray.map ~f:of_ses args))
  | Ap1 (Select idx, rcd) ->
      select ~rcd:(of_ses rcd) ~idx:(integer (Z.of_int idx))
  | Ap2 (Update idx, rcd, elt) ->
      update ~rcd:(of_ses rcd)
        ~idx:(integer (Z.of_int idx))
        ~elt:(of_ses elt)
  | ApN (Record, elts) ->
      record (IArray.to_array (IArray.map ~f:of_ses elts))
  | RecRecord i -> rec_record i

let f_of_ses e = embed_into_fml (of_ses e)

let v_map_ses : (var -> var) -> Ses.Var.t -> Ses.Var.t =
 fun f x ->
  let v = v_of_ses x in
  let v' = f v in
  if v' == v then x else v_to_ses v'

let ses_map : (Ses.Term.t -> Ses.Term.t) -> exp -> exp =
 fun f x ->
  let e = to_ses x in
  let e' = f e in
  if e' == e then x else of_ses e'

let f_ses_map : (Ses.Term.t -> Ses.Term.t) -> fml -> fml =
 fun f x ->
  let e = f_to_ses x in
  let e' = f e in
  if e' == e then x else f_of_ses e'

(*
 * Contexts
 *)

module Context = struct
  type t = Ses.Equality.t [@@deriving sexp]
  type classes = exp list Term.Map.t

  let classes_of_ses clss =
    Ses.Term.Map.fold clss ~init:Term.Map.empty
      ~f:(fun ~key:rep ~data:cls clss ->
        let rep' = of_ses rep in
        let cls' = List.map ~f:of_ses cls in
        Term.Map.set ~key:rep' ~data:cls' clss )

  let classes x = classes_of_ses (Ses.Equality.classes x)
  let diff_classes x y = classes_of_ses (Ses.Equality.diff_classes x y)
  let pp = Ses.Equality.pp
  let ppx_cls x = List.pp "@ = " (Term.ppx x)

  let ppx_classes x fs clss =
    List.pp "@ @<2>∧ "
      (fun fs (rep, cls) ->
        Format.fprintf fs "@[%a@ = %a@]" (Term.ppx x) rep (ppx_cls x)
          (List.sort ~compare:Term.compare cls) )
      fs (Term.Map.to_alist clss)

  let pp_classes fs r = ppx_classes (fun _ -> None) fs (classes r)
  let invariant = Ses.Equality.invariant
  let true_ = Ses.Equality.true_

  let and_formula vs f x =
    let vs', x' = Ses.Equality.and_term (vs_to_ses vs) (f_to_ses f) x in
    (vs_of_ses vs', x')

  let and_ vs x y =
    let vs', z = Ses.Equality.and_ (vs_to_ses vs) x y in
    (vs_of_ses vs', z)

  let orN vs xs =
    let vs', z = Ses.Equality.orN (vs_to_ses vs) xs in
    (vs_of_ses vs', z)

  let rename x sub = Ses.Equality.rename x (v_map_ses (Var.Subst.apply sub))
  let fv x = vs_of_ses (Ses.Equality.fv x)
  let is_true x = Ses.Equality.is_true x
  let is_false x = Ses.Equality.is_false x
  let entails_eq x e f = Ses.Equality.entails_eq x (to_ses e) (to_ses f)
  let class_of x e = List.map ~f:of_ses (Ses.Equality.class_of x (to_ses e))
  let normalize x e = ses_map (Ses.Equality.normalize x) e
  let normalizef x e = f_ses_map (Ses.Equality.normalize x) e
  let difference x e f = Ses.Equality.difference x (to_ses e) (to_ses f)

  let fold_terms ~init x ~f =
    Ses.Equality.fold_terms x ~init ~f:(fun s e -> f s (of_ses e))

  module Subst = struct
    type t = Ses.Equality.Subst.t [@@deriving sexp]

    let pp = Ses.Equality.Subst.pp
    let is_empty = Ses.Equality.Subst.is_empty

    let fold s ~init ~f =
      Ses.Equality.Subst.fold s ~init ~f:(fun ~key ~data ->
          f ~key:(of_ses key) ~data:(of_ses data) )

    let subst s = ses_map (Ses.Equality.Subst.subst s)
    let substf s = f_ses_map (Ses.Equality.Subst.subst s)

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
    | Normalize of t * exp
    | Normalizef of t * fml
    | And_formula of Var.Set.t * fml * t
    | And_ of Var.Set.t * t * t
    | OrN of Var.Set.t * t list
    | Rename of t * Var.Subst.t
    | Apply_subst of Var.Set.t * Subst.t * t
    | Solve_for_vars of Var.Set.t list * t
  [@@deriving sexp]

  let replay c =
    match call_of_sexp (Sexp.of_string c) with
    | Normalize (r, e) -> normalize r e |> ignore
    | Normalizef (r, e) -> normalizef r e |> ignore
    | And_formula (us, e, r) -> and_formula us e r |> ignore
    | And_ (us, r, s) -> and_ us r s |> ignore
    | OrN (us, rs) -> orN us rs |> ignore
    | Rename (r, s) -> rename r s |> ignore
    | Apply_subst (us, s, r) -> apply_subst us s r |> ignore
    | Solve_for_vars (vss, r) -> solve_for_vars vss r |> ignore

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
      try f () with exn -> raise_s ([%sexp_of: exn * call] (exn, call ()))

  let normalize_tmr = Timer.create "normalize" ~at_exit:report
  let and_formula_tmr = Timer.create "and_formula" ~at_exit:report
  let and_tmr = Timer.create "and_" ~at_exit:report
  let orN_tmr = Timer.create "orN" ~at_exit:report
  let rename_tmr = Timer.create "rename" ~at_exit:report
  let apply_subst_tmr = Timer.create "apply_subst" ~at_exit:report
  let solve_for_vars_tmr = Timer.create "solve_for_vars" ~at_exit:report

  let normalize r e =
    wrap normalize_tmr (fun () -> normalize r e) (fun () -> Normalize (r, e))

  let normalizef r e =
    wrap normalize_tmr
      (fun () -> normalizef r e)
      (fun () -> Normalizef (r, e))

  let and_formula us e r =
    wrap and_formula_tmr
      (fun () -> and_formula us e r)
      (fun () -> And_formula (us, e, r))

  let and_ us r s =
    wrap and_tmr (fun () -> and_ us r s) (fun () -> And_ (us, r, s))

  let orN us rs = wrap orN_tmr (fun () -> orN us rs) (fun () -> OrN (us, rs))

  let rename r s =
    wrap rename_tmr (fun () -> rename r s) (fun () -> Rename (r, s))

  let apply_subst us s r =
    wrap apply_subst_tmr
      (fun () -> apply_subst us s r)
      (fun () -> Apply_subst (us, s, r))

  let solve_for_vars vss r =
    wrap solve_for_vars_tmr
      (fun () -> solve_for_vars vss r)
      (fun () -> Solve_for_vars (vss, r))
end

(*
 * Convert from Llair
 *)

module Term_of_Llair = struct
  let rec uap_ttt : 'a. (exp -> exp -> 'a) -> _ -> _ -> _ -> 'a =
   fun f typ a b ->
    let bits = Llair.Typ.bit_size_of typ in
    f (Term.unsigned bits (exp a)) (Term.unsigned bits (exp b))

  and uap_ttf (f : exp -> exp -> fml) typ a b = `Fml (uap_ttt f typ a b)
  and ap_tt : 'a. (exp -> 'a) -> _ -> 'a = fun f a -> f (exp a)
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
    | Label {parent; name} -> label ~parent ~name
    | Integer {typ= _; data} -> integer data
    | Float {data; typ= _} -> float data
    | Ap1 (Signed {bits}, _, e) -> ap_tt (signed bits) e
    | Ap1 (Unsigned {bits}, _, e) -> ap_tt (unsigned bits) e
    | Ap1 (Convert {src}, dst, e) -> ap_tt (convert src ~to_:dst) e
    | Ap2 (Eq, Integer {bits= 1; _}, d, e) -> ap_fff iff d e
    | Ap2 (Dq, Integer {bits= 1; _}, d, e) -> ap_fff xor d e
    | Ap2 ((Gt | Ugt), Integer {bits= 1; _}, d, e) -> ap_fff nimp d e
    | Ap2 ((Lt | Ult), Integer {bits= 1; _}, d, e) -> ap_fff nimp e d
    | Ap2 ((Ge | Uge), Integer {bits= 1; _}, d, e) -> ap_fff imp e d
    | Ap2 ((Le | Ule), Integer {bits= 1; _}, d, e) -> ap_fff imp d e
    | Ap2 (Eq, _, d, e) -> ap_ttf eq d e
    | Ap2 (Dq, _, d, e) -> ap_ttf dq d e
    | Ap2 (Gt, _, d, e) -> ap_ttf lt e d
    | Ap2 (Lt, _, d, e) -> ap_ttf lt d e
    | Ap2 (Ge, _, d, e) -> ap_ttf le e d
    | Ap2 (Le, _, d, e) -> ap_ttf le d e
    | Ap2 (Ugt, typ, d, e) -> uap_ttf lt typ e d
    | Ap2 (Ult, typ, d, e) -> uap_ttf lt typ d e
    | Ap2 (Uge, typ, d, e) -> uap_ttf le typ e d
    | Ap2 (Ule, typ, d, e) -> uap_ttf le typ d e
    | Ap2 (Ord, _, d, e) -> ap_ttf ord d e
    | Ap2 (Uno, _, d, e) -> ap_ttf uno d e
    | Ap2 (Add, Integer {bits= 1; _}, d, e) -> ap_fff xor d e
    | Ap2 (Sub, Integer {bits= 1; _}, d, e) -> ap_fff xor d e
    | Ap2 (Mul, Integer {bits= 1; _}, d, e) -> ap_fff and_ d e
    | Ap2 (Add, _, d, e) -> ap_ttt add d e
    | Ap2 (Sub, _, d, e) -> ap_ttt sub d e
    | Ap2 (Mul, _, d, e) -> ap_ttt mul d e
    | Ap2 (Div, _, d, e) -> ap_ttt div d e
    | Ap2 (Rem, _, d, e) -> ap_ttt rem d e
    | Ap2 (Udiv, typ, d, e) -> uap_ttt div typ d e
    | Ap2 (Urem, typ, d, e) -> uap_ttt rem typ d e
    | Ap2 (And, Integer {bits= 1; _}, d, e) -> ap_fff and_ d e
    | Ap2 (Or, Integer {bits= 1; _}, d, e) -> ap_fff or_ d e
    | Ap2 (Xor, Integer {bits= 1; _}, d, e) -> ap_fff xor d e
    | Ap2 (And, _, d, e) -> ap_ttt band d e
    | Ap2 (Or, _, d, e) -> ap_ttt bor d e
    | Ap2 (Xor, _, d, e) -> ap_ttt bxor d e
    | Ap2 (Shl, _, d, e) -> ap_ttt bshl d e
    | Ap2 (Lshr, _, d, e) -> ap_ttt blshr d e
    | Ap2 (Ashr, _, d, e) -> ap_ttt bashr d e
    | Ap3 (Conditional, Integer {bits= 1; _}, cnd, pos, neg) ->
        ap_ffff _Cond cnd pos neg
    | Ap3 (Conditional, _, cnd, thn, els) ->
        ite ~cnd:(embed_into_fml (exp cnd)) ~thn:(exp thn) ~els:(exp els)
    | Ap1 (Select idx, _, rcd) ->
        select ~rcd:(exp rcd) ~idx:(integer (Z.of_int idx))
    | Ap2 (Update idx, _, rcd, elt) ->
        update ~rcd:(exp rcd) ~idx:(integer (Z.of_int idx)) ~elt:(exp elt)
    | ApN (Record, _, elts) ->
        record (IArray.to_array (IArray.map ~f:exp elts))
    | RecRecord (i, _) -> rec_record i
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
