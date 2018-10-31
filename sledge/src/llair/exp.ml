(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Expressions *)

(** Z wrapped to treat bounded and unsigned operations *)
module Z = struct
  type t = Z.t [@@deriving compare, hash, sexp]

  let equal = Z.equal
  let pp = Z.pp_print
  let zero = Z.zero
  let one = Z.one
  let to_int = Z.to_int
  let numbits = Z.numbits
  let fits_int = Z.fits_int

  (* the signed 1-bit integers are -1 and 0 *)
  let true_ = Z.minus_one
  let false_ = Z.zero
  let of_bool = function true -> true_ | false -> false_
  let is_true = Z.equal true_
  let is_false = Z.equal false_

  (** Interpret as a bounded integer with specified signedness and width. *)
  let clamp ~signed bits z =
    if signed then Z.signed_extract z 0 bits else Z.extract z 0 bits

  let clamp_cmp ~signed bits op x y =
    op (clamp ~signed bits x) (clamp ~signed bits y)

  let neg ~bits z = Z.neg (clamp bits ~signed:true z)

  let clamp_bop ~signed bits op x y =
    clamp ~signed bits (op (clamp ~signed bits x) (clamp ~signed bits y))

  let eq ~bits x y = clamp_cmp ~signed:true bits Z.equal x y
  let leq ~bits x y = clamp_cmp ~signed:true bits Z.leq x y
  let geq ~bits x y = clamp_cmp ~signed:true bits Z.geq x y
  let lt ~bits x y = clamp_cmp ~signed:true bits Z.lt x y
  let gt ~bits x y = clamp_cmp ~signed:true bits Z.gt x y
  let uleq ~bits x y = clamp_cmp ~signed:false bits Z.leq x y
  let ugeq ~bits x y = clamp_cmp ~signed:false bits Z.geq x y
  let ult ~bits x y = clamp_cmp ~signed:false bits Z.lt x y
  let ugt ~bits x y = clamp_cmp ~signed:false bits Z.gt x y
  let add ~bits x y = clamp_bop ~signed:true bits Z.add x y
  let sub ~bits x y = clamp_bop ~signed:true bits Z.sub x y
  let mul ~bits x y = clamp_bop ~signed:true bits Z.mul x y
  let div ~bits x y = clamp_bop ~signed:true bits Z.div x y
  let rem ~bits x y = clamp_bop ~signed:true bits Z.rem x y
  let udiv ~bits x y = clamp_bop ~signed:false bits Z.div x y
  let urem ~bits x y = clamp_bop ~signed:false bits Z.rem x y
  let logand ~bits x y = clamp_bop ~signed:true bits Z.logand x y
  let logor ~bits x y = clamp_bop ~signed:true bits Z.logor x y
  let logxor ~bits x y = clamp_bop ~signed:true bits Z.logxor x y
  let shift_left ~bits z i = Z.shift_left (clamp bits ~signed:true z) i
  let shift_right ~bits z i = Z.shift_right (clamp bits ~signed:true z) i

  let shift_right_trunc ~bits z i =
    Z.shift_right_trunc (clamp bits ~signed:true z) i
end

module T0 = struct
  type t =
    | Var of {id: int; name: string}
    | Nondet of {msg: string}
    | Label of {parent: string; name: string}
    | App of {op: t; arg: t}
    (* pointer and memory constants and operations *)
    | Null
    | Splat
    | Memory
    | Concat
    (* numeric constants *)
    | Integer of {data: Z.t; typ: Typ.t}
    | Float of {data: string}
    (* binary: comparison *)
    | Eq
    | Dq
    | Gt
    | Ge
    | Lt
    | Le
    | Ugt
    | Uge
    | Ult
    | Ule
    | Ord
    | Uno
    (* binary: arithmetic, numeric and pointer *)
    | Add
    | Sub
    | Mul
    | Div
    | Udiv
    | Rem
    | Urem
    (* binary: boolean / bitwise *)
    | And
    | Or
    | Xor
    | Shl
    | Lshr
    | Ashr
    (* ternary: conditional *)
    | Conditional
    (* array/struct constants and operations *)
    | Record
    | Select
    | Update
    | Struct_rec of {elts: t vector}  (** NOTE: may be cyclic *)
    (* unary: conversion *)
    | Convert of {signed: bool; dst: Typ.t; src: Typ.t}
  [@@deriving compare, hash, sexp]

  let equal = [%compare.equal: t]
end

module T = struct
  include T0
  include Comparator.Make (T0)

  let fix (f : (t -> 'a as 'f) -> 'f) (bot : 'f) (e : t) : 'a =
    let rec fix_f seen e =
      match e with
      | Struct_rec _ ->
          if List.mem ~equal:( == ) seen e then f bot e
          else f (fix_f (e :: seen)) e
      | _ -> f (fix_f seen) e
    in
    let rec fix_f_seen_nil e =
      match e with
      | Struct_rec _ -> f (fix_f [e]) e
      | _ -> f fix_f_seen_nil e
    in
    fix_f_seen_nil e

  let fix_flip (f : ('z -> t -> 'a as 'f) -> 'f) (bot : 'f) (z : 'z) (e : t)
      =
    fix (fun f' e z -> f (fun z e -> f' e z) z e) (fun e z -> bot z e) e z

  let uncurry =
    let rec uncurry_ args = function
      | App {op; arg} -> uncurry_ (arg :: args) op
      | op -> (op, args)
    in
    uncurry_ []

  let rec pp fs exp =
    let pp_ pp fs exp =
      let pf fmt =
        Format.pp_open_box fs 2 ;
        Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
      in
      match exp with
      | Var {name; id= 0} -> pf "%%%s" name
      | Var {name; id} -> pf "%%%s_%d" name id
      | Nondet {msg} -> pf "nondet \"%s\"" msg
      | Label {name} -> pf "%s" name
      | Null -> pf "null"
      | Splat -> pf "^"
      | Memory -> pf "⟨_,_⟩"
      | App {op= App {op= Memory; arg= siz}; arg= bytes} ->
          pf "@<1>⟨%a,%a@<1>⟩" pp siz pp bytes
      | Concat -> pf "^"
      | Integer {data} -> pf "%a" Z.pp data
      | Float {data} -> pf "%s" data
      | Eq -> pf "="
      | Dq -> pf "!="
      | Gt -> pf ">"
      | Ge -> pf ">="
      | Lt -> pf "<"
      | Le -> pf "<="
      | Ugt -> pf "u>"
      | Uge -> pf "u>="
      | Ult -> pf "u<"
      | Ule -> pf "u<="
      | Ord -> pf "ord"
      | Uno -> pf "uno"
      | Add -> pf "+"
      | Sub -> pf "-"
      | Mul -> pf "*"
      | Div -> pf "/"
      | Udiv -> pf "udiv"
      | Rem -> pf "rem"
      | Urem -> pf "urem"
      | And -> pf "&&"
      | Or -> pf "||"
      | Xor -> pf "xor"
      | App
          { op= App {op= Xor; arg}
          ; arg= Integer {data; typ= Integer {bits= 1}} }
        when Z.is_true data ->
          pf "¬%a" pp arg
      | App
          { op= App {op= Xor; arg= Integer {data; typ= Integer {bits= 1}}}
          ; arg }
        when Z.is_true data ->
          pf "¬%a" pp arg
      | Shl -> pf "shl"
      | Lshr -> pf "lshr"
      | Ashr -> pf "ashr"
      | Conditional -> pf "(_?_:_)"
      | App
          {op= App {op= App {op= Conditional; arg= cnd}; arg= thn}; arg= els}
        ->
          pf "(%a@ ? %a@ : %a)" pp cnd pp thn pp els
      | Select -> pf "_[_]"
      | App {op= App {op= Select; arg= rcd}; arg= idx} ->
          pf "%a[%a]" pp rcd pp idx
      | Update -> pf "[_|_→_]"
      | App {op= App {op= App {op= Update; arg= rcd}; arg= elt}; arg= idx}
        ->
          pf "[%a@ @[| %a → %a@]]" pp rcd pp idx pp elt
      | Record -> pf "{_}"
      | App {op; arg} -> (
        match uncurry exp with
        | Record, elts -> pf "{%a}" pp_record elts
        | op, [x; y] -> pf "(%a@ %a %a)" pp x pp op pp y
        | _ -> pf "(%a@ %a)" pp op pp arg )
      | Struct_rec {elts} -> pf "{|%a|}" (Vector.pp ",@ " pp) elts
      | Convert {dst; src} -> pf "(%a)(%a)" Typ.pp dst Typ.pp src
    in
    fix_flip pp_ (fun _ _ -> ()) fs exp

  and pp_record fs elts =
    [%Trace.fprintf
      fs "%a"
        (fun fs elts ->
          let elta = Array.of_list elts in
          match
            String.init (Array.length elta) ~f:(fun i ->
                match elta.(i) with
                | Integer {data} -> Char.of_int_exn (Z.to_int data)
                | _ -> raise (Invalid_argument "not a string") )
          with
          | s -> Format.fprintf fs "@[<h>%s@]" (String.escaped s)
          | exception _ ->
              Format.fprintf fs "@[<h>%a@]" (List.pp ",@ " pp) elts )
        elts]
end

include T

type exp = t

(** Invariant *)

let invariant ?(partial = false) e =
  Invariant.invariant [%here] e [%sexp_of: t]
  @@ fun () ->
  let op, args = uncurry e in
  let assert_arity arity =
    let nargs = List.length args in
    assert (nargs = arity || (partial && nargs < arity))
  in
  match op with
  | Integer {data; typ= Integer {bits}} ->
      assert_arity 0 ;
      assert (Z.numbits data <= bits)
  | Var _ | Nondet _ | Label _ | Null | Integer _ | Float _ ->
      assert_arity 0
  | Convert {dst; src} ->
      ( match args with
      | [Integer {typ}] -> assert (Typ.equal src typ)
      | _ -> assert_arity 1 ) ;
      assert (Typ.convertible src dst)
  | Eq | Dq | Gt | Ge | Lt | Le | Ugt | Uge | Ult | Ule | Add | Sub | Mul
   |Div | Udiv | Rem | Urem | And | Or | Xor | Shl | Lshr | Ashr -> (
    match args with
    | [Integer {typ= Integer {bits= m}}; Integer {typ= Integer {bits= n}}]
      ->
        assert (m = n)
    | _ -> assert_arity 2 )
  | Splat | Memory | Concat | Ord | Uno | Select -> assert_arity 2
  | Conditional | Update -> assert_arity 3
  | Record -> assert (partial || not (List.is_empty args))
  | Struct_rec {elts} ->
      assert (not (Vector.is_empty elts)) ;
      assert_arity 0
  | App _ -> fail "uncurry cannot return App" ()

(** Variables are the expressions constructed by [Var] *)
module Var = struct
  include T

  type var = t

  module Set = struct
    include (
      Set :
        module type of Set with type ('elt, 'cmp) t := ('elt, 'cmp) Set.t )

    type t = Set.M(T).t [@@deriving compare, sexp]

    let pp vs = Set.pp T.pp vs
    let empty = Set.empty (module T)
    let of_vector = Set.of_vector (module T)
  end

  let demangle =
    let open Ctypes in
    let cxa_demangle =
      (* char *__cxa_demangle(const char *, char *, size_t *, int * ) *)
      Foreign.foreign "__cxa_demangle"
        ( string @-> ptr char @-> ptr size_t @-> ptr int
        @-> returning string_opt )
    in
    let null_ptr_char = from_voidp char null in
    let null_ptr_size_t = from_voidp size_t null in
    let status = allocate int 0 in
    fun mangled ->
      let demangled =
        cxa_demangle mangled null_ptr_char null_ptr_size_t status
      in
      if !@status = 0 then demangled else None

  let pp_demangled fs = function
    | Var {name} -> (
      match demangle name with
      | Some demangled when not (String.equal name demangled) ->
          Format.fprintf fs "“%s”" demangled
      | _ -> () )
    | _ -> ()

  let invariant x =
    Invariant.invariant [%here] x [%sexp_of: t]
    @@ fun () -> match x with Var _ -> invariant x | _ -> assert false

  let id = function Var {id} -> id | x -> violates invariant x
  let name = function Var {name} -> name | x -> violates invariant x

  let of_exp = function
    | Var _ as v -> Some (v |> check invariant)
    | _ -> None

  let program name = Var {id= 0; name} |> check invariant

  let fresh name ~(wrt : Set.t) =
    let max = match Set.max_elt wrt with None -> 0 | Some max -> id max in
    let x' = Var {name; id= max + 1} in
    (x', Set.add wrt x')

  (** Variable renaming substitutions *)
  module Subst = struct
    type t = T.t Map.M(T).t [@@deriving compare, sexp]

    let invariant s =
      Invariant.invariant [%here] s [%sexp_of: t]
      @@ fun () ->
      let domain, range =
        Map.fold s ~init:(Set.empty, Set.empty)
          ~f:(fun ~key ~data (domain, range) ->
            assert (not (Set.mem range data)) ;
            (Set.add domain key, Set.add range data) )
      in
      assert (Set.disjoint domain range)

    let pp fs s =
      Format.fprintf fs "@[<1>[%a]@]"
        (List.pp ",@ " (fun fs (k, v) ->
             Format.fprintf fs "@[[%a ↦ %a]@]" T.pp k T.pp v ))
        (Map.to_alist s)

    let empty = Map.empty (module T)
    let is_empty = Map.is_empty

    let freshen vs ~wrt =
      let xs = Set.inter wrt vs in
      let wrt = Set.union wrt vs in
      Set.fold xs ~init:(empty, wrt) ~f:(fun (sub, wrt) x ->
          let x', wrt = fresh (name x) ~wrt in
          let sub = Map.add_exn sub ~key:x ~data:x' in
          (sub, wrt) )
      |> fst |> check invariant

    let extend sub ~replace ~with_ =
      ( match Map.add sub ~key:replace ~data:with_ with
      | `Duplicate -> sub
      | `Ok sub ->
          Map.map_preserving_phys_equal sub ~f:(fun v ->
              if equal v replace then with_ else v ) )
      |> check invariant

    let invert sub =
      Map.fold sub ~init:empty ~f:(fun ~key ~data sub' ->
          Map.add_exn sub' ~key:data ~data:key )
      |> check invariant

    let exclude sub vs =
      Set.fold vs ~init:sub ~f:Map.remove |> check invariant

    let domain sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key ~data:_ domain ->
          Set.add domain key )

    let range sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key:_ ~data range ->
          Set.add range data )

    let apply sub v = try Map.find_exn sub v with Caml.Not_found -> v

    let apply_set sub vs =
      Map.fold sub ~init:vs ~f:(fun ~key ~data vs ->
          let vs' = Set.remove vs key in
          if Set.to_tree vs' == Set.to_tree vs then vs
          else (
            assert (not (Set.equal vs' vs)) ;
            Set.add vs' data ) )

    let close_set sub vs =
      Map.fold sub ~init:vs ~f:(fun ~key:_ ~data vs -> Set.add vs data)
  end
end

let fold_exps e ~init ~f =
  let fold_exps_ fold_exps_ e z =
    let z =
      match e with
      | App {op; arg} -> fold_exps_ op (fold_exps_ arg z)
      | Struct_rec {elts} ->
          Vector.fold elts ~init:z ~f:(fun z elt -> fold_exps_ elt z)
      | _ -> z
    in
    f z e
  in
  fix fold_exps_ (fun _ z -> z) e init

let fold_vars e ~init ~f =
  fold_exps e ~init ~f:(fun z -> function
    | Var _ as v -> f z (v :> Var.t) | _ -> z )

let fv e = fold_vars e ~f:Set.add ~init:Var.Set.empty

(** Construct *)

let var x = x
let nondet msg = Nondet {msg} |> check invariant
let label ~parent ~name = Label {parent; name} |> check invariant
let null = Null |> check invariant
let integer data typ = Integer {data; typ} |> check invariant
let bool b = integer (Z.of_bool b) Typ.bool
let float data = Float {data} |> check invariant

let simp_convert signed (dst : Typ.t) src arg =
  match (dst, arg) with
  | Integer {bits= m}, Integer {data; typ= Integer {bits= n}} ->
      integer (Z.clamp ~signed (min m n) data) dst
  | _ -> App {op= Convert {signed; dst; src}; arg}

let simp_gt x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j; typ= Integer {bits}} ->
      bool (Z.gt ~bits i j)
  | _ -> App {op= App {op= Gt; arg= x}; arg= y}

let simp_ugt x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j; typ= Integer {bits}} ->
      bool (Z.ugt ~bits i j)
  | _ -> App {op= App {op= Ugt; arg= x}; arg= y}

let simp_ge x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j; typ= Integer {bits}} ->
      bool (Z.geq ~bits i j)
  | _ -> App {op= App {op= Ge; arg= x}; arg= y}

let simp_uge x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j; typ= Integer {bits}} ->
      bool (Z.ugeq ~bits i j)
  | _ -> App {op= App {op= Uge; arg= x}; arg= y}

let simp_lt x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j; typ= Integer {bits}} ->
      bool (Z.lt ~bits i j)
  | _ -> App {op= App {op= Lt; arg= x}; arg= y}

let simp_ult x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j; typ= Integer {bits}} ->
      bool (Z.ult ~bits i j)
  | _ -> App {op= App {op= Ult; arg= x}; arg= y}

let simp_le x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j; typ= Integer {bits}} ->
      bool (Z.leq ~bits i j)
  | _ -> App {op= App {op= Le; arg= x}; arg= y}

let simp_ule x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j; typ= Integer {bits}} ->
      bool (Z.uleq ~bits i j)
  | _ -> App {op= App {op= Ule; arg= x}; arg= y}

let simp_ord x y = App {op= App {op= Ord; arg= x}; arg= y}
let simp_uno x y = App {op= App {op= Uno; arg= x}; arg= y}

let simp_cond cnd thn els =
  match cnd with
  (* ¬(true ? t : e) ==> t *)
  | Integer {data; typ= Integer {bits= 1}} when Z.is_true data -> thn
  (* ¬(false ? t : e) ==> e *)
  | Integer {data; typ= Integer {bits= 1}} when Z.is_false data -> els
  | _ ->
      App {op= App {op= App {op= Conditional; arg= cnd}; arg= thn}; arg= els}

let rec simp_not (typ : Typ.t) exp =
  match (exp, typ) with
  (* ¬(x = y) ==> x != y *)
  | App {op= App {op= Eq; arg= x}; arg= y}, _ -> simp_dq x y
  (* ¬(x != y) ==> x = y *)
  | App {op= App {op= Dq; arg= x}; arg= y}, _ -> simp_eq x y
  (* ¬(x > y) ==> x <= y *)
  | App {op= App {op= Gt; arg= x}; arg= y}, _ -> simp_le x y
  (* ¬(x >= y) ==> x < y *)
  | App {op= App {op= Ge; arg= x}; arg= y}, _ -> simp_lt x y
  (* ¬(x < y) ==> x >= y *)
  | App {op= App {op= Lt; arg= x}; arg= y}, _ -> simp_ge x y
  (* ¬(x <= y) ==> x > y *)
  | App {op= App {op= Le; arg= x}; arg= y}, _ -> simp_gt x y
  (* ¬(x u> y) ==> x u<= y *)
  | App {op= App {op= Ugt; arg= x}; arg= y}, _ -> simp_ule x y
  (* ¬(x u>= y) ==> x u< y *)
  | App {op= App {op= Uge; arg= x}; arg= y}, _ -> simp_ult x y
  (* ¬(x u< y) ==> x u>= y *)
  | App {op= App {op= Ult; arg= x}; arg= y}, _ -> simp_uge x y
  (* ¬(x u<= y) ==> x u> y *)
  | App {op= App {op= Ule; arg= x}; arg= y}, _ -> simp_ugt x y
  (* ¬(x != nan ∧ y != nan) ==> x = nan ∨ y = nan *)
  | App {op= App {op= Ord; arg= x}; arg= y}, _ -> simp_uno x y
  (* ¬(x = nan ∨ y = nan) ==> x != nan ∧ y != nan *)
  | App {op= App {op= Uno; arg= x}; arg= y}, _ -> simp_ord x y
  (* ¬(a ∧ b) ==> ¬a ∨ ¬b *)
  | App {op= App {op= And; arg= x}; arg= y}, Integer {bits= 1} ->
      simp_or (simp_not typ x) (simp_not typ y)
  (* ¬(a ∨ b) ==> ¬a ∧ ¬b *)
  | App {op= App {op= Or; arg= x}; arg= y}, Integer {bits= 1} ->
      simp_and (simp_not typ x) (simp_not typ y)
  (* ¬(c ? t : e) ==> c ? ¬t : ¬e *)
  | ( App {op= App {op= App {op= Conditional; arg= cnd}; arg= thn}; arg= els}
    , Integer {bits= 1} ) ->
      simp_cond cnd (simp_not typ thn) (simp_not typ els)
  (* ¬b ==> false = b *)
  | b, Integer {bits= 1} -> App {op= App {op= Eq; arg= bool false}; arg= b}
  (* ¬e ==> true xor e *)
  | e, _ ->
      App {op= App {op= Xor; arg= integer (Z.of_bool true) typ}; arg= e}

and simp_eq x y =
  match (x, y) with
  (* i = j *)
  | Integer {data= i; typ= Integer {bits}}, Integer {data= j} ->
      bool (Z.eq ~bits i j)
  (* e+i = j ==> e = j-i *)
  | ( App {op= App {op= Add; arg= e}; arg= Integer {data= i}}
    , Integer {data= j; typ= Integer {bits} as typ} ) ->
      simp_eq e (integer (Z.sub ~bits j i) typ)
  (* b = false ==> ¬b *)
  | b, Integer {data; typ= Integer {bits= 1}}
   |Integer {data; typ= Integer {bits= 1}}, b
    when Z.is_false data ->
      simp_not Typ.bool b
  (* b = true ==> b *)
  | b, Integer {data; typ= Integer {bits= 1}}
   |Integer {data; typ= Integer {bits= 1}}, b
    when Z.is_true data ->
      b
  | _ ->
      let c = compare x y in
      (* e = e ==> true *)
      if c = 0 then bool true
      else if c < 0 then App {op= App {op= Eq; arg= y}; arg= x}
      else App {op= App {op= Eq; arg= x}; arg= y}

and simp_dq x y =
  match (x, y) with
  (* i != j *)
  | Integer {data= i; typ= Integer {bits}}, Integer {data= j} ->
      bool (not (Z.eq ~bits i j))
  (* e+i != j ==> e != j-i *)
  | ( App {op= App {op= Add; arg= e}; arg= Integer {data= i}}
    , Integer {data= j; typ= Integer {bits} as typ} ) ->
      simp_dq e (integer (Z.sub ~bits j i) typ)
  (* b != false ==> b *)
  | b, Integer {data; typ= Integer {bits= 1}}
   |Integer {data; typ= Integer {bits= 1}}, b
    when Z.is_false data ->
      b
  (* b != true ==> ¬b *)
  | b, Integer {data; typ= Integer {bits= 1}}
   |Integer {data; typ= Integer {bits= 1}}, b
    when Z.is_true data ->
      simp_not Typ.bool b
  | _ ->
      let c = compare x y in
      (* e = e ==> false *)
      if c = 0 then bool false
      else if c < 0 then App {op= App {op= Dq; arg= x}; arg= y}
      else App {op= App {op= Dq; arg= y}; arg= x}

and simp_and x y =
  match (x, y) with
  (* i && j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}} ->
      integer (Z.logand ~bits i j) typ
  (* e && true ==> e *)
  | Integer {data; typ= Integer {bits= 1}}, e
   |e, Integer {data; typ= Integer {bits= 1}}
    when Z.is_true data ->
      e
  (* e && false ==> 0 *)
  | (Integer {data; typ= Integer {bits= 1}} as f), _
   |_, (Integer {data; typ= Integer {bits= 1}} as f)
    when Z.is_false data ->
      f
  | _ ->
      let c = compare x y in
      (* e && e ==> e *)
      if c = 0 then x
      else if c < 0 then App {op= App {op= And; arg= x}; arg= y}
      else App {op= App {op= And; arg= y}; arg= x}

and simp_or x y =
  match (x, y) with
  (* i || j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}} ->
      integer (Z.logor ~bits i j) typ
  (* e || true ==> true *)
  | (Integer {data; typ= Integer {bits= 1}} as t), _
   |_, (Integer {data; typ= Integer {bits= 1}} as t)
    when Z.is_true data ->
      t
  (* e || false ==> e *)
  | Integer {data; typ= Integer {bits= 1}}, e
   |e, Integer {data; typ= Integer {bits= 1}}
    when Z.is_false data ->
      e
  | _ ->
      let c = compare x y in
      (* e || e ==> e *)
      if c = 0 then x
      else if c < 0 then App {op= App {op= Or; arg= x}; arg= y}
      else App {op= App {op= Or; arg= y}; arg= x}

let simp_xor x y =
  match (x, y) with
  (* i xor j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}} ->
      integer (Z.logxor ~bits i j) typ
  (* true xor b ==> ¬b *)
  | Integer {data; typ= Integer {bits= 1}}, b
   |b, Integer {data; typ= Integer {bits= 1}}
    when Z.is_true data ->
      simp_not Typ.bool b
  | _ ->
      let c = compare x y in
      if c <= 0 then App {op= App {op= Xor; arg= x}; arg= y}
      else App {op= App {op= Xor; arg= y}; arg= x}

let simp_shl x y =
  match (x, y) with
  (* i shl j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}}
    when Z.fits_int j ->
      integer (Z.shift_left ~bits i (Z.to_int j)) typ
  (* e shl 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> App {op= App {op= Shl; arg= x}; arg= y}

let simp_lshr x y =
  match (x, y) with
  (* i lshr j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}}
    when Z.fits_int j ->
      integer (Z.shift_right_trunc ~bits i (Z.to_int j)) typ
  (* e lshr 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> App {op= App {op= Lshr; arg= x}; arg= y}

let simp_ashr x y =
  match (x, y) with
  (* i ashr j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}}
    when Z.fits_int j ->
      integer (Z.shift_right ~bits i (Z.to_int j)) typ
  (* e ashr 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> App {op= App {op= Ashr; arg= x}; arg= y}

let rec simp_add x y =
  match (x, y) with
  (* i + j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}} ->
      integer (Z.add ~bits i j) typ
  (* i + e ==> e + i *)
  | Integer _, _ -> simp_add y x
  (* e + 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  (* (e+i) + j ==> e+(i+j) *)
  | ( App
        { op= App {op= Add; arg}
        ; arg= Integer {data= i; typ= Integer {bits} as typ} }
    , Integer {data= j} ) ->
      simp_add arg (integer (Z.add ~bits i j) typ)
  (* (i-e) + j ==> (i+j)-e *)
  | ( App
        { op=
            App {op= Sub; arg= Integer {data= i; typ= Integer {bits} as typ}}
        ; arg }
    , Integer {data= j} ) ->
      simp_sub (integer (Z.add ~bits i j) typ) arg
  | _ -> App {op= App {op= Add; arg= x}; arg= y}

and simp_sub x y =
  match (x, y) with
  (* i - j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}} ->
      integer (Z.sub ~bits i j) typ
  (* e - i ==> e + (-i) *)
  | _, Integer {data; typ= Integer {bits} as typ} ->
      simp_add x (integer (Z.neg ~bits data) typ)
  | _ -> App {op= App {op= Sub; arg= x}; arg= y}

let simp_mul x y =
  match (x, y) with
  (* i * j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}} ->
      integer (Z.mul ~bits i j) typ
  (* e * 1 ==> e *)
  | Integer {data}, e when Z.equal Z.one data -> e
  | e, Integer {data} when Z.equal Z.one data -> e
  | _ -> App {op= App {op= Mul; arg= x}; arg= y}

let simp_div x y =
  match (x, y) with
  (* i / j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}} ->
      integer (Z.div ~bits i j) typ
  (* e / 1 ==> e *)
  | Integer {data}, e when Z.equal Z.one data -> e
  | _ -> App {op= App {op= Div; arg= x}; arg= y}

let simp_udiv x y =
  match (x, y) with
  (* i u/ j *)
  | Integer {data= i; typ= Integer {bits} as typ}, Integer {data= j} ->
      integer (Z.udiv ~bits i j) typ
  (* e u/ 1 ==> e *)
  | Integer {data}, e when Z.equal Z.one data -> e
  | _ -> App {op= App {op= Udiv; arg= x}; arg= y}

let simp_rem x y =
  match (x, y) with
  (* i % j *)
  | Integer {data= i; typ}, Integer {data= j; typ= Integer {bits}} ->
      integer (Z.rem ~bits i j) typ
  (* e % 1 ==> 0 *)
  | _, Integer {data; typ} when Z.equal Z.one data -> integer Z.zero typ
  | _ -> App {op= App {op= Rem; arg= x}; arg= y}

let simp_urem x y =
  match (x, y) with
  (* i u% j *)
  | Integer {data= i; typ= Integer {bits} as typ}, Integer {data= j} ->
      integer (Z.urem ~bits i j) typ
  (* e u% 1 ==> 0 *)
  | _, Integer {data; typ} when Z.equal Z.one data -> integer Z.zero typ
  | _ -> App {op= App {op= Urem; arg= x}; arg= y}

let app1 ?(partial = false) op arg =
  ( match (op, arg) with
  | App {op= Eq; arg= x}, y -> simp_eq x y
  | App {op= Dq; arg= x}, y -> simp_dq x y
  | App {op= Gt; arg= x}, y -> simp_gt x y
  | App {op= Ge; arg= x}, y -> simp_ge x y
  | App {op= Lt; arg= x}, y -> simp_lt x y
  | App {op= Le; arg= x}, y -> simp_le x y
  | App {op= Ugt; arg= x}, y -> simp_ugt x y
  | App {op= Uge; arg= x}, y -> simp_uge x y
  | App {op= Ult; arg= x}, y -> simp_ult x y
  | App {op= Ule; arg= x}, y -> simp_ule x y
  | App {op= Ord; arg= x}, y -> simp_ord x y
  | App {op= Uno; arg= x}, y -> simp_uno x y
  | App {op= Add; arg= x}, y -> simp_add x y
  | App {op= Sub; arg= x}, y -> simp_sub x y
  | App {op= Mul; arg= x}, y -> simp_mul x y
  | App {op= Div; arg= x}, y -> simp_div x y
  | App {op= Udiv; arg= x}, y -> simp_udiv x y
  | App {op= Rem; arg= x}, y -> simp_rem x y
  | App {op= Urem; arg= x}, y -> simp_urem x y
  | App {op= And; arg= x}, y -> simp_and x y
  | App {op= Or; arg= x}, y -> simp_or x y
  | App {op= Xor; arg= x}, y -> simp_xor x y
  | App {op= Shl; arg= x}, y -> simp_shl x y
  | App {op= Lshr; arg= x}, y -> simp_lshr x y
  | App {op= Ashr; arg= x}, y -> simp_ashr x y
  | App {op= App {op= Conditional; arg= x}; arg= y}, z -> simp_cond x y z
  | Convert {signed; dst; src}, x -> simp_convert signed dst src x
  | _ -> App {op; arg} )
  |> check (invariant ~partial)

let app2 op x y = app1 (app1 ~partial:true op x) y
let app3 op x y z = app1 (app1 ~partial:true (app1 ~partial:true op x) y) z
let appN op xs = List.fold xs ~init:op ~f:app1
let splat ~byt ~siz = app2 Splat byt siz
let memory ~siz ~arr = app2 Memory siz arr
let concat = app2 Concat
let eq = app2 Eq
let dq = app2 Dq
let gt = app2 Gt
let ge = app2 Ge
let lt = app2 Lt
let le = app2 Le
let ugt = app2 Ugt
let uge = app2 Uge
let ult = app2 Ult
let ule = app2 Ule
let ord = app2 Ord
let uno = app2 Uno
let add = app2 Add
let sub = app2 Sub
let mul = app2 Mul
let div = app2 Div
let udiv = app2 Udiv
let rem = app2 Rem
let urem = app2 Urem
let and_ = app2 And
let or_ = app2 Or
let xor = app2 Xor
let not_ = simp_not
let shl = app2 Shl
let lshr = app2 Lshr
let ashr = app2 Ashr
let conditional ~cnd ~thn ~els = app3 Conditional cnd thn els
let record elts = appN Record elts |> check invariant
let select ~rcd ~idx = app2 Select rcd idx
let update ~rcd ~elt ~idx = app3 Update rcd elt idx

let struct_rec key =
  let memo_id = Hashtbl.create key in
  let dummy = Null in
  Staged.stage
  @@ fun ~id elt_thks ->
  match Hashtbl.find memo_id id with
  | None ->
      (* Add placeholder to prevent computing [elts] in calls to
         [struct_rec] from [elt_thks] for recursive occurrences of [id]. *)
      let elta = Array.create ~len:(Vector.length elt_thks) dummy in
      let elts = Vector.of_array elta in
      Hashtbl.set memo_id ~key:id ~data:elts ;
      Vector.iteri elt_thks ~f:(fun i (lazy elt) -> elta.(i) <- elt) ;
      Struct_rec {elts} |> check invariant
  | Some elts ->
      (* Do not check invariant as invariant will be checked above after the
         thunks are forced, before which invariant-checking may spuriously
         fail. Note that it is important that the value constructed here
         shares the array in the memo table, so that the update after
         forcing the recursive thunks also updates this value. *)
      Struct_rec {elts}

let convert ?(signed = false) ~dst ~src exp =
  app1 (Convert {signed; dst; src}) exp

(** Access *)

let fold e ~init:z ~f =
  match e with
  | App {op; arg; _} ->
      let z = f z op in
      let z = f z arg in
      z
  | _ -> z

let fold_map e ~init:z ~f =
  match e with
  | App {op; arg} ->
      let z, op' = f z op in
      let z, arg' = f z arg in
      if op' == op && arg' == arg then (z, e)
      else (z, app1 ~partial:true op' arg')
  | _ -> (z, e)

let map e ~f =
  match e with
  | App {op; arg} ->
      let op' = f op in
      let arg' = f arg in
      if op' == op && arg' == arg then e else app1 ~partial:true op' arg'
  | _ -> e

(** Update *)

let rename e sub =
  let rec rename_ e sub =
    match e with
    | Var _ -> Var.Subst.apply sub e
    | _ -> map e ~f:(fun f -> rename_ f sub)
  in
  rename_ e sub |> check (invariant ~partial:true)

(** Query *)

let is_true = function
  | Integer {data; typ= Integer {bits= 1}} -> Z.is_true data
  | _ -> false

let is_false = function
  | Integer {data; typ= Integer {bits= 1}} -> Z.is_false data
  | _ -> false

let rec is_constant = function
  | Var _ | Nondet _ -> false
  | App {op; arg} -> is_constant arg && is_constant op
  | _ -> true
