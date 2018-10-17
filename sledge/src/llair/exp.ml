(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Expressions *)

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
    | Integer of {data: Z.t}
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
      | Integer {data} -> pf "%a" Z.pp_print data
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
      | App {op= App {op= Xor; arg}; arg= Integer {data}}
        when Z.equal Z.minus_one data ->
          pf "¬%a" pp arg
      | App {op= App {op= Xor; arg= Integer {data}}; arg}
        when Z.equal Z.minus_one data ->
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
  | Var _ | Nondet _ | Label _ | Null | Integer _ | Float _ ->
      assert_arity 0
  | Convert {dst; src} ->
      assert (Typ.convertible src dst) ;
      assert_arity 1
  | Splat | Memory | Concat | Eq | Dq | Gt | Ge | Lt | Le | Ugt | Uge
   |Ult | Ule | Ord | Uno | Add | Sub | Mul | Div | Udiv | Rem | Urem
   |And | Or | Xor | Shl | Lshr | Ashr | Select ->
      assert_arity 2
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
let integer data = Integer {data} |> check invariant
let bool b = integer (Z.of_int (Bool.to_int b))
let float data = Float {data} |> check invariant

let simp_convert signed (dst : Typ.t) (src : Typ.t) arg =
  match (signed, dst, src, arg) with
  | _, Integer {bits}, _, Integer {data} when Z.numbits data <= bits ->
      integer data
  | false, Integer {bits= m}, Integer {bits= n}, _ when m >= n -> arg
  | _ -> App {op= Convert {signed; dst; src}; arg}

let rec simp_eq x y =
  match (x, y) with
  (* i = j ==> i=j *)
  | Integer {data= i}, Integer {data= j} -> bool (Z.equal i j)
  (* e+i = j ==> e = j-i *)
  | ( App {op= App {op= Add; arg= e}; arg= Integer {data= i}}
    , Integer {data= j} ) ->
      simp_eq e (integer (Z.sub j i))
  (* e = e ==> 1 *)
  | _ when equal x y -> bool true
  | _ -> App {op= App {op= Eq; arg= x}; arg= y}

let simp_dq x y =
  match (x, y) with
  (* i != j ==> i!=j *)
  | Integer {data= i}, Integer {data= j} -> bool (not (Z.equal i j))
  (* e = e ==> 0 *)
  | _ when equal x y -> bool false
  | _ -> App {op= App {op= Dq; arg= x}; arg= y}

let simp_gt x y =
  match (x, y) with
  (* i > j ==> i>j *)
  | Integer {data= i}, Integer {data= j} -> bool (Z.gt i j)
  | _ -> App {op= App {op= Gt; arg= x}; arg= y}

let simp_ge x y =
  match (x, y) with
  (* i >= j ==> i>=j *)
  | Integer {data= i}, Integer {data= j} -> bool (Z.geq i j)
  | _ -> App {op= App {op= Ge; arg= x}; arg= y}

let simp_lt x y =
  match (x, y) with
  (* i < j ==> i<j *)
  | Integer {data= i}, Integer {data= j} -> bool (Z.lt i j)
  | _ -> App {op= App {op= Lt; arg= x}; arg= y}

let simp_le x y =
  match (x, y) with
  (* i <= j ==> i<=j *)
  | Integer {data= i}, Integer {data= j} -> bool (Z.leq i j)
  | _ -> App {op= App {op= Le; arg= x}; arg= y}

let rec simp_add x y =
  match (x, y) with
  (* i + j ==> i+j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.add i j)
  (* i + e ==> e + i *)
  | Integer _, _ -> simp_add y x
  (* e + 0 ==> e *)
  | _, Integer {data} when Z.equal Z.zero data -> x
  (* (e+i) + j ==> e+(i+j) *)
  | App {op= App {op= Add; arg}; arg= Integer {data= i}}, Integer {data= j}
    ->
      simp_add arg (integer (Z.add i j))
  (* (i-e) + j ==> (i+j)-e *)
  | App {op= App {op= Sub; arg= Integer {data= i}}; arg}, Integer {data= j}
    ->
      simp_sub (integer (Z.add i j)) arg
  | _ -> App {op= App {op= Add; arg= x}; arg= y}

and simp_sub x y =
  match (x, y) with
  (* i - j ==> i-j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.sub i j)
  (* e - i ==> e + (-i) *)
  | _, Integer {data} -> simp_add x (integer (Z.neg data))
  (* e - e ==> 0 *)
  | _ when equal x y -> integer Z.zero
  | _ -> App {op= App {op= Sub; arg= x}; arg= y}

let simp_mul x y =
  match (x, y) with
  (* i * j ==> i*j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.mul i j)
  (* e * 1 ==> e *)
  | (Integer {data}, e | e, Integer {data}) when Z.equal Z.one data -> e
  | _ -> App {op= App {op= Mul; arg= x}; arg= y}

let simp_div x y =
  match (x, y) with
  (* i / j ==> i/j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.div i j)
  | _ -> App {op= App {op= Div; arg= x}; arg= y}

let simp_rem x y =
  match (x, y) with
  (* i % j ==> i%j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.( mod ) i j)
  | _ -> App {op= App {op= Rem; arg= x}; arg= y}

let simp_and x y =
  match (x, y) with
  (* i && j ==> i logand j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.logand i j)
  (* e && 1 ==> e *)
  | (Integer {data}, e | e, Integer {data}) when Z.equal Z.one data -> e
  (* e && 0 ==> 0 *)
  | ((Integer {data} as z), _ | _, (Integer {data} as z))
    when Z.equal Z.zero data ->
      z
  | _ -> App {op= App {op= And; arg= x}; arg= y}

let simp_or x y =
  match (x, y) with
  (* i || j ==> i logor j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.logor i j)
  (* e || 1 ==> e *)
  | (Integer {data}, _ | _, Integer {data}) when Z.equal Z.one data ->
      integer Z.one
  (* e || 0 ==> e *)
  | (Integer {data}, e | e, Integer {data}) when Z.equal Z.zero data -> e
  | _ -> App {op= App {op= Or; arg= x}; arg= y}

let simp_xor x y =
  match (x, y) with
  (* i xor j ==> i logxor j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.logxor i j)
  (* ¬(x=y) ==> x!=y *)
  | App {op= App {op= Eq; arg= x}; arg= y}, Integer {data}
   |Integer {data}, App {op= App {op= Eq; arg= x}; arg= y}
    when Z.equal Z.minus_one data ->
      simp_dq x y
  (* ¬(x!=y) ==> x=y *)
  | App {op= App {op= Dq; arg= x}; arg= y}, Integer {data}
   |Integer {data}, App {op= App {op= Dq; arg= x}; arg= y}
    when Z.equal Z.minus_one data ->
      simp_eq x y
  | _ -> App {op= App {op= Xor; arg= x}; arg= y}

let app1 ?(partial = false) op arg =
  ( match (op, arg) with
  | Convert {signed; dst; src}, x -> simp_convert signed dst src x
  | App {op= Eq; arg= x}, y -> simp_eq x y
  | App {op= Dq; arg= x}, y -> simp_dq x y
  | App {op= Gt; arg= x}, y -> simp_gt x y
  | App {op= Ge; arg= x}, y -> simp_ge x y
  | App {op= Lt; arg= x}, y -> simp_lt x y
  | App {op= Le; arg= x}, y -> simp_le x y
  | App {op= Add; arg= x}, y -> simp_add x y
  | App {op= Sub; arg= x}, y -> simp_sub x y
  | App {op= Mul; arg= x}, y -> simp_mul x y
  | App {op= Div; arg= x}, y -> simp_div x y
  | App {op= Rem; arg= x}, y -> simp_rem x y
  | App {op= And; arg= x}, y -> simp_and x y
  | App {op= Or; arg= x}, y -> simp_or x y
  | App {op= Xor; arg= x}, y -> simp_xor x y
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

let is_true = function Integer {data} -> Z.equal Z.one data | _ -> false
let is_false = function Integer {data} -> Z.equal Z.zero data | _ -> false

let rec is_constant = function
  | Var _ | Nondet _ -> false
  | App {op; arg} -> is_constant arg && is_constant op
  | _ -> true
