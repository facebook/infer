(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Expressions *)

type t =
  | Var of {name: string; typ: Typ.t; loc: Loc.t}
  | Global of {name: string; init: t option; typ: Typ.t; loc: Loc.t}
  | Nondet of {typ: Typ.t; loc: Loc.t; msg: string}
  | Label of {parent: string; name: string; loc: Loc.t}
  | Null of {typ: Typ.t}
  | App of {op: t; arg: t; loc: Loc.t}
  | AppN of {op: t; args: t vector; loc: Loc.t}
  (* NOTE: may be cyclic *)
  | PtrFld of {fld: int}
  | PtrIdx
  | PrjFld of {fld: int}
  | PrjIdx
  | UpdFld of {fld: int}
  | UpdIdx
  | Integer of {data: Z.t; typ: Typ.t}
  | Float of {data: string; typ: Typ.t}
  | Array of {typ: Typ.t}
  | Struct of {typ: Typ.t}
  | Cast of {typ: Typ.t}
  | Conv of {signed: bool; typ: Typ.t}
  | Select
  (* binary: comparison *)
  | Eq
  | Ne
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
  (* binary: boolean / bitwise *)
  | And
  | Or
  | Xor
  | Shl
  | LShr
  | AShr
  (* binary: arithmetic *)
  | Add
  | Sub
  | Mul
  | Div
  | UDiv
  | Rem
  | URem
[@@deriving compare, sexp]

let equal = [%compare.equal : t]

let uncurry exp =
  let rec uncurry_ args op =
    match op with
    | App {op; arg} -> uncurry_ (arg :: args) op
    | AppN {op; args} -> (op, Vector.to_list args)
    | _ -> (op, args)
  in
  uncurry_ [] exp

let rec fmt ff exp =
  let pf fmt =
    Format.pp_open_box ff 2 ;
    Format.kfprintf (fun ff -> Format.pp_close_box ff ()) ff fmt
  in
  match[@warning "p"] uncurry exp with
  | Var {name}, [] -> pf "%%%s" name
  | Global {name}, [] ->
      pf "@%s%t" name (fun ff ->
          let demangled = Llvm.demangle name in
          if not (String.is_empty demangled || String.equal name demangled)
          then Format.fprintf ff "“%s”" demangled )
  | Nondet {msg}, [] -> pf "nondet \"%s\"" msg
  | Label {name}, [] -> pf "%s" name
  | Null _, [] -> pf "null"
  | Integer {data}, [] -> pf "%a" Z.pp_print data
  | Float {data}, [] -> pf "%s" data
  | PtrFld {fld}, [ptr] -> pf "%a ⊕ %i" fmt ptr fld
  | PtrIdx, [arr; idx] -> pf "%a ⊕ %a" fmt arr fmt idx
  | PrjFld {fld}, [ptr] -> pf "%a[%i]" fmt ptr fld
  | PrjIdx, [arr; idx] -> pf "%a[%a]" fmt arr fmt idx
  | UpdFld {fld}, [agg; elt] ->
      pf "{%a@ @[| %i → %a@]}" fmt agg fld fmt elt
  | UpdIdx, [agg; elt; idx] -> pf "[%a | %a → %a]" fmt agg fmt idx fmt elt
  | Array _, elts -> pf "[%a]" (list_fmt ",@ " fmt) elts
  | Struct _, elts -> pf "{%a}" (list_fmt ",@ " fmt) elts
  | Cast {typ}, [arg] -> pf "(@[(%a)@ %a@])" Typ.fmt typ fmt arg
  | Conv {typ}, [arg] -> pf "(@[%c%a>@ %a@])" '<' Typ.fmt typ fmt arg
  | Select, [cnd; thn; els] -> pf "(%a@ ? %a@ : %a)" fmt cnd fmt thn fmt els
  | Eq, [x; y] -> pf "(%a@ = %a)" fmt x fmt y
  | Ne, [x; y] -> pf "(%a@ != %a)" fmt x fmt y
  | Gt, [x; y] -> pf "(%a@ > %a)" fmt x fmt y
  | Ge, [x; y] -> pf "(%a@ >= %a)" fmt x fmt y
  | Lt, [x; y] -> pf "(%a@ < %a)" fmt x fmt y
  | Le, [x; y] -> pf "(%a@ <= %a)" fmt x fmt y
  | Ugt, [x; y] -> pf "(%a@ u> %a)" fmt x fmt y
  | Uge, [x; y] -> pf "(%a@ u>= %a)" fmt x fmt y
  | Ult, [x; y] -> pf "(%a@ u< %a)" fmt x fmt y
  | Ule, [x; y] -> pf "(%a@ u<= %a)" fmt x fmt y
  | Ord, [x; y] -> pf "(%a@ ord %a)" fmt x fmt y
  | Uno, [x; y] -> pf "(%a@ uno %a)" fmt x fmt y
  | And, [x; y] -> pf "(%a@ && %a)" fmt x fmt y
  | Or, [x; y] -> pf "(%a@ || %a)" fmt x fmt y
  | Xor, [x; y] -> pf "(%a@ ^ %a)" fmt x fmt y
  | Shl, [x; y] -> pf "(%a@ << %a)" fmt x fmt y
  | LShr, [x; y] -> pf "(%a@ >> %a)" fmt x fmt y
  | AShr, [x; y] -> pf "(%a@ >>a %a)" fmt x fmt y
  | Add, [x; y] -> pf "(%a@ + %a)" fmt x fmt y
  | Sub, [x; y] -> pf "(%a@ - %a)" fmt x fmt y
  | Mul, [x; y] -> pf "(%a@ * %a)" fmt x fmt y
  | Div, [x; y] -> pf "(%a@ / %a)" fmt x fmt y
  | UDiv, [x; y] -> pf "(%a@ u/ %a)" fmt x fmt y
  | Rem, [x; y] -> pf "(%a@ %% %a)" fmt x fmt y
  | URem, [x; y] -> pf "(%a@ u%% %a)" fmt x fmt y

(** Queries *)

let rec typ_of : t -> Typ.t = function[@warning "p"]
  | Var {typ}
   |Global {typ}
   |Nondet {typ}
   |Null {typ}
   |Integer {typ}
   |Float {typ}
   |Array {typ}
   |Struct {typ}
   |App {op= Cast {typ} | Conv {typ}} ->
      typ
  | Label _ -> Typ.i8p
  | App {op= PtrFld {fld}; arg} -> (
    match[@warning "p"] typ_of arg
    with Pointer {elt= Tuple {elts} | Struct {elts}} ->
      Typ.mkPointer ~elt:(Vector.get elts fld) )
  | App {op= App {op= PtrIdx; arg}} -> (
    match[@warning "p"] typ_of arg with Pointer {elt= Array {elt}} ->
      Typ.mkPointer ~elt )
  | App {op= PrjFld {fld}; arg} -> (
    match[@warning "p"] typ_of arg with
    | Tuple {elts} | Struct {elts} -> Vector.get elts fld )
  | App {op= App {op= PrjIdx; arg}} -> (
    match[@warning "p"] typ_of arg with Array {elt} -> elt )
  | App {op= App {op= UpdFld _; arg}}
   |App {op= App {op= App {op= UpdIdx; arg}}} ->
      typ_of arg
  | App
      { op=
          App
            { op=
                ( Eq | Ne | Gt | Ge | Lt | Le | Ugt | Uge | Ult | Ule | Ord
                | Uno ) } } ->
      Typ.i1
  | App
      { op=
          App
            { op=
                ( And | Or | Xor | Shl | LShr | AShr | Add | Sub | Mul | Div
                | UDiv | Rem | URem )
            ; arg } }
   |App {op= App {op= App {op= Select}}; arg} ->
      typ_of arg
  | AppN {op} -> typ_of op

let valid_fld fld elts = 0 <= fld && fld < Vector.length elts

(** Re-exported modules *)

(* Variables are the expressions constructed by [Var] *)
module Var = struct
  module T = struct
    type nonrec t = t [@@deriving compare, sexp]

    let equal = equal
  end

  include T
  include Comparator.Make (T)

  let fmt = fmt

  let mk ?(loc= Loc.none) name typ = Var {name; typ; loc}

  let name = function[@warning "p"] Var {name} -> name

  let typ = function[@warning "p"] Var {typ} -> typ

  let loc = function[@warning "p"] Var {loc} -> loc
end

(* Globals are the expressions constructed by [Global] *)
module Global = struct
  type init = t

  module T = struct
    type nonrec t = t [@@deriving compare, sexp]

    let equal = equal

    let hash = Hashtbl.hash
  end

  include T
  include Comparator.Make (T)

  let fmt_defn ff g =
    let[@warning "p"] Global {init; typ} = g in
    let[@warning "p"] Typ.Pointer {elt= typ} = typ in
    Format.fprintf ff "@[<2>%a %a%a@]" Typ.fmt typ fmt g
      (option_fmt " =@ @[%a@]" fmt)
      init

  let fmt = fmt

  let mk ?init ?(loc= Loc.none) name typ =
    assert (
      Option.for_all init ~f:(fun exp ->
          Typ.equal typ (Typ.mkPointer ~elt:(typ_of exp)) ) ) ;
    Global {name; init; typ; loc}

  let of_exp e = match e with Global _ -> Some e | _ -> None

  let name = function[@warning "p"] Global {name} -> name

  let typ = function[@warning "p"] Global {typ} -> typ

  let loc = function[@warning "p"] Global {loc} -> loc
end

(** Constructors *)

let locate loc exp =
  match exp with
  | Var {name; typ} -> Var {name; typ; loc}
  | Global {name; init; typ} -> Global {name; init; typ; loc}
  | Nondet {typ; msg} -> Nondet {typ; loc; msg}
  | Label {parent; name} -> Label {parent; name; loc}
  | App {op; arg} -> App {op; arg; loc}
  | AppN {op; args} -> AppN {op; args; loc}
  | _ -> exp

let mkApp1 op arg = App {op; arg; loc= Loc.none}

let mkApp2 op x y = mkApp1 (mkApp1 op x) y

let mkApp3 op x y z = mkApp1 (mkApp1 (mkApp1 op x) y) z

let mkAppN op args = AppN {op; args; loc= Loc.none}

let mkVar = Fn.id

let mkGlobal = Fn.id

let mkNondet (typ: Typ.t) msg =
  assert (match typ with Function _ -> false | _ -> true) ;
  Nondet {typ; loc= Loc.none; msg}

let mkLabel ~parent ~name = Label {parent; name; loc= Loc.none}

let mkNull (typ: Typ.t) =
  assert (match typ with Opaque _ | Function _ -> false | _ -> true) ;
  Null {typ}

let mkPtrFld ~ptr ~fld =
  assert (
    match typ_of ptr with
    | Pointer {elt= Tuple {elts} | Struct {elts}} -> valid_fld fld elts
    | _ -> false ) ;
  mkApp1 (PtrFld {fld}) ptr

let mkPtrIdx ~ptr ~idx =
  assert (
    match (typ_of ptr, typ_of idx) with
    | Pointer {elt= Array _}, Integer _ -> true
    | _ -> false ) ;
  mkApp2 PtrIdx ptr idx

let mkPrjFld ~agg ~fld =
  assert (
    match typ_of agg with
    | Tuple {elts} | Struct {elts} -> valid_fld fld elts
    | _ -> false ) ;
  mkApp1 (PrjFld {fld}) agg

let mkPrjIdx ~arr ~idx =
  assert (
    match (typ_of arr, typ_of idx) with
    | Array _, Integer _ -> true
    | _ -> false ) ;
  mkApp2 PrjIdx arr idx

let mkUpdFld ~agg ~elt ~fld =
  assert (
    match typ_of agg with
    | Tuple {elts} | Struct {elts} ->
        valid_fld fld elts && Typ.equal (Vector.get elts fld) (typ_of elt)
    | _ -> false ) ;
  mkApp2 (UpdFld {fld}) agg elt

let mkUpdIdx ~arr ~elt ~idx =
  assert (
    match (typ_of arr, typ_of idx) with
    | Array {elt= typ}, Integer _ -> Typ.equal typ (typ_of elt)
    | _ -> false ) ;
  mkApp3 UpdIdx arr elt idx

let mkInteger data (typ: Typ.t) =
  assert (
    let in_range num bits =
      let lb = Z.(-(if bits = 1 then ~$1 else ~$1 lsl Int.(bits - 1)))
      and ub = Z.(~$1 lsl bits) in
      Z.(leq lb num && lt num ub)
    in
    match typ with Integer {bits} -> in_range data bits | _ -> false ) ;
  Integer {data; typ}

let mkBool b = mkInteger (Z.of_int (Bool.to_int b)) Typ.i1

let mkFloat data (typ: Typ.t) =
  assert (match typ with Float _ -> true | _ -> false) ;
  Float {data; typ}

let mkArray elts (typ: Typ.t) =
  assert (
    match typ with
    | Array {elt= elt_typ; len} ->
        Vector.for_all elts ~f:(fun elt -> Typ.equal (typ_of elt) elt_typ)
        && Vector.length elts = len
    | _ -> false ) ;
  mkAppN (Array {typ}) elts

let mkStruct elts (typ: Typ.t) =
  assert (
    match typ with
    | Tuple {elts= elt_typs} | Struct {elts= elt_typs} ->
        Vector.for_all2_exn elts elt_typs ~f:(fun elt elt_typ ->
            Typ.equal (typ_of elt) elt_typ )
    | _ -> false ) ;
  mkAppN (Struct {typ}) elts

let mkStruct_rec key =
  let memo_id = Hashtbl.create key () in
  let dummy = Null {typ= Typ.mkBytes} in
  let mkStruct_ ~id elt_thks typ =
    match Hashtbl.find memo_id id with
    | None ->
        (* Add placeholder to prevent computing [elts] in calls to
           [mkStruct_rec] from [elts] for recursive occurrences of [id]. *)
        let elta = Array.create ~len:(Vector.length elt_thks) dummy in
        let elts = Vector.of_array elta in
        Hashtbl.set memo_id ~key:id ~data:elts ;
        Vector.iteri elt_thks ~f:(fun i elt_thk ->
            elta.(i) <- Lazy.force elt_thk ) ;
        mkStruct elts typ
    | Some elts ->
        (* Do not call [mkStruct] as types will be checked by the call to
           [mkStruct] above after the thunks are forced, before which
           type-checking may spuriously fail. Note that it is important that
           the value constructed here shares the array in the memo table, so
           that the update after forcing the recursive thunks also updates
           this value. *)
        mkAppN (Struct {typ}) elts
  in
  Staged.stage mkStruct_

let mkCast exp typ =
  assert (Typ.compatible (typ_of exp) typ) ;
  mkApp1 (Cast {typ}) exp

let mkConv exp ?(signed= false) typ =
  assert (Typ.compatible (typ_of exp) typ) ;
  mkApp1 (Conv {signed; typ}) exp

let mkSelect ~cnd ~thn ~els =
  assert (
    match (typ_of cnd, typ_of thn, typ_of els) with
    | Integer {bits= 1}, s, t -> Typ.equal s t
    | Array {elt= Integer {bits= 1}; len= m}, (Array {len= n} as s), t ->
        m = n && Typ.equal s t
    | _ -> false ) ;
  mkApp3 Select cnd thn els

let binop op x y =
  assertf
    (let typ = typ_of x in
     match (op, typ) with
     | ( (Eq | Ne | Gt | Ge | Lt | Le | Ugt | Uge | Ult | Ule)
       , (Integer _ | Float _ | Pointer _) )
      |(Add | Sub | Mul | Div | Rem), (Integer _ | Float _)
      |(And | Or | Xor | Shl | LShr | AShr | UDiv | URem), Integer _
      |(Ord | Uno), Float _ ->
         Typ.equal typ (typ_of y)
     | _ -> false)
    "ill-typed: %a" fmt (mkApp2 op x y) () ;
  mkApp2 op x y

let mkEq = binop Eq

let mkNe = binop Ne

let mkGt = binop Gt

let mkGe = binop Ge

let mkLt = binop Lt

let mkLe = binop Le

let mkUgt = binop Ugt

let mkUge = binop Uge

let mkUlt = binop Ult

let mkUle = binop Ule

let mkAnd = binop And

let mkOr = binop Or

let mkXor = binop Xor

let mkShl = binop Shl

let mkLShr = binop LShr

let mkAShr = binop AShr

let mkAdd = binop Add

let mkSub = binop Sub

let mkMul = binop Mul

let mkDiv = binop Div

let mkUDiv = binop UDiv

let mkRem = binop Rem

let mkURem = binop URem

let mkOrd = binop Ord

let mkUno = binop Uno

(** Queries *)

let typ = typ_of
