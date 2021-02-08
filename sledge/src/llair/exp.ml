(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Expressions *)

[@@@warning "+9"]

module T = struct
  type op1 =
    (* conversion *)
    | Signed of {bits: int}
    | Unsigned of {bits: int}
    | Convert of {src: Typ.t}
    (* array/struct operations *)
    | Splat
    | Select of int
  [@@deriving compare, equal, hash, sexp]

  type op2 =
    (* comparison *)
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
    (* arithmetic, numeric and pointer *)
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Udiv
    | Urem
    (* boolean / bitwise *)
    | And
    | Or
    | Xor
    | Shl
    | Lshr
    | Ashr
    (* array/struct operations *)
    | Update of int
  [@@deriving compare, equal, hash, sexp]

  type op3 = (* if-then-else *)
    | Conditional
  [@@deriving compare, equal, hash, sexp]

  type opN = (* array/struct constants *)
    | Record
  [@@deriving compare, equal, hash, sexp]

  type t =
    | Reg of {id: int; name: string; typ: Typ.t}
    | Global of {name: string; typ: Typ.t [@ignore]}
    | Function of {name: string; typ: Typ.t [@ignore]}
    | Label of {parent: string; name: string}
    | Integer of {data: Z.t; typ: Typ.t}
    | Float of {data: string; typ: Typ.t}
    | Ap1 of op1 * Typ.t * t
    | Ap2 of op2 * Typ.t * t * t
    | Ap3 of op3 * Typ.t * t * t * t
    | ApN of opN * Typ.t * t iarray
  [@@deriving compare, equal, hash, sexp]

  let demangle = ref (fun _ -> None)

  let pp_demangled ppf name =
    match !demangle name with
    | Some demangled when not (String.equal name demangled) ->
        Format.fprintf ppf "“%s”" demangled
    | _ -> ()

  let pp_op2 fs op =
    let pf fmt = Format.fprintf fs fmt in
    match op with
    | Eq -> pf "="
    | Dq -> pf "@<1>≠"
    | Gt -> pf ">"
    | Ge -> pf "@<1>≥"
    | Lt -> pf "<"
    | Le -> pf "@<1>≤"
    | Ugt -> pf "u>"
    | Uge -> pf "@<2>u≥"
    | Ult -> pf "u<"
    | Ule -> pf "@<2>u≤"
    | Ord -> pf "ord"
    | Uno -> pf "uno"
    | Add -> pf "+"
    | Sub -> pf "-"
    | Mul -> pf "@<1>×"
    | Div -> pf "/"
    | Udiv -> pf "udiv"
    | Rem -> pf "rem"
    | Urem -> pf "urem"
    | And -> pf "&&"
    | Or -> pf "||"
    | Xor -> pf "xor"
    | Shl -> pf "shl"
    | Lshr -> pf "lshr"
    | Ashr -> pf "ashr"
    | Update idx -> pf "[_|%i→_]" idx

  let rec pp fs exp =
    let pf fmt =
      Format.pp_open_box fs 2 ;
      Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
    in
    match exp with
    | Reg {name; id} -> pf "%%%s!%i" name id
    | Global {name} -> pf "%@%s%a" name pp_demangled name
    | Function {name} -> pf "&%s%a" name pp_demangled name
    | Label {name} -> pf "%s" name
    | Integer {data; typ= Pointer _} when Z.equal Z.zero data -> pf "null"
    | Integer {data} -> Trace.pp_styled `Magenta "%a" fs Z.pp data
    | Float {data} -> pf "%s" data
    | Ap1 (Signed {bits}, dst, arg) ->
        pf "((%a)(s%i)@ %a)" Typ.pp dst bits pp arg
    | Ap1 (Unsigned {bits}, dst, arg) ->
        pf "((%a)(u%i)@ %a)" Typ.pp dst bits pp arg
    | Ap1 (Convert {src}, dst, arg) ->
        pf "((%a)(%a)@ %a)" Typ.pp dst Typ.pp src pp arg
    | Ap1 (Splat, _, byt) -> pf "%a^" pp byt
    | Ap1 (Select idx, _, rcd) -> pf "%a[%i]" pp rcd idx
    | Ap2 (Update idx, _, rcd, elt) ->
        pf "[%a@ @[| %i → %a@]]" pp rcd idx pp elt
    | Ap2 (Xor, Integer {bits= 1}, Integer {data}, x) when Z.is_true data ->
        pf "¬%a" pp x
    | Ap2 (Xor, Integer {bits= 1}, x, Integer {data}) when Z.is_true data ->
        pf "¬%a" pp x
    | Ap2 (op, _, x, y) -> pf "(%a@ %a %a)" pp x pp_op2 op pp y
    | Ap3 (Conditional, _, cnd, thn, els) ->
        pf "(%a@ ? %a@ : %a)" pp cnd pp thn pp els
    | ApN (Record, _, elts) -> pf "{%a}" pp_record elts
    [@@warning "-9"]

  and pp_record fs elts =
    match
      String.init (IArray.length elts) ~f:(fun i ->
          match IArray.get elts i with
          | Integer {data; typ= Integer {byts= 1; _}} ->
              Char.of_int_exn (Z.to_int data)
          | _ -> raise_notrace (Invalid_argument "not a string") )
    with
    | s -> Format.fprintf fs "@[<h>%s@]" (String.escaped s)
    | exception _ ->
        Format.fprintf fs "@[<hv>%a@]" (IArray.pp ",@ " pp) elts
end

include T

module Set = struct
  include Set.Make (T)
  include Provide_hash (T)
  include Provide_of_sexp (T)
end

module Map = Map.Make (T)
module Tbl = HashTable.Make (T)

(** Invariant *)

let valid_idx idx elts = 0 <= idx && idx < IArray.length elts

let rec invariant exp =
  let@ () = Invariant.invariant [%here] exp [%sexp_of: t] in
  match exp with
  | Reg {typ} | Global {typ} -> assert (Typ.is_sized typ)
  | Function {typ= Pointer {elt= Function _}} -> ()
  | Function _ -> assert false
  | Integer {data; typ} -> (
    match typ with
    | Integer {bits} ->
        (* data in −(2^(bits − 1)) to 2^(bits − 1) − 1 *)
        let n = Z.shift_left Z.one (bits - 1) in
        assert (Z.(Compare.(neg n <= data && data < n)))
    | Pointer _ -> assert (Z.equal Z.zero data)
    | _ -> assert false )
  | Float {typ} -> (
    match typ with Float _ -> assert true | _ -> assert false )
  | Label _ -> assert true
  | Ap1 (Signed {bits}, dst, arg) -> (
    match (dst, typ_of arg) with
    | Integer {bits= dst_bits}, Typ.Integer _ -> assert (bits <= dst_bits)
    | _ -> assert false )
  | Ap1 (Unsigned {bits}, dst, arg) -> (
    match (dst, typ_of arg) with
    | Integer {bits= dst_bits}, Typ.Integer _ -> assert (bits < dst_bits)
    | _ -> assert false )
  | Ap1 (Convert {src= Integer _}, Integer _, _) -> assert false
  | Ap1 (Convert {src}, dst, arg) ->
      assert (Typ.convertible src dst) ;
      assert (Typ.castable src (typ_of arg)) ;
      assert (not (Typ.equal src dst) (* avoid redundant representations *))
  | Ap1 (Select idx, typ, rcd) -> (
      assert (Typ.castable typ (typ_of rcd)) ;
      match typ with
      | Array _ -> assert true
      | Tuple {elts} | Struct {elts} -> assert (valid_idx idx elts)
      | _ -> assert false )
  | Ap1 (Splat, typ, byt) ->
      assert (Typ.convertible Typ.byt (typ_of byt)) ;
      assert (Typ.is_sized typ)
  | Ap2 (Update idx, typ, rcd, elt) -> (
      assert (Typ.castable typ (typ_of rcd)) ;
      match typ with
      | Tuple {elts} | Struct {elts} ->
          assert (valid_idx idx elts) ;
          assert (Typ.castable (snd (IArray.get elts idx)) (typ_of elt))
      | Array {elt= typ_elt} -> assert (Typ.castable typ_elt (typ_of elt))
      | _ -> assert false )
  | Ap2 (op, typ, x, y) -> (
    match (op, typ) with
    | (Eq | Dq | Gt | Ge | Lt | Le), (Integer _ | Float _ | Pointer _)
     |(Ugt | Uge | Ult | Ule), (Integer _ | Pointer _)
     |(Ord | Uno), Float _
     |(Add | Sub), (Integer _ | Float _ | Pointer _)
     |(Mul | Div | Rem), (Integer _ | Float _)
     |(Udiv | Urem | And | Or | Xor | Shl | Lshr | Ashr), Integer _ ->
        let typ_x = typ_of x and typ_y = typ_of y in
        assert (Typ.castable typ typ_x) ;
        assert (Typ.castable typ_x typ_y)
    | _ -> assert false )
  | Ap3 (Conditional, typ, cnd, thn, els) ->
      assert (Typ.is_sized typ) ;
      assert (Typ.castable Typ.bool (typ_of cnd)) ;
      assert (Typ.castable typ (typ_of thn)) ;
      assert (Typ.castable typ (typ_of els))
  | ApN (Record, typ, args) -> (
    match typ with
    | Array {elt} ->
        assert (
          IArray.for_all args ~f:(fun arg -> Typ.castable elt (typ_of arg))
        )
    | Tuple {elts} | Struct {elts} ->
        assert (IArray.length elts = IArray.length args) ;
        assert (
          IArray.for_all2_exn elts args ~f:(fun (_, typ) arg ->
              Typ.castable typ (typ_of arg) ) )
    | _ -> assert false )
  [@@warning "-9"]

(** Type query *)

and typ_of exp =
  match exp with
  | Reg {typ} | Global {typ} | Function {typ} | Integer {typ} | Float {typ}
    ->
      typ
  | Label _ -> Typ.ptr
  | Ap1 ((Signed _ | Unsigned _ | Convert _ | Splat), dst, _) -> dst
  | Ap1 (Select idx, typ, _) -> (
    match typ with
    | Array {elt} -> elt
    | Tuple {elts} | Struct {elts} -> snd (IArray.get elts idx)
    | _ -> violates invariant exp )
  | Ap2
      ( (Eq | Dq | Gt | Ge | Lt | Le | Ugt | Uge | Ult | Ule | Ord | Uno)
      , _
      , _
      , _ ) ->
      Typ.bool
  | Ap2
      ( ( Add | Sub | Mul | Div | Rem | Udiv | Urem | And | Or | Xor | Shl
        | Lshr | Ashr | Update _ )
      , typ
      , _
      , _ )
   |Ap3 (Conditional, typ, _, _, _)
   |ApN (Record, typ, _) ->
      typ
  [@@warning "-9"]

(** Registers are the expressions constructed by [Reg] *)
module Reg = struct
  include T

  module Set = struct
    include Set
    include Provide_pp (T)
  end

  let invariant x =
    let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
    match x with Reg _ -> invariant x | _ -> assert false

  let id = function Reg x -> x.id | r -> violates invariant r
  let name = function Reg x -> x.name | r -> violates invariant r
  let typ = function Reg x -> x.typ | r -> violates invariant r

  let of_exp = function
    | Reg _ as e -> Some (e |> check invariant)
    | _ -> None

  let mk typ id name = Reg {id; name; typ} |> check invariant
end

(** Globals are the expressions constructed by [Global] *)
module Global = struct
  include T

  module Set = struct
    include Set
    include Provide_pp (T)
  end

  let invariant x =
    let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
    match x with Global _ -> invariant x | _ -> assert false

  let name = function Global x -> x.name | r -> violates invariant r
  let typ = function Global x -> x.typ | r -> violates invariant r

  let of_exp = function
    | Global _ as e -> Some (e |> check invariant)
    | _ -> None

  let mk typ name = Global {name; typ} |> check invariant
end

(** Function names are the expressions constructed by [Function] *)
module Function = struct
  include T

  let name = function Function x -> x.name | r -> violates invariant r
  let typ = function Function x -> x.typ | r -> violates invariant r

  let invariant x =
    let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
    match x with Function _ -> invariant x | _ -> assert false

  let of_exp = function
    | Function _ as e -> Some (e |> check invariant)
    | _ -> None

  let mk typ name = Function {name; typ} |> check invariant

  let counterfeit =
    let dummy_function_type =
      Typ.pointer ~elt:(Typ.function_ ~args:IArray.empty ~return:None)
    in
    fun name -> mk dummy_function_type name

  module Map = Map
  module Tbl = Tbl
end

(** Construct *)

(* registers *)

let reg x = x

(* constants *)

let function_ f = f
let global g = g
let label ~parent ~name = Label {parent; name} |> check invariant
let integer typ data = Integer {data; typ} |> check invariant
let null = integer Typ.ptr Z.zero
let bool b = integer Typ.bool (Z.of_bool b)
let true_ = bool true
let false_ = bool false
let float typ data = Float {data; typ} |> check invariant

(* type conversions *)

let signed bits x ~to_:typ = Ap1 (Signed {bits}, typ, x) |> check invariant

let unsigned bits x ~to_:typ =
  Ap1 (Unsigned {bits}, typ, x) |> check invariant

let convert src ~to_:dst exp =
  Ap1 (Convert {src}, dst, exp) |> check invariant

(* comparisons *)

let binary op ?typ x y =
  let typ = match typ with Some typ -> typ | None -> typ_of x in
  Ap2 (op, typ, x, y) |> check invariant

let ubinary op ?typ x y =
  let typ = match typ with Some typ -> typ | None -> typ_of x in
  binary op ~typ x y

let eq = binary Eq
let dq = binary Dq
let gt = binary Gt
let ge = binary Ge
let lt = binary Lt
let le = binary Le
let ugt = ubinary Ugt
let uge = ubinary Uge
let ult = ubinary Ult
let ule = ubinary Ule
let ord = binary Ord
let uno = binary Uno

(* arithmetic *)

let add = binary Add
let sub = binary Sub
let mul = binary Mul
let div = binary Div
let rem = binary Rem
let udiv = ubinary Udiv
let urem = ubinary Urem

(* boolean / bitwise *)

let and_ = binary And
let or_ = binary Or

(* bitwise *)

let xor = binary Xor
let shl = binary Shl
let lshr = binary Lshr
let ashr = binary Ashr

(* if-then-else *)

let conditional ?typ ~cnd ~thn ~els =
  let typ = match typ with Some typ -> typ | None -> typ_of thn in
  Ap3 (Conditional, typ, cnd, thn, els) |> check invariant

(* sequences *)

let splat typ byt = Ap1 (Splat, typ, byt) |> check invariant

(* records (struct / array values) *)

let record typ elts = ApN (Record, typ, elts) |> check invariant
let select typ rcd idx = Ap1 (Select idx, typ, rcd) |> check invariant

let update typ ~rcd idx ~elt =
  Ap2 (Update idx, typ, rcd, elt) |> check invariant

(** Traverse *)

let rec fold_exps e z ~f =
  f e
    ( match e with
    | Ap1 (_, _, x) -> fold_exps ~f x z
    | Ap2 (_, _, x, y) -> fold_exps ~f y (fold_exps ~f x z)
    | Ap3 (_, _, w, x, y) ->
        fold_exps ~f w (fold_exps ~f y (fold_exps ~f x z))
    | ApN (_, _, xs) -> IArray.fold xs z ~f:(fold_exps ~f)
    | _ -> z )

let fold_regs e z ~f =
  fold_exps e z ~f:(fun x z ->
      match x with Reg _ -> f (x :> Reg.t) z | _ -> z )

(** Query *)

let is_true e =
  match e with
  | Integer {data; typ= Integer {bits= 1; _}} -> Z.is_true data
  | _ -> false

let is_false e =
  match e with
  | Integer {data; typ= Integer {bits= 1; _}} -> Z.is_false data
  | _ -> false
