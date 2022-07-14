(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Fol
module T = Term
module F = Formula

module RegTbl = HashTable.Make (struct
  type t = Llair.Reg.t * ThreadID.t [@@deriving equal]

  let hash = Poly.hash
end)

let reg_tbl : Var.t RegTbl.t = RegTbl.create ()

let lookup_func lookup term =
  match Term.get_trm term with
  | Some (Apply (Uninterp name, [||])) -> lookup name
  | _ -> None

let uconst name = T.apply (Funsym.uninterp name) [||]
let global g = uconst (Llair.Global.name g)

let reg tid r =
  RegTbl.find_or_add reg_tbl (r, tid) ~default:(fun () ->
      let name = Llair.Reg.name r in
      Var.identified ~name tid (Llair.Reg.id r) )

let regs tid rs =
  Var.Set.of_iter (Iter.map ~f:(reg tid) (Llair.Reg.Set.to_iter rs))

let uap0 f = T.apply f [||]
let uap1 f a = T.apply f [|a|]
let uap2 f a b = T.apply f [|a; b|]
let lit2 p a b = F.lit p [|a; b|]
let nlit2 p a b = F.not_ (lit2 p a b)

let rec ap_ttt : 'a. (T.t -> T.t -> 'a) -> _ -> _ -> _ -> 'a =
 fun f tid a b -> f (term tid a) (term tid b)

and ap_ttf (f : T.t -> T.t -> F.t) tid a b = F.inject (ap_ttt f tid a b)

and ap_fff (f : F.t -> F.t -> F.t) tid a b =
  F.inject (f (formula tid a) (formula tid b))

and ap_uut : 'a. (T.t -> T.t -> 'a) -> _ -> _ -> _ -> _ -> 'a =
 fun f typ tid a b ->
  let bits = Llair.Typ.bit_size_of typ in
  let unsigned x = uap1 (Unsigned bits) x in
  f (unsigned (term tid a)) (unsigned (term tid b))

and ap_uuf (f : T.t -> T.t -> F.t) typ tid a b =
  F.inject (ap_uut f typ tid a b)

and term : ThreadID.t -> Llair.Exp.t -> T.t =
 fun tid e ->
  let imp p q = F.or_ (F.not_ p) q in
  let nimp p q = F.and_ p (F.not_ q) in
  let if_ p q = F.or_ p (F.not_ q) in
  let nif p q = F.and_ (F.not_ p) q in
  match e with
  (* formulas *)
  | Ap2 (Eq, Integer {bits= 1; _}, p, q) -> ap_fff F.iff tid p q
  | Ap2 (Dq, Integer {bits= 1; _}, p, q) -> ap_fff F.xor tid p q
  | Ap2 ((Gt | Ugt), Integer {bits= 1; _}, p, q) -> ap_fff nimp tid p q
  | Ap2 ((Lt | Ult), Integer {bits= 1; _}, p, q) -> ap_fff nif tid p q
  | Ap2 ((Ge | Uge), Integer {bits= 1; _}, p, q) -> ap_fff if_ tid p q
  | Ap2 ((Le | Ule), Integer {bits= 1; _}, p, q) -> ap_fff imp tid p q
  | Ap2 (Add, Integer {bits= 1; _}, p, q) -> ap_fff F.xor tid p q
  | Ap2 (Sub, Integer {bits= 1; _}, p, q) -> ap_fff F.xor tid p q
  | Ap2 (Mul, Integer {bits= 1; _}, p, q) -> ap_fff F.and_ tid p q
  (* div and rem are not formulas even if bits=1 due to division by 0 *)
  | Ap2 (And, Integer {bits= 1; _}, p, q) -> ap_fff F.and_ tid p q
  | Ap2 (Or, Integer {bits= 1; _}, p, q) -> ap_fff F.or_ tid p q
  | Ap2 (Xor, Integer {bits= 1; _}, p, q) -> ap_fff F.xor tid p q
  | Ap2 ((Shl | Lshr), Integer {bits= 1; _}, p, q) -> ap_fff nimp tid p q
  | Ap2 (Ashr, Integer {bits= 1; _}, p, q) -> ap_fff F.or_ tid p q
  | Ap3 (Conditional, Integer {bits= 1; _}, cnd, pos, neg) ->
      F.inject
        (F.cond ~cnd:(formula tid cnd) ~pos:(formula tid pos)
           ~neg:(formula tid neg) )
  (* terms *)
  | Reg _ -> T.var (reg tid (Llair.Reg.of_exp e |> Option.get_exn))
  | Global {name; typ= _} | FuncName {name; typ= _} -> uconst name
  | Label {parent; name} ->
      uap0 (Funsym.uninterp ("label_" ^ parent ^ "_" ^ name))
  | Integer {typ= _; data} -> T.integer data
  | Float {data; typ= _} -> (
    match Q.of_float (Float.of_string_exn data) with
    | q when Q.is_real q -> T.rational q
    | _ | (exception Invalid_argument _) ->
        uap0 (Funsym.uninterp ("float_" ^ data)) )
  | Ap1 (Signed {bits}, _, e) ->
      let a = term tid e in
      if bits = 1 then
        match F.project a with
        | Some fml -> F.inject fml
        | _ -> uap1 (Signed bits) a
      else uap1 (Signed bits) a
  | Ap1 (Unsigned {bits}, _, e) ->
      let a = term tid e in
      if bits = 1 then
        match F.project a with
        | Some fml -> F.inject fml
        | _ -> uap1 (Unsigned bits) a
      else uap1 (Unsigned bits) a
  | Ap1 (Convert {src}, dst, e) when Llair.Typ.equivalent src dst ->
      term tid e
  | Ap1 (Convert {src= Float _}, Float _, e) -> term tid e
  | Ap1 (Convert {src}, dst, e) ->
      let s =
        Format.asprintf "convert_%a_of_%a" Llair.Typ.pp dst Llair.Typ.pp src
      in
      uap1 (Funsym.uninterp s) (term tid e)
  | Ap2 (Eq, _, d, e) -> ap_ttf F.eq tid d e
  | Ap2 (Dq, _, d, e) -> ap_ttf F.dq tid d e
  | Ap2 (Gt, _, d, e) -> ap_ttf F.gt tid d e
  | Ap2 (Lt, _, d, e) -> ap_ttf F.lt tid d e
  | Ap2 (Ge, _, d, e) -> ap_ttf F.ge tid d e
  | Ap2 (Le, _, d, e) -> ap_ttf F.le tid d e
  | Ap2 (Ugt, typ, d, e) -> ap_uuf F.gt typ tid d e
  | Ap2 (Ult, typ, d, e) -> ap_uuf F.lt typ tid d e
  | Ap2 (Uge, typ, d, e) -> ap_uuf F.ge typ tid d e
  | Ap2 (Ule, typ, d, e) -> ap_uuf F.le typ tid d e
  | Ap2 (Ord, _, d, e) -> ap_ttf (lit2 (Predsym.uninterp "ord")) tid d e
  | Ap2 (Uno, _, d, e) -> ap_ttf (nlit2 (Predsym.uninterp "ord")) tid d e
  | Ap2 (Add, _, d, e) -> ap_ttt T.add tid d e
  | Ap2 (Sub, _, d, e) -> ap_ttt T.sub tid d e
  | Ap2 (Mul, _, d, e) -> ap_ttt T.mul tid d e
  | Ap2 (Div, _, d, e) -> ap_ttt T.div tid d e
  | Ap2 (Rem, _, d, e) -> ap_ttt (uap2 Rem) tid d e
  | Ap2 (Udiv, typ, d, e) -> ap_uut T.div typ tid d e
  | Ap2 (Urem, typ, d, e) -> ap_uut (uap2 Rem) typ tid d e
  | Ap2 (And, _, d, e) -> ap_ttt (uap2 BitAnd) tid d e
  | Ap2 (Or, _, d, e) -> ap_ttt (uap2 BitOr) tid d e
  | Ap2 (Xor, _, d, e) -> ap_ttt (uap2 BitXor) tid d e
  | Ap2 (Shl, _, d, e) -> ap_ttt (uap2 BitShl) tid d e
  | Ap2 (Lshr, _, d, e) -> ap_ttt (uap2 BitLshr) tid d e
  | Ap2 (Ashr, _, d, e) -> ap_ttt (uap2 BitAshr) tid d e
  | Ap3 (Conditional, _, cnd, thn, els) ->
      T.ite ~cnd:(formula tid cnd) ~thn:(term tid thn) ~els:(term tid els)
  | Ap1 (Select idx, typ, rcd) ->
      let off, len = Llair.Typ.offset_length_of_elt typ idx in
      let off = T.integer (Z.of_int off) in
      let len = T.integer (Z.of_int len) in
      let seq = term tid rcd in
      let siz = T.integer (Z.of_int (Llair.Typ.size_of typ)) in
      T.extract ~seq ~siz ~off ~len
  | Ap2 (Update idx, typ, rcd, elt) ->
      let oI, lI = Llair.Typ.offset_length_of_elt typ idx in
      let oJ = oI + lI in
      let off0 = T.zero in
      let len0 = T.integer (Z.of_int oI) in
      let len1 = T.integer (Z.of_int lI) in
      let off2 = T.integer (Z.of_int oJ) in
      let len2 = T.integer (Z.of_int (Llair.Typ.size_of typ - oI - lI)) in
      let seq = term tid rcd in
      let siz = T.integer (Z.of_int (Llair.Typ.size_of typ)) in
      T.concat
        [| {seq= T.extract ~seq ~siz ~off:off0 ~len:len0; siz= len0}
         ; {seq= term tid elt; siz= len1}
         ; {seq= T.extract ~seq ~siz ~off:off2 ~len:len2; siz= len2} |]
  | ApN (Record, typ, elts) ->
      let elt_siz i =
        T.integer (Z.of_int (snd (Llair.Typ.offset_length_of_elt typ i)))
      in
      T.concat
        (Array.mapi (IArray.to_array elts) ~f:(fun i elt ->
             {T.seq= term tid elt; siz= elt_siz i} ) )
  | Ap1 (Splat, _, byt) -> T.splat (term tid byt)

and formula tid e = F.dq0 (term tid e)
