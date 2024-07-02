(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for Propositions (i.e., Symbolic Heaps) *)

(* Module for normalization *)
module Normalize = struct
  let ( -- ) = IntLit.sub

  let ( ++ ) = IntLit.add

  let sym_eval tenv e =
    let lookup = Tenv.lookup tenv in
    let rec eval (e : Exp.t) : Exp.t =
      (* L.d_str " ["; BiabductionPredicates.d_exp e; L.d_str"] "; *)
      match e with
      | Var _ ->
          e
      | Closure c ->
          let captured_vars =
            List.map ~f:(fun (exp, pvar, typ, mode) -> (eval exp, pvar, typ, mode)) c.captured_vars
          in
          Closure {c with captured_vars}
      | Const _ ->
          e
      | Sizeof {typ= {desc= Tarray {elt= {desc= Tint ik}}}; dynamic_length= Some l}
        when Typ.ikind_is_char ik && Language.curr_language_is Clang ->
          eval l
      | Sizeof {typ= {desc= Tarray {elt= {desc= Tint ik}; length= Some l}}}
        when Typ.ikind_is_char ik && Language.curr_language_is Clang ->
          Const (Cint l)
      | Sizeof _ ->
          e
      | Cast (_, e1) ->
          eval e1
      | UnOp (Unop.LNot, e1, topt) -> (
        match eval e1 with
        | Const (Cint i) when IntLit.iszero i ->
            Exp.one
        | Const (Cint _) ->
            Exp.zero
        | UnOp (LNot, e1', _) ->
            e1'
        | e1' ->
            UnOp (LNot, e1', topt) )
      | UnOp (Neg, e1, topt) -> (
        match eval e1 with
        | UnOp (Neg, e2', _) ->
            e2'
        | Const (Cint i) ->
            Exp.int (IntLit.neg i)
        | Const (Cfloat v) ->
            Exp.float ~-.v
        | Var id ->
            UnOp (Neg, Var id, topt)
        | e1' ->
            UnOp (Neg, e1', topt) )
      | UnOp (BNot, e1, topt) -> (
        match eval e1 with
        | UnOp (BNot, e2', _) ->
            e2'
        | Const (Cint i) ->
            Exp.int (IntLit.lognot i)
        | e1' ->
            UnOp (BNot, e1', topt) )
      | BinOp (Le, e1, e2) -> (
        match (eval e1, eval e2) with
        | Const (Cint n), Const (Cint m) ->
            Exp.bool (IntLit.leq n m)
        | Const (Cfloat v), Const (Cfloat w) ->
            Exp.bool Float.(v <= w)
        | BinOp (PlusA _, e3, Const (Cint n)), Const (Cint m) ->
            BinOp (Le, e3, Exp.int (m -- n))
        | e1', e2' ->
            Exp.le e1' e2' )
      | BinOp (Lt, e1, e2) -> (
        match (eval e1, eval e2) with
        | Const (Cint n), Const (Cint m) ->
            Exp.bool (IntLit.lt n m)
        | Const (Cfloat v), Const (Cfloat w) ->
            Exp.bool Float.(v < w)
        | Const (Cint n), BinOp ((MinusA _ as ominus), f1, f2) ->
            BinOp (Le, BinOp (ominus, f2, f1), Exp.int (IntLit.minus_one -- n))
        | BinOp ((MinusA _ as ominus), f1, f2), Const (Cint n) ->
            Exp.le (BinOp (ominus, f1, f2)) (Exp.int (n -- IntLit.one))
        | BinOp (PlusA _, e3, Const (Cint n)), Const (Cint m) ->
            BinOp (Lt, e3, Exp.int (m -- n))
        | e1', e2' ->
            Exp.lt e1' e2' )
      | BinOp (Ge, e1, e2) ->
          eval (Exp.le e2 e1)
      | BinOp (Gt, e1, e2) ->
          eval (Exp.lt e2 e1)
      | BinOp (Eq, e1, e2) -> (
        match (eval e1, eval e2) with
        | Const (Cint n), Const (Cint m) ->
            Exp.bool (IntLit.eq n m)
        | Const (Cfloat v), Const (Cfloat w) ->
            Exp.bool (Float.equal v w)
        | Const (Cint _), Exp.Lvar _ | Exp.Lvar _, Const (Cint _) ->
            (* Comparing pointer with nonzero integer is undefined behavior in ISO C++ *)
            (* Assume they are not equal *)
            Exp.zero
        | e1', e2' ->
            Exp.eq e1' e2' )
      | BinOp (Ne, e1, e2) -> (
        match (eval e1, eval e2) with
        | Const (Cint n), Const (Cint m) ->
            Exp.bool (IntLit.neq n m)
        | Const (Cfloat v), Const (Cfloat w) ->
            Exp.bool Float.(v <> w)
        | Const (Cint _), Exp.Lvar _ | Exp.Lvar _, Const (Cint _) ->
            (* Comparing pointer with nonzero integer is undefined behavior in ISO C++ *)
            (* Assume they are not equal *)
            Exp.one
        | e1', e2' ->
            Exp.ne e1' e2' )
      | BinOp (LAnd, e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const (Cint i), _ when IntLit.iszero i ->
              e1'
          | Const (Cint _), _ ->
              e2'
          | _, Const (Cint i) when IntLit.iszero i ->
              e2'
          | _, Const (Cint _) ->
              e1'
          | _ ->
              BinOp (LAnd, e1', e2') )
      | BinOp (LOr, e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const (Cint i), _ when IntLit.iszero i ->
              e2'
          | Const (Cint _), _ ->
              e1'
          | _, Const (Cint i) when IntLit.iszero i ->
              e1'
          | _, Const (Cint _) ->
              e2'
          | _ ->
              BinOp (LOr, e1', e2') )
      | BinOp (PlusPI, Lindex (ep, e1), e2) ->
          (* array access with pointer arithmetic *)
          let e' : Exp.t = BinOp (PlusA None, e1, e2) in
          eval (Exp.Lindex (ep, e'))
      | BinOp (PlusPI, BinOp (PlusPI, e11, e12), e2) ->
          (* take care of pattern ((ptr + off1) + off2) *)
          (* progress: convert inner +I to +A *)
          let e2' : Exp.t = BinOp (PlusA None, e12, e2) in
          eval (Exp.BinOp (PlusPI, e11, e2'))
      | BinOp ((PlusA _ as oplus), e1, e2) | BinOp ((PlusPI as oplus), e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          let isPlusA, ominus =
            match oplus with
            | Binop.PlusA topt ->
                (true, Binop.MinusA topt)
            | _ ->
                (false, Binop.MinusPI)
          in
          let ( +++ ) (x : Exp.t) (y : Exp.t) : Exp.t =
            match (x, y) with
            | _, Const (Cint i) when IntLit.iszero i ->
                x
            | Const (Cint i), Const (Cint j) ->
                Const (Cint (IntLit.add i j))
            | _ ->
                BinOp (oplus, x, y)
          in
          let ( --- ) (x : Exp.t) (y : Exp.t) : Exp.t =
            match (x, y) with
            | _, Const (Cint i) when IntLit.iszero i ->
                x
            | Const (Cint i), Const (Cint j) ->
                Const (Cint (IntLit.sub i j))
            | _ ->
                BinOp (ominus, x, y)
          in
          (* test if the extensible array at the end of [typ] has elements of type [elt] *)
          let extensible_array_element_typ_equal elt typ =
            Option.exists ~f:(Typ.equal elt) (Struct.get_extensible_array_element_typ ~lookup typ)
          in
          match (e1', e2') with
          (* pattern for arrays and extensible structs:
             sizeof(struct s {... t[l]}) + k * sizeof(t)) = sizeof(struct s {... t[l + k]}) *)
          | ( Sizeof ({typ; dynamic_length= len1_opt} as sizeof_data)
            , BinOp (Mult _, len2, Sizeof {typ= elt; dynamic_length= None}) )
            when isPlusA && extensible_array_element_typ_equal elt typ ->
              let len = match len1_opt with Some len1 -> len1 +++ len2 | None -> len2 in
              Sizeof {sizeof_data with dynamic_length= Some len}
          | Const c, _ when Const.iszero_int_float c ->
              e2'
          | _, Const c when Const.iszero_int_float c ->
              e1'
          | Const (Cint n), Const (Cint m) ->
              Exp.int (n ++ m)
          | Const (Cfloat v), Const (Cfloat w) ->
              Exp.float (v +. w)
          | UnOp (Neg, f1, _), f2 | f2, UnOp (Neg, f1, _) ->
              BinOp (ominus, f2, f1)
          | BinOp (PlusA _, e, Const (Cint n1)), Const (Cint n2)
          | BinOp (PlusPI, e, Const (Cint n1)), Const (Cint n2)
          | Const (Cint n2), BinOp (PlusA _, e, Const (Cint n1))
          | Const (Cint n2), BinOp (PlusPI, e, Const (Cint n1)) ->
              e +++ Exp.int (n1 ++ n2)
          | BinOp (MinusA _, Const (Cint n1), e), Const (Cint n2)
          | Const (Cint n2), BinOp (MinusA _, Const (Cint n1), e) ->
              Exp.int (n1 ++ n2) --- e
          | BinOp (MinusA _, e1, e2), e3 ->
              (* (e1-e2)+e3 --> e1 + (e3-e2) *)
              (* progress: brings + to the outside *)
              eval (e1 +++ (e3 --- e2))
          | _, Const _ ->
              e1' +++ e2'
          | Const _, _ ->
              if isPlusA then e2' +++ e1' else e1' +++ e2'
          | Var _, Var _ ->
              e1' +++ e2'
          | _ ->
              e1' +++ e2' )
      | BinOp ((MinusA _ as ominus), e1, e2) | BinOp ((MinusPI as ominus), e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          let oplus =
            match ominus with Binop.MinusA topt -> Binop.PlusA topt | _ -> Binop.PlusPI
          in
          let ( +++ ) x y : Exp.t = BinOp (oplus, x, y) in
          let ( --- ) x y : Exp.t = BinOp (ominus, x, y) in
          if Exp.equal e1' e2' then Exp.zero
          else
            match (e1', e2') with
            | Const c, _ when Const.iszero_int_float c ->
                eval (Exp.UnOp (Neg, e2', None))
            | _, Const c when Const.iszero_int_float c ->
                e1'
            | Const (Cint n), Const (Cint m) ->
                Exp.int (n -- m)
            | Const (Cfloat v), Const (Cfloat w) ->
                Exp.float (v -. w)
            | _, UnOp (Neg, f2, _) ->
                eval (e1 +++ f2)
            | _, Const (Cint n) ->
                eval (e1' +++ Exp.int (IntLit.neg n))
            | Const _, _ ->
                e1' --- e2'
            | Var _, Var _ ->
                e1' --- e2'
            | _, _ ->
                e1' --- e2' )
      | BinOp (MinusPP, e1, e2) ->
          BinOp (MinusPP, eval e1, eval e2)
      | BinOp ((Mult _ as omult), e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const c, _ when Const.iszero_int_float c ->
              Exp.zero
          | Const c, _ when Const.isone_int_float c ->
              e2'
          | Const c, _ when Const.isminusone_int_float c ->
              eval (Exp.UnOp (Neg, e2', None))
          | _, Const c when Const.iszero_int_float c ->
              Exp.zero
          | _, Const c when Const.isone_int_float c ->
              e1'
          | _, Const c when Const.isminusone_int_float c ->
              eval (Exp.UnOp (Neg, e1', None))
          | Const (Cint n), Const (Cint m) ->
              Exp.int (IntLit.mul n m)
          | Const (Cfloat v), Const (Cfloat w) ->
              Exp.float (v *. w)
          | Var _, Var _ ->
              BinOp (omult, e1', e2')
          | _, Sizeof _ | Sizeof _, _ ->
              BinOp (omult, e1', e2')
          | _, _ ->
              BinOp (omult, e1', e2') )
      | BinOp (((DivI | DivF) as div), e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | _, Const c when Const.iszero_int_float c ->
              Exp.get_undefined false
          | Const c, _ when Const.iszero_int_float c ->
              e1'
          | _, Const c when Const.isone_int_float c ->
              e1'
          | Const (Cint n), Const (Cint m) ->
              Exp.int (IntLit.div n m)
          | Const (Cfloat v), Const (Cfloat w) ->
              Exp.float (v /. w)
          | ( Sizeof {typ= {desc= Tarray {elt}}; dynamic_length= Some len}
            , Sizeof {typ= elt2; dynamic_length= None} )
          (* pattern: sizeof(elt[len]) / sizeof(elt) = len *)
            when Typ.equal elt elt2 ->
              len
          | ( Sizeof {typ= {desc= Tarray {elt; length= Some len}}; dynamic_length= None}
            , Sizeof {typ= elt2; dynamic_length= None} )
          (* pattern: sizeof(elt[len]) / sizeof(elt) = len *)
            when Typ.equal elt elt2 ->
              Const (Cint len)
          | _ ->
              BinOp (div, e1', e2') )
      | BinOp (Mod, e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | _, Const (Cint i) when IntLit.iszero i ->
              Exp.get_undefined false
          | Const (Cint i), _ when IntLit.iszero i ->
              e1'
          | _, Const (Cint i) when IntLit.isone i ->
              Exp.zero
          | Const (Cint n), Const (Cint m) ->
              Exp.int (IntLit.rem n m)
          | _ ->
              BinOp (Mod, e1', e2') )
      | BinOp (Shiftlt, e1, e2) -> (
        match (e1, e2) with
        | Const (Cint n), Const (Cint m) -> (
          try Exp.int (IntLit.shift_left n m)
          with IntLit.OversizedShift -> BinOp (Shiftlt, eval e1, eval e2) )
        | _, Const (Cint m) when IntLit.iszero m ->
            eval e1
        | _, Const (Cint m) when IntLit.isone m ->
            eval (Exp.BinOp (PlusA None, e1, e1))
        | Const (Cint m), _ when IntLit.iszero m ->
            e1
        | _ ->
            BinOp (Shiftlt, eval e1, eval e2) )
      | BinOp (Shiftrt, e1, e2) -> (
        match (e1, e2) with
        | Const (Cint n), Const (Cint m) -> (
          try Exp.int (IntLit.shift_right n m)
          with IntLit.OversizedShift -> BinOp (Shiftrt, eval e1, eval e2) )
        | _, Const (Cint m) when IntLit.iszero m ->
            eval e1
        | Const (Cint m), _ when IntLit.iszero m ->
            e1
        | _ ->
            BinOp (Shiftrt, eval e1, eval e2) )
      | BinOp (BAnd, e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const (Cint i), _ when IntLit.iszero i ->
              e1'
          | _, Const (Cint i) when IntLit.iszero i ->
              e2'
          | Const (Cint i1), Const (Cint i2) ->
              Exp.int (IntLit.logand i1 i2)
          | _ ->
              BinOp (BAnd, e1', e2') )
      | BinOp (BOr, e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const (Cint i), _ when IntLit.iszero i ->
              e2'
          | _, Const (Cint i) when IntLit.iszero i ->
              e1'
          | Const (Cint i1), Const (Cint i2) ->
              Exp.int (IntLit.logor i1 i2)
          | _ ->
              BinOp (BOr, e1', e2') )
      | BinOp (BXor, e1, e2) -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const (Cint i), _ when IntLit.iszero i ->
              e2'
          | _, Const (Cint i) when IntLit.iszero i ->
              e1'
          | Const (Cint i1), Const (Cint i2) ->
              Exp.int (IntLit.logxor i1 i2)
          | _ ->
              BinOp (BXor, e1', e2') )
      | Exn _ ->
          e
      | Lvar _ ->
          e
      | Lfield (e1, fld, typ) ->
          let e1' = eval e1 in
          Lfield (e1', fld, typ)
      | Lindex (Lvar pv, e2)
        when false (* removed: it interferes with re-arrangement and error messages *) ->
          (* &x[n]  -->  &x + n *)
          eval (Exp.BinOp (PlusPI, Lvar pv, e2))
      | Lindex (BinOp (PlusPI, ep, e1), e2) ->
          (* array access with pointer arithmetic *)
          let e' : Exp.t = BinOp (PlusA None, e1, e2) in
          eval (Exp.Lindex (ep, e'))
      | Lindex (e1, e2) ->
          let e1' = eval e1 in
          let e2' = eval e2 in
          Lindex (e1', e2')
    in
    let e' = eval e in
    (* L.d_str "sym_eval "; BiabductionPredicates.d_exp e; L.d_str" --> "; BiabductionPredicates.d_exp e'; L.d_ln (); *)
    if Exp.equal e e' then e else e'


  let exp_normalize_noabs tenv exp = sym_eval tenv exp
end

(* Export for interface *)
let exp_normalize_noabs = Normalize.exp_normalize_noabs
