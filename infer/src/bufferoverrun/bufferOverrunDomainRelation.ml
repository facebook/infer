(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Core
open AbsLoc
open! AbstractDomain.Types
module F = Format
open Apron

module type S = sig
  module Var : sig
    type t
  end

  module Sym : sig
    include AbstractDomain.S

    val bot : t

    val top : t

    val of_loc : Loc.t -> t

    val of_loc_offset : Loc.t -> t

    val of_loc_size : Loc.t -> t

    val of_allocsite_offset : Allocsite.t -> t

    val of_allocsite_size : Allocsite.t -> t

    val get_var : t -> Var.t option
  end

  module SymExp : sig
    type t [@@deriving compare]

    val pp_opt : F.formatter -> t option -> unit

    val zero : t

    val of_sym : Sym.t -> t option

    val of_exp : get_sym_f:(Exp.t -> Sym.t) -> Exp.t -> t option

    val of_exps :
         get_int_sym_f:(Exp.t -> Sym.t)
      -> get_offset_sym_f:(Exp.t -> Sym.t)
      -> get_size_sym_f:(Exp.t -> Sym.t)
      -> Exp.t
      -> t option * t option * t option

    val of_exp_opt : get_sym_f:(Exp.t -> Sym.t) -> Exp.t option -> t option

    val plus : t -> t -> t

    val minus : t -> t -> t
  end

  module Constraints : sig
    type t

    val of_exp : get_sym_f:(Exp.t -> Sym.t) -> Exp.t -> t
  end

  module SubstMap : sig
    type t

    val empty : t

    val add : Var.t -> SymExp.t option -> t -> t

    val symexp_subst_opt : t -> SymExp.t option -> SymExp.t option
  end

  include AbstractDomain.S

  val set_deserialize : unit -> unit

  val compare : t -> t -> int

  val empty : t

  val bot : t

  val is_unsat : t -> bool

  val lt_sat_opt : SymExp.t option -> SymExp.t option -> t -> bool

  val le_sat_opt : SymExp.t option -> SymExp.t option -> t -> bool

  val meet_constraints : Constraints.t -> t -> t

  val store_relation : PowLoc.t -> SymExp.t option * SymExp.t option * SymExp.t option -> t -> t

  val init_param : Loc.t -> t -> t

  val init_array :
    Allocsite.t -> offset_opt:Itv.t option -> size:Itv.t -> size_exp_opt:SymExp.t option -> t -> t

  val forget_locs : PowLoc.t -> t -> t

  val instantiate : caller:t -> callee:t -> SubstMap.t -> t
end

module NoRelation = struct
  module UnitDom = struct
    type t = unit [@@deriving compare]

    let f1 _ = ()

    let f2 _ _ = ()

    let f3 _ _ _ = ()

    let f1_none _ = None

    let f2_none _ _ = None

    let f1_false _ = false

    let f3_false _ _ _ = false

    let ( <= ) ~lhs:() ~rhs:() = true

    let join = f2

    let widen ~prev:() ~next:() ~num_iters:_ = ()

    let pp = f2

    let bot = ()

    let top = ()
  end

  module Var = UnitDom

  module Sym = struct
    include UnitDom

    let of_loc = f1

    let of_loc_offset = f1

    let of_loc_size = f1

    let of_allocsite_offset = f1

    let of_allocsite_size = f1

    let get_var = f1_none
  end

  module SymExp = struct
    include UnitDom

    let pp_opt = f2

    let zero = ()

    let of_exp ~get_sym_f:_ = f1_none

    let of_exps ~get_int_sym_f:_ ~get_offset_sym_f:_ ~get_size_sym_f:_ _x = (None, None, None)

    let of_exp_opt ~get_sym_f:_ = f1_none

    let of_sym = f1_none

    let plus = f2

    let minus = f2
  end

  module Constraints = struct
    include UnitDom

    let of_exp ~get_sym_f:_ = f1
  end

  module SubstMap = struct
    include UnitDom

    let empty = ()

    let add = f3

    let symexp_subst_opt = f2_none
  end

  include UnitDom

  let set_deserialize = f1

  let empty = ()

  let is_unsat = f1_false

  let lt_sat_opt = f3_false

  let le_sat_opt = f3_false

  let meet_constraints = f2

  let store_relation = f3

  let init_param = f2

  let init_array _allocsite ~offset_opt:_ ~size:_ ~size_exp_opt:_ = f1

  let forget_locs = f2

  let instantiate ~caller:_ ~callee:_ = f1
end

module type Manager_S = sig
  type domain_t

  val alloc_man : unit -> domain_t Manager.t
end

module Make (Manager : Manager_S) = struct
  let man = Manager.alloc_man ()

  let set_deserialize () = Apron.Manager.set_deserialize man

  module Compares = struct
    let lift int_of x y = int_of x - int_of y

    let int_of_unop = function Texpr0.Neg -> 0 | Texpr0.Cast -> 1 | Texpr0.Sqrt -> 2

    let int_of_binop = function
      | Texpr0.Add ->
          0
      | Texpr0.Sub ->
          1
      | Texpr0.Mul ->
          2
      | Texpr0.Div ->
          3
      | Texpr0.Mod ->
          4
      | Texpr0.Pow ->
          5


    let int_of_typ = function
      | Texpr0.Real ->
          0
      | Texpr0.Int ->
          1
      | Texpr0.Single ->
          2
      | Texpr0.Double ->
          3
      | Texpr0.Extended ->
          4
      | Texpr0.Quad ->
          5


    let int_of_round = function
      | Texpr0.Near ->
          0
      | Texpr0.Zero ->
          1
      | Texpr0.Up ->
          2
      | Texpr0.Down ->
          3
      | Texpr0.Rnd ->
          4


    let compare_unop = lift int_of_unop

    let compare_binop = lift int_of_binop

    let compare_typ = lift int_of_typ

    let compare_round = lift int_of_round

    let ( <?> ) n (cmp, x, y) = if n <> 0 then n else cmp x y

    let rec compare_texpr0_expr x y =
      let int_of_texpr0_expr = function
        | Texpr0.Cst _ ->
            0
        | Texpr0.Dim _ ->
            1
        | Texpr0.Unop _ ->
            2
        | Texpr0.Binop _ ->
            3
      in
      match (x, y) with
      | Texpr0.Cst c1, Texpr0.Cst c2 ->
          Coeff.cmp c1 c2
      | Texpr0.Dim i1, Texpr0.Dim i2 ->
          i1 - i2
      | Texpr0.Unop (uop1, e1, t1, r1), Texpr0.Unop (uop2, e2, t2, r2) ->
          compare_unop uop1 uop2 <?> (compare_texpr0_expr, e1, e2) <?> (compare_typ, t1, t2)
          <?> (compare_round, r1, r2)
      | Texpr0.Binop (bop1, le1, re1, t1, r1), Texpr0.Binop (bop2, le2, re2, t2, r2) ->
          compare_binop bop1 bop2 <?> (compare_texpr0_expr, le1, le2)
          <?> (compare_texpr0_expr, re1, re2) <?> (compare_typ, t1, t2) <?> (compare_round, r1, r2)
      | _, _ ->
          int_of_texpr0_expr x - int_of_texpr0_expr y


    let compare_texpr0 x y = compare_texpr0_expr (Texpr0.to_expr x) (Texpr0.to_expr y)

    let compare_texpr1 x y =
      compare_texpr0 (Texpr1.get_texpr0 x) (Texpr1.get_texpr0 y)
      <?> (Environment.compare, Texpr1.get_env x, Texpr1.get_env y)


    let compare_abstract1 x y = Abstract1.hash man x - Abstract1.hash man y
  end

  module TexprToLinexpr = struct
    let scalar_op (float_op, mpqf_op, mpfrf_op) s1 s2 =
      match (s1, s2) with
      | Scalar.Float f1, Scalar.Float f2 ->
          Scalar.Float (float_op f1 f2)
      | Scalar.Float f1, Scalar.Mpqf m2 | Scalar.Mpqf m2, Scalar.Float f1 ->
          Scalar.Mpqf (mpqf_op (Mpqf.of_float f1) m2)
      | Scalar.Float f1, Scalar.Mpfrf m2 | Scalar.Mpfrf m2, Scalar.Float f1 ->
          Scalar.Mpfrf (mpfrf_op (Mpfrf.of_float f1 Mpfr.Near) m2 Mpfr.Near)
      | Scalar.Mpqf m1, Scalar.Mpqf m2 ->
          Scalar.Mpqf (mpqf_op m1 m2)
      | Scalar.Mpqf m1, Scalar.Mpfrf m2 | Scalar.Mpfrf m2, Scalar.Mpqf m1 ->
          Scalar.Mpfrf (mpfrf_op (Mpfrf.of_mpq m1 Mpfr.Near) m2 Mpfr.Near)
      | Scalar.Mpfrf m1, Scalar.Mpfrf m2 ->
          Scalar.Mpfrf (mpfrf_op m1 m2 Mpfr.Near)


    let scalar_add = scalar_op (( +. ), Mpqf.add, Mpfrf.add)

    let scalar_mult = scalar_op (( *. ), Mpqf.mul, Mpfrf.mul)

    let coeff_plus c1 c2 =
      match (c1, c2) with
      | Coeff.Scalar s1, Coeff.Scalar s2 ->
          Coeff.Scalar (scalar_add s1 s2)
      | _, _ ->
          assert false


    let coeff_minus c1 c2 = coeff_plus c1 (Coeff.neg c2)

    let coeff_mult c1 c2 =
      match (c1, c2) with
      | Coeff.Scalar s1, Coeff.Scalar s2 ->
          Coeff.Scalar (scalar_mult s1 s2)
      | _, _ ->
          assert false


    let rec is_constant = function
      | Texpr1.Cst c ->
          Some c
      | Texpr1.Unop (Texpr1.Neg, re1, _, _) ->
          Option.map (is_constant re1) ~f:Coeff.neg
      | Texpr1.Binop (Texpr1.Add, re1, re2, _, _) ->
          Option.map2 (is_constant re1) (is_constant re2) ~f:coeff_plus
      | Texpr1.Binop (Texpr1.Sub, re1, re2, _, _) ->
          Option.map2 (is_constant re1) (is_constant re2) ~f:coeff_minus
      | Texpr1.Binop (Texpr1.Mul, re1, re2, _, _) ->
          Option.map2 (is_constant re1) (is_constant re2) ~f:coeff_mult
      | _ ->
          None


    let rec add_coeffs ~coeff re x =
      match re with
      | Texpr1.Cst c ->
          let c' = Linexpr1.get_cst x in
          Linexpr1.set_cst x (coeff_plus c' (coeff_mult c coeff)) ;
          Some x
      | Texpr1.Var var ->
          let c' = Linexpr1.get_coeff x var in
          Linexpr1.set_coeff x var (coeff_plus c' coeff) ;
          Some x
      | Texpr1.Unop (Texpr1.Neg, re1, _, _) ->
          add_coeffs ~coeff:(Coeff.neg coeff) re1 x
      | Texpr1.Binop (Texpr1.Add, re1, re2, _, _) ->
          Option.bind (add_coeffs ~coeff re1 x) ~f:(add_coeffs ~coeff re2)
      | Texpr1.Binop (Texpr1.Sub, re1, re2, _, _) ->
          Option.bind (add_coeffs ~coeff re1 x) ~f:(add_coeffs ~coeff:(Coeff.neg coeff) re2)
      | Texpr1.Binop (Texpr1.Mul, re1, re2, _, _) -> (
        match is_constant re1 with
        | None ->
            Option.value_map (is_constant re2) ~default:None ~f:(fun c ->
                add_coeffs ~coeff:(coeff_mult coeff c) re1 x )
        | Some c ->
            add_coeffs ~coeff:(coeff_mult coeff c) re2 x )
      | _ ->
          assert false


    let trans x =
      let lin = Linexpr1.make (Texpr1.get_env x) in
      add_coeffs ~coeff:(Coeff.s_of_int 1) (Texpr1.to_expr x) lin
  end

  module Var = struct
    include Apron.Var

    let equal = [%compare.equal: t]

    let pp = print

    let dummy = of_string "dummy"

    let return = of_string "return"

    let param_prefix = "__inferbo_param_"

    let temp_param_prefix = "__inferbo_temp_param_"

    let loc_offset_prefix = "__inferbo_loc_offset_"

    let loc_size_prefix = "__inferbo_loc_size_"

    let allocsite_offset_prefix = "__inferbo_allocsite_offset_"

    let allocsite_size_prefix = "__inferbo_allocsite_size_"

    let of_loc loc = of_string (Loc.to_string loc)

    let of_loc_offset loc = of_string (loc_offset_prefix ^ Loc.to_string loc)

    let of_loc_size loc = of_string (loc_size_prefix ^ Loc.to_string loc)

    let of_allocsite_offset allocsite =
      of_string (allocsite_offset_prefix ^ Allocsite.to_string allocsite)


    let of_allocsite_size allocsite =
      of_string (allocsite_size_prefix ^ Allocsite.to_string allocsite)


    let param_of var = of_string (param_prefix ^ to_string var)

    let temp_param_of var = of_string (temp_param_prefix ^ to_string var)

    let param_of_loc loc = of_string (param_prefix ^ Loc.to_string loc)

    let array_of_var var = Array.create ~len:1 var

    let array_of_powloc of_loc locs =
      let len = PowLoc.cardinal locs in
      let a = Array.create ~len dummy in
      let i = ref 0 in
      PowLoc.iter
        (fun loc ->
          a.(!i) <- of_loc loc ;
          i := !i + 1 )
        locs ;
      a


    let int_array_of_powloc locs = array_of_powloc of_loc locs

    let offset_array_of_powloc locs = array_of_powloc of_loc_offset locs

    let size_array_of_powloc locs = array_of_powloc of_loc_size locs
  end

  module VarSet = struct
    include PrettyPrintable.MakePPSet (Var)

    let of_array var_array = Array.fold var_array ~init:empty ~f:(fun acc var -> add var acc)

    let to_array x =
      let a = Array.create ~len:(cardinal x) Var.dummy in
      let n = ref 0 in
      iter
        (fun var ->
          a.(!n) <- var ;
          n := !n + 1 )
        x ;
      a


    let of_powloc var_of_loc locs =
      PowLoc.fold (fun loc acc -> add (var_of_loc loc) acc) locs empty


    let int_of_powloc locs = of_powloc Var.of_loc locs

    let offset_of_powloc locs = of_powloc Var.of_loc_offset locs

    let size_of_powloc locs = of_powloc Var.of_loc_size locs
  end

  module VarMap = struct
    include PrettyPrintable.MakePPMap (Var)

    let iter x ~f = iter f x

    let fold x ~init ~f = fold f x init

    let fold2 x y ~init ~f =
      let m = merge (fun _ v1 v2 -> Some (v1, v2)) x y in
      fold m ~init ~f:(fun k (v1, v2) acc -> f k v1 v2 acc)
  end

  module Sym = struct
    include AbstractDomain.Flat (Var)

    let bot = empty

    let lift f x = v (f x)

    let of_loc = lift Var.of_loc

    let of_loc_offset = lift Var.of_loc_offset

    let of_loc_size = lift Var.of_loc_size

    let of_allocsite_offset = lift Var.of_allocsite_offset

    let of_allocsite_size = lift Var.of_allocsite_size

    let get_var = get
  end

  module Env = struct
    type t = Environment.t

    let empty : t = Environment.make [||] [||]

    let join env1 env2 =
      let vars, _ = Environment.vars env2 in
      let vars = Array.filter vars ~f:(fun var -> not (Environment.mem_var env1 var)) in
      Environment.add env1 vars [||]


    let of_vars_array vars = Environment.make vars [||]

    let of_vars_set vars = of_vars_array (VarSet.to_array vars)

    let to_vars_set x =
      let vars, _ = Environment.vars x in
      VarSet.of_array vars
  end

  module SymExp = struct
    (* raw tree expression without environments *)
    type raw = Texpr1.expr

    (* efficient tree expression with environments *)
    type t = Texpr1.t

    let string_of_binop = function
      | Texpr1.Add ->
          "+"
      | Texpr1.Sub ->
          "-"
      | Texpr1.Mul ->
          "*"
      | Texpr1.Div ->
          "/"
      | Texpr1.Mod ->
          "%"
      | Texpr1.Pow ->
          "^"


    let rec pp_raw ~need_paren fmt = function
      | Texpr1.Cst coeff ->
          Coeff.print fmt coeff
      | Texpr1.Var x ->
          Var.print fmt x
      | Texpr1.Unop (Texpr1.Neg, e, _, _) ->
          F.fprintf fmt "-%a" (pp_raw ~need_paren:true) e
      | Texpr1.Unop (Texpr1.Cast, e, typ, _) ->
          F.fprintf fmt "(%a)%a" Texpr1.print_typ typ (pp_raw ~need_paren:true) e
      | Texpr1.Unop (Texpr1.Sqrt, e, _, _) ->
          F.fprintf fmt "sqrt(%a)" (pp_raw ~need_paren:false) e
      | Texpr1.Binop (bop, e1, e2, _, _) ->
          (if need_paren then F.fprintf fmt "(%a%s%a)" else F.fprintf fmt "%a%s%a")
            (pp_raw ~need_paren:true) e1 (string_of_binop bop) (pp_raw ~need_paren:true) e2


    let pp fmt x = pp_raw ~need_paren:false fmt (Texpr1.to_expr x)

    let pp_opt fmt = function None -> F.fprintf fmt "None" | Some x -> pp fmt x

    let compare = Compares.compare_texpr1

    (* NOTE: We consider only integer values as of now. *)
    let default_round = Texpr1.Near

    let raw_uop_make typ re = Texpr1.Unop (typ, re, Texpr1.Int, default_round)

    let raw_bop_make typ re1 re2 = Texpr1.Binop (typ, re1, re2, Texpr1.Int, default_round)

    let vars_array_of_raw re =
      let rec vars_set_of = function
        | Texpr1.Cst _ ->
            VarSet.empty
        | Texpr1.Var x ->
            VarSet.singleton x
        | Texpr1.Unop (_, re, _, _) ->
            vars_set_of re
        | Texpr1.Binop (_, re1, re2, _, _) ->
            VarSet.union (vars_set_of re1) (vars_set_of re2)
      in
      VarSet.to_array (vars_set_of re)


    let vars_set_of x = Env.to_vars_set (Texpr1.get_env x)

    let vars_set_of_opt x_opt = Option.value_map x_opt ~default:VarSet.empty ~f:vars_set_of

    let env_of_raw re = Env.of_vars_array (vars_array_of_raw re)

    let raw_of_exp ~get_sym_f e : raw option =
      let try_get_sym_f e = get_sym_f e |> Sym.get_var |> Option.map ~f:(fun x -> Texpr1.Var x) in
      let rec raw_of_exp' e =
        match e with
        | Exp.UnOp (Unop.Neg, e', _) -> (
          match raw_of_exp' e' with
          | Some re ->
              Some (raw_uop_make Texpr1.Neg re)
          | None ->
              try_get_sym_f e )
        | Exp.BinOp (bop, e1, e2) -> (
          match (raw_of_exp' e1, raw_of_exp' e2) with
          | Some re1, Some re2 -> (
            match bop with
            | Binop.PlusA _ ->
                Some (raw_bop_make Texpr1.Add re1 re2)
            | Binop.MinusA _ ->
                Some (raw_bop_make Texpr1.Sub re1 re2)
            | Binop.Mult _ ->
                Some (raw_bop_make Texpr1.Mul re1 re2)
            | _ ->
                try_get_sym_f e )
          | _, _ ->
              try_get_sym_f e )
        | Exp.Const (Const.Cint i) ->
            Option.map (IntLit.to_int i) ~f:(fun n -> Texpr1.Cst (Coeff.s_of_int n))
        | _ ->
            try_get_sym_f e
      in
      raw_of_exp' e


    let raw_offset_of_exp ~get_int_sym_f ~get_offset_sym_f e =
      let try_get_offset_sym_f e =
        get_offset_sym_f e |> Sym.get_var |> Option.map ~f:(fun x -> Texpr1.Var x)
      in
      let rec raw_offset_of_exp' e =
        match e with
        | Exp.BinOp (bop, e1, e2) -> (
          match (raw_offset_of_exp' e1, raw_of_exp ~get_sym_f:get_int_sym_f e2) with
          | Some re1, Some re2 -> (
            match bop with
            | Binop.PlusPI ->
                Some (raw_bop_make Texpr1.Add re1 re2)
            | Binop.MinusPI ->
                Some (raw_bop_make Texpr1.Sub re1 re2)
            | _ ->
                try_get_offset_sym_f e )
          | _, _ ->
              try_get_offset_sym_f e )
        | _ ->
            try_get_offset_sym_f e
      in
      raw_offset_of_exp' e


    let raw_size_of_exp ~get_size_sym_f e =
      let try_get_size_sym_f e =
        get_size_sym_f e |> Sym.get_var |> Option.map ~f:(fun x -> Texpr1.Var x)
      in
      let rec raw_size_of_exp' e =
        match e with
        | Exp.BinOp (bop, e1, _e2) -> (
          match raw_size_of_exp' e1 with
          | Some re1 -> (
            match bop with Binop.PlusPI | Binop.MinusPI -> Some re1 | _ -> try_get_size_sym_f e )
          | _ ->
              try_get_size_sym_f e )
        | _ ->
            try_get_size_sym_f e
      in
      raw_size_of_exp' e


    let of_raw re = Texpr1.of_expr (env_of_raw re) re

    let of_exp ~get_sym_f e = Option.map (raw_of_exp ~get_sym_f e) ~f:of_raw

    let offset_of_exp ~get_int_sym_f ~get_offset_sym_f e : t option =
      Option.map (raw_offset_of_exp ~get_int_sym_f ~get_offset_sym_f e) ~f:of_raw


    let size_of_exp ~get_size_sym_f e : t option =
      Option.map (raw_size_of_exp ~get_size_sym_f e) ~f:of_raw


    let of_exps ~get_int_sym_f ~get_offset_sym_f ~get_size_sym_f e : t option * t option * t option
        =
      let int_sym = of_exp ~get_sym_f:get_int_sym_f e in
      let offset_sym = offset_of_exp ~get_int_sym_f ~get_offset_sym_f e in
      let size_sym = size_of_exp ~get_size_sym_f e in
      (int_sym, offset_sym, size_sym)


    let of_exp_opt ~get_sym_f opt_e : t option = Option.find_map opt_e ~f:(of_exp ~get_sym_f)

    let of_big_int i =
      Texpr1.cst Env.empty (Coeff.s_of_mpq (Mpq.of_mpz (Mpz.of_string (Z.to_string i))))


    let zero = of_big_int Z.zero

    let one = of_big_int Z.one

    let of_sym s = Sym.get_var s |> Option.map ~f:(fun x -> of_raw (Texpr1.Var x))

    let of_var var = of_raw (Texpr1.Var var)

    let dummy = of_var Var.dummy

    let linexpr_dummy = Linexpr1.make Env.empty

    let to_var x = match Texpr1.to_expr x with Texpr1.Var x' -> Some x' | _ -> None

    let is_var x = match to_var x with Some _ -> true | None -> false

    let bop_make typ x y =
      let env = Env.join (Texpr1.get_env x) (Texpr1.get_env y) in
      let re = raw_bop_make typ (Texpr1.to_expr x) (Texpr1.to_expr y) in
      Texpr1.of_expr env re


    let plus = bop_make Texpr1.Add

    let minus = bop_make Texpr1.Sub

    let to_linexpr = TexprToLinexpr.trans
  end

  module SubstMap = struct
    type t = SymExp.t option VarMap.t

    let add = VarMap.add

    let empty = VarMap.empty

    let fold = VarMap.fold

    let mem = VarMap.mem

    let singleton = VarMap.singleton

    let map_opt ~f x = VarMap.map (Option.value_map ~default:None ~f) x

    let to_arrays dummy x =
      let x =
        VarMap.fold x ~init:VarMap.empty ~f:(fun k v_opt acc ->
            Option.value_map v_opt ~default:acc ~f:(fun v -> VarMap.add k v acc) )
      in
      let keys = Array.create ~len:(VarMap.cardinal x) Var.dummy in
      let values = Array.create ~len:(VarMap.cardinal x) dummy in
      let n = ref 0 in
      VarMap.iter x ~f:(fun key value ->
          keys.(!n) <- key ;
          values.(!n) <- value ;
          n := !n + 1 ) ;
      (keys, values)


    let to_arrays_symexp = to_arrays SymExp.dummy

    let to_arrays_linexpr = to_arrays SymExp.linexpr_dummy

    let rec symexp_raw_subst subst_map x =
      match x with
      | Texpr1.Cst _ ->
          Some x
      | Texpr1.Var var ->
          if mem var subst_map then Option.map (VarMap.find var subst_map) ~f:Texpr1.to_expr
          else None
      | Texpr1.Unop (uop, re, typ, round) ->
          Option.map (symexp_raw_subst subst_map re) ~f:(fun re' ->
              Texpr1.Unop (uop, re', typ, round) )
      | Texpr1.Binop (bop, re1, re2, typ, round) ->
          Option.map2 (symexp_raw_subst subst_map re1) (symexp_raw_subst subst_map re2)
            ~f:(fun re1' re2' -> Texpr1.Binop (bop, re1', re2', typ, round) )


    let symexp_subst subst_map x =
      let re_opt = symexp_raw_subst subst_map (Texpr1.to_expr x) in
      Option.map re_opt ~f:SymExp.of_raw


    let symexp_subst_opt subst_map x_opt =
      Option.value_map x_opt ~default:None ~f:(symexp_subst subst_map)
  end

  module Constraints = struct
    type t = Tcons1.earray

    let empty = Tcons1.array_make Env.empty 0

    let singleton e =
      let tcons_array = Tcons1.array_make (Tcons1.get_env e) 1 in
      Tcons1.array_set tcons_array 0 e ; tcons_array


    let doubleton e1 e2 =
      let env = Env.join (Tcons1.get_env e1) (Tcons1.get_env e2) in
      let tcons_array = Tcons1.array_make env 2 in
      Tcons1.array_set tcons_array 0 e1 ; Tcons1.array_set tcons_array 1 e2 ; tcons_array


    let and_ x y =
      let env = Env.join (Tcons1.array_get_env x) (Tcons1.array_get_env y) in
      let x, y = (Tcons1.array_extend_environment x env, Tcons1.array_extend_environment y env) in
      let len1, len2 = (Tcons1.array_length x, Tcons1.array_length y) in
      let tcons_array = Tcons1.array_make env (len1 + len2) in
      for i = 0 to len1 - 1 do
        Tcons1.array_set tcons_array i (Tcons1.array_get x i)
      done ;
      for i = 0 to len2 - 1 do
        Tcons1.array_set tcons_array (len1 + i) (Tcons1.array_get y i)
      done ;
      tcons_array


    let eq_of var1 var2 =
      let sym_exp1, sym_exp2 = (SymExp.of_var var1, SymExp.of_var var2) in
      let sym_exp = SymExp.minus sym_exp1 sym_exp2 in
      singleton (Tcons1.make sym_exp Tcons1.EQ)


    let eq_of_sym sym1 sym_exp2 =
      Option.map (SymExp.of_sym sym1) ~f:(fun sym_exp1 ->
          let sym_exp = SymExp.minus sym_exp1 sym_exp2 in
          singleton (Tcons1.make sym_exp Tcons1.EQ) )


    let itv_of sym itv =
      match itv with
      | Bottom ->
          empty
      | NonBottom itv_pure ->
          let lb, ub = (Itv.ItvPure.lb itv_pure, Itv.ItvPure.ub itv_pure) in
          Option.value_map (SymExp.of_sym sym) ~default:empty ~f:(fun sym_exp ->
              let tcons_lb =
                Option.map (Itv.Bound.is_const lb) ~f:(fun lb ->
                    let sym_minus_lb = SymExp.minus sym_exp (SymExp.of_big_int lb) in
                    Tcons1.make sym_minus_lb Tcons1.SUPEQ )
              in
              let tcons_ub =
                Option.map (Itv.Bound.is_const ub) ~f:(fun ub ->
                    let ub_minus_sym = SymExp.minus (SymExp.of_big_int ub) sym_exp in
                    Tcons1.make ub_minus_sym Tcons1.SUPEQ )
              in
              match (tcons_lb, tcons_ub) with
              | Some tcons_lb, Some tcons_ub ->
                  doubleton tcons_lb tcons_ub
              | Some tcons, None | None, Some tcons ->
                  singleton tcons
              | None, None ->
                  empty )


    let of_raw_symexp re typ = singleton (Tcons1.make (SymExp.of_raw re) typ)

    let of_exp ~get_sym_f e : t =
      let of_bin_compare bop e1 e2 =
        match (SymExp.raw_of_exp ~get_sym_f e1, SymExp.raw_of_exp ~get_sym_f e2) with
        | Some re1, Some re2 ->
            of_raw_symexp (SymExp.raw_bop_make Texpr1.Sub re1 re2) bop
        | _, _ ->
            empty
      in
      match e with
      | Exp.Var _ | Exp.Lvar _ | Exp.Cast _ | Exp.Lfield _ | Exp.Lindex _ -> (
        match get_sym_f e |> Sym.get_var with
        | Some x ->
            of_raw_symexp (Texpr1.Var x) Tcons1.DISEQ
        | None ->
            empty )
      | Exp.BinOp (Binop.Eq, e1, e2) | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Ne, e1, e2), _) ->
          of_bin_compare Tcons1.EQ e1 e2
      | Exp.BinOp (Binop.Ne, e1, e2) | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Eq, e1, e2), _) ->
          of_bin_compare Tcons1.DISEQ e1 e2
      | Exp.BinOp (Binop.Gt, e1, e2)
      | Exp.BinOp (Binop.Lt, e2, e1)
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Le, e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Ge, e2, e1), _) ->
          of_bin_compare Tcons1.SUP e1 e2
      | Exp.BinOp (Binop.Ge, e1, e2)
      | Exp.BinOp (Binop.Le, e2, e1)
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Lt, e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Gt, e2, e1), _) ->
          of_bin_compare Tcons1.SUPEQ e1 e2
      | _ ->
          empty


    let vars_set_of x =
      let vars = ref VarSet.empty in
      for i = 0 to Tcons1.array_length x - 1 do
        vars := VarSet.union !vars (SymExp.vars_set_of (Tcons1.get_texpr1 (Tcons1.array_get x i)))
      done ;
      !vars


    let remove_strict_ineq_tcons1 x =
      match Tcons1.get_typ x with
      | Tcons1.SUP ->
          let e_minus_one = SymExp.minus (Tcons1.get_texpr1 x) SymExp.one in
          Tcons1.make e_minus_one Tcons1.SUPEQ
      | _ ->
          x


    let remove_strict_ineq x =
      let length = Tcons1.array_length x in
      let x' = Tcons1.array_make (Tcons1.array_get_env x) length in
      for i = 0 to length - 1 do
        let tcons1 = Tcons1.array_get x i in
        Tcons1.array_set x' i (remove_strict_ineq_tcons1 tcons1)
      done ;
      x'
  end

  module Val = struct
    type t = Manager.domain_t Abstract1.t

    let compare = Compares.compare_abstract1

    let bot = Abstract1.bottom man Env.empty

    let top = Abstract1.top man Env.empty

    let is_bot x = Abstract1.is_bottom man x

    let is_top x = Abstract1.is_top man x

    let sync_env x y =
      let x, y = (Abstract1.minimize_environment man x, Abstract1.minimize_environment man y) in
      let env = Env.join (Abstract1.env x) (Abstract1.env y) in
      let x = Abstract1.change_environment man x env false in
      let y = Abstract1.change_environment man y env false in
      (x, y)


    let extend_env env x =
      let x = Abstract1.minimize_environment man x in
      let new_env = Env.join (Abstract1.env x) env in
      Abstract1.change_environment man x new_env false


    let sync_env_lift f x y =
      let x, y = sync_env x y in
      f x y


    let join = sync_env_lift (Abstract1.join man)

    let meet = sync_env_lift (Abstract1.meet man)

    let widen ~prev ~next ~num_iters:_ = sync_env_lift (Abstract1.widening man) prev next

    let pp fmt x = Abstract1.print fmt x

    let ( <= ) ~lhs ~rhs = sync_env_lift (Abstract1.is_leq man) lhs rhs

    let sat_tcons tcons x =
      let tcons = Constraints.remove_strict_ineq_tcons1 tcons in
      let x = extend_env (Tcons1.get_env tcons) x in
      Abstract1.sat_tcons man x tcons


    let is_unsat_constraint constr x =
      let symexp = Tcons1.get_texpr1 constr in
      let typ = Tcons1.get_typ constr in
      let neg_constr_opt =
        match typ with
        | Tcons1.EQ ->
            Some (Tcons1.make symexp Tcons1.DISEQ)
        | Tcons1.DISEQ ->
            Some (Tcons1.make symexp Tcons1.EQ)
        | Tcons1.SUPEQ | Tcons1.SUP -> (
            let env = Tcons1.get_env constr in
            let neg_symexp =
              Texpr1.of_expr env (SymExp.raw_uop_make Texpr1.Neg (Texpr1.to_expr symexp))
            in
            match typ with
            | Tcons1.SUPEQ ->
                Some (Tcons1.make neg_symexp Tcons1.SUP)
            | Tcons1.SUP ->
                Some (Tcons1.make neg_symexp Tcons1.SUPEQ)
            | _ ->
                assert false )
        | _ ->
            None
      in
      Option.value_map neg_constr_opt ~default:false ~f:(fun neg_constr -> sat_tcons neg_constr x)


    let is_unsat_constraints constrs x =
      let sat = ref true in
      for i = 0 to Tcons1.array_length constrs - 1 do
        let constr = Tcons1.array_get constrs i in
        if is_unsat_constraint constr x then sat := false
      done ;
      not !sat


    let meet_constraints constrs x =
      let constrs = Constraints.remove_strict_ineq constrs in
      let x = extend_env (Tcons1.array_get_env constrs) x in
      if is_unsat_constraints constrs x then bot else Abstract1.meet_tcons_array man x constrs


    let forget_vars_array vars x =
      let x = extend_env (Env.of_vars_array vars) x in
      Abstract1.forget_array man x vars false


    let forget_vars_set vars x = forget_vars_array (VarSet.to_array vars) x

    let assign_vars vars texpr x =
      let x = extend_env (Env.join (Texpr1.get_env texpr) (Env.of_vars_array vars)) x in
      let texprs = Array.create ~len:(Array.length vars) texpr in
      Abstract1.assign_texpr_array man x vars texprs None


    let store_relation var_array_of_powloc locs texpr_opt x =
      let vars = var_array_of_powloc locs in
      if can_strong_update locs then
        match texpr_opt with
        | Some texpr ->
            assign_vars vars texpr x
        | None ->
            forget_vars_array vars x
      else forget_vars_array vars x


    let store_relation_int locs texpr_opt x =
      store_relation Var.int_array_of_powloc locs texpr_opt x


    let store_relation_offset locs texpr_opt x =
      store_relation Var.offset_array_of_powloc locs texpr_opt x


    let store_relation_size locs texpr_opt x =
      store_relation Var.size_array_of_powloc locs texpr_opt x


    let forget_var var x = forget_vars_array (Var.array_of_var var) x

    let lt_sat e1 e2 x = sat_tcons (Tcons1.make (SymExp.minus e2 e1) Tcons1.SUP) x

    let le_sat e1 e2 x = sat_tcons (Tcons1.make (SymExp.minus e2 e1) Tcons1.SUPEQ) x

    let subst : forget_free:bool -> SubstMap.t -> t -> t =
      let forget_free_vars vars_in_subst_map x =
        let free_vars = VarSet.diff (Env.to_vars_set (Abstract1.env x)) vars_in_subst_map in
        let free_vars = VarSet.remove Var.return free_vars in
        forget_vars_set free_vars x
      in
      let filter_vars_to_none subst_map x =
        let vars_to_none =
          SubstMap.fold subst_map ~init:VarSet.empty ~f:(fun k v acc ->
              match v with None -> VarSet.add k acc | Some _ -> acc )
        in
        forget_vars_set vars_to_none x
      in
      let extend_and_substitute subst_map to_arrays new_env substitute x =
        let x = filter_vars_to_none subst_map x in
        let vars, exps = to_arrays subst_map in
        let x = extend_env new_env x in
        substitute man x vars exps None
      in
      fun ~forget_free subst_map x ->
        let vars_in_subst_map =
          SubstMap.fold subst_map ~init:VarSet.empty ~f:(fun var sym_exp_opt acc ->
              acc |> VarSet.add var
              |> VarSet.add (Var.param_of var)
              |> VarSet.union (SymExp.vars_set_of_opt sym_exp_opt) )
        in
        let new_env = Env.of_vars_set vars_in_subst_map in
        let x = if forget_free then forget_free_vars vars_in_subst_map x else x in
        match Config.bo_relational_domain with
        | None ->
            assert false
        | Some `Bo_relational_domain_oct ->
            extend_and_substitute subst_map SubstMap.to_arrays_symexp new_env
              Abstract1.substitute_texpr_array x
        | Some `Bo_relational_domain_poly ->
            let subst_map = SubstMap.map_opt ~f:SymExp.to_linexpr subst_map in
            extend_and_substitute subst_map SubstMap.to_arrays_linexpr new_env
              Abstract1.substitute_linexpr_array x
  end

  module Pack = struct
    type t = int [@@deriving compare]

    let equal = Int.equal

    let pp fmt x = F.fprintf fmt "%d" x

    let subst ~from ~to_ x = if equal x from then to_ else x
  end

  module PackSet = struct
    include PrettyPrintable.MakePPSet (Pack)

    let subst ~from ~to_ x = if mem x from then to_ else x
  end

  module PackMap = struct
    include PrettyPrintable.MakePPMap (Pack)

    let remove_packs pack_ids x = PackSet.fold remove pack_ids x
  end

  module PackedVal = struct
    type t = {pack_ids: Pack.t VarMap.t; packs: Val.t PackMap.t} [@@deriving compare]

    let empty = {pack_ids= VarMap.empty; packs= PackMap.empty}

    let pp_packs = PackMap.pp ~pp_value:Val.pp

    let pp fmt x = pp_packs fmt x.packs

    let sync_pack x y =
      let id_ref = ref 0 in
      let get_new_id () : Pack.t =
        id_ref := !id_ref + 1 ;
        !id_ref
      in
      let add_subst_partial var id (pack_ids, subst) =
        match PackMap.find id subst with
        | id' ->
            (VarMap.add var id' pack_ids, subst)
        | exception Caml.Not_found ->
            let id' = get_new_id () in
            (VarMap.add var id' pack_ids, PackMap.add id id' subst)
      in
      let add_subst var id1_opt id2_opt ((pack_ids, subst1, subst2) as subst_res) =
        match (id1_opt, id2_opt) with
        | Some id1, Some id2 -> (
          match (PackMap.find_opt id1 subst1, PackMap.find_opt id2 subst2) with
          | Some id1', Some id2' ->
              if Pack.equal id1' id2' then (VarMap.add var id1' pack_ids, subst1, subst2)
              else
                let pack_ids =
                  pack_ids |> VarMap.add var id1' |> VarMap.map (Pack.subst ~from:id2' ~to_:id1')
                in
                let subst1 = PackMap.map (Pack.subst ~from:id2' ~to_:id1') subst1 in
                let subst2 = PackMap.map (Pack.subst ~from:id2' ~to_:id1') subst2 in
                (pack_ids, subst1, subst2)
          | Some id1', None ->
              (VarMap.add var id1' pack_ids, subst1, PackMap.add id2 id1' subst2)
          | None, Some id2' ->
              (VarMap.add var id2' pack_ids, PackMap.add id1 id2' subst1, subst2)
          | None, None ->
              let id' = get_new_id () in
              (VarMap.add var id' pack_ids, PackMap.add id1 id' subst1, PackMap.add id2 id' subst2)
          )
        | Some id1, None ->
            let pack_ids, subst1 = add_subst_partial var id1 (pack_ids, subst1) in
            (pack_ids, subst1, subst2)
        | None, Some id2 ->
            let pack_ids, subst2 = add_subst_partial var id2 (pack_ids, subst2) in
            (pack_ids, subst1, subst2)
        | None, None ->
            subst_res
      in
      let get_subst_map pack_ids1 pack_ids2 =
        VarMap.fold2 pack_ids1 pack_ids2 ~f:add_subst
          ~init:(VarMap.empty, PackMap.empty, PackMap.empty)
      in
      let subst subst_map packs =
        let subst_helper pack_id v acc =
          match PackMap.find pack_id subst_map with
          | pack_id' ->
              let v =
                Option.value_map (PackMap.find_opt pack_id' acc) ~default:v ~f:(fun v' ->
                    Val.meet v' v )
              in
              PackMap.add pack_id' v acc
          | exception Caml.Not_found ->
              acc
        in
        PackMap.fold subst_helper packs PackMap.empty
      in
      let pack_ids, subst_map_x, subst_map_y = get_subst_map x.pack_ids y.pack_ids in
      (pack_ids, subst subst_map_x x.packs, subst subst_map_y y.packs)


    let le_synced_packs ~lhs ~rhs =
      let ge_than_lhs pack_id rhs =
        match PackMap.find pack_id lhs with
        | lhs ->
            Val.( <= ) ~lhs ~rhs
        | exception Caml.Not_found ->
            Val.is_top rhs
      in
      PackMap.for_all ge_than_lhs rhs


    let join_synced_packs x y =
      let join_opt _ v1_opt v2_opt = Option.map2 v1_opt v2_opt ~f:Val.join in
      PackMap.merge join_opt x y


    let meet_synced_packs x y =
      let exception BottomByMeet in
      let meet_opt _ v1_opt v2_opt =
        match (v1_opt, v2_opt) with
        | Some v1, Some v2 ->
            let v = Val.meet v1 v2 in
            if Val.is_bot v then raise BottomByMeet else Some v
        | Some v, None | None, Some v ->
            Some v
        | None, None ->
            None
      in
      match PackMap.merge meet_opt x y with packs -> Some packs | exception BottomByMeet -> None


    let widen_synced_packs ~prev ~next ~num_iters =
      let widen_opt _ prev_opt next_opt =
        Option.map2 prev_opt next_opt ~f:(fun prev next -> Val.widen ~prev ~next ~num_iters)
      in
      PackMap.merge widen_opt prev next


    let ( <= ) ~lhs ~rhs =
      let _, packs_lhs, packs_rhs = sync_pack lhs rhs in
      le_synced_packs ~lhs:packs_lhs ~rhs:packs_rhs


    let join x y =
      let pack_ids, packs_x, packs_y = sync_pack x y in
      if le_synced_packs ~lhs:packs_x ~rhs:packs_y then y
      else if le_synced_packs ~lhs:packs_y ~rhs:packs_x then x
      else {pack_ids; packs= join_synced_packs packs_x packs_y}


    let meet x y =
      let pack_ids, packs_x, packs_y = sync_pack x y in
      if le_synced_packs ~lhs:packs_x ~rhs:packs_y then NonBottom x
      else if le_synced_packs ~lhs:packs_y ~rhs:packs_x then NonBottom y
      else
        Option.value_map (meet_synced_packs packs_x packs_y) ~default:Bottom ~f:(fun packs ->
            NonBottom {pack_ids; packs} )


    let widen ~prev ~next ~num_iters =
      let pack_ids, packs_prev, packs_next = sync_pack prev next in
      {pack_ids; packs= widen_synced_packs ~prev:packs_prev ~next:packs_next ~num_iters}


    let pack_id_of_var var x = VarMap.find var x.pack_ids

    let pack_id_of_var_opt var x = VarMap.find_opt var x.pack_ids

    let pack_ids_of_vars vars x =
      let add_id var acc =
        Option.value_map (pack_id_of_var_opt var x) ~default:acc ~f:(fun id -> PackSet.add id acc)
      in
      VarSet.fold add_id vars PackSet.empty


    let val_of_pack_id_opt pack_id x = PackMap.find_opt pack_id x.packs

    let val_of_pack_id pack_id x = Option.value (val_of_pack_id_opt pack_id x) ~default:Val.top

    let val_of_pack_ids ids x =
      let meet_val id acc =
        Option.value_map (val_of_pack_id_opt id x) ~default:acc ~f:(fun v -> Val.meet acc v)
      in
      PackSet.fold meet_val ids Val.top


    let val_of_vars vars x = val_of_pack_ids (pack_ids_of_vars vars x) x

    let lt_sat e1 e2 x =
      let vars = VarSet.union (SymExp.vars_set_of e1) (SymExp.vars_set_of e2) in
      Val.lt_sat e1 e2 (val_of_vars vars x)


    let le_sat e1 e2 x =
      let vars = VarSet.union (SymExp.vars_set_of e1) (SymExp.vars_set_of e2) in
      Val.le_sat e1 e2 (val_of_vars vars x)


    let lift_sat_opt f e1_opt e2_opt x =
      match (e1_opt, e2_opt) with Some e1, Some e2 -> f e1 e2 x | _, _ -> false


    let lt_sat_opt = lift_sat_opt lt_sat

    let le_sat_opt = lift_sat_opt le_sat

    let repack_with_vars vars x =
      let id_ref =
        let max_pack_id =
          VarMap.fold x.pack_ids ~init:0 ~f:(fun _ pack_id acc -> max acc pack_id)
        in
        ref max_pack_id
      in
      let get_new_id () : Pack.t =
        id_ref := !id_ref + 1 ;
        !id_ref
      in
      let set_pack_id_of_vars vars id pack_ids =
        VarSet.fold (fun var acc -> VarMap.add var id acc) vars pack_ids
      in
      let vars_ids = pack_ids_of_vars vars x in
      match PackSet.is_singleton_or_more vars_ids with
      | IContainer.Empty ->
          let id = get_new_id () in
          {x with pack_ids= set_pack_id_of_vars vars id x.pack_ids}
      | IContainer.Singleton id ->
          {x with pack_ids= set_pack_id_of_vars vars id x.pack_ids}
      | IContainer.More ->
          let id = PackSet.min_elt vars_ids in
          let other_ids = PackSet.remove id vars_ids in
          let pack_ids =
            x.pack_ids |> set_pack_id_of_vars vars id
            |> VarMap.map (PackSet.subst ~from:other_ids ~to_:id)
          in
          let packs =
            let v = val_of_pack_ids vars_ids x in
            x.packs |> PackMap.remove_packs other_ids |> PackMap.add id v
          in
          {pack_ids; packs}


    let subst ~forget_free subst_map x =
      let exception BottomBySubst in
      let repack_for_subst subst_map x =
        SubstMap.fold subst_map ~init:x ~f:(fun var sym_exp_opt acc ->
            let vars = VarSet.add var (SymExp.vars_set_of_opt sym_exp_opt) in
            repack_with_vars vars acc )
      in
      let pack_subst_map x subst_map =
        let add_subst var exp acc =
          let pack_id = pack_id_of_var var x in
          match PackMap.find pack_id acc with
          | subst_map ->
              PackMap.add pack_id (SubstMap.add var exp subst_map) acc
          | exception Caml.Not_found ->
              PackMap.add pack_id (SubstMap.singleton var exp) acc
        in
        SubstMap.fold subst_map ~init:PackMap.empty ~f:add_subst
      in
      let do_subst packed_subst_map pack_id v acc =
        let subst_map =
          Option.value (PackMap.find_opt pack_id packed_subst_map) ~default:SubstMap.empty
        in
        let v = Val.subst ~forget_free subst_map v in
        if Val.is_bot v then raise BottomBySubst else PackMap.add pack_id v acc
      in
      let x = repack_for_subst subst_map x in
      let packed_subst_map = pack_subst_map x subst_map in
      match PackMap.fold (do_subst packed_subst_map) x.packs PackMap.empty with
      | packs ->
          NonBottom {x with packs}
      | exception BottomBySubst ->
          Bottom


    let meet_constraints constrs x =
      let vars = Constraints.vars_set_of constrs in
      if VarSet.is_empty vars then
        if Val.is_unsat_constraints constrs Val.top then Bottom else NonBottom x
      else
        let x = repack_with_vars vars x in
        let pack_id = pack_id_of_var (VarSet.choose vars) x in
        let v = Val.meet_constraints constrs (val_of_pack_id pack_id x) in
        if Val.is_bot v then Bottom else NonBottom {x with packs= PackMap.add pack_id v x.packs}


    let store_relation locs (int_exp, offset_exp, size_exp) x =
      let store_relation' varset_of_locs exp_opt val_store_relation x =
        let vars_of_exp = VarSet.union (varset_of_locs locs) (SymExp.vars_set_of_opt exp_opt) in
        let x = repack_with_vars vars_of_exp x in
        let pack_id = pack_id_of_var (VarSet.choose vars_of_exp) x in
        let v = val_store_relation locs exp_opt (val_of_pack_id pack_id x) in
        if Val.is_bot v then Bottom else NonBottom {x with packs= PackMap.add pack_id v x.packs}
      in
      let ( ||> ) x f = match x with Bottom -> Bottom | NonBottom x -> f x in
      if PowLoc.is_empty locs then NonBottom x
      else
        store_relation' VarSet.int_of_powloc int_exp Val.store_relation_int x
        ||> store_relation' VarSet.offset_of_powloc offset_exp Val.store_relation_offset
        ||> store_relation' VarSet.size_of_powloc size_exp Val.store_relation_size


    let forget_var var x =
      let forget_var_in pack_id =
        let pack_ids = VarMap.remove var x.pack_ids in
        let packs =
          Option.value_map (val_of_pack_id_opt pack_id x) ~default:x.packs ~f:(fun v ->
              PackMap.add pack_id (Val.forget_var var v) x.packs )
        in
        {pack_ids; packs}
      in
      Option.value_map (pack_id_of_var_opt var x) ~default:x ~f:forget_var_in


    let forget_loc loc x = forget_var (Var.of_loc loc) x

    let forget_locs locs x = PowLoc.fold forget_loc locs x

    let forget_vars vars x = VarSet.fold forget_var vars x

    let init_param loc x =
      let param_var = Var.param_of_loc loc in
      let var = Var.of_loc loc in
      meet_constraints (Constraints.eq_of param_var var) x


    let init_array allocsite ~offset_opt ~size ~size_exp_opt x =
      let size_sym = Sym.of_allocsite_size allocsite in
      let size_constrs =
        match size_exp_opt with
        | None ->
            Constraints.itv_of size_sym size
        | Some size_exp -> (
          match Constraints.eq_of_sym size_sym size_exp with
          | None ->
              Constraints.itv_of size_sym size
          | Some constr ->
              constr )
      in
      let constraints =
        match offset_opt with
        | Some offset ->
            let offset_sym = Sym.of_allocsite_offset allocsite in
            let offset_constrs = Constraints.itv_of offset_sym offset in
            Constraints.and_ offset_constrs size_constrs
        | None ->
            size_constrs
      in
      meet_constraints constraints x


    let subst_param_caller subst_map caller =
      let accum_rev_if_var k v acc =
        Option.value_map (SymExp.to_var v) ~default:acc ~f:(fun k' ->
            SubstMap.add k' (Some (SymExp.of_var (Var.temp_param_of k))) acc )
      in
      let accum_rev k v_opt acc =
        Option.value_map v_opt ~default:acc ~f:(fun v -> accum_rev_if_var k v acc)
      in
      let rev_subst_map = SubstMap.fold subst_map ~init:SubstMap.empty ~f:accum_rev in
      subst ~forget_free:false rev_subst_map caller


    let subst_callee subst_map callee =
      let accum_param_subst k v acc =
        let v =
          match v with
          | Some v' when SymExp.is_var v' ->
              Some (SymExp.of_var (Var.temp_param_of k))
          | _ ->
              v
        in
        SubstMap.add (Var.param_of k) v acc
      in
      let param_subst_map = SubstMap.fold subst_map ~init:SubstMap.empty ~f:accum_param_subst in
      subst ~forget_free:true param_subst_map callee


    let forget_temp_param subst_map x =
      let temps =
        SubstMap.fold subst_map ~init:VarSet.empty ~f:(fun k _ acc ->
            VarSet.add (Var.temp_param_of k) acc )
      in
      forget_vars temps x


    let instantiate ~caller ~callee subst_map =
      match subst_param_caller subst_map caller with
      | Bottom ->
          Bottom
      | NonBottom caller -> (
        match subst_callee subst_map callee with
        | Bottom ->
            Bottom
        | NonBottom callee -> (
          match meet caller callee with
          | Bottom ->
              Bottom
          | NonBottom relation ->
              NonBottom (forget_temp_param subst_map relation) ) )
  end

  include AbstractDomain.BottomLifted (PackedVal)

  let compare x y =
    match (x, y) with
    | Bottom, Bottom ->
        0
    | Bottom, _ ->
        -1
    | _, Bottom ->
        1
    | NonBottom x', NonBottom y' ->
        PackedVal.compare x' y'


  let empty : t = NonBottom PackedVal.empty

  let bot : t = Bottom

  let is_unsat : t -> bool = function Bottom -> true | NonBottom _ -> false

  let lift_default : default:'a -> (PackedVal.t -> 'a) -> t -> 'a =
   fun ~default f -> function Bottom -> default | NonBottom x -> f x


  let lift : (PackedVal.t -> PackedVal.t) -> t -> t =
   fun f -> function Bottom -> Bottom | NonBottom x -> NonBottom (f x)


  let lift2 : (PackedVal.t -> PackedVal.t -> t) -> t -> t -> t =
   fun f x y ->
    match (x, y) with Bottom, _ | _, Bottom -> Bottom | NonBottom x', NonBottom y' -> f x' y'


  let lt_sat_opt : SymExp.t option -> SymExp.t option -> t -> bool =
   fun e1_opt e2_opt -> lift_default ~default:true (PackedVal.lt_sat_opt e1_opt e2_opt)


  let le_sat_opt : SymExp.t option -> SymExp.t option -> t -> bool =
   fun e1_opt e2_opt -> lift_default ~default:true (PackedVal.le_sat_opt e1_opt e2_opt)


  let meet_constraints : Constraints.t -> t -> t =
   fun constrs -> lift_default ~default:Bottom (PackedVal.meet_constraints constrs)


  let store_relation : PowLoc.t -> SymExp.t option * SymExp.t option * SymExp.t option -> t -> t =
   fun locs texpr_opts -> lift_default ~default:Bottom (PackedVal.store_relation locs texpr_opts)


  let init_param : Loc.t -> t -> t =
   fun loc -> lift_default ~default:Bottom (PackedVal.init_param loc)


  let init_array :
         Allocsite.t
      -> offset_opt:Itv.t option
      -> size:Itv.t
      -> size_exp_opt:SymExp.t option
      -> t
      -> t =
   fun allocsite ~offset_opt ~size ~size_exp_opt ->
    lift_default ~default:Bottom (PackedVal.init_array allocsite ~offset_opt ~size ~size_exp_opt)


  let forget_locs : PowLoc.t -> t -> t = fun locs -> lift (PackedVal.forget_locs locs)

  let instantiate : caller:t -> callee:t -> SubstMap.t -> t =
   fun ~caller ~callee subst_map ->
    lift2 (fun caller callee -> PackedVal.instantiate ~caller ~callee subst_map) caller callee
end

module ApronOctagonManager = struct
  type domain_t = Oct.t

  let alloc_man : unit -> domain_t Manager.t = Oct.manager_alloc
end

module ElinaPolyManager = struct
  type domain_t = Elina_poly.loose Elina_poly.t

  let alloc_man : unit -> domain_t Manager.t = Elina_poly.manager_alloc_loose
end

include ( val match Config.bo_relational_domain with
              | None ->
                  (module NoRelation : S)
              | Some `Bo_relational_domain_oct ->
                  (module Make (ApronOctagonManager) : S)
              | Some `Bo_relational_domain_poly ->
                  (module Make (ElinaPolyManager) : S) )

(* NOTE: Globally only one manager (of a relational domain depends on
   Apron) can set deserialization functions. *)
let () = set_deserialize ()
