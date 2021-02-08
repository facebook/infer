(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Interval abstract domain *)

open Apron

let equal_apron_typ =
  (* Apron.Texpr1.typ is a sum of nullary constructors *)
  Poly.equal

(** Apron-managed map from variables to intervals *)
type t = Box.t Abstract1.t

let equal : t -> t -> bool = Poly.equal
let compare : t -> t -> int = Poly.compare
let man = lazy (Box.manager_alloc ())
let join l r = Some (Abstract1.join (Lazy.force man) l r)
let is_false x = Abstract1.is_bottom (Lazy.force man) x

let bindings (itv : t) =
  let itv = Abstract1.minimize_environment (Lazy.force man) itv in
  let box = Abstract1.to_box (Lazy.force man) itv in
  let vars =
    Environment.vars box.box1_env |> fun (i, r) -> Array.append i r
  in
  Array.combine_exn vars box.interval_array

let sexp_of_t (itv : t) =
  let sexps =
    Array.fold_right (bindings itv) [] ~f:(fun (v, {inf; sup}) acc ->
        Sexp.List
          [ Sexp.Atom (Var.to_string v)
          ; Sexp.Atom (Scalar.to_string inf)
          ; Sexp.Atom (Scalar.to_string sup) ]
        :: acc )
  in
  Sexp.List sexps

let pp fs =
  let pp_pair a_pp b_pp fs (a, b) =
    Format.fprintf fs "@[(%a@,%a)@]" a_pp a b_pp b
  in
  bindings >> Array.pp "@," (pp_pair Var.print Interval.print) fs

let report_fmt_thunk = Fun.flip pp
let init _gs = Abstract1.top (Lazy.force man) (Environment.make [||] [||])
let apron_var_of_name = (fun nm -> "%" ^ nm) >> Apron.Var.of_string
let apron_var_of_reg = Llair.Reg.name >> apron_var_of_name

let rec apron_typ_of_llair_typ : Llair.Typ.t -> Texpr1.typ option = function
  | Pointer {elt= _} -> apron_typ_of_llair_typ Llair.Typ.siz
  | Integer {bits= _} -> Some Texpr1.Int
  | Float {bits= 32; enc= `IEEE} -> Some Texpr1.Single
  | Float {bits= 64; enc= `IEEE} -> Some Texpr1.Double
  | Float {bits= 80; enc= `Extended} -> Some Texpr1.Extended
  | Float {bits= 128; enc= `IEEE} -> Some Texpr1.Quad
  | t ->
      warn "No corresponding apron type for llair type %a " Llair.Typ.pp t
        () ;
      None

let rec apron_texpr_of_llair_exp exp q =
  match (exp : Llair.Exp.t) with
  | Reg {name} | Global {name} | Function {name} ->
      Some (Texpr1.Var (apron_var_of_name name))
  | Integer {data} -> Some (Texpr1.Cst (Coeff.s_of_int (Z.to_int data)))
  | Float {data} -> (
    match Float.of_string_exn data with
    | f -> Some (Texpr1.Cst (Coeff.s_of_float f))
    | exception Invalid_argument _ -> None )
  | Ap1 (Signed {bits}, _, _) ->
      let n = Int.shift_left 1 (bits - 1) in
      Some (Texpr1.Cst (Coeff.i_of_int (-n) (n - 1)))
  | Ap1 (Unsigned {bits}, _, _) ->
      let n = Int.shift_left 1 (bits - 1) in
      Some (Texpr1.Cst (Coeff.i_of_int 0 n))
  | Ap1 (Convert {src}, dst, x) ->
      let* src' = apron_typ_of_llair_typ src in
      let* dst' = apron_typ_of_llair_typ dst in
      let subtm = apron_texpr_of_llair_exp x q in
      if equal_apron_typ src' dst' then subtm
      else
        let+ t = subtm in
        Texpr1.Unop (Texpr1.Cast, t, dst', Texpr0.Rnd)
  | Ap2 (op, typ, x, y) -> (
      let* typ' = apron_typ_of_llair_typ typ in
      let* x' = apron_texpr_of_llair_exp x q in
      let* y' = apron_texpr_of_llair_exp y q in
      (* abstract evaluation of boolean binary operation [te1 op te2] at [q]
         by translation to [te1 - te2 op 0] and intersection with [q] *)
      let bool_binop q op x' y' =
        let env = Abstract1.env q in
        let lhs = Texpr1.Binop (Texpr1.Sub, x', y', typ', Texpr0.Rnd) in
        let tcons = Tcons1.make (Texpr1.of_expr env lhs) op in
        let ea =
          Tcons1.array_make env 1 $> fun ea -> Tcons1.array_set ea 0 tcons
        in
        (* if meet of q with tree constraint encoding of binop is: (bottom
           -> binop definitely false); (unchanged from q -> binop definitely
           true); (else -> binop may be true or false) *)
        let meet = Abstract1.meet_tcons_array (Lazy.force man) q ea in
        if is_false meet then Some (Texpr1.Cst (Coeff.s_of_int 0))
        else if equal meet q then Some (Texpr1.Cst (Coeff.s_of_int (-1)))
        else Some (Texpr1.Cst (Coeff.i_of_int (-1) 0))
      in
      let arith_bop op x' y' =
        Some (Texpr1.Binop (op, x', y', typ', Texpr0.Rnd))
      in
      match op with
      | Eq -> bool_binop q Tcons0.EQ x' y'
      | Dq -> bool_binop q Tcons0.DISEQ x' y'
      | Gt -> bool_binop q Tcons0.SUP x' y'
      | Ge -> bool_binop q Tcons0.SUPEQ x' y'
      | Lt -> bool_binop q Tcons0.SUP y' x'
      | Le -> bool_binop q Tcons0.SUPEQ y' x'
      | Ugt | Uge | Ult | Ule | Ord | Uno -> None
      | Add -> arith_bop Texpr1.Add x' y'
      | Sub -> arith_bop Texpr1.Sub x' y'
      | Mul -> arith_bop Texpr1.Mul x' y'
      | Div -> arith_bop Texpr1.Div x' y'
      | Rem -> arith_bop Texpr1.Mod x' y'
      | Udiv | Urem -> None
      | And | Or | Xor | Shl | Lshr | Ashr | Update _ -> None )
  | Label _
   |Ap1 ((Splat | Select _), _, _)
   |Ap3 (Conditional, _, _, _, _)
   |ApN (Record, _, _) ->
      None

let assign reg exp q =
  [%Trace.call fun {pf} ->
    pf "@ {%a}@\n%a := %a" pp q Llair.Reg.pp reg Llair.Exp.pp exp]
  ;
  let lval = apron_var_of_reg reg in
  ( match apron_texpr_of_llair_exp exp q with
  | Some e ->
      let env = Abstract1.env q in
      let new_env =
        match
          ( Environment.mem_var env lval
          , apron_typ_of_llair_typ (Llair.Reg.typ reg) )
        with
        | true, _ -> env
        | false, Some Texpr1.Int -> Environment.add env [|lval|] [||]
        | false, _ -> Environment.add env [||] [|lval|]
      in
      let man = Lazy.force man in
      let q = Abstract1.change_environment man q new_env true in
      Abstract1.assign_texpr man q lval (Texpr1.of_expr new_env e) None
  | _ -> q )
  |>
  [%Trace.retn fun {pf} r -> pf "{%a}" pp r]

(** block if [e] is known to be false; skip otherwise *)
let exec_assume q e =
  match apron_texpr_of_llair_exp e q with
  | Some e ->
      let cond =
        Abstract1.bound_texpr (Lazy.force man) q (Texpr1.of_expr q.env e)
      in
      if Interval.is_zero cond then None else Some q
  | _ -> Some q

(** existentially quantify killed register [r] out of state [q] *)
let exec_kill r q =
  let apron_v = apron_var_of_reg r in
  if Environment.mem_var (Abstract1.env q) apron_v then
    Abstract1.forget_array (Lazy.force man) q [|apron_v|] false
  else q

(** perform a series [move_vec] of reg:=exp moves at state [q] *)
let exec_move move_vec q =
  let defs, uses =
    IArray.fold move_vec (Llair.Reg.Set.empty, Llair.Reg.Set.empty)
      ~f:(fun (r, e) (defs, uses) ->
        ( Llair.Reg.Set.add r defs
        , Llair.Exp.fold_regs ~f:Llair.Reg.Set.add e uses ) )
  in
  if not (Llair.Reg.Set.disjoint defs uses) then
    todo "overwritten variables in Domain_itv" () ;
  IArray.fold ~f:(fun (r, e) q -> assign r e q) move_vec q

let exec_inst i q =
  match (i : Llair.inst) with
  | Move {reg_exps; loc= _} -> Some (exec_move reg_exps q)
  | Store {ptr; exp; len= _; loc= _} -> (
    match Llair.Reg.of_exp ptr with
    | Some reg -> Some (assign reg exp q)
    | None -> Some q )
  | Load {reg; ptr; len= _; loc= _} -> Some (assign reg ptr q)
  | Nondet {reg= Some reg; msg= _; loc= _} -> Some (exec_kill reg q)
  | Nondet {reg= None; msg= _; loc= _} | Alloc _ | Free _ -> Some q
  | Abort _ -> None
  | Intrinsic {reg= Some reg; _} -> Some (exec_kill reg q)
  | Intrinsic {reg= None; _} -> Some q

type from_call = {areturn: Llair.Reg.t option; caller_q: t}
[@@deriving sexp_of]

let recursion_beyond_bound = `prune

(** existentially quantify locals *)
let post locals _ (q : t) =
  let locals =
    Llair.Reg.Set.fold locals [] ~f:(fun r a ->
        let v = apron_var_of_reg r in
        if Environment.mem_var q.env v then v :: a else a )
    |> Array.of_list
  in
  Abstract1.forget_array (Lazy.force man) q locals false

(** drop caller-local variables, add returned value to caller state *)
let retn _ freturn {areturn; caller_q} callee_q =
  match (areturn, freturn) with
  | Some aret, Some fret ->
      let env_fret_only =
        match apron_typ_of_llair_typ (Llair.Reg.typ fret) with
        | None -> Environment.make [||] [||]
        | Some Texpr1.Int -> Environment.make [|apron_var_of_reg fret|] [||]
        | _ -> Environment.make [||] [|apron_var_of_reg fret|]
      in
      let env = Environment.lce env_fret_only (Abstract1.env caller_q) in
      let man = Lazy.force man in
      let callee_fret =
        (* drop all callee vars, scope to (caller + freturn) env *)
        Abstract1.change_environment man callee_q env_fret_only false
        |> fun q -> Abstract1.change_environment man q env false
      in
      let caller_q = Abstract1.change_environment man caller_q env false in
      let result = Abstract1.meet man callee_fret caller_q in
      Abstract1.rename_array man result
        [|apron_var_of_reg fret|]
        [|apron_var_of_reg aret|]
  | Some aret, None -> exec_kill aret caller_q
  | None, _ -> caller_q

(** map actuals to formals (via temporary registers), stash constraints on
    caller-local variables. Note that this exploits the non-relational-ness
    of Box to ignore all variables other than the formal/actual params/
    returns; this will not be possible if extended to a relational domain *)
let call ~summaries ~globals:_ ~actuals ~areturn ~formals ~freturn:_
    ~locals:_ q =
  if summaries then
    todo "Summaries not yet implemented for interval analysis" ()
  else
    let mangle r =
      Llair.Reg.mk (Llair.Reg.typ r) 0 ("__tmp__" ^ Llair.Reg.name r)
    in
    let args = IArray.combine_exn formals actuals in
    let q' =
      IArray.fold ~f:(fun (f, a) q -> assign (mangle f) a q) args q
    in
    let callee_env =
      IArray.fold formals ([], []) ~f:(fun f (is, fs) ->
          match apron_typ_of_llair_typ (Llair.Reg.typ f) with
          | None -> (is, fs)
          | Some Texpr1.Int -> (apron_var_of_reg (mangle f) :: is, fs)
          | _ -> (is, apron_var_of_reg (mangle f) :: fs) )
      |> fun (is, fs) ->
      Environment.make (Array.of_list is) (Array.of_list fs)
    in
    let man = Lazy.force man in
    let q'' = Abstract1.change_environment man q' callee_env false in
    let q''' =
      Abstract1.rename_array man q''
        (Array.map
           ~f:(mangle >> apron_var_of_reg)
           (IArray.to_array formals))
        (Array.map ~f:apron_var_of_reg (IArray.to_array formals))
    in
    (q''', {areturn; caller_q= q})

let dnf q = [q]
let resolve_callee _ _ _ = []

type summary = t

let pp_summary = pp
let apply_summary _ _ = None
let create_summary ~locals:_ ~formals:_ q = (q, q)
