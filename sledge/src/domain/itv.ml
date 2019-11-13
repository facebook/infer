(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Interval abstract domain *)

open Apron
open Option.Let_syntax

type apron_typ = [%import: Apron.Texpr0.typ] [@@deriving equal]

(** Apron-managed map from variables to intervals *)
type t = Box.t Abstract1.t

let man = lazy (Box.manager_alloc ())
let join l r = Some (Abstract1.join (Lazy.force man) l r)
let equal l r = Abstract1.is_eq (Lazy.force man) l r
let is_false x = Abstract1.is_bottom (Lazy.force man) x

let bindings (itv : t) =
  let itv = Abstract1.minimize_environment (Lazy.force man) itv in
  let box = Abstract1.to_box (Lazy.force man) itv in
  let vars =
    Environment.vars box.box1_env |> fun (i, r) -> Array.append i r
  in
  Array.zip_exn vars box.interval_array

let sexp_of_t (itv : t) =
  let sexps =
    Array.fold (bindings itv) ~init:[] ~f:(fun acc (v, {inf; sup}) ->
        Sexp.List
          [ Sexp.Atom (Var.to_string v); Sexp.Atom (Scalar.to_string inf)
          ; Sexp.Atom (Scalar.to_string sup) ]
        :: acc )
  in
  Sexp.List sexps

let pp fs =
  let pp_pair a_pp b_pp fs (a, b) =
    Format.fprintf fs "@[(%a@,%a)@]" a_pp a b_pp b
  in
  bindings >> Array.pp "@," (pp_pair Var.print Interval.print) fs

let report_fmt_thunk = Fn.flip pp
let init _gs = Abstract1.top (Lazy.force man) (Environment.make [||] [||])
let apron_var_of_name = (fun nm -> "%" ^ nm) >> Apron.Var.of_string
let apron_var_of_reg = Reg.name >> apron_var_of_name

let rec apron_typ_of_llair_typ : Typ.t -> Texpr1.typ option = function
  | Pointer {elt= _} -> apron_typ_of_llair_typ Typ.siz
  | Integer {bits= _} -> Some Texpr1.Int
  | Float {bits= 32; enc= `IEEE} -> Some Texpr1.Single
  | Float {bits= 64; enc= `IEEE} -> Some Texpr1.Double
  | Float {bits= 80; enc= `Extended} -> Some Texpr1.Extended
  | Float {bits= 128; enc= `IEEE} -> Some Texpr1.Quad
  | t ->
      warn "No corresponding apron type for llair type %a " Typ.pp t () ;
      None

let apron_of_q = Q.to_float >> fun fp -> Texpr1.Cst (Coeff.s_of_float fp)

let rec pow base typ = function
  | 1 -> base
  | z ->
      Texpr1.Binop (Texpr1.Mul, base, pow base typ (z - 1), typ, Texpr0.Rnd)

(* An n-ary term with [subtms] {(q0, e0), ..., (qn, en)} is interpreted as:
 *    ∑ᵢ eᵢ*qᵢ (when [op] is [Texpr1.Add])
 *    ∏ᵢ eᵢ^qᵢ (when [op] is [Texpr1.Mul])
 * (See sledge/src/llair/term.ml functions assert_(mono|poly)mial for details)
 *)
let rec texpr_of_nary_term subtms typ q op =
  assert (Qset.length subtms >= 2) ;
  let term_to_texpr (tm, coeff) =
    let%bind base = apron_texpr_of_llair_term tm q typ in
    match op with
    | Texpr1.Add ->
        Some
          (Texpr1.Binop (Texpr1.Mul, base, apron_of_q coeff, typ, Texpr0.Rnd))
    | Texpr1.Mul
    (* only handle positive integer exponents *)
      when Z.equal Z.one (Q.den coeff) && Q.sign coeff = 1 ->
        Some (pow base typ (Q.to_int coeff))
    | _ -> None
  in
  match Qset.to_list subtms with
  | hd :: tl ->
      List.fold tl ~init:(term_to_texpr hd) ~f:(fun acc curr ->
          let%bind c = term_to_texpr curr in
          let%map a = acc in
          Texpr1.Binop (op, c, a, typ, Texpr0.Rnd) )
  | _ -> assert false

and apron_texpr_of_llair_term tm q typ =
  match (tm : Term.t) with
  | Add terms -> texpr_of_nary_term terms typ q Texpr1.Add
  | Mul terms -> texpr_of_nary_term terms typ q Texpr1.Mul
  | Var {name} -> Some (Texpr1.Var (apron_var_of_name name))
  | Integer {data} -> Some (Texpr1.Cst (Coeff.s_of_int (Z.to_int data)))
  | Float {data} ->
      let f =
        try Float.of_string data with _ -> failwith "malformed float: %s"
      in
      Some (Texpr1.Cst (Coeff.s_of_float f))
  | Ap1 (Convert {dst; src}, t) -> (
    match (apron_typ_of_llair_typ dst, apron_typ_of_llair_typ src) with
    | None, _ | _, None -> None
    | Some dst, Some src ->
        let subtm = apron_texpr_of_llair_term t q src in
        if equal_apron_typ src dst then subtm
        else
          let%bind t = subtm in
          Some (Texpr1.Unop (Texpr1.Cast, t, dst, Texpr0.Rnd)) )
  (* extraction to unsigned 1-bit int is llvm encoding of C boolean;
     restrict to [0,1] *)
  | Ap1 (Unsigned {bits= 1}, _t) -> Some (Texpr1.Cst (Coeff.i_of_int 0 1))
  (* "t xor true" and "true xor t" are negation *)
  | Ap2 (Xor, t, Integer {data}) when Z.is_true data ->
      let%map t = apron_texpr_of_llair_term t q typ in
      Texpr1.Unop (Texpr1.Neg, t, typ, Texpr0.Rnd)
  | Ap2 (Xor, Integer {data}, t) when Z.is_true data ->
      let%map t = apron_texpr_of_llair_term t q typ in
      Texpr1.Unop (Texpr1.Neg, t, typ, Texpr0.Rnd)
  (* query apron for abstract evaluation of binary operations*)
  | Ap2 (op, t1, t2) ->
      let%bind f =
        match op with
        | Rem -> Some (mk_arith_binop typ Texpr0.Mod)
        | Div -> Some (mk_arith_binop typ Texpr0.Div)
        | Eq -> Some (mk_bool_binop typ q Tcons0.EQ)
        | Dq -> Some (mk_bool_binop typ q Tcons0.DISEQ)
        | Lt -> Some (Fn.flip (mk_bool_binop typ q Tcons0.SUP))
        | Le -> Some (Fn.flip (mk_bool_binop typ q Tcons0.SUPEQ))
        | _ -> None
      in
      let%bind te1 = apron_texpr_of_llair_term t1 q typ in
      let%map te2 = apron_texpr_of_llair_term t2 q typ in
      f te1 te2
  | x ->
      [%Trace.info
        "No corresponding apron term for llair term: %a" Term.pp x] ;
      None

and mk_arith_binop typ op te1 te2 =
  Texpr1.Binop (op, te1, te2, typ, Texpr0.Rnd)

(** abstract evaluation of boolean binary operation [te1 op te2] at [q] by
    translation to [te1 - te2 op 0] and intersection with [q]*)
and mk_bool_binop typ q op te1 te2 =
  let env = Abstract1.env q in
  let lhs = Texpr1.Binop (Texpr1.Sub, te1, te2, typ, Texpr0.Rnd) in
  let tcons = Tcons1.make (Texpr1.of_expr env lhs) op in
  let ea =
    Tcons1.array_make env 1 $> fun ea -> Tcons1.array_set ea 0 tcons
  in
  (* if meet of q with tree constraint encoding of binop is: (bottom ->
     binop definitely false); (unchanged from q -> binop definitely true);
     (else -> binop may be true or false) *)
  let meet = Abstract1.meet_tcons_array (Lazy.force man) q ea in
  if is_false meet then Texpr1.Cst (Coeff.s_of_int 0)
  else if equal meet q then Texpr1.Cst (Coeff.s_of_int (-1))
  else Texpr1.Cst (Coeff.i_of_int (-1) 0)

let assign reg exp q =
  [%Trace.call fun {pf} -> pf "{%a}@\n%a := %a" pp q Reg.pp reg Exp.pp exp]
  ;
  let lval = apron_var_of_reg reg in
  ( match
      apron_typ_of_llair_typ (Reg.typ reg)
      >>= apron_texpr_of_llair_term (Exp.term exp) q
    with
  | Some e ->
      let env = Abstract1.env q in
      let new_env =
        match
          ( Environment.mem_var env lval
          , apron_typ_of_llair_typ (Reg.typ reg) )
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
  match
    apron_typ_of_llair_typ (Exp.typ e)
    >>= apron_texpr_of_llair_term (Exp.term e) q
  with
  | Some e ->
      let cond =
        Abstract1.bound_texpr (Lazy.force man) q (Texpr1.of_expr q.env e)
      in
      if Interval.is_zero cond then None else Some q
  | _ -> Some q

(** existentially quantify killed register [r] out of state [q] *)
let exec_kill q r =
  let apron_v = apron_var_of_reg r in
  if Environment.mem_var (Abstract1.env q) apron_v then
    Abstract1.forget_array (Lazy.force man) q [|apron_v|] false
  else q

(** perform a series [move_vec] of reg:=exp moves at state [q] *)
let exec_move q move_vec =
  let defs, uses =
    Vector.fold move_vec ~init:(Reg.Set.empty, Reg.Set.empty)
      ~f:(fun (defs, uses) (r, e) ->
        (Set.add defs r, Exp.fold_regs e ~init:uses ~f:Set.add) )
  in
  assert (Set.disjoint defs uses) ;
  Vector.fold move_vec ~init:q ~f:(fun a (r, e) -> assign r e a)

let exec_inst q i =
  match (i : Llair.inst) with
  | Move {reg_exps; loc= _} -> Some (exec_move q reg_exps)
  | Store {ptr; exp; len= _; loc= _} -> (
    match Reg.of_exp ptr with
    | Some reg -> Some (assign reg exp q)
    | None -> Some q )
  | Load {reg; ptr; len= _; loc= _} -> Some (assign reg ptr q)
  | Nondet {reg= Some reg; msg= _; loc= _} -> Some (exec_kill q reg)
  | Nondet {reg= None; msg= _; loc= _}
   |Alloc _ | Memset _ | Memcpy _ | Memmov _ | Free _ ->
      Some q
  | Abort _ -> None

(** Treat any intrinsic function as havoc on the return register [aret] *)
let exec_intrinsic ~skip_throw:_ pre aret i _ =
  let name = Reg.name i in
  if
    List.exists
      [ "malloc"; "aligned_alloc"; "calloc"; "posix_memalign"; "realloc"
      ; "mallocx"; "rallocx"; "xallocx"; "sallocx"; "dallocx"; "sdallocx"
      ; "nallocx"; "malloc_usable_size"; "mallctl"; "mallctlnametomib"
      ; "mallctlbymib"; "malloc_stats_print"; "strlen"
      ; "__cxa_allocate_exception"; "_ZN5folly13usingJEMallocEv" ]
      ~f:(String.equal name)
  then aret >>| (exec_kill pre >> Option.some)
  else None

type from_call = {areturn: Reg.t option; caller_q: t} [@@deriving sexp_of]

let recursion_beyond_bound = `prune

(** existentially quantify locals *)
let post locals _ (q : t) =
  let locals =
    Set.fold locals ~init:[] ~f:(fun a r ->
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
        match apron_typ_of_llair_typ (Reg.typ fret) with
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
  | Some aret, None -> exec_kill caller_q aret
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
    let mangle r = Reg.program (Reg.typ r) ("__tmp__" ^ Reg.name r) in
    let args = List.zip_exn formals actuals in
    let q' =
      List.fold args ~init:q ~f:(fun q (f, a) -> assign (mangle f) a q)
    in
    let callee_env =
      List.fold formals ~init:([], []) ~f:(fun (is, fs) f ->
          match apron_typ_of_llair_typ (Reg.typ f) with
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
        (Array.of_list_map ~f:(mangle >> apron_var_of_reg) formals)
        (Array.of_list_map ~f:apron_var_of_reg formals)
    in
    (q''', {areturn; caller_q= q})

let dnf q = [q]

let resolve_callee lookup ptr q =
  match Reg.of_exp ptr with
  | Some callee -> (lookup (Reg.name callee), q)
  | None -> ([], q)

type summary = t

let pp_summary = pp
let apply_summary _ _ = None
let create_summary ~locals:_ ~formals:_ q = (q, q)
