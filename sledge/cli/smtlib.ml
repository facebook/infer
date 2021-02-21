(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Process SMT-LIB benchmarks using SLEdge's first-order theory solver. *)

module Smt = Smtlib_utils.V_2_6
open Fol
module VarEnv = Map.Make (String)

type var_env = Term.t VarEnv.t
type frame = {mutable asserts: Smt.Ast.term list; mutable var_env: var_env}

let init_stack = [{asserts= []; var_env= VarEnv.empty}]
let stack = ref init_stack
let top () = List.hd_exn !stack

let push () =
  let {asserts; var_env} = top () in
  stack := {asserts; var_env} :: !stack

let pop () =
  match !stack with
  | [] -> assert false
  | [_] -> ()
  | _ :: tl -> stack := tl

let reset () = stack := init_stack

let id =
  let count = ref 0 in
  fun () ->
    incr count ;
    !count

let decl_var name =
  let v = Term.var (Var.identified ~name ~id:(id ())) in
  let top = top () in
  top.var_env <- VarEnv.add_exn ~key:name ~data:v top.var_env

let assert_term term =
  let top = top () in
  top.asserts <- term :: top.asserts

let rec x_let env nes =
  List.fold nes env ~f:(fun (name, term) ->
      VarEnv.add_exn ~key:name ~data:(x_trm env term) )

and x_trm : var_env -> Smt.Ast.term -> Term.t =
 fun n term ->
  match term with
  | Const s -> (
    try VarEnv.find_exn s n
    with _ -> (
      try Term.rational (Q.of_string s)
      with _ -> (
        try Term.rational (Q.of_float (Float.of_string_exn s))
        with _ -> fail "not a rational: %a" Smt.Ast.pp_term term () ) ) )
  | Arith (Add, e :: es) ->
      List.fold ~f:(fun e -> Term.add (x_trm n e)) es (x_trm n e)
  | Arith (Minus, e :: es) ->
      List.fold ~f:(fun e -> Term.sub (x_trm n e)) es (x_trm n e)
  | Arith (Mult, es) -> (
    match List.map ~f:(x_trm n) es with
    | e :: es ->
        List.fold es e ~f:(fun e p ->
            match Term.get_q e with
            | Some q -> Term.mulq q p
            | None -> (
              match Term.get_q p with
              | Some q -> Term.mulq q e
              | None -> fail "nonlinear: %a" Smt.Ast.pp_term term () ) )
    | [] -> fail "malformed: %a" Smt.Ast.pp_term term () )
  | Arith (Div, es) -> (
    match List.map ~f:(x_trm n) es with
    | e :: es ->
        List.fold es e ~f:(fun e p ->
            match Term.get_q e with
            | Some q -> Term.mulq (Q.inv q) p
            | None -> fail "nonlinear: %a" Smt.Ast.pp_term term () )
    | [] -> fail "malformed: %a" Smt.Ast.pp_term term () )
  | If (c, t, e) ->
      Term.ite ~cnd:(x_fml n c) ~thn:(x_trm n t) ~els:(x_trm n e)
  | App _ -> todo "%a" Smt.Ast.pp_term term ()
  | Let (nes, e) -> x_trm (x_let n nes) e
  | Attr (e, _) -> x_trm n e
  | Fun _ | HO_app _ -> fail "higher-order: %a" Smt.Ast.pp_term term ()
  | Match _ -> fail "datatype: %a" Smt.Ast.pp_term term ()
  | Cast _ -> fail "cast: %a" Smt.Ast.pp_term term ()
  | Arith ((Add | Minus), _) -> fail "malformed: %a" Smt.Ast.pp_term term ()
  | True | False
   |Arith ((Leq | Lt | Geq | Gt), _)
   |Is_a _ | Eq _ | Imply _ | And _ | Or _ | Not _ | Distinct _ | Forall _
   |Exists _ ->
      Formula.inject (x_fml n term)

and x_fml : var_env -> Smt.Ast.term -> Formula.t =
 fun n term ->
  match term with
  | True -> Formula.tt
  | False -> Formula.ff
  | If (cnd, pos, neg) ->
      Formula.cond ~cnd:(x_fml n cnd) ~pos:(x_fml n pos) ~neg:(x_fml n neg)
  | App _ -> todo "%a" Smt.Ast.pp_term term ()
  | Let (nes, b) -> x_fml (x_let n nes) b
  | Eq (d, e) -> Formula.eq (x_trm n d) (x_trm n e)
  | Imply (a, b) -> x_fml n (Or [Not a; b])
  | And bs -> Formula.andN (List.map ~f:(x_fml n) bs)
  | Or bs -> Formula.orN (List.map ~f:(x_fml n) bs)
  | Distinct es ->
      es
      |> List.map ~f:(x_trm n)
      |> Iter.diagonal_l
      |> Iter.map ~f:(fun (d, e) -> Formula.dq d e)
      |> Iter.to_list
      |> Formula.andN
  | Not b -> Formula.not_ (x_fml n b)
  | Attr (b, _) -> x_fml n b
  | Cast _ -> fail "cast: %a" Smt.Ast.pp_term term ()
  | Arith ((Leq | Lt | Geq | Gt), _) ->
      fail "inequality: %a" Smt.Ast.pp_term term ()
  | Fun _ | HO_app _ -> fail "higher-order: %a" Smt.Ast.pp_term term ()
  | Match _ | Is_a _ -> fail "datatype: %a" Smt.Ast.pp_term term ()
  | Forall _ | Exists _ -> fail "quantifier: %a" Smt.Ast.pp_term term ()
  | Const _ | Arith ((Add | Minus | Mult | Div), _) ->
      Formula.dq0 (x_trm n term)

let x_context {asserts; var_env} =
  Context.dnf (Formula.andN (List.map ~f:(x_fml var_env) asserts))

let check_unsat (_, asserts, ctx) =
  [%Trace.call fun {pf} ->
    pf "@ %a@ %a@ %a" Formula.pp asserts Context.pp ctx Context.pp_raw ctx]
  ;
  ( Context.is_unsat ctx
  || Formula.equal Formula.ff
       (Formula.map_terms ~f:(Context.normalize ctx) asserts) )
  |>
  [%Trace.retn fun {pf} -> pf "%b"]

exception Unsound
exception Incomplete

let expect_unsat = ref false

let check_sat () =
  let unsat = Iter.for_all ~f:check_unsat (x_context (top ())) in
  if (not unsat) && !expect_unsat then raise Incomplete
  else if unsat && not !expect_unsat then raise Unsound

let process_stmt (stmt : Smt.Ast.statement) =
  match stmt.stmt with
  | Stmt_set_logic _ -> ()
  | Stmt_set_info (":status", "unsat") -> expect_unsat := true
  | Stmt_set_info (":status", _) -> expect_unsat := false
  | Stmt_set_info _ | Stmt_set_option _ | Stmt_decl_sort _ -> ()
  | Stmt_decl {fun_name; fun_args= []} -> decl_var fun_name
  | Stmt_decl _ -> todo "%a" Smt.Ast.pp_stmt stmt ()
  | Stmt_fun_def {fr_decl= {fun_name; fun_args= []}; fr_body} ->
      assert_term (Eq (Const fun_name, fr_body))
  | Stmt_fun_def _ | Stmt_fun_rec _ | Stmt_funs_rec _ ->
      fail "function definition: %a" Smt.Ast.pp_stmt stmt ()
  | Stmt_data _ -> fail "datatype definition" ()
  | Stmt_assert term -> assert_term term
  | Stmt_get_assertions | Stmt_get_assignment | Stmt_get_info _
   |Stmt_get_model | Stmt_get_option _ | Stmt_get_proof
   |Stmt_get_unsat_assumptions | Stmt_get_unsat_core | Stmt_get_value _ ->
      ()
  | Stmt_check_sat -> check_sat ()
  | Stmt_check_sat_assuming _ -> fail "check-sat-assuming" ()
  | Stmt_pop n ->
      for _ = 1 to n do
        pop ()
      done
  | Stmt_push n ->
      for _ = 1 to n do
        push ()
      done
  | Stmt_reset | Stmt_reset_assertions -> reset ()
  | Stmt_exit -> ()

let process filename =
  try
    List.iter ~f:process_stmt (Smt.parse_file_exn filename) ;
    Report.Ok
  with
  | Unsound -> Report.Unsound
  | Incomplete -> Report.Incomplete
