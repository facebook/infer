(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

type t = Sh.t [@@deriving equal, sexp_of]

let pp_full fs s = [%Trace.fprintf fs "%a" Sh.pp s]
let pp fs s = [%Trace.fprintf fs "%a" Sh.pp (Sh.simplify s)]
let pp fs s = pp_full fs s ; pp fs s

let init globals =
  Vector.fold globals ~init:Sh.emp ~f:(fun q -> function
    | {Global.var; init= Some (arr, siz)} ->
        let loc = Exp.var var in
        let len = Exp.integer (Z.of_int siz) Typ.siz in
        Sh.star q (Sh.seg {loc; bas= loc; len; siz= len; arr})
    | _ -> q )

let join = Sh.or_
let exec_assume = Exec.assume
let exec_return = Exec.return
let exec_inst = Exec.inst
let exec_intrinsic = Exec.intrinsic

type from_call = Var.Subst.t [@@deriving compare, equal, sexp]

(** Express formula in terms of formals instead of actuals, and enter scope
    of locals: rename formals to fresh vars in formula and actuals, add
    equations between each formal and actual, and quantify the temps and
    fresh vars. *)
let call actuals formals locals ?temps q =
  [%Trace.call fun {pf} ->
    pf
      "@[<hv>actuals: (@[%a@])@ formals: (@[%a@])@ locals: {@[%a@]}@ q: %a@]"
      (List.pp ",@ " Exp.pp) (List.rev actuals) (List.pp ",@ " Var.pp)
      (List.rev formals) Var.Set.pp locals Sh.pp q]
  ;
  let wrt = Set.add_list formals locals in
  let q', freshen_locals = Sh.freshen q ~wrt in
  let and_eq q formal actual =
    let actual' = Exp.rename actual freshen_locals in
    Sh.and_ (Exp.eq (Exp.var formal) actual') q
  in
  let and_eqs formals actuals q =
    List.fold2_exn ~f:and_eq formals actuals ~init:q
  in
  ( Sh.extend_us locals
      (Option.fold ~f:(Fn.flip Sh.exists) temps
         ~init:(and_eqs formals actuals q'))
  , freshen_locals )
  |>
  [%Trace.retn fun {pf} (q', s) ->
    pf "@[<hv>subst: %a@ q': %a@]" Var.Subst.pp s Sh.pp q']

(** Leave scope of locals: existentially quantify locals. *)
let post locals q =
  [%Trace.call fun {pf} ->
    pf "@[<hv>locals: {@[%a@]}@ q: %a@]" Var.Set.pp locals Sh.pp q]
  ;
  Sh.exists locals q
  |>
  [%Trace.retn fun {pf} -> pf "%a" Sh.pp]

(** Express in terms of actuals instead of formals: existentially quantify
    formals, and apply inverse of fresh variables for formals renaming to
    restore the shadowed variables. *)
let retn formals freshen_locals q =
  [%Trace.call fun {pf} ->
    pf "@[<hv>formals: {@[%a@]}@ subst: %a@ q: %a@]" (List.pp ", " Var.pp)
      formals Var.Subst.pp
      (Var.Subst.invert freshen_locals)
      Sh.pp q]
  ;
  Sh.rename
    (Var.Subst.invert freshen_locals)
    (Sh.exists (Var.Set.of_list formals) q)
  |>
  [%Trace.retn fun {pf} -> pf "%a" Sh.pp]

let resolve_callee lookup ptr _ =
  match Var.of_exp ptr with
  | Some callee_name -> lookup callee_name
  | None -> []
