(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Functions for Propositions (i.e., Symbolic Heaps) *)

module L = Logging
module F = Format

(** type to describe different strategies for initializing fields of a structure. [No_init] does not
    initialize any fields of the struct. [Fld_init] initializes the fields of the struct with fresh
    variables (C) or default values (Java). *)
type struct_init_mode =
  | No_init
  | Fld_init

let unSome = function
  | Some x -> x
  | _ -> assert false

type normal (** kind for normal props, i.e. normalized *)

type exposed (** kind for exposed props *)

type pi = Sil.atom list
type sigma = Sil.hpred list


module Core : sig

  (** the kind 'a should range over [normal] and [exposed] *)
  type 'a t = private
    {
      sigma: sigma;  (** spatial part *)
      sub: Sil.subst;  (** substitution *)
      pi: pi;  (** pure part *)
      sigma_fp : sigma;  (** abduced spatial part *)
      pi_fp: pi;  (** abduced pure part *)
    }

  (** Proposition [true /\ emp]. *)
  val prop_emp : normal t

  (** Set individual fields of the prop. *)
  val set : ?sub:Sil.subst -> ?pi:pi -> ?sigma:sigma -> ?pi_fp:pi -> ?sigma_fp:sigma ->
    'a t -> exposed t

  (** Cast an exposed prop to a normalized one by just changing the type *)
  val unsafe_cast_to_normal : exposed t -> normal t

end = struct

  (** A proposition. The following invariants are mantained. [sub] is of
      the form id1 = e1 ... idn = en where: the id's are distinct and do not
      occur in the e's nor in [pi] or [sigma]; the id's are in sorted
      order; the id's are not existentials; if idn = yn (for yn not
      existential) then idn < yn in the order on ident's. [pi] is sorted
      and normalized, and does not contain x = e. [sigma] is sorted and
      normalized. *)
  type 'a t =
    {
      sigma: sigma;  (** spatial part *)
      sub: Sil.subst;  (** substitution *)
      pi: pi;  (** pure part *)
      sigma_fp : sigma;  (** abduced spatial part *)
      pi_fp: pi;  (** abduced pure part *)
    }

  (** Proposition [true /\ emp]. *)
  let prop_emp : normal t =
    {
      sub = Sil.sub_empty;
      pi = [];
      sigma = [];
      pi_fp = [];
      sigma_fp = [];
    }

  let set ?sub ?pi ?sigma ?pi_fp ?sigma_fp p =
    let set_ p
        ?(sub=p.sub) ?(pi=p.pi) ?(sigma=p.sigma) ?(pi_fp=p.pi_fp) ?(sigma_fp=p.sigma_fp) ()
      =
      { sub; pi; sigma; pi_fp; sigma_fp }
    in
    set_ p ?sub ?pi ?sigma ?pi_fp ?sigma_fp ()

  let unsafe_cast_to_normal (p: exposed t) : normal t =
    (p :> normal t)

end

include Core

exception Cannot_star of L.ml_loc

(** {2 Basic Functions for Propositions} *)

(** {1 Functions for Comparison} *)

(** Comparison between lists of equalities and disequalities. Lexicographical order. *)
let rec pi_compare pi1 pi2 =
  if pi1 == pi2 then 0
  else match (pi1, pi2) with
    | ([],[]) -> 0
    | ([], _:: _) -> - 1
    | (_:: _,[]) -> 1
    | (a1:: pi1', a2:: pi2') ->
        let n = Sil.atom_compare a1 a2 in
        if n <> 0 then n
        else pi_compare pi1' pi2'

let pi_equal pi1 pi2 =
  pi_compare pi1 pi2 = 0

(** Comparsion between lists of heap predicates. Lexicographical order. *)
let rec sigma_compare sigma1 sigma2 =
  if sigma1 == sigma2 then 0
  else match (sigma1, sigma2) with
    | ([],[]) -> 0
    | ([], _:: _) -> - 1
    | (_:: _,[]) -> 1
    | (h1:: sigma1', h2:: sigma2') ->
        let n = Sil.hpred_compare h1 h2 in
        if n <> 0 then n
        else sigma_compare sigma1' sigma2'

let sigma_equal sigma1 sigma2 =
  sigma_compare sigma1 sigma2 = 0

(** Comparison between propositions. Lexicographical order. *)
let prop_compare p1 p2 =
  sigma_compare p1.sigma p2.sigma
  |> next Sil.sub_compare p1.sub p2.sub
  |> next pi_compare p1.pi p2.pi
  |> next sigma_compare p1.sigma_fp p2.sigma_fp
  |> next pi_compare p1.pi_fp p2.pi_fp

(** Check the equality of two propositions *)
let prop_equal p1 p2 =
  prop_compare p1 p2 = 0

(** {1 Functions for Pretty Printing} *)

(** Pretty print a footprint. *)
let pp_footprint _pe f fp =
  let pe = { _pe with pe_cmap_norm = _pe.pe_cmap_foot } in
  let pp_pi f () =
    if fp.pi_fp != [] then
      F.fprintf f "%a ;@\n" (pp_semicolon_seq_oneline pe (Sil.pp_atom pe)) fp.pi_fp in
  if fp.pi_fp != [] || fp.sigma_fp != [] then
    F.fprintf f "@\n[footprint@\n  @[%a%a@]  ]"
      pp_pi () (pp_semicolon_seq pe (Sil.pp_hpred pe)) fp.sigma_fp

let pp_texp_simple pe = match pe.pe_opt with
  | PP_SIM_DEFAULT -> Sil.pp_texp pe
  | PP_SIM_WITH_TYP -> Sil.pp_texp_full pe

(** Pretty print a pointsto representing a stack variable as an equality *)
let pp_hpred_stackvar pe0 f hpred =
  let pe, changed = Sil.color_pre_wrapper pe0 f hpred in
  begin match hpred with
    | Sil.Hpointsto (Exp.Lvar pvar, se, te) ->
        let pe' = match se with
          | Sil.Eexp (Exp.Var _, _) when not (Pvar.is_global pvar) ->
              { pe with pe_obj_sub = None } (* dont use obj sub on the var defining it *)
          | _ -> pe in
        (match pe'.pe_kind with
         | PP_TEXT | PP_HTML ->
             F.fprintf f "%a = %a:%a"
               (Pvar.pp_value pe') pvar (Sil.pp_sexp pe') se (pp_texp_simple pe') te
         | PP_LATEX ->
             F.fprintf f "%a{=}%a" (Pvar.pp_value pe') pvar (Sil.pp_sexp pe') se)
    | Sil.Hpointsto _ | Sil.Hlseg _ | Sil.Hdllseg _ -> assert false (* should not happen *)
  end;
  Sil.color_post_wrapper changed pe0 f

(** Pretty print a substitution. *)
let pp_sub pe f sub =
  let pi_sub = IList.map (fun (id, e) -> Sil.Aeq(Exp.Var id, e)) (Sil.sub_to_list sub) in
  (pp_semicolon_seq_oneline pe (Sil.pp_atom pe)) f pi_sub

(** Dump a substitution. *)
let d_sub (sub: Sil.subst) = L.add_print_action (L.PTsub, Obj.repr sub)

let pp_sub_entry pe0 f entry =
  let pe, changed = Sil.color_pre_wrapper pe0 f entry in
  let (x, e) = entry in
  begin
    match pe.pe_kind with
    | PP_TEXT | PP_HTML ->
        F.fprintf f "%a = %a" (Ident.pp pe) x (Sil.pp_exp pe) e
    | PP_LATEX ->
        F.fprintf f "%a{=}%a" (Ident.pp pe) x (Sil.pp_exp pe) e
  end;
  Sil.color_post_wrapper changed pe0 f

(** Pretty print a substitution as a list of (ident,exp) pairs *)
let pp_subl pe =
  if Config.smt_output then pp_semicolon_seq pe (pp_sub_entry pe)
  else pp_semicolon_seq_oneline pe (pp_sub_entry pe)

(** Pretty print a pi. *)
let pp_pi pe =
  if Config.smt_output then pp_semicolon_seq pe (Sil.pp_atom pe)
  else pp_semicolon_seq_oneline pe (Sil.pp_atom pe)

(** Dump a pi. *)
let d_pi (pi: pi) = L.add_print_action (L.PTpi, Obj.repr pi)

(** Pretty print a sigma. *)
let pp_sigma pe =
  pp_semicolon_seq pe (Sil.pp_hpred pe)

(** Split sigma into stack and nonstack parts.
    The boolean indicates whether the stack should only include local variales. *)
let sigma_get_stack_nonstack only_local_vars sigma =
  let hpred_is_stack_var = function
    | Sil.Hpointsto (Exp.Lvar pvar, _, _) -> not only_local_vars || Pvar.is_local pvar
    | _ -> false in
  IList.partition hpred_is_stack_var sigma

(** Pretty print a sigma in simple mode. *)
let pp_sigma_simple pe env fmt sigma =
  let sigma_stack, sigma_nonstack = sigma_get_stack_nonstack false sigma in
  let pp_stack fmt _sg =
    let sg = IList.sort Sil.hpred_compare _sg in
    if sg != [] then Format.fprintf fmt "%a" (pp_semicolon_seq pe (pp_hpred_stackvar pe)) sg in
  let pp_nl fmt doit = if doit then
      (match pe.pe_kind with
       | PP_TEXT | PP_HTML -> Format.fprintf fmt " ;@\n"
       | PP_LATEX -> Format.fprintf fmt " ; \\\\@\n") in
  let pp_nonstack fmt = pp_semicolon_seq pe (Sil.pp_hpred_env pe (Some env)) fmt in
  if sigma_stack != [] || sigma_nonstack != [] then
    Format.fprintf fmt "%a%a%a"
      pp_stack sigma_stack pp_nl
      (sigma_stack != [] && sigma_nonstack != []) pp_nonstack sigma_nonstack

(** Dump a sigma. *)
let d_sigma (sigma: sigma) = L.add_print_action (L.PTsigma, Obj.repr sigma)

(** Dump a pi and a sigma *)
let d_pi_sigma pi sigma =
  let d_separator () = if pi != [] && sigma != [] then L.d_strln " *" in
  d_pi pi; d_separator (); d_sigma sigma

let pi_of_subst sub =
  IList.map (fun (id1, e2) -> Sil.Aeq (Exp.Var id1, e2)) (Sil.sub_to_list sub)

(** Return the pure part of [prop]. *)
let get_pure (p: 'a t) : pi =
  pi_of_subst p.sub @ p.pi

(** Print existential quantification *)
let pp_evars pe f evars =
  if evars != []
  then match pe.pe_kind with
    | PP_TEXT | PP_HTML ->
        F.fprintf f "exists [%a]. " (pp_comma_seq (Ident.pp pe)) evars
    | PP_LATEX ->
        F.fprintf f "\\exists %a. " (pp_comma_seq (Ident.pp pe)) evars

(** Print an hpara in simple mode *)
let pp_hpara_simple _pe env n f pred =
  let pe = pe_reset_obj_sub _pe in (* no free vars: disable object substitution *)
  match pe.pe_kind with
  | PP_TEXT | PP_HTML ->
      F.fprintf f "P%d = %a%a"
        n (pp_evars pe) pred.Sil.evars
        (pp_semicolon_seq pe (Sil.pp_hpred_env pe (Some env))) pred.Sil.body
  | PP_LATEX ->
      F.fprintf f "P_{%d} = %a%a\\\\"
        n (pp_evars pe) pred.Sil.evars
        (pp_semicolon_seq pe (Sil.pp_hpred_env pe (Some env))) pred.Sil.body

(** Print an hpara_dll in simple mode *)
let pp_hpara_dll_simple _pe env n f pred =
  let pe = pe_reset_obj_sub _pe in (* no free vars: disable object substitution *)
  match pe.pe_kind with
  | PP_TEXT | PP_HTML ->
      F.fprintf f "P%d = %a%a"
        n (pp_evars pe) pred.Sil.evars_dll
        (pp_semicolon_seq pe (Sil.pp_hpred_env pe (Some env))) pred.Sil.body_dll
  | PP_LATEX ->
      F.fprintf f "P_{%d} = %a%a"
        n (pp_evars pe) pred.Sil.evars_dll
        (pp_semicolon_seq pe (Sil.pp_hpred_env pe (Some env))) pred.Sil.body_dll

(** Create an environment mapping (ident) expressions to the program variables containing them *)
let create_pvar_env (sigma: sigma) : (Exp.t -> Exp.t) =
  let env = ref [] in
  let filter = function
    | Sil.Hpointsto (Exp.Lvar pvar, Sil.Eexp (Exp.Var v, _), _) ->
        if not (Pvar.is_global pvar) then env := (Exp.Var v, Exp.Lvar pvar) :: !env
    | _ -> () in
  IList.iter filter sigma;
  let find e =
    try
      snd (IList.find (fun (e1, _) -> Exp.equal e1 e) !env)
    with Not_found -> e in
  find

(** Update the object substitution given the stack variables in the prop *)
let prop_update_obj_sub pe prop =
  if !Config.pp_simple
  then pe_set_obj_sub pe (create_pvar_env prop.sigma)
  else pe

(** Pretty print a footprint in simple mode. *)
let pp_footprint_simple _pe env f fp =
  let pe = { _pe with pe_cmap_norm = _pe.pe_cmap_foot } in
  let pp_pure f pi =
    if pi != [] then
      F.fprintf f "%a *@\n" (pp_pi pe) pi in
  if fp.pi_fp != [] || fp.sigma_fp != [] then
    F.fprintf f "@\n[footprint@\n   @[%a%a@]  ]"
      pp_pure fp.pi_fp
      (pp_sigma_simple pe env) fp.sigma_fp

(** Create a predicate environment for a prop *)
let prop_pred_env prop =
  let env = Sil.Predicates.empty_env () in
  IList.iter (Sil.Predicates.process_hpred env) prop.sigma;
  IList.iter (Sil.Predicates.process_hpred env) prop.sigma_fp;
  env

(** Pretty print a proposition. *)
let pp_prop pe0 f prop =
  let pe = prop_update_obj_sub pe0 prop in
  let latex = pe.pe_kind == PP_LATEX in
  let do_print f () =
    let subl = Sil.sub_to_list prop.sub in
    (* since prop diff is based on physical equality, we need to extract the sub verbatim *)
    let pi = prop.pi in
    let pp_pure f () =
      if subl != [] then F.fprintf f "%a ;@\n" (pp_subl pe) subl;
      if pi != [] then F.fprintf f "%a ;@\n" (pp_pi pe) pi in
    if !Config.pp_simple || latex then
      begin
        let env = prop_pred_env prop in
        let iter_f n hpara = F.fprintf f "@,@[<h>%a@]" (pp_hpara_simple pe env n) hpara in
        let iter_f_dll n hpara_dll =
          F.fprintf f "@,@[<h>%a@]" (pp_hpara_dll_simple pe env n) hpara_dll in
        let pp_predicates _ () =
          if Sil.Predicates.is_empty env
          then ()
          else if latex then
            begin
              F.fprintf f "@\n\\\\\\textsf{where }";
              Sil.Predicates.iter env iter_f iter_f_dll
            end
          else
            begin
              F.fprintf f "@,where";
              Sil.Predicates.iter env iter_f iter_f_dll
            end in
        F.fprintf f "%a%a%a%a"
          pp_pure () (pp_sigma_simple pe env) prop.sigma
          (pp_footprint_simple pe env) prop pp_predicates ()
      end
    else
      F.fprintf f "%a%a%a" pp_pure () (pp_sigma pe) prop.sigma (pp_footprint pe) prop in
  if !Config.forcing_delayed_prints then (* print in html mode *)
    F.fprintf f "%a%a%a" Io_infer.Html.pp_start_color Blue do_print () Io_infer.Html.pp_end_color ()
  else
    do_print f () (** print in text mode *)

let pp_prop_with_typ pe f p = pp_prop { pe with pe_opt = PP_SIM_WITH_TYP } f p

(** Dump a proposition. *)
let d_prop (prop: 'a t) = L.add_print_action (L.PTprop, Obj.repr prop)

(** Dump a proposition. *)
let d_prop_with_typ (prop: 'a t) = L.add_print_action (L.PTprop_with_typ, Obj.repr prop)

(** Print a list of propositions, prepending each one with the given string *)
let pp_proplist_with_typ pe f plist =
  let rec pp_seq_newline f = function
    | [] -> ()
    | [x] -> F.fprintf f "@[%a@]" (pp_prop_with_typ pe) x
    | x:: l -> F.fprintf f "@[%a@]@\n(||)@\n%a" (pp_prop_with_typ pe) x pp_seq_newline l in
  F.fprintf f "@[<v>%a@]" pp_seq_newline plist

(** dump a proplist *)
let d_proplist_with_typ (pl: 'a t list) =
  L.add_print_action (L.PTprop_list_with_typ, Obj.repr pl)

(** {1 Functions for computing free non-program variables} *)

let pi_fav_add fav pi =
  IList.iter (Sil.atom_fav_add fav) pi

let pi_fav =
  Sil.fav_imperative_to_functional pi_fav_add

let sigma_fav_add fav sigma =
  IList.iter (Sil.hpred_fav_add fav) sigma

let sigma_fav =
  Sil.fav_imperative_to_functional sigma_fav_add

let prop_footprint_fav_add fav prop =
  sigma_fav_add fav prop.sigma_fp;
  pi_fav_add fav prop.pi_fp

(** Find fav of the footprint part of the prop *)
let prop_footprint_fav prop =
  Sil.fav_imperative_to_functional prop_footprint_fav_add prop

let prop_fav_add fav prop =
  sigma_fav_add fav prop.sigma;
  sigma_fav_add fav prop.sigma_fp;
  Sil.sub_fav_add fav prop.sub;
  pi_fav_add fav prop.pi;
  pi_fav_add fav prop.pi_fp

let prop_fav p =
  Sil.fav_imperative_to_functional prop_fav_add p

(** free vars of the prop, excluding the pure part *)
let prop_fav_nonpure_add fav prop =
  sigma_fav_add fav prop.sigma;
  sigma_fav_add fav prop.sigma_fp

(** free vars, except pi and sub, of current and footprint parts *)
let prop_fav_nonpure =
  Sil.fav_imperative_to_functional prop_fav_nonpure_add

let hpred_fav_in_pvars_add fav = function
  | Sil.Hpointsto (Exp.Lvar _, sexp, _) -> Sil.strexp_fav_add fav sexp
  | Sil.Hpointsto _ | Sil.Hlseg _ | Sil.Hdllseg _ -> ()

let sigma_fav_in_pvars_add fav sigma =
  IList.iter (hpred_fav_in_pvars_add fav) sigma

let sigma_fpv sigma =
  IList.flatten (IList.map Sil.hpred_fpv sigma)

let pi_fpv pi =
  IList.flatten (IList.map Sil.atom_fpv pi)

let prop_fpv prop =
  (Sil.sub_fpv prop.sub) @
  (pi_fpv prop.pi) @
  (pi_fpv prop.pi_fp) @
  (sigma_fpv prop.sigma_fp) @
  (sigma_fpv prop.sigma)

(** {2 Functions for Subsitition} *)

let pi_sub (subst: Sil.subst) pi =
  let f = Sil.atom_sub subst in
  IList.map f pi

let sigma_sub subst sigma =
  let f = Sil.hpred_sub subst in
  IList.map f sigma

(** {2 Functions for normalization} *)

(** This function assumes that if (x,Exp.Var(y)) in sub, then compare x y = 1 *)
let sub_normalize sub =
  let f (id, e) = (not (Ident.is_primed id)) && (not (Sil.ident_in_exp id e)) in
  let sub' = Sil.sub_filter_pair f sub in
  if Sil.sub_equal sub sub' then sub else sub'

let (--) = IntLit.sub
let (++) = IntLit.add

let sym_eval abs e =
  let rec eval e =
    (* L.d_str " ["; Sil.d_exp e; L.d_str"] "; *)
    match e with
    | Exp.Var _ ->
        e
    | Exp.Closure c ->
        let captured_vars =
          IList.map (fun (exp, pvar, typ) -> (eval exp, pvar, typ)) c.captured_vars in
        Exp.Closure { c with captured_vars; }
    | Exp.Const _ ->
        e
    | Exp.Sizeof (Typ.Tarray (Typ.Tint ik, _), Some l, _)
      when Typ.ikind_is_char ik && !Config.curr_language = Config.Clang ->
        eval l
    | Exp.Sizeof (Typ.Tarray (Typ.Tint ik, Some l), _, _)
      when Typ.ikind_is_char ik && !Config.curr_language = Config.Clang ->
        Exp.Const (Const.Cint l)
    | Exp.Sizeof _ ->
        e
    | Exp.Cast (_, e1) ->
        eval e1
    | Exp.UnOp (Unop.LNot, e1, topt) ->
        begin
          match eval e1 with
          | Exp.Const (Const.Cint i) when IntLit.iszero i ->
              Exp.one
          | Exp.Const (Const.Cint _) ->
              Exp.zero
          | Exp.UnOp(Unop.LNot, e1', _) ->
              e1'
          | e1' ->
              if abs then Exp.get_undefined false else Exp.UnOp(Unop.LNot, e1', topt)
        end
    | Exp.UnOp (Unop.Neg, e1, topt) ->
        begin
          match eval e1 with
          | Exp.UnOp (Unop.Neg, e2', _) ->
              e2'
          | Exp.Const (Const.Cint i) ->
              Exp.int (IntLit.neg i)
          | Exp.Const (Const.Cfloat v) ->
              Exp.float (-. v)
          | Exp.Var id ->
              Exp.UnOp (Unop.Neg, Exp.Var id, topt)
          | e1' ->
              if abs then Exp.get_undefined false else Exp.UnOp (Unop.Neg, e1', topt)
        end
    | Exp.UnOp (Unop.BNot, e1, topt) ->
        begin
          match eval e1 with
          | Exp.UnOp(Unop.BNot, e2', _) ->
              e2'
          | Exp.Const (Const.Cint i) ->
              Exp.int (IntLit.lognot i)
          | e1' ->
              if abs then Exp.get_undefined false else Exp.UnOp (Unop.BNot, e1', topt)
        end
    | Exp.BinOp (Binop.Le, e1, e2) ->
        begin
          match eval e1, eval e2 with
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.bool (IntLit.leq n m)
          | Exp.Const (Const.Cfloat v), Exp.Const (Const.Cfloat w) ->
              Exp.bool (v <= w)
          | Exp.BinOp (Binop.PlusA, e3, Exp.Const (Const.Cint n)), Exp.Const (Const.Cint m) ->
              Exp.BinOp (Binop.Le, e3, Exp.int (m -- n))
          | e1', e2' ->
              Exp.le e1' e2'
        end
    | Exp.BinOp (Binop.Lt, e1, e2) ->
        begin
          match eval e1, eval e2 with
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.bool (IntLit.lt n m)
          | Exp.Const (Const.Cfloat v), Exp.Const (Const.Cfloat w) ->
              Exp.bool (v < w)
          | Exp.Const (Const.Cint n), Exp.BinOp (Binop.MinusA, f1, f2) ->
              Exp.BinOp
                (Binop.Le, Exp.BinOp (Binop.MinusA, f2, f1), Exp.int (IntLit.minus_one -- n))
          | Exp.BinOp(Binop.MinusA, f1 , f2), Exp.Const(Const.Cint n) ->
              Exp.le (Exp.BinOp(Binop.MinusA, f1 , f2)) (Exp.int (n -- IntLit.one))
          | Exp.BinOp (Binop.PlusA, e3, Exp.Const (Const.Cint n)), Exp.Const (Const.Cint m) ->
              Exp.BinOp (Binop.Lt, e3, Exp.int (m -- n))
          | e1', e2' ->
              Exp.lt e1' e2'
        end
    | Exp.BinOp (Binop.Ge, e1, e2) ->
        eval (Exp.le e2 e1)
    | Exp.BinOp (Binop.Gt, e1, e2) ->
        eval (Exp.lt e2 e1)
    | Exp.BinOp (Binop.Eq, e1, e2) ->
        begin
          match eval e1, eval e2 with
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.bool (IntLit.eq n m)
          | Exp.Const (Const.Cfloat v), Exp.Const (Const.Cfloat w) ->
              Exp.bool (v = w)
          | e1', e2' ->
              Exp.eq e1' e2'
        end
    | Exp.BinOp (Binop.Ne, e1, e2) ->
        begin
          match eval e1, eval e2 with
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.bool (IntLit.neq n m)
          | Exp.Const (Const.Cfloat v), Exp.Const (Const.Cfloat w) ->
              Exp.bool (v <> w)
          | e1', e2' ->
              Exp.ne e1' e2'
        end
    | Exp.BinOp (Binop.LAnd, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin match e1', e2' with
          | Exp.Const (Const.Cint i), _ when IntLit.iszero i ->
              e1'
          | Exp.Const (Const.Cint _), _ ->
              e2'
          | _, Exp.Const (Const.Cint i) when IntLit.iszero i ->
              e2'
          | _, Exp.Const (Const.Cint _) ->
              e1'
          | _ ->
              Exp.BinOp (Binop.LAnd, e1', e2')
        end
    | Exp.BinOp (Binop.LOr, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e1', e2' with
          | Exp.Const (Const.Cint i), _ when IntLit.iszero i ->
              e2'
          | Exp.Const (Const.Cint _), _ ->
              e1'
          | _, Exp.Const (Const.Cint i) when IntLit.iszero i ->
              e1'
          | _, Exp.Const (Const.Cint _) ->
              e2'
          | _ ->
              Exp.BinOp (Binop.LOr, e1', e2')
        end
    | Exp.BinOp(Binop.PlusPI, Exp.Lindex (ep, e1), e2) -> (* array access with pointer arithmetic *)
        let e' = Exp.BinOp (Binop.PlusA, e1, e2) in
        eval (Exp.Lindex (ep, e'))
    | Exp.BinOp (Binop.PlusPI, (Exp.BinOp (Binop.PlusPI, e11, e12)), e2) ->
        (* take care of pattern ((ptr + off1) + off2) *)
        (* progress: convert inner +I to +A *)
        let e2' = Exp.BinOp (Binop.PlusA, e12, e2) in
        eval (Exp.BinOp (Binop.PlusPI, e11, e2'))
    | Exp.BinOp (Binop.PlusA as oplus, e1, e2)
    | Exp.BinOp (Binop.PlusPI as oplus, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        let isPlusA = oplus = Binop.PlusA in
        let ominus = if isPlusA then Binop.MinusA else Binop.MinusPI in
        let (+++) x y = match x, y with
          | _, Exp.Const (Const.Cint i) when IntLit.iszero i -> x
          | Exp.Const (Const.Cint i), Exp.Const (Const.Cint j) ->
              Exp.Const (Const.Cint (IntLit.add i j))
          | _ -> Exp.BinOp (oplus, x, y) in
        let (---) x y = match x, y with
          | _, Exp.Const (Const.Cint i) when IntLit.iszero i -> x
          | Exp.Const (Const.Cint i), Exp.Const (Const.Cint j) ->
              Exp.Const (Const.Cint (IntLit.sub i j))
          | _ -> Exp.BinOp (ominus, x, y) in
        (* test if the extensible array at the end of [typ] has elements of type [elt] *)
        let extensible_array_element_typ_equal elt typ =
          Option.map_default (Typ.equal elt) false (Typ.get_extensible_array_element_typ typ) in
        begin
          match e1', e2' with
          (* pattern for arrays and extensible structs:
             sizeof(struct s {... t[l]}) + k * sizeof(t)) = sizeof(struct s {... t[l + k]}) *)
          | Exp.Sizeof (typ, len1_opt, st), Exp.BinOp (Binop.Mult, len2, Exp.Sizeof (elt, None, _))
            when isPlusA && (extensible_array_element_typ_equal elt typ) ->
              let len = match len1_opt with Some len1 -> len1 +++ len2 | None -> len2 in
              Exp.Sizeof (typ, Some len, st)
          | Exp.Const c, _ when Const.iszero_int_float c ->
              e2'
          | _, Exp.Const c when Const.iszero_int_float c ->
              e1'
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.int (n ++ m)
          | Exp.Const (Const.Cfloat v), Exp.Const (Const.Cfloat w) ->
              Exp.float (v +. w)
          | Exp.UnOp(Unop.Neg, f1, _), f2
          | f2, Exp.UnOp(Unop.Neg, f1, _) ->
              Exp.BinOp (ominus, f2, f1)
          | Exp.BinOp (Binop.PlusA, e, Exp.Const (Const.Cint n1)), Exp.Const (Const.Cint n2)
          | Exp.BinOp (Binop.PlusPI, e, Exp.Const (Const.Cint n1)), Exp.Const (Const.Cint n2)
          | Exp.Const (Const.Cint n2), Exp.BinOp (Binop.PlusA, e, Exp.Const (Const.Cint n1))
          | Exp.Const (Const.Cint n2), Exp.BinOp (Binop.PlusPI, e, Exp.Const (Const.Cint n1)) ->
              e +++ (Exp.int (n1 ++ n2))
          | Exp.BinOp (Binop.MinusA, Exp.Const (Const.Cint n1), e), Exp.Const (Const.Cint n2)
          | Exp.Const (Const.Cint n2), Exp.BinOp (Binop.MinusA, Exp.Const (Const.Cint n1), e) ->
              Exp.int (n1 ++ n2) --- e
          | Exp.BinOp (Binop.MinusA, e1, e2), e3 -> (* (e1-e2)+e3 --> e1 + (e3-e2) *)
              (* progress: brings + to the outside *)
              eval (e1 +++ (e3 --- e2))
          | _, Exp.Const _ ->
              e1' +++ e2'
          | Exp.Const _, _ ->
              if isPlusA then e2' +++ e1' else e1' +++ e2'
          | Exp.Var _, Exp.Var _ ->
              e1' +++ e2'
          | _ ->
              if abs && isPlusA then Exp.get_undefined false else
              if abs && not isPlusA then e1' +++ (Exp.get_undefined false)
              else e1' +++ e2'
        end
    | Exp.BinOp (Binop.MinusA as ominus, e1, e2)
    | Exp.BinOp (Binop.MinusPI as ominus, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        let isMinusA = ominus = Binop.MinusA in
        let oplus = if isMinusA then Binop.PlusA else Binop.PlusPI in
        let (+++) x y = Exp.BinOp (oplus, x, y) in
        let (---) x y = Exp.BinOp (ominus, x, y) in
        if Exp.equal e1' e2' then Exp.zero
        else begin
          match e1', e2' with
          | Exp.Const c, _ when Const.iszero_int_float c ->
              eval (Exp.UnOp(Unop.Neg, e2', None))
          | _, Exp.Const c when Const.iszero_int_float c ->
              e1'
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.int (n -- m)
          | Exp.Const (Const.Cfloat v), Exp.Const (Const.Cfloat w) ->
              Exp.float (v -. w)
          | _, Exp.UnOp (Unop.Neg, f2, _) ->
              eval (e1 +++ f2)
          | _ , Exp.Const(Const.Cint n) ->
              eval (e1' +++ (Exp.int (IntLit.neg n)))
          | Exp.Const _, _ ->
              e1' --- e2'
          | Exp.Var _, Exp.Var _ ->
              e1' --- e2'
          | _, _ ->
              if abs then Exp.get_undefined false else e1' --- e2'
        end
    | Exp.BinOp (Binop.MinusPP, e1, e2) ->
        if abs then Exp.get_undefined false
        else Exp.BinOp (Binop.MinusPP, eval e1, eval e2)
    | Exp.BinOp (Binop.Mult, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e1', e2' with
          | Exp.Const c, _ when Const.iszero_int_float c ->
              Exp.zero
          | Exp.Const c, _ when Const.isone_int_float c ->
              e2'
          | Exp.Const c, _ when Const.isminusone_int_float c ->
              eval (Exp.UnOp (Unop.Neg, e2', None))
          | _, Exp.Const c when Const.iszero_int_float c ->
              Exp.zero
          | _, Exp.Const c when Const.isone_int_float c ->
              e1'
          | _, Exp.Const c when Const.isminusone_int_float c ->
              eval (Exp.UnOp (Unop.Neg, e1', None))
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.int (IntLit.mul n m)
          | Exp.Const (Const.Cfloat v), Exp.Const (Const.Cfloat w) ->
              Exp.float (v *. w)
          | Exp.Var _, Exp.Var _ ->
              Exp.BinOp(Binop.Mult, e1', e2')
          | _, Exp.Sizeof _
          | Exp.Sizeof _, _ ->
              Exp.BinOp(Binop.Mult, e1', e2')
          | _, _ ->
              if abs then Exp.get_undefined false else Exp.BinOp(Binop.Mult, e1', e2')
        end
    | Exp.BinOp (Binop.Div, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e1', e2' with
          | _, Exp.Const c when Const.iszero_int_float c ->
              Exp.get_undefined false
          | Exp.Const c, _ when Const.iszero_int_float c ->
              e1'
          | _, Exp.Const c when Const.isone_int_float c ->
              e1'
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.int (IntLit.div n m)
          | Exp.Const (Const.Cfloat v), Exp.Const (Const.Cfloat w) ->
              Exp.float (v /.w)
          | Exp.Sizeof (Typ.Tarray (elt, _), Some len, _), Exp.Sizeof (elt2, None, _)
            (* pattern: sizeof(elt[len]) / sizeof(elt) = len *)
            when Typ.equal elt elt2 ->
              len
          | Exp.Sizeof (Typ.Tarray (elt, Some len), None, _), Exp.Sizeof (elt2, None, _)
            (* pattern: sizeof(elt[len]) / sizeof(elt) = len *)
            when Typ.equal elt elt2 ->
              Exp.Const (Const.Cint len)
          | _ ->
              if abs then Exp.get_undefined false else Exp.BinOp (Binop.Div, e1', e2')
        end
    | Exp.BinOp (Binop.Mod, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e1', e2' with
          | _, Exp.Const (Const.Cint i) when IntLit.iszero i ->
              Exp.get_undefined false
          | Exp.Const (Const.Cint i), _ when IntLit.iszero i ->
              e1'
          | _, Exp.Const (Const.Cint i) when IntLit.isone i ->
              Exp.zero
          | Exp.Const (Const.Cint n), Exp.Const (Const.Cint m) ->
              Exp.int (IntLit.rem n m)
          | _ ->
              if abs then Exp.get_undefined false else Exp.BinOp (Binop.Mod, e1', e2')
        end
    | Exp.BinOp (Binop.Shiftlt, e1, e2) ->
        if abs then Exp.get_undefined false else Exp.BinOp (Binop.Shiftlt, eval e1, eval e2)
    | Exp.BinOp (Binop.Shiftrt, e1, e2) ->
        if abs then Exp.get_undefined false else Exp.BinOp (Binop.Shiftrt, eval e1, eval e2)
    | Exp.BinOp (Binop.BAnd, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin match e1', e2' with
          | Exp.Const (Const.Cint i), _ when IntLit.iszero i ->
              e1'
          | _, Exp.Const (Const.Cint i) when IntLit.iszero i ->
              e2'
          | Exp.Const (Const.Cint i1), Exp.Const(Const.Cint i2) ->
              Exp.int (IntLit.logand i1 i2)
          | _ ->
              if abs then Exp.get_undefined false else Exp.BinOp (Binop.BAnd, e1', e2')
        end
    | Exp.BinOp (Binop.BOr, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin match e1', e2' with
          | Exp.Const (Const.Cint i), _ when IntLit.iszero i ->
              e2'
          | _, Exp.Const (Const.Cint i) when IntLit.iszero i ->
              e1'
          | Exp.Const (Const.Cint i1), Exp.Const(Const.Cint i2) ->
              Exp.int (IntLit.logor i1 i2)
          | _ ->
              if abs then Exp.get_undefined false else Exp.BinOp (Binop.BOr, e1', e2')
        end
    | Exp.BinOp (Binop.BXor, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin match e1', e2' with
          | Exp.Const (Const.Cint i), _ when IntLit.iszero i ->
              e2'
          | _, Exp.Const (Const.Cint i) when IntLit.iszero i ->
              e1'
          | Exp.Const (Const.Cint i1), Exp.Const(Const.Cint i2) ->
              Exp.int (IntLit.logxor i1 i2)
          | _ ->
              if abs then Exp.get_undefined false else Exp.BinOp (Binop.BXor, e1', e2')
        end
    | Exp.BinOp (Binop.PtrFld, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e2' with
          | Exp.Const (Const.Cptr_to_fld (fn, typ)) ->
              eval (Exp.Lfield(e1', fn, typ))
          | Exp.Const (Const.Cint i) when IntLit.iszero i ->
              Exp.zero (* cause a NULL dereference *)
          | _ -> Exp.BinOp (Binop.PtrFld, e1', e2')
        end
    | Exp.Exn _ ->
        e
    | Exp.Lvar _ ->
        e
    | Exp.Lfield (e1, fld, typ) ->
        let e1' = eval e1 in
        Exp.Lfield (e1', fld, typ)
    | Exp.Lindex(Exp.Lvar pv, e2) when false
      (* removed: it interferes with re-arrangement and error messages *)
      -> (* &x[n]  -->  &x + n *)
        eval (Exp.BinOp (Binop.PlusPI, Exp.Lvar pv, e2))
    | Exp.Lindex (Exp.BinOp(Binop.PlusPI, ep, e1), e2) -> (* array access with pointer arithmetic *)
        let e' = Exp.BinOp (Binop.PlusA, e1, e2) in
        eval (Exp.Lindex (ep, e'))
    | Exp.Lindex (e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        Exp.Lindex(e1', e2') in
  let e' = eval e in
  (* L.d_str "sym_eval "; Sil.d_exp e; L.d_str" --> "; Sil.d_exp e'; L.d_ln (); *)
  e'

let exp_normalize sub exp =
  let exp' = Sil.exp_sub sub exp in
  if !Config.abs_val >= 1 then sym_eval true exp'
  else sym_eval false exp'

let texp_normalize sub exp = match exp with
  | Exp.Sizeof (typ, len, st) -> Exp.Sizeof (typ, Option.map (exp_normalize sub) len, st)
  | _ -> exp_normalize sub exp

let exp_normalize_noabs sub exp =
  Config.run_with_abs_val_equal_zero (exp_normalize sub) exp

(** Return [true] if the atom is an inequality *)
let atom_is_inequality = function
  | Sil.Aeq (Exp.BinOp (Binop.Le, _, _), Exp.Const (Const.Cint i)) when IntLit.isone i -> true
  | Sil.Aeq (Exp.BinOp (Binop.Lt, _, _), Exp.Const (Const.Cint i)) when IntLit.isone i -> true
  | _ -> false

(** If the atom is [e<=n] return [e,n] *)
let atom_exp_le_const = function
  | Sil.Aeq(Exp.BinOp (Binop.Le, e1, Exp.Const (Const.Cint n)), Exp.Const (Const.Cint i))
    when IntLit.isone i ->
      Some (e1, n)
  | _ -> None

(** If the atom is [n<e] return [n,e] *)
let atom_const_lt_exp = function
  | Sil.Aeq(Exp.BinOp (Binop.Lt, Exp.Const (Const.Cint n), e1), Exp.Const (Const.Cint i))
    when IntLit.isone i ->
      Some (n, e1)
  | _ -> None

(** Turn an inequality expression into an atom *)
let mk_inequality e =
  match e with
  | Exp.BinOp (Binop.Le, base, Exp.Const (Const.Cint n)) ->
      (* base <= n case *)
      let nbase = exp_normalize_noabs Sil.sub_empty base in
      (match nbase with
       | Exp.BinOp(Binop.PlusA, base', Exp.Const (Const.Cint n')) ->
           let new_offset = Exp.int (n -- n') in
           let new_e = Exp.BinOp (Binop.Le, base', new_offset) in
           Sil.Aeq (new_e, Exp.one)
       | Exp.BinOp(Binop.PlusA, Exp.Const (Const.Cint n'), base') ->
           let new_offset = Exp.int (n -- n') in
           let new_e = Exp.BinOp (Binop.Le, base', new_offset) in
           Sil.Aeq (new_e, Exp.one)
       | Exp.BinOp(Binop.MinusA, base', Exp.Const (Const.Cint n')) ->
           let new_offset = Exp.int (n ++ n') in
           let new_e = Exp.BinOp (Binop.Le, base', new_offset) in
           Sil.Aeq (new_e, Exp.one)
       | Exp.BinOp(Binop.MinusA, Exp.Const (Const.Cint n'), base') ->
           let new_offset = Exp.int (n' -- n -- IntLit.one) in
           let new_e = Exp.BinOp (Binop.Lt, new_offset, base') in
           Sil.Aeq (new_e, Exp.one)
       | Exp.UnOp(Unop.Neg, new_base, _) ->
           (* In this case, base = -new_base. Construct -n-1 < new_base. *)
           let new_offset = Exp.int (IntLit.zero -- n -- IntLit.one) in
           let new_e = Exp.BinOp (Binop.Lt, new_offset, new_base) in
           Sil.Aeq (new_e, Exp.one)
       | _ -> Sil.Aeq (e, Exp.one))
  | Exp.BinOp (Binop.Lt, Exp.Const (Const.Cint n), base) ->
      (* n < base case *)
      let nbase = exp_normalize_noabs Sil.sub_empty base in
      (match nbase with
       | Exp.BinOp(Binop.PlusA, base', Exp.Const (Const.Cint n')) ->
           let new_offset = Exp.int (n -- n') in
           let new_e = Exp.BinOp (Binop.Lt, new_offset, base') in
           Sil.Aeq (new_e, Exp.one)
       | Exp.BinOp(Binop.PlusA, Exp.Const (Const.Cint n'), base') ->
           let new_offset = Exp.int (n -- n') in
           let new_e = Exp.BinOp (Binop.Lt, new_offset, base') in
           Sil.Aeq (new_e, Exp.one)
       | Exp.BinOp(Binop.MinusA, base', Exp.Const (Const.Cint n')) ->
           let new_offset = Exp.int (n ++ n') in
           let new_e = Exp.BinOp (Binop.Lt, new_offset, base') in
           Sil.Aeq (new_e, Exp.one)
       | Exp.BinOp(Binop.MinusA, Exp.Const (Const.Cint n'), base') ->
           let new_offset = Exp.int (n' -- n -- IntLit.one) in
           let new_e = Exp.BinOp (Binop.Le, base', new_offset) in
           Sil.Aeq (new_e, Exp.one)
       | Exp.UnOp(Unop.Neg, new_base, _) ->
           (* In this case, base = -new_base. Construct new_base <= -n-1 *)
           let new_offset = Exp.int (IntLit.zero -- n -- IntLit.one) in
           let new_e = Exp.BinOp (Binop.Le, new_base, new_offset) in
           Sil.Aeq (new_e, Exp.one)
       | _ -> Sil.Aeq (e, Exp.one))
  | _ -> Sil.Aeq (e, Exp.one)

(** Normalize an inequality *)
let inequality_normalize a =
  (* turn an expression into a triple (pos,neg,off) of positive and negative occurrences, and
     integer offset representing inequality [sum(pos) - sum(neg) + off <= 0] *)
  let rec exp_to_posnegoff e = match e with
    | Exp.Const (Const.Cint n) -> [],[], n
    | Exp.BinOp(Binop.PlusA, e1, e2) | Exp.BinOp(Binop.PlusPI, e1, e2) ->
        let pos1, neg1, n1 = exp_to_posnegoff e1 in
        let pos2, neg2, n2 = exp_to_posnegoff e2 in
        (pos1@pos2, neg1@neg2, n1 ++ n2)
    | Exp.BinOp(Binop.MinusA, e1, e2)
    | Exp.BinOp(Binop.MinusPI, e1, e2)
    | Exp.BinOp(Binop.MinusPP, e1, e2) ->
        let pos1, neg1, n1 = exp_to_posnegoff e1 in
        let pos2, neg2, n2 = exp_to_posnegoff e2 in
        (pos1@neg2, neg1@pos2, n1 -- n2)
    | Exp.UnOp(Unop.Neg, e1, _) ->
        let pos1, neg1, n1 = exp_to_posnegoff e1 in
        (neg1, pos1, IntLit.zero -- n1)
    | _ -> [e],[], IntLit.zero in
  (* sort and filter out expressions appearing in both the positive and negative part *)
  let normalize_posnegoff (pos, neg, off) =
    let pos' = IList.sort Exp.compare pos in
    let neg' = IList.sort Exp.compare neg in
    let rec combine pacc nacc = function
      | x:: ps, y:: ng ->
          (match Exp.compare x y with
           | n when n < 0 -> combine (x:: pacc) nacc (ps, y :: ng)
           | 0 -> combine pacc nacc (ps, ng)
           | _ -> combine pacc (y:: nacc) (x :: ps, ng))
      | ps, ng -> (IList.rev pacc) @ ps, (IList.rev nacc) @ ng in
    let pos'', neg'' = combine [] [] (pos', neg') in
    (pos'', neg'', off) in
  (* turn a non-empty list of expressions into a sum expression *)
  let rec exp_list_to_sum = function
    | [] -> assert false
    | [e] -> e
    | e:: el -> Exp.BinOp(Binop.PlusA, e, exp_list_to_sum el) in
  let norm_from_exp e =
    match normalize_posnegoff (exp_to_posnegoff e) with
    | [],[], n -> Exp.BinOp(Binop.Le, Exp.int n, Exp.zero)
    | [], neg, n -> Exp.BinOp(Binop.Lt, Exp.int (n -- IntLit.one), exp_list_to_sum neg)
    | pos, [], n -> Exp.BinOp(Binop.Le, exp_list_to_sum pos, Exp.int (IntLit.zero -- n))
    | pos, neg, n ->
        let lhs_e = Exp.BinOp(Binop.MinusA, exp_list_to_sum pos, exp_list_to_sum neg) in
        Exp.BinOp(Binop.Le, lhs_e, Exp.int (IntLit.zero -- n)) in
  let ineq = match a with
    | Sil.Aeq (ineq, Exp.Const (Const.Cint i)) when IntLit.isone i ->
        ineq
    | _ -> assert false in
  match ineq with
  | Exp.BinOp(Binop.Le, e1, e2) ->
      let e = Exp.BinOp(Binop.MinusA, e1, e2) in
      mk_inequality (norm_from_exp e)
  | Exp.BinOp(Binop.Lt, e1, e2) ->
      let e = Exp.BinOp(Binop.MinusA, Exp.BinOp(Binop.MinusA, e1, e2), Exp.minus_one) in
      mk_inequality (norm_from_exp e)
  | _ -> a

let exp_reorder e1 e2 = if Exp.compare e1 e2 <= 0 then (e1, e2) else (e2, e1)

(** Normalize an atom.
    We keep the convention that inequalities with constants
    are only of the form [e <= n] and [n < e]. *)
let atom_normalize sub a0 =
  let a = Sil.atom_sub sub a0 in
  let rec normalize_eq eq = match eq with
    | Exp.BinOp(Binop.PlusA, e1, Exp.Const (Const.Cint n1)), Exp.Const (Const.Cint n2)
    (* e1+n1==n2 ---> e1==n2-n1 *)
    | Exp.BinOp(Binop.PlusPI, e1, Exp.Const (Const.Cint n1)), Exp.Const (Const.Cint n2) ->
        (e1, Exp.int (n2 -- n1))
    | Exp.BinOp(Binop.MinusA, e1, Exp.Const (Const.Cint n1)), Exp.Const (Const.Cint n2)
    (* e1-n1==n2 ---> e1==n1+n2 *)
    | Exp.BinOp(Binop.MinusPI, e1, Exp.Const (Const.Cint n1)), Exp.Const (Const.Cint n2) ->
        (e1, Exp.int (n1 ++ n2))
    | Exp.BinOp(Binop.MinusA, Exp.Const (Const.Cint n1), e1), Exp.Const (Const.Cint n2) ->
        (* n1-e1 == n2 -> e1==n1-n2 *)
        (e1, Exp.int (n1 -- n2))
    | Exp.Lfield (e1', fld1, _), Exp.Lfield (e2', fld2, _) ->
        if Ident.fieldname_equal fld1 fld2
        then normalize_eq (e1', e2')
        else eq
    | Exp.Lindex (e1', idx1), Exp.Lindex (e2', idx2) ->
        if Exp.equal idx1 idx2 then normalize_eq (e1', e2')
        else if Exp.equal e1' e2' then normalize_eq (idx1, idx2)
        else eq
    | _ -> eq in
  let handle_unary_negation e1 e2 =
    match e1, e2 with
    | Exp.UnOp (Unop.LNot, e1', _), Exp.Const (Const.Cint i)
    | Exp.Const (Const.Cint i), Exp.UnOp (Unop.LNot, e1', _) when IntLit.iszero i ->
        (e1', Exp.zero, true)
    | _ -> (e1, e2, false) in
  let handle_boolean_operation from_equality e1 e2 =
    let ne1 = exp_normalize sub e1 in
    let ne2 = exp_normalize sub e2 in
    let ne1', ne2', op_negated = handle_unary_negation ne1 ne2 in
    let (e1', e2') = normalize_eq (ne1', ne2') in
    let (e1'', e2'') = exp_reorder e1' e2' in
    let use_equality =
      if op_negated then not from_equality else from_equality in
    if use_equality then
      Sil.Aeq (e1'', e2'')
    else
      Sil.Aneq (e1'', e2'') in
  let a' = match a with
    | Sil.Aeq (e1, e2) ->
        handle_boolean_operation true e1 e2
    | Sil.Aneq (e1, e2) ->
        handle_boolean_operation false e1 e2
    | Sil.Apred (a, es) ->
        Sil.Apred (a, IList.map (fun e -> exp_normalize sub e) es)
    | Sil.Anpred (a, es) ->
        Sil.Anpred (a, IList.map (fun e -> exp_normalize sub e) es) in
  if atom_is_inequality a' then inequality_normalize a' else a'

(** Negate an atom *)
let atom_negate = function
  | Sil.Aeq (Exp.BinOp (Binop.Le, e1, e2), Exp.Const (Const.Cint i)) when IntLit.isone i ->
      mk_inequality (Exp.lt e2 e1)
  | Sil.Aeq (Exp.BinOp (Binop.Lt, e1, e2), Exp.Const (Const.Cint i)) when IntLit.isone i ->
      mk_inequality (Exp.le e2 e1)
  | Sil.Aeq (e1, e2) -> Sil.Aneq (e1, e2)
  | Sil.Aneq (e1, e2) -> Sil.Aeq (e1, e2)
  | Sil.Apred (a, es) -> Sil.Anpred (a, es)
  | Sil.Anpred (a, es) -> Sil.Apred (a, es)

let rec strexp_normalize sub se =
  match se with
  | Sil.Eexp (e, inst) ->
      Sil.Eexp (exp_normalize sub e, inst)
  | Sil.Estruct (fld_cnts, inst) ->
      begin
        match fld_cnts with
        | [] -> se
        | _ ->
            let fld_cnts' =
              IList.map (fun (fld, cnt) ->
                  fld, strexp_normalize sub cnt) fld_cnts in
            let fld_cnts'' = IList.sort Sil.fld_strexp_compare fld_cnts' in
            Sil.Estruct (fld_cnts'', inst)
      end
  | Sil.Earray (len, idx_cnts, inst) ->
      begin
        let len' = exp_normalize_noabs sub len in
        match idx_cnts with
        | [] ->
            if Exp.equal len len' then se else Sil.Earray (len', idx_cnts, inst)
        | _ ->
            let idx_cnts' =
              IList.map (fun (idx, cnt) ->
                  let idx' = exp_normalize sub idx in
                  idx', strexp_normalize sub cnt) idx_cnts in
            let idx_cnts'' =
              IList.sort Sil.exp_strexp_compare idx_cnts' in
            Sil.Earray (len', idx_cnts'', inst)
      end

(** create a strexp of the given type, populating the structures if [expand_structs] is true *)
let rec create_strexp_of_type tenvo struct_init_mode typ len inst =
  let init_value () =
    let create_fresh_var () =
      let fresh_id =
        (Ident.create_fresh (if !Config.footprint then Ident.kfootprint else Ident.kprimed)) in
      Exp.Var fresh_id in
    if !Config.curr_language = Config.Java && inst = Sil.Ialloc
    then
      match typ with
      | Typ.Tfloat _ -> Exp.Const (Const.Cfloat 0.0)
      | _ -> Exp.zero
    else
      create_fresh_var () in
  match typ, len with
  | (Typ.Tint _ | Typ.Tfloat _ | Typ.Tvoid | Typ.Tfun _ | Typ.Tptr _), None ->
      Sil.Eexp (init_value (), inst)
  | Typ.Tstruct { Typ.instance_fields }, _ -> (
      match struct_init_mode with
      | No_init ->
          Sil.Estruct ([], inst)
      | Fld_init ->
          (* pass len as an accumulator, so that it is passed to create_strexp_of_type for the last
             field, but always return None so that only the last field receives len *)
          let f (fld, t, a) (flds, len) =
            if Typ.is_objc_ref_counter_field (fld, t, a) then
              ((fld, Sil.Eexp (Exp.one, inst)) :: flds, None)
            else
              ((fld, create_strexp_of_type tenvo struct_init_mode t len inst) :: flds, None) in
          let flds, _ = IList.fold_right f instance_fields ([], len) in
          Sil.Estruct (flds, inst)
    )
  | Typ.Tarray (_, len_opt), None ->
      let len = match len_opt with
        | None -> Exp.get_undefined false
        | Some len -> Exp.Const (Const.Cint len) in
      Sil.Earray (len, [], inst)
  | Typ.Tarray _, Some len ->
      Sil.Earray (len, [], inst)
  | Typ.Tvar _, _
  | (Typ.Tint _ | Typ.Tfloat _ | Typ.Tvoid | Typ.Tfun _ | Typ.Tptr _), Some _ ->
      assert false

(** Exp.Construct a pointsto. *)
let mk_ptsto lexp sexp te =
  let nsexp = strexp_normalize Sil.sub_empty sexp in
  Sil.Hpointsto(lexp, nsexp, te)

(** Construct a points-to predicate for an expression using
    either the provided expression [name] as
    base for fresh identifiers. If [expand_structs] is true,
    initialize the fields of structs with fresh variables. *)
let mk_ptsto_exp tenvo struct_init_mode (exp, te, expo) inst : Sil.hpred =
  let default_strexp () = match te with
    | Exp.Sizeof (typ, len, _) ->
        create_strexp_of_type tenvo struct_init_mode typ len inst
    | Exp.Var _ ->
        Sil.Estruct ([], inst)
    | te ->
        L.err "trying to create ptsto with type: %a@\n@." (Sil.pp_texp_full pe_text) te;
        assert false in
  let strexp = match expo with
    | Some e -> Sil.Eexp (e, inst)
    | None -> default_strexp () in
  mk_ptsto exp strexp te

let replace_array_contents hpred esel = match hpred with
  | Sil.Hpointsto (root, Sil.Earray (len, [], inst), te) ->
      Sil.Hpointsto (root, Sil.Earray (len, esel, inst), te)
  | _ -> assert false

let rec hpred_normalize sub hpred =
  let replace_hpred hpred' =
    L.d_strln "found array with sizeof(..) size";
    L.d_str "converting original hpred: "; Sil.d_hpred hpred; L.d_ln ();
    L.d_str "into the following: "; Sil.d_hpred hpred'; L.d_ln ();
    hpred' in
  match hpred with
  | Sil.Hpointsto (root, cnt, te) ->
      let normalized_root = exp_normalize sub root in
      let normalized_cnt = strexp_normalize sub cnt in
      let normalized_te = texp_normalize sub te in
      begin match normalized_cnt, normalized_te with
        | Sil.Earray (Exp.Sizeof _ as size, [], inst), Exp.Sizeof (Typ.Tarray _, _, _) ->
            (* check for an empty array whose size expression is (Sizeof type), and turn the array
               into a strexp of the given type *)
            let hpred' = mk_ptsto_exp None Fld_init (root, size, None) inst in
            replace_hpred hpred'
        | ( Sil.Earray (Exp.BinOp (Binop.Mult, Exp.Sizeof (t, None, st1), x), esel, inst)
          | Sil.Earray (Exp.BinOp (Binop.Mult, x, Exp.Sizeof (t, None, st1)), esel, inst)),
          Exp.Sizeof (Typ.Tarray (elt, _) as arr, _, _)
          when Typ.equal t elt ->
            let len = Some x in
            let hpred' = mk_ptsto_exp None Fld_init (root, Exp.Sizeof (arr, len, st1), None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | ( Sil.Earray (Exp.BinOp (Binop.Mult, Exp.Sizeof (t, Some len, st1), x), esel, inst)
          | Sil.Earray (Exp.BinOp (Binop.Mult, x, Exp.Sizeof (t, Some len, st1)), esel, inst)),
          Exp.Sizeof (Typ.Tarray (elt, _) as arr, _, _)
          when Typ.equal t elt ->
            let len = Some (Exp.BinOp(Binop.Mult, x, len)) in
            let hpred' = mk_ptsto_exp None Fld_init (root, Exp.Sizeof (arr, len, st1), None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | _ -> Sil.Hpointsto (normalized_root, normalized_cnt, normalized_te)
      end
  | Sil.Hlseg (k, para, e1, e2, elist) ->
      let normalized_e1 = exp_normalize sub e1 in
      let normalized_e2 = exp_normalize sub e2 in
      let normalized_elist = IList.map (exp_normalize sub) elist in
      let normalized_para = hpara_normalize para in
      Sil.Hlseg (k, normalized_para, normalized_e1, normalized_e2, normalized_elist)
  | Sil.Hdllseg (k, para, e1, e2, e3, e4, elist) ->
      let norm_e1 = exp_normalize sub e1 in
      let norm_e2 = exp_normalize sub e2 in
      let norm_e3 = exp_normalize sub e3 in
      let norm_e4 = exp_normalize sub e4 in
      let norm_elist = IList.map (exp_normalize sub) elist in
      let norm_para = hpara_dll_normalize para in
      Sil.Hdllseg (k, norm_para, norm_e1, norm_e2, norm_e3, norm_e4, norm_elist)

and hpara_normalize para =
  let normalized_body = IList.map (hpred_normalize Sil.sub_empty) (para.Sil.body) in
  let sorted_body = IList.sort Sil.hpred_compare normalized_body in
  { para with Sil.body = sorted_body }

and hpara_dll_normalize para =
  let normalized_body = IList.map (hpred_normalize Sil.sub_empty) (para.Sil.body_dll) in
  let sorted_body = IList.sort Sil.hpred_compare normalized_body in
  { para with Sil.body_dll = sorted_body }

let pi_tighten_ineq pi =
  let ineq_list, nonineq_list = IList.partition atom_is_inequality pi in
  let diseq_list =
    let get_disequality_info acc = function
      | Sil.Aneq(Exp.Const (Const.Cint n), e)
      | Sil.Aneq(e, Exp.Const (Const.Cint n)) -> (e, n) :: acc
      | _ -> acc in
    IList.fold_left get_disequality_info [] nonineq_list in
  let is_neq e n =
    IList.exists (fun (e', n') -> Exp.equal e e' && IntLit.eq n n') diseq_list in
  let le_list_tightened =
    let get_le_inequality_info acc a =
      match atom_exp_le_const a with
      | Some (e, n) -> (e, n):: acc
      | _ -> acc in
    let rec le_tighten le_list_done = function
      | [] -> IList.rev le_list_done
      | (e, n):: le_list_todo -> (* e <= n *)
          if is_neq e n then le_tighten le_list_done ((e, n -- IntLit.one):: le_list_todo)
          else le_tighten ((e, n):: le_list_done) (le_list_todo) in
    let le_list = IList.rev (IList.fold_left get_le_inequality_info [] ineq_list) in
    le_tighten [] le_list in
  let lt_list_tightened =
    let get_lt_inequality_info acc a =
      match atom_const_lt_exp a with
      | Some (n, e) -> (n, e):: acc
      | _ -> acc in
    let rec lt_tighten lt_list_done = function
      | [] -> IList.rev lt_list_done
      | (n, e):: lt_list_todo -> (* n < e *)
          let n_plus_one = n ++ IntLit.one in
          if is_neq e n_plus_one then lt_tighten lt_list_done ((n ++ IntLit.one, e):: lt_list_todo)
          else lt_tighten ((n, e):: lt_list_done) (lt_list_todo) in
    let lt_list = IList.rev (IList.fold_left get_lt_inequality_info [] ineq_list) in
    lt_tighten [] lt_list in
  let ineq_list' =
    let le_ineq_list =
      IList.map
        (fun (e, n) -> mk_inequality (Exp.BinOp(Binop.Le, e, Exp.int n)))
        le_list_tightened in
    let lt_ineq_list =
      IList.map
        (fun (n, e) -> mk_inequality (Exp.BinOp(Binop.Lt, Exp.int n, e)))
        lt_list_tightened in
    le_ineq_list @ lt_ineq_list in
  let nonineq_list' =
    IList.filter
      (function
        | Sil.Aneq(Exp.Const (Const.Cint n), e)
        | Sil.Aneq(e, Exp.Const (Const.Cint n)) ->
            (not (IList.exists
                    (fun (e', n') -> Exp.equal e e' && IntLit.lt n' n)
                    le_list_tightened)) &&
            (not (IList.exists
                    (fun (n', e') -> Exp.equal e e' && IntLit.leq n n')
                    lt_list_tightened))
        | _ -> true)
      nonineq_list in
  (ineq_list', nonineq_list')

(** remove duplicate atoms and redundant inequalities from a sorted pi *)
let rec pi_sorted_remove_redundant = function
  | (Sil.Aeq (Exp.BinOp (Binop.Le, e1, Exp.Const (Const.Cint n1)),
              Exp.Const (Const.Cint i1)) as a1) ::
    Sil.Aeq (Exp.BinOp (Binop.Le, e2, Exp.Const (Const.Cint n2)),
             Exp.Const (Const.Cint i2)) :: rest
    when IntLit.isone i1 && IntLit.isone i2 && Exp.equal e1 e2 && IntLit.lt n1 n2 ->
      (* second inequality redundant *)
      pi_sorted_remove_redundant (a1 :: rest)
  | Sil.Aeq (Exp.BinOp (Binop.Lt, Exp.Const (Const.Cint n1), e1), Exp.Const (Const.Cint i1)) ::
    (Sil.Aeq (Exp.BinOp (Binop.Lt, Exp.Const (Const.Cint n2), e2), Exp.Const (Const.Cint i2)) as a2)
    :: rest
    when IntLit.isone i1 && IntLit.isone i2 && Exp.equal e1 e2 && IntLit.lt n1 n2 ->
      (* first inequality redundant *)
      pi_sorted_remove_redundant (a2 :: rest)
  | a1:: a2:: rest ->
      if Sil.atom_equal a1 a2 then pi_sorted_remove_redundant (a2 :: rest)
      else a1 :: pi_sorted_remove_redundant (a2 :: rest)
  | [a] -> [a]
  | [] -> []

(** find the unsigned expressions in sigma (immediately inside a pointsto, for now) *)
let sigma_get_unsigned_exps sigma =
  let uexps = ref [] in
  let do_hpred = function
    | Sil.Hpointsto (_, Sil.Eexp (e, _), Exp.Sizeof (Typ.Tint ik, _, _))
      when Typ.ikind_is_unsigned ik ->
        uexps := e :: !uexps
    | _ -> () in
  IList.iter do_hpred sigma;
  !uexps

(** Normalization of pi.
    The normalization filters out obviously - true disequalities, such as e <> e + 1. *)
let pi_normalize sub sigma pi0 =
  let pi = IList.map (atom_normalize sub) pi0 in
  let ineq_list, nonineq_list = pi_tighten_ineq pi in
  let syntactically_different = function
    | Exp.BinOp(op1, e1, Exp.Const(c1)), Exp.BinOp(op2, e2, Exp.Const(c2))
      when Exp.equal e1 e2 ->
        Binop.equal op1 op2 && Binop.injective op1 && not (Const.equal c1 c2)
    | e1, Exp.BinOp(op2, e2, Exp.Const(c2))
      when Exp.equal e1 e2 ->
        Binop.injective op2 &&
        Binop.is_zero_runit op2 &&
        not (Const.equal (Const.Cint IntLit.zero) c2)
    | Exp.BinOp(op1, e1, Exp.Const(c1)), e2
      when Exp.equal e1 e2 ->
        Binop.injective op1 &&
        Binop.is_zero_runit op1 &&
        not (Const.equal (Const.Cint IntLit.zero) c1)
    | _ -> false in
  let filter_useful_atom =
    let unsigned_exps = lazy (sigma_get_unsigned_exps sigma) in
    function
    | Sil.Aneq ((Exp.Var _) as e, Exp.Const (Const.Cint n)) when IntLit.isnegative n ->
        not (IList.exists (Exp.equal e) (Lazy.force unsigned_exps))
    | Sil.Aneq(e1, e2) ->
        not (syntactically_different (e1, e2))
    | Sil.Aeq(Exp.Const c1, Exp.Const c2) ->
        not (Const.equal c1 c2)
    | _ -> true in
  let pi' =
    IList.stable_sort
      Sil.atom_compare
      ((IList.filter filter_useful_atom nonineq_list) @ ineq_list) in
  let pi'' = pi_sorted_remove_redundant pi' in
  if pi_equal pi0 pi'' then pi0 else pi''

let sigma_normalize sub sigma =
  let sigma' =
    IList.stable_sort Sil.hpred_compare (IList.map (hpred_normalize sub) sigma) in
  if sigma_equal sigma sigma' then sigma else sigma'

(** normalize the footprint part, and rename any primed vars
    in the footprint with fresh footprint vars *)
let footprint_normalize prop =
  let nsigma = sigma_normalize Sil.sub_empty prop.sigma_fp in
  let npi = pi_normalize Sil.sub_empty nsigma prop.pi_fp in
  let fp_vars =
    let fav = pi_fav npi in
    sigma_fav_add fav nsigma;
    fav in
  (* TODO (t4893479): make this check less angelic *)
  if Sil.fav_exists fp_vars Ident.is_normal && not Config.angelic_execution then
    begin
      L.d_strln "footprint part contains normal variables";
      d_pi npi; L.d_ln ();
      d_sigma nsigma; L.d_ln ();
      assert false
    end;
  Sil.fav_filter_ident fp_vars Ident.is_primed; (* only keep primed vars *)
  let npi', nsigma' =
    if Sil.fav_is_empty fp_vars then npi, nsigma
    else (* replace primed vars by fresh footprint vars *)
      let ids_primed = Sil.fav_to_list fp_vars in
      let ids_footprint =
        IList.map (fun id -> (id, Ident.create_fresh Ident.kfootprint)) ids_primed in
      let ren_sub =
        Sil.sub_of_list (IList.map (fun (id1, id2) -> (id1, Exp.Var id2)) ids_footprint) in
      let nsigma' = sigma_normalize Sil.sub_empty (sigma_sub ren_sub nsigma) in
      let npi' = pi_normalize Sil.sub_empty nsigma' (pi_sub ren_sub npi) in
      (npi', nsigma') in
  set prop ~pi_fp:npi' ~sigma_fp:nsigma'

let exp_normalize_prop prop exp =
  Config.run_with_abs_val_equal_zero (exp_normalize prop.sub) exp

let lexp_normalize_prop p lexp =
  let root = Exp.root_of_lexp lexp in
  let offsets = Sil.exp_get_offsets lexp in
  let nroot = exp_normalize_prop p root in
  let noffsets =
    IList.map (fun n -> match n with
        | Sil.Off_fld _ -> n
        | Sil.Off_index e -> Sil.Off_index (exp_normalize_prop p e)
      ) offsets in
  Sil.exp_add_offsets nroot noffsets

(** Collapse consecutive indices that should be added. For instance,
    this function reduces x[1][1] to x[2]. The [typ] argument is used
    to ensure the soundness of this collapsing. *)
let exp_collapse_consecutive_indices_prop typ exp =
  let typ_is_base = function
    | Typ.Tint _ | Typ.Tfloat _ | Typ.Tstruct _ | Typ.Tvoid | Typ.Tfun _ -> true
    | _ -> false in
  let typ_is_one_step_from_base =
    match typ with
    | Typ.Tptr (t, _) | Typ.Tarray (t, _) -> typ_is_base t
    | _ -> false in
  let rec exp_remove e0 =
    match e0 with
    | Exp.Lindex(Exp.Lindex(base, e1), e2) ->
        let e0' = Exp.Lindex(base, Exp.BinOp(Binop.PlusA, e1, e2)) in
        exp_remove e0'
    | _ -> e0 in
  begin
    if typ_is_one_step_from_base then exp_remove exp else exp
  end

let atom_normalize_prop prop atom =
  Config.run_with_abs_val_equal_zero (atom_normalize prop.sub) atom

let strexp_normalize_prop prop strexp =
  Config.run_with_abs_val_equal_zero (strexp_normalize prop.sub) strexp

let hpred_normalize_prop prop hpred =
  Config.run_with_abs_val_equal_zero (hpred_normalize prop.sub) hpred

let sigma_normalize_prop prop sigma =
  Config.run_with_abs_val_equal_zero (sigma_normalize prop.sub) sigma

let pi_normalize_prop prop pi =
  Config.run_with_abs_val_equal_zero (pi_normalize prop.sub prop.sigma) pi

(** {2 Compaction} *)

(** Return a compact representation of the prop *)
let prop_compact sh (prop : normal t) : normal t =
  let sigma' = IList.map (Sil.hpred_compact sh) prop.sigma in
  unsafe_cast_to_normal (set prop ~sigma:sigma')

(** {2 Function for replacing occurrences of expressions.} *)

let sigma_replace_exp epairs sigma =
  let sigma' = IList.map (Sil.hpred_replace_exp epairs) sigma in
  sigma_normalize Sil.sub_empty sigma'

(** {2 Query about Proposition} *)

(** Check if the sigma part of the proposition is emp *)
let prop_is_emp p = match p.sigma with
  | [] -> true
  | _ -> false

(** {2 Functions for changing and generating propositions} *)

(** Construct an atom. *)
let mk_atom atom =
  Config.run_with_abs_val_equal_zero (fun () -> atom_normalize Sil.sub_empty atom) ()

(** Exp.Construct a disequality. *)
let mk_neq e1 e2 = mk_atom (Aneq (e1, e2))

(** Exp.Construct an equality. *)
let mk_eq e1 e2 = mk_atom (Aeq (e1, e2))

(** Construct a pred. *)
let mk_pred a es = mk_atom (Apred (a, es))

(** Construct a negated pred. *)
let mk_npred a es = mk_atom (Anpred (a, es))

(** Construct a points-to predicate for a single program variable.
    If [expand_structs] is true, initialize the fields of structs with fresh variables. *)
let mk_ptsto_lvar tenv expand_structs inst ((pvar: Pvar.t), texp, expo) : Sil.hpred =
  mk_ptsto_exp tenv expand_structs (Exp.Lvar pvar, texp, expo) inst

(** Exp.Construct a lseg predicate *)
let mk_lseg k para e_start e_end es_shared =
  let npara = hpara_normalize para in
  Sil.Hlseg (k, npara, e_start, e_end, es_shared)

(** Exp.Construct a dllseg predicate *)
let mk_dllseg k para exp_iF exp_oB exp_oF exp_iB exps_shared =
  let npara = hpara_dll_normalize para in
  Sil.Hdllseg (k, npara, exp_iF, exp_oB , exp_oF, exp_iB, exps_shared)

(** Exp.Construct a hpara *)
let mk_hpara root next svars evars body =
  let para =
    { Sil.root = root;
      next = next;
      svars = svars;
      evars = evars;
      body = body } in
  hpara_normalize para

(** Exp.Construct a dll_hpara *)
let mk_dll_hpara iF oB oF svars evars body =
  let para =
    { Sil.cell = iF;
      blink = oB;
      flink = oF;
      svars_dll = svars;
      evars_dll = evars;
      body_dll = body } in
  hpara_dll_normalize para

(** Conjoin a heap predicate by separating conjunction. *)
let prop_hpred_star (p : 'a t) (h : Sil.hpred) : exposed t =
  let sigma' = h:: p.sigma in
  set p ~sigma:sigma'

let prop_sigma_star (p : 'a t) (sigma : sigma) : exposed t =
  let sigma' = sigma @ p.sigma in
  set p ~sigma:sigma'

(** return the set of subexpressions of [strexp] *)
let strexp_get_exps strexp =
  let rec strexp_get_exps_rec exps = function
    | Sil.Eexp (Exp.Exn e, _) -> Exp.Set.add e exps
    | Sil.Eexp (e, _) -> Exp.Set.add e exps
    | Sil.Estruct (flds, _) ->
        IList.fold_left (fun exps (_, strexp) -> strexp_get_exps_rec exps strexp) exps flds
    | Sil.Earray (_, elems, _) ->
        IList.fold_left (fun exps (_, strexp) -> strexp_get_exps_rec exps strexp) exps elems in
  strexp_get_exps_rec Exp.Set.empty strexp

(** get the set of expressions on the righthand side of [hpred] *)
let hpred_get_targets = function
  | Sil.Hpointsto (_, rhs, _) -> strexp_get_exps rhs
  | Sil.Hlseg (_, _, _, e, el) ->
      IList.fold_left (fun exps e -> Exp.Set.add e exps) Exp.Set.empty (e :: el)
  | Sil.Hdllseg (_, _, _, oB, oF, iB, el) ->
      (* only one direction supported for now *)
      IList.fold_left (fun exps e -> Exp.Set.add e exps) Exp.Set.empty (oB :: oF :: iB :: el)

(** return the set of hpred's and exp's in [sigma] that are reachable from an expression in
    [exps] *)
let compute_reachable_hpreds sigma exps =
  let rec compute_reachable_hpreds_rec sigma (reach, exps) =
    let add_hpred_if_reachable (reach, exps) = function
      | Sil.Hpointsto (lhs, _, _) as hpred when Exp.Set.mem lhs exps->
          let reach' = Sil.HpredSet.add hpred reach in
          let reach_exps = hpred_get_targets hpred in
          (reach', Exp.Set.union exps reach_exps)
      | _ -> reach, exps in
    let reach', exps' = IList.fold_left add_hpred_if_reachable (reach, exps) sigma in
    if (Sil.HpredSet.cardinal reach) = (Sil.HpredSet.cardinal reach') then (reach, exps)
    else compute_reachable_hpreds_rec sigma (reach', exps') in
  compute_reachable_hpreds_rec sigma (Sil.HpredSet.empty, exps)

(** if possible, produce a (fieldname, typ) path from one of the [src_exps] to [snk_exp] using
    [reachable_hpreds]. *)
let get_fld_typ_path_opt src_exps snk_exp_ reachable_hpreds_ =
  let strexp_matches target_exp = function
    | (_, Sil.Eexp (e, _)) -> Exp.equal target_exp e
    | _ -> false in
  let extend_path hpred (snk_exp, path, reachable_hpreds) = match hpred with
    | Sil.Hpointsto (lhs, Sil.Estruct (flds, _), Exp.Sizeof (typ, _, _)) ->
        (try
           let fld, _ = IList.find (fun fld -> strexp_matches snk_exp fld) flds in
           let reachable_hpreds' = Sil.HpredSet.remove hpred reachable_hpreds in
           (lhs, (Some fld, typ) :: path, reachable_hpreds')
         with Not_found -> (snk_exp, path, reachable_hpreds))
    | Sil.Hpointsto (lhs, Sil.Earray (_, elems, _), Exp.Sizeof (typ, _, _)) ->
        if IList.exists (fun pair -> strexp_matches snk_exp pair) elems
        then
          let reachable_hpreds' = Sil.HpredSet.remove hpred reachable_hpreds in
          (* None means "no field name" ~=~ nameless array index *)
          (lhs, (None, typ) :: path, reachable_hpreds')
        else (snk_exp, path, reachable_hpreds)
    | _ -> (snk_exp, path, reachable_hpreds) in
  (* terminates because [reachable_hpreds] is shrinking on each recursive call *)
  let rec get_fld_typ_path snk_exp path reachable_hpreds =
    let (snk_exp', path', reachable_hpreds') =
      Sil.HpredSet.fold extend_path reachable_hpreds (snk_exp, path, reachable_hpreds) in
    if Exp.Set.mem snk_exp' src_exps
    then Some path'
    else
    if Sil.HpredSet.cardinal reachable_hpreds' >= Sil.HpredSet.cardinal reachable_hpreds
    then None (* can't find a path from [src_exps] to [snk_exp] *)
    else get_fld_typ_path snk_exp' path' reachable_hpreds' in
  get_fld_typ_path snk_exp_ [] reachable_hpreds_

(** filter [pi] by removing the pure atoms that do not contain an expression in [exps] *)
let compute_reachable_atoms pi exps =
  let rec exp_contains = function
    | exp when Exp.Set.mem exp exps -> true
    | Exp.UnOp (_, e, _) | Exp.Cast (_, e) | Exp.Lfield (e, _, _) -> exp_contains e
    | Exp.BinOp (_, e0, e1) | Exp.Lindex (e0, e1) -> exp_contains e0 || exp_contains e1
    | _ -> false in
  IList.filter
    (function
      | Sil.Aeq (lhs, rhs) | Aneq (lhs, rhs) -> exp_contains lhs || exp_contains rhs
      | Sil.Apred (_, es) | Anpred (_, es) -> IList.exists exp_contains es)
    pi

(** Eliminates all empty lsegs from sigma, and collect equalities
    The empty lsegs include
    (a) "lseg_pe para 0 e elist",
    (b) "dllseg_pe para iF oB oF iB elist" with iF = 0 or iB = 0,
    (c) "lseg_pe para e1 e2 elist" and the rest of sigma contains the "cell" e1,
    (d) "dllseg_pe para iF oB oF iB elist" and the rest of sigma contains
    cell iF or iB. *)
let sigma_remove_emptylseg sigma =
  let alloc_set =
    let rec f_alloc set = function
      | [] ->
          set
      | Sil.Hpointsto (e, _, _) :: sigma' | Sil.Hlseg (Sil.Lseg_NE, _, e, _, _) :: sigma' ->
          f_alloc (Exp.Set.add e set) sigma'
      | Sil.Hdllseg (Sil.Lseg_NE, _, iF, _, _, iB, _) :: sigma' ->
          f_alloc (Exp.Set.add iF (Exp.Set.add iB set)) sigma'
      | _ :: sigma' ->
          f_alloc set sigma' in
    f_alloc Exp.Set.empty sigma
  in
  let rec f eqs_zero sigma_passed = function
    | [] ->
        (IList.rev eqs_zero, IList.rev sigma_passed)
    | Sil.Hpointsto _ as hpred :: sigma' ->
        f eqs_zero (hpred :: sigma_passed) sigma'
    | Sil.Hlseg (Sil.Lseg_PE, _, e1, e2, _) :: sigma'
      when (Exp.equal e1 Exp.zero) || (Exp.Set.mem e1 alloc_set) ->
        f (Sil.Aeq(e1, e2) :: eqs_zero) sigma_passed sigma'
    | Sil.Hlseg _ as hpred :: sigma' ->
        f eqs_zero (hpred :: sigma_passed) sigma'
    | Sil.Hdllseg (Sil.Lseg_PE, _, iF, oB, oF, iB, _) :: sigma'
      when (Exp.equal iF Exp.zero) || (Exp.Set.mem iF alloc_set)
           || (Exp.equal iB Exp.zero) || (Exp.Set.mem iB alloc_set) ->
        f (Sil.Aeq(iF, oF):: Sil.Aeq(iB, oB):: eqs_zero) sigma_passed sigma'
    | Sil.Hdllseg _ as hpred :: sigma' ->
        f eqs_zero (hpred :: sigma_passed) sigma'
  in
  f [] [] sigma

let sigma_intro_nonemptylseg e1 e2 sigma =
  let rec f sigma_passed = function
    | [] ->
        IList.rev sigma_passed
    | Sil.Hpointsto _ as hpred :: sigma' ->
        f (hpred :: sigma_passed) sigma'
    | Sil.Hlseg (Sil.Lseg_PE, para, f1, f2, shared) :: sigma'
      when (Exp.equal e1 f1 && Exp.equal e2 f2)
        || (Exp.equal e2 f1 && Exp.equal e1 f2) ->
        f (Sil.Hlseg (Sil.Lseg_NE, para, f1, f2, shared) :: sigma_passed) sigma'
    | Sil.Hlseg _ as hpred :: sigma' ->
        f (hpred :: sigma_passed) sigma'
    | Sil.Hdllseg (Sil.Lseg_PE, para, iF, oB, oF, iB, shared) :: sigma'
      when (Exp.equal e1 iF && Exp.equal e2 oF)
        || (Exp.equal e2 iF && Exp.equal e1 oF)
        || (Exp.equal e1 iB && Exp.equal e2 oB)
        || (Exp.equal e2 iB && Exp.equal e1 oB) ->
        f (Sil.Hdllseg (Sil.Lseg_NE, para, iF, oB, oF, iB, shared) :: sigma_passed) sigma'
    | Sil.Hdllseg _ as hpred :: sigma' ->
        f (hpred :: sigma_passed) sigma'
  in
  f [] sigma

let normalize_and_strengthen_atom (p : normal t) (a : Sil.atom) : Sil.atom =
  let a' = atom_normalize p.sub a in
  match a' with
  | Sil.Aeq (Exp.BinOp (Binop.Le, Exp.Var id, Exp.Const (Const.Cint n)), Exp.Const (Const.Cint i))
    when IntLit.isone i ->
      let lower = Exp.int (n -- IntLit.one) in
      let a_lower = Sil.Aeq (Exp.BinOp (Binop.Lt, lower, Exp.Var id), Exp.one) in
      if not (IList.mem Sil.atom_equal a_lower p.pi) then a'
      else Sil.Aeq (Exp.Var id, Exp.int n)
  | Sil.Aeq (Exp.BinOp (Binop.Lt, Exp.Const (Const.Cint n), Exp.Var id), Exp.Const (Const.Cint i))
    when IntLit.isone i ->
      let upper = Exp.int (n ++ IntLit.one) in
      let a_upper = Sil.Aeq (Exp.BinOp (Binop.Le, Exp.Var id, upper), Exp.one) in
      if not (IList.mem Sil.atom_equal a_upper p.pi) then a'
      else Sil.Aeq (Exp.Var id, upper)
  | Sil.Aeq (Exp.BinOp (Binop.Ne, e1, e2), Exp.Const (Const.Cint i)) when IntLit.isone i ->
      Sil.Aneq (e1, e2)
  | _ -> a'

(** Conjoin a pure atomic predicate by normal conjunction. *)
let rec prop_atom_and ?(footprint=false) (p : normal t) a : normal t =
  let a' = normalize_and_strengthen_atom p a in
  if IList.mem Sil.atom_equal a' p.pi then p
  else begin
    let p' =
      match a' with
      | Sil.Aeq (Exp.Var i, e) when Sil.ident_in_exp i e -> p
      | Sil.Aeq (Exp.Var i, e) ->
          let sub_list = [(i, e)] in
          let mysub = Sil.sub_of_list sub_list in
          let p_sub = Sil.sub_filter (fun i' -> not (Ident.equal i i')) p.sub in
          let sub' = Sil.sub_join mysub (Sil.sub_range_map (Sil.exp_sub mysub) p_sub) in
          let (nsub', npi', nsigma') =
            let nsigma' = sigma_normalize sub' p.sigma in
            (sub_normalize sub', pi_normalize sub' nsigma' p.pi, nsigma') in
          let (eqs_zero, nsigma'') = sigma_remove_emptylseg nsigma' in
          let p' =
            unsafe_cast_to_normal
              (set p ~sub:nsub' ~pi:npi' ~sigma:nsigma'') in
          IList.fold_left (prop_atom_and ~footprint) p' eqs_zero
      | Sil.Aeq (e1, e2) when (Exp.compare e1 e2 = 0) ->
          p
      | Sil.Aneq (e1, e2) ->
          let sigma' = sigma_intro_nonemptylseg e1 e2 p.sigma in
          let pi' = pi_normalize p.sub sigma' (a':: p.pi) in
          unsafe_cast_to_normal
            (set p ~pi:pi' ~sigma:sigma')
      | _ ->
          let pi' = pi_normalize p.sub p.sigma (a':: p.pi) in
          unsafe_cast_to_normal
            (set p ~pi:pi') in
    if not footprint then p'
    else begin
      let fav_a' = Sil.atom_fav a' in
      let fav_nofootprint_a' =
        Sil.fav_copy_filter_ident fav_a' (fun id -> not (Ident.is_footprint id)) in
      let predicate_warning =
        not (Sil.fav_is_empty fav_nofootprint_a') in
      let p'' =
        if predicate_warning then footprint_normalize p'
        else
          match a' with
          | Sil.Aeq (Exp.Var i, e) when not (Sil.ident_in_exp i e) ->
              let mysub = Sil.sub_of_list [(i, e)] in
              let sigma_fp' = sigma_normalize mysub p'.sigma_fp in
              let pi_fp' = a' :: pi_normalize mysub sigma_fp' p'.pi_fp in
              footprint_normalize
                (set p' ~pi_fp:pi_fp' ~sigma_fp:sigma_fp')
          | _ ->
              footprint_normalize
                (set p' ~pi_fp:(a' :: p'.pi_fp)) in
      if predicate_warning then (L.d_warning "dropping non-footprint "; Sil.d_atom a'; L.d_ln ());
      unsafe_cast_to_normal p''
    end
  end

(** Conjoin [exp1]=[exp2] with a symbolic heap [prop]. *)
let conjoin_eq ?(footprint = false) exp1 exp2 prop =
  prop_atom_and ~footprint prop (Sil.Aeq(exp1, exp2))

(** Conjoin [exp1!=exp2] with a symbolic heap [prop]. *)
let conjoin_neq ?(footprint = false) exp1 exp2 prop =
  prop_atom_and ~footprint prop (Sil.Aneq (exp1, exp2))

(** Reset every inst in the prop using the given map *)
let prop_reset_inst inst_map prop =
  let sigma' = IList.map (Sil.hpred_instmap inst_map) prop.sigma in
  let sigma_fp' = IList.map (Sil.hpred_instmap inst_map) prop.sigma_fp in
  set prop ~sigma:sigma' ~sigma_fp:sigma_fp'


(** {1 Functions for transforming footprints into propositions.} *)

(** The ones used for abstraction add/remove local stacks in order to
    stop the firing of some abstraction rules. The other usual
    transforation functions do not use this hack. *)

(** Extract the footprint and return it as a prop *)
let extract_footprint p =
  set prop_emp ~pi:p.pi_fp ~sigma:p.sigma_fp

(** Extract the (footprint,current) pair *)
let extract_spec (p : normal t) : normal t * normal t =
  let pre = extract_footprint p in
  let post = set p ~pi_fp:[] ~sigma_fp:[] in
  (unsafe_cast_to_normal pre, unsafe_cast_to_normal post)

(** [prop_set_fooprint p p_foot] sets proposition [p_foot] as footprint of [p]. *)
let prop_set_footprint p p_foot =
  let pi =
    (IList.map
       (fun (i, e) -> Sil.Aeq(Exp.Var i, e))
       (Sil.sub_to_list p_foot.sub)) @ p_foot.pi in
  set p ~pi_fp:pi ~sigma_fp:p_foot.sigma

(** {2 Functions for renaming primed variables by "canonical names"} *)

module ExpStack : sig
  val init : Exp.t list -> unit
  val final : unit -> unit
  val is_empty : unit -> bool
  val push : Exp.t -> unit
  val pop : unit -> Exp.t
end = struct
  let stack = Stack.create ()
  let init es =
    Stack.clear stack;
    IList.iter (fun e -> Stack.push e stack) (IList.rev es)
  let final () = Stack.clear stack
  let is_empty () = Stack.is_empty stack
  let push e = Stack.push e stack
  let pop () = Stack.pop stack
end

let sigma_get_start_lexps_sort sigma =
  let exp_compare_neg e1 e2 = - (Exp.compare e1 e2) in
  let filter e = Sil.fav_for_all (Sil.exp_fav e) Ident.is_normal in
  let lexps = Sil.hpred_list_get_lexps filter sigma in
  IList.sort exp_compare_neg lexps

let sigma_dfs_sort sigma =

  let init () =
    let start_lexps = sigma_get_start_lexps_sort sigma in
    ExpStack.init start_lexps in

  let final () = ExpStack.final () in

  let rec handle_strexp = function
    | Sil.Eexp (e, _) -> ExpStack.push e
    | Sil.Estruct (fld_se_list, _) ->
        IList.iter (fun (_, se) -> handle_strexp se) fld_se_list
    | Sil.Earray (_, idx_se_list, _) ->
        IList.iter (fun (_, se) -> handle_strexp se) idx_se_list in

  let rec handle_e visited seen e = function
    | [] -> (visited, IList.rev seen)
    | hpred :: cur ->
        begin
          match hpred with
          | Sil.Hpointsto (e', se, _) when Exp.equal e e' ->
              handle_strexp se;
              (hpred:: visited, IList.rev_append cur seen)
          | Sil.Hlseg (_, _, root, next, shared) when Exp.equal e root ->
              IList.iter ExpStack.push (next:: shared);
              (hpred:: visited, IList.rev_append cur seen)
          | Sil.Hdllseg (_, _, iF, oB, oF, iB, shared)
            when Exp.equal e iF || Exp.equal e iB ->
              IList.iter ExpStack.push (oB:: oF:: shared);
              (hpred:: visited, IList.rev_append cur seen)
          | _ ->
              handle_e visited (hpred:: seen) e cur
        end in

  let rec handle_sigma visited = function
    | [] -> IList.rev visited
    | cur ->
        if ExpStack.is_empty () then
          let cur' = sigma_normalize Sil.sub_empty cur in
          IList.rev_append cur' visited
        else
          let e = ExpStack.pop () in
          let (visited', cur') = handle_e visited [] e cur in
          handle_sigma visited' cur' in

  init ();
  let sigma' = handle_sigma [] sigma in
  final ();
  sigma'

let prop_dfs_sort p =
  let sigma = p.sigma in
  let sigma' = sigma_dfs_sort sigma in
  let sigma_fp = p.sigma_fp in
  let sigma_fp' = sigma_dfs_sort sigma_fp in
  let p' = set p ~sigma:sigma' ~sigma_fp:sigma_fp' in
  (* L.err "@[<2>P SORTED:@\n%a@\n@." pp_prop p'; *)
  p'

let prop_fav_add_dfs fav prop =
  prop_fav_add fav (prop_dfs_sort prop)

let rec strexp_get_array_indices acc = function
  | Sil.Eexp _ -> acc
  | Sil.Estruct (fsel, _) ->
      let se_list = IList.map snd fsel in
      IList.fold_left strexp_get_array_indices acc se_list
  | Sil.Earray (_, isel, _) ->
      let acc_new = IList.fold_left (fun acc' (idx, _) -> idx:: acc') acc isel in
      let se_list = IList.map snd isel in
      IList.fold_left strexp_get_array_indices acc_new se_list

let hpred_get_array_indices acc = function
  | Sil.Hpointsto (_, se, _) -> strexp_get_array_indices acc se
  | Sil.Hlseg _ | Sil.Hdllseg _ -> acc

let sigma_get_array_indices sigma =
  let indices = IList.fold_left hpred_get_array_indices [] sigma in
  IList.rev indices

let compute_reindexing fav_add get_id_offset list =
  let rec select list_passed list_seen = function
    | [] -> list_passed
    | x :: list_rest ->
        let id_offset_opt = get_id_offset x in
        let list_passed_new = match id_offset_opt with
          | None -> list_passed
          | Some (id, _) ->
              let fav = Sil.fav_new () in
              IList.iter (fav_add fav) list_seen;
              IList.iter (fav_add fav) list_passed;
              if (Sil.fav_exists fav (Ident.equal id))
              then list_passed
              else (x:: list_passed) in
        let list_seen_new = x:: list_seen in
        select list_passed_new list_seen_new list_rest in
  let list_passed = select [] [] list in
  let transform x =
    let id, offset = match get_id_offset x with None -> assert false | Some io -> io in
    let base_new = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let offset_new = Exp.int (IntLit.neg offset) in
    let exp_new = Exp.BinOp(Binop.PlusA, base_new, offset_new) in
    (id, exp_new) in
  let reindexing = IList.map transform list_passed in
  Sil.sub_of_list reindexing

let compute_reindexing_from_indices indices =
  let get_id_offset = function
    | Exp.BinOp (Binop.PlusA, Exp.Var id, Exp.Const(Const.Cint offset)) ->
        if Ident.is_primed id then Some (id, offset) else None
    | _ -> None in
  let fav_add = Sil.exp_fav_add in
  compute_reindexing fav_add get_id_offset indices

let apply_reindexing subst prop =
  let nsigma = sigma_normalize subst prop.sigma in
  let npi = pi_normalize subst nsigma prop.pi in
  let nsub, atoms =
    let dom_subst = IList.map fst (Sil.sub_to_list subst) in
    let in_dom_subst id = IList.exists (Ident.equal id) dom_subst in
    let sub' = Sil.sub_filter (fun id -> not (in_dom_subst id)) prop.sub in
    let contains_substituted_id e = Sil.fav_exists (Sil.exp_fav e) in_dom_subst in
    let sub_eqs, sub_keep = Sil.sub_range_partition contains_substituted_id sub' in
    let eqs = Sil.sub_to_list sub_eqs in
    let atoms = IList.map (fun (id, e) -> Sil.Aeq (Exp.Var id, exp_normalize subst e)) eqs in
    (sub_keep, atoms) in
  let p' =
    unsafe_cast_to_normal
      (set prop ~sub:nsub ~pi:npi ~sigma:nsigma) in
  IList.fold_left prop_atom_and p' atoms

let prop_rename_array_indices prop =
  if !Config.footprint then prop
  else begin
    let indices = sigma_get_array_indices prop.sigma in
    let not_same_base_lt_offsets e1 e2 =
      match e1, e2 with
      | Exp.BinOp(Binop.PlusA, e1', Exp.Const (Const.Cint n1')),
        Exp.BinOp(Binop.PlusA, e2', Exp.Const (Const.Cint n2')) ->
          not (Exp.equal e1' e2' && IntLit.lt n1' n2')
      | _ -> true in
    let rec select_minimal_indices indices_seen = function
      | [] -> IList.rev indices_seen
      | index:: indices_rest ->
          let indices_seen' = IList.filter (not_same_base_lt_offsets index) indices_seen in
          let indices_seen_new = index:: indices_seen' in
          let indices_rest_new = IList.filter (not_same_base_lt_offsets index) indices_rest in
          select_minimal_indices indices_seen_new indices_rest_new in
    let minimal_indices = select_minimal_indices [] indices in
    let subst = compute_reindexing_from_indices minimal_indices in
    apply_reindexing subst prop
  end

let compute_renaming fav =
  let ids = Sil.fav_to_list fav in
  let ids_primed, ids_nonprimed = IList.partition Ident.is_primed ids in
  let ids_footprint = IList.filter Ident.is_footprint ids_nonprimed in

  let id_base_primed = Ident.create Ident.kprimed 0 in
  let id_base_footprint = Ident.create Ident.kfootprint 0 in

  let rec f id_base index ren_subst = function
    | [] -> ren_subst
    | id:: ids ->
        let new_id = Ident.set_stamp id_base index in
        if Ident.equal id new_id then
          f id_base (index + 1) ren_subst ids
        else
          f id_base (index + 1) ((id, new_id):: ren_subst) ids in

  let ren_primed = f id_base_primed 0 [] ids_primed in
  let ren_footprint = f id_base_footprint 0 [] ids_footprint in

  ren_primed @ ren_footprint

let rec idlist_assoc id = function
  | [] -> raise Not_found
  | (i, x):: l -> if Ident.equal i id then x else idlist_assoc id l

let ident_captured_ren ren id =
  try (idlist_assoc id ren)
  with Not_found -> id
(* If not defined in ren, id should be mapped to itself *)

let rec exp_captured_ren ren = function
  | Exp.Var id -> Exp.Var (ident_captured_ren ren id)
  | Exp.Exn e -> Exp.Exn (exp_captured_ren ren e)
  | Exp.Closure _ as e -> e     (* TODO: why captured vars not renamed? *)
  | Exp.Const _ as e -> e
  | Exp.Sizeof (t, len, st) -> Exp.Sizeof (t, Option.map (exp_captured_ren ren) len, st)
  | Exp.Cast (t, e) -> Exp.Cast (t, exp_captured_ren ren e)
  | Exp.UnOp (op, e, topt) -> Exp.UnOp (op, exp_captured_ren ren e, topt)
  | Exp.BinOp (op, e1, e2) ->
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      Exp.BinOp (op, e1', e2')
  | Exp.Lvar id -> Exp.Lvar id
  | Exp.Lfield (e, fld, typ) -> Exp.Lfield (exp_captured_ren ren e, fld, typ)
  | Exp.Lindex (e1, e2) ->
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      Exp.Lindex(e1', e2')

let atom_captured_ren ren = function
  | Sil.Aeq (e1, e2) ->
      Sil.Aeq (exp_captured_ren ren e1, exp_captured_ren ren e2)
  | Sil.Aneq (e1, e2) ->
      Sil.Aneq (exp_captured_ren ren e1, exp_captured_ren ren e2)
  | Sil.Apred (a, es) ->
      Sil.Apred (a, IList.map (fun e -> exp_captured_ren ren e) es)
  | Sil.Anpred (a, es) ->
      Sil.Anpred (a, IList.map (fun e -> exp_captured_ren ren e) es)

let rec strexp_captured_ren ren = function
  | Sil.Eexp (e, inst) ->
      Sil.Eexp (exp_captured_ren ren e, inst)
  | Sil.Estruct (fld_se_list, inst) ->
      let f (fld, se) = (fld, strexp_captured_ren ren se) in
      Sil.Estruct (IList.map f fld_se_list, inst)
  | Sil.Earray (len, idx_se_list, inst) ->
      let f (idx, se) =
        let idx' = exp_captured_ren ren idx in
        (idx', strexp_captured_ren ren se) in
      let len' = exp_captured_ren ren len in
      Sil.Earray (len', IList.map f idx_se_list, inst)

and hpred_captured_ren ren = function
  | Sil.Hpointsto (base, se, te) ->
      let base' = exp_captured_ren ren base in
      let se' = strexp_captured_ren ren se in
      let te' = exp_captured_ren ren te in
      Sil.Hpointsto (base', se', te')
  | Sil.Hlseg (k, para, e1, e2, elist) ->
      let para' = hpara_ren para in
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      let elist' = IList.map (exp_captured_ren ren) elist in
      Sil.Hlseg (k, para', e1', e2', elist')
  | Sil.Hdllseg (k, para, e1, e2, e3, e4, elist) ->
      let para' = hpara_dll_ren para in
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      let e3' = exp_captured_ren ren e3 in
      let e4' = exp_captured_ren ren e4 in
      let elist' = IList.map (exp_captured_ren ren) elist in
      Sil.Hdllseg (k, para', e1', e2', e3', e4', elist')

and hpara_ren para =
  let av = Sil.hpara_shallow_av para in
  let ren = compute_renaming av in
  let root' = ident_captured_ren ren para.Sil.root in
  let next' = ident_captured_ren ren para.Sil.next in
  let svars' = IList.map (ident_captured_ren ren) para.Sil.svars in
  let evars' = IList.map (ident_captured_ren ren) para.Sil.evars in
  let body' = IList.map (hpred_captured_ren ren) para.Sil.body in
  { Sil.root = root'; Sil.next = next'; Sil.svars = svars'; Sil.evars = evars'; Sil.body = body'}

and hpara_dll_ren para =
  let av = Sil.hpara_dll_shallow_av para in
  let ren = compute_renaming av in
  let iF = ident_captured_ren ren para.Sil.cell in
  let oF = ident_captured_ren ren para.Sil.flink in
  let oB = ident_captured_ren ren para.Sil.blink in
  let svars' = IList.map (ident_captured_ren ren) para.Sil.svars_dll in
  let evars' = IList.map (ident_captured_ren ren) para.Sil.evars_dll in
  let body' = IList.map (hpred_captured_ren ren) para.Sil.body_dll in
  { Sil.cell = iF;
    flink = oF;
    blink = oB;
    svars_dll = svars';
    evars_dll = evars';
    body_dll = body'}

let pi_captured_ren ren pi =
  IList.map (atom_captured_ren ren) pi

let sigma_captured_ren ren sigma =
  IList.map (hpred_captured_ren ren) sigma

let sub_captured_ren ren sub =
  Sil.sub_map (ident_captured_ren ren) (exp_captured_ren ren) sub

(** Canonicalize the names of primed variables and footprint vars. *)
let prop_rename_primed_footprint_vars (p : normal t) : normal t =
  let p = prop_rename_array_indices p in
  let bound_vars =
    let filter id = Ident.is_footprint id || Ident.is_primed id in
    let p_dfs = prop_dfs_sort p in
    let fvars_in_p = prop_fav p_dfs in
    Sil.fav_filter_ident fvars_in_p filter;
    fvars_in_p in
  let ren = compute_renaming bound_vars in
  let sub' = sub_captured_ren ren p.sub in
  let pi' = pi_captured_ren ren p.pi in
  let sigma' = sigma_captured_ren ren p.sigma in
  let pi_fp' = pi_captured_ren ren p.pi_fp in
  let sigma_fp' = sigma_captured_ren ren p.sigma_fp in

  let sub_for_normalize = Sil.sub_empty in
  (* It is fine to use the empty substituion during normalization
     because the renaming maintains that a substitution is normalized *)
  let nsub' = sub_normalize sub' in
  let nsigma' = sigma_normalize sub_for_normalize sigma' in
  let npi' = pi_normalize sub_for_normalize nsigma' pi' in
  let p' = footprint_normalize
      (set prop_emp ~sub:nsub' ~pi:npi' ~sigma:nsigma' ~pi_fp:pi_fp' ~sigma_fp:sigma_fp') in
  unsafe_cast_to_normal p'

(** {2 Functions for changing and generating propositions} *)

let mem_idlist i l =
  IList.exists (fun id -> Ident.equal i id) l

let expose (p : normal t) : exposed t = Obj.magic p

(** normalize a prop *)
let normalize (eprop : 'a t) : normal t =
  let p0 =
    unsafe_cast_to_normal
      (set prop_emp ~sigma: (sigma_normalize Sil.sub_empty eprop.sigma)) in
  let nprop = IList.fold_left prop_atom_and p0 (get_pure eprop) in
  unsafe_cast_to_normal
    (footprint_normalize (set nprop ~pi_fp:eprop.pi_fp ~sigma_fp:eprop.sigma_fp))

(** Apply subsitution to prop. *)
let prop_sub subst (prop: 'a t) : exposed t =
  let pi = pi_sub subst (prop.pi @ pi_of_subst prop.sub) in
  let sigma = sigma_sub subst prop.sigma in
  let pi_fp = pi_sub subst prop.pi_fp in
  let sigma_fp = sigma_sub subst prop.sigma_fp in
  set prop_emp ~pi ~sigma ~pi_fp ~sigma_fp

(** Apply renaming substitution to a proposition. *)
let prop_ren_sub (ren_sub: Sil.subst) (prop: normal t) : normal t =
  normalize (prop_sub ren_sub prop)

(** Existentially quantify the [fav] in [prop].
    [fav] should not contain any primed variables. *)
let exist_quantify fav (prop : normal t) : normal t =
  let ids = Sil.fav_to_list fav in
  if IList.exists Ident.is_primed ids then assert false; (* sanity check *)
  if ids == [] then prop else
    let gen_fresh_id_sub id = (id, Exp.Var (Ident.create_fresh Ident.kprimed)) in
    let ren_sub = Sil.sub_of_list (IList.map gen_fresh_id_sub ids) in
    let prop' =
      (* throw away x=E if x becomes _x *)
      let sub = Sil.sub_filter (fun i -> not (mem_idlist i ids)) prop.sub in
      if Sil.sub_equal sub prop.sub then prop
      else unsafe_cast_to_normal (set prop ~sub) in
    (*
    L.out "@[<2>.... Existential Quantification ....\n";
    L.out "SUB:%a\n" pp_sub prop'.sub;
    L.out "PI:%a\n" pp_pi prop'.pi;
    L.out "PROP:%a\n@." pp_prop prop';
    *)
    prop_ren_sub ren_sub prop'

(** Apply the substitution [fe] to all the expressions in the prop. *)
let prop_expmap (fe: Exp.t -> Exp.t) prop =
  let f (e, sil_opt) = (fe e, sil_opt) in
  let pi = IList.map (Sil.atom_expmap fe) prop.pi in
  let sigma = IList.map (Sil.hpred_expmap f) prop.sigma in
  let pi_fp = IList.map (Sil.atom_expmap fe) prop.pi_fp in
  let sigma_fp = IList.map (Sil.hpred_expmap f) prop.sigma_fp in
  set prop ~pi ~sigma ~pi_fp ~sigma_fp

(** convert identifiers in fav to kind [k] *)
let vars_make_unprimed fav prop =
  let ids = Sil.fav_to_list fav in
  let ren_sub =
    Sil.sub_of_list (IList.map
                       (fun i -> (i, Exp.Var (Ident.create_fresh Ident.knormal)))
                       ids) in
  prop_ren_sub ren_sub prop

(** convert the normal vars to primed vars. *)
let prop_normal_vars_to_primed_vars p =
  let fav = prop_fav p in
  Sil.fav_filter_ident fav Ident.is_normal;
  exist_quantify fav p

(** convert the primed vars to normal vars. *)
let prop_primed_vars_to_normal_vars (p : normal t) : normal t =
  let fav = prop_fav p in
  Sil.fav_filter_ident fav Ident.is_primed;
  vars_make_unprimed fav p

let from_pi pi =
  set prop_emp ~pi

let from_sigma sigma =
  set prop_emp ~sigma

(** Rename free variables in a prop replacing them with existentially quantified vars *)
let prop_rename_fav_with_existentials (p : normal t) : normal t =
  let fav = Sil.fav_new () in
  prop_fav_add fav p;
  let ids = Sil.fav_to_list fav in
  let ids' = IList.map (fun i -> (i, Ident.create_fresh Ident.kprimed)) ids in
  let ren_sub = Sil.sub_of_list (IList.map (fun (i, i') -> (i, Exp.Var i')) ids') in
  let p' = prop_sub ren_sub p in
  (*L.d_strln "Prop after renaming:"; d_prop p'; L.d_strln "";*)
  normalize p'

(** {2 Prop iterators} *)

(** Iterator state over sigma. *)
type 'a prop_iter =
  { pit_sub : Sil.subst; (** substitution for equalities *)
    pit_pi : pi;    (** pure part *)
    pit_newpi : (bool * Sil.atom) list;   (** newly added atoms. *)
    (* The first records !Config.footprint. *)
    pit_old : sigma; (** sigma already visited *)
    pit_curr : Sil.hpred;      (** current element *)
    pit_state : 'a; (** state of current element *)
    pit_new : sigma; (** sigma not yet visited *)
    pit_pi_fp : pi; (** pure part of the footprint *)
    pit_sigma_fp : sigma; (** sigma part of the footprint *)
  }

let prop_iter_create prop =
  match prop.sigma with
  | hpred:: sigma' -> Some
                        { pit_sub = prop.sub;
                          pit_pi = prop.pi;
                          pit_newpi = [];
                          pit_old = [];
                          pit_curr = hpred;
                          pit_state = ();
                          pit_new = sigma';
                          pit_pi_fp = prop.pi_fp;
                          pit_sigma_fp = prop.sigma_fp }
  | _ -> None

(** Return the prop associated to the iterator. *)
let prop_iter_to_prop iter =
  let sigma = IList.rev_append iter.pit_old (iter.pit_curr:: iter.pit_new) in
  let prop =
    normalize
      (set prop_emp
         ~sub:iter.pit_sub
         ~pi:iter.pit_pi
         ~sigma:sigma
         ~pi_fp:iter.pit_pi_fp
         ~sigma_fp:iter.pit_sigma_fp) in
  IList.fold_left
    (fun p (footprint, atom) -> prop_atom_and ~footprint: footprint p atom)
    prop iter.pit_newpi

(** Add an atom to the pi part of prop iter. The
    first parameter records whether it is done
    during footprint or during re - execution. *)
let prop_iter_add_atom footprint iter atom =
  { iter with pit_newpi = (footprint, atom):: iter.pit_newpi }

(** Remove the current element of the iterator, and return the prop
    associated to the resulting iterator *)
let prop_iter_remove_curr_then_to_prop iter : normal t =
  let sigma = IList.rev_append iter.pit_old iter.pit_new in
  let normalized_sigma = sigma_normalize iter.pit_sub sigma in
  let prop =
    set prop_emp
      ~sub:iter.pit_sub
      ~pi:iter.pit_pi
      ~sigma:normalized_sigma
      ~pi_fp:iter.pit_pi_fp
      ~sigma_fp:iter.pit_sigma_fp in
  unsafe_cast_to_normal prop

(** Return the current hpred and state. *)
let prop_iter_current iter =
  let curr = hpred_normalize iter.pit_sub iter.pit_curr in
  let prop =
    unsafe_cast_to_normal
      (set prop_emp ~sigma:[curr]) in
  let prop' =
    IList.fold_left
      (fun p (footprint, atom) -> prop_atom_and ~footprint: footprint p atom)
      prop iter.pit_newpi in
  match prop'.sigma with
  | [curr'] -> (curr', iter.pit_state)
  | _ -> assert false

(** Update the current element of the iterator. *)
let prop_iter_update_current iter hpred =
  { iter with pit_curr = hpred }

(** Update the current element of the iterator by a nonempty list of elements. *)
let prop_iter_update_current_by_list iter = function
  | [] -> assert false (* the list should be nonempty *)
  | hpred:: hpred_list ->
      let pit_new' = hpred_list@iter.pit_new in
      { iter with pit_curr = hpred; pit_state = (); pit_new = pit_new'}

let prop_iter_next iter =
  match iter.pit_new with
  | [] -> None
  | hpred':: new' -> Some
                       { iter with
                         pit_old = iter.pit_curr:: iter.pit_old;
                         pit_curr = hpred';
                         pit_state = ();
                         pit_new = new'}

let prop_iter_remove_curr_then_next iter =
  match iter.pit_new with
  | [] -> None
  | hpred':: new' -> Some
                       { iter with
                         pit_old = iter.pit_old;
                         pit_curr = hpred';
                         pit_state = ();
                         pit_new = new'}

(** Insert before the current element of the iterator. *)
let prop_iter_prev_then_insert iter hpred =
  { iter with
    pit_new = iter.pit_curr:: iter.pit_new;
    pit_curr = hpred }

(** Scan sigma to find an [hpred] satisfying the filter function. *)
let rec prop_iter_find iter filter =
  match filter iter.pit_curr with
  | Some st -> Some { iter with pit_state = st }
  | None ->
      (match prop_iter_next iter with
       | None -> None
       | Some iter' -> prop_iter_find iter' filter)

(** Set the state of the iterator *)
let prop_iter_set_state iter state =
  { iter with pit_state = state }

let prop_iter_make_id_primed id iter =
  let pid = Ident.create_fresh Ident.kprimed in
  let sub_id = Sil.sub_of_list [(id, Exp.Var pid)] in

  let normalize (id, e) =
    let eq' = Sil.Aeq(Sil.exp_sub sub_id (Exp.Var id), Sil.exp_sub sub_id e) in
    atom_normalize Sil.sub_empty eq' in

  let rec split pairs_unpid pairs_pid = function
    | [] -> (IList.rev pairs_unpid, IList.rev pairs_pid)
    | eq:: eqs_cur ->
        begin
          match eq with
          | Sil.Aeq (Exp.Var id1, e1) when Sil.ident_in_exp id1 e1 ->
              L.out "@[<2>#### ERROR: an assumption of the analyzer broken ####@\n";
              L.out "Broken Assumption: id notin e for all (id,e) in sub@\n";
              L.out "(id,e) : (%a,%a)@\n" (Ident.pp pe_text) id1 (Sil.pp_exp pe_text) e1;
              L.out "PROP : %a@\n@." (pp_prop pe_text) (prop_iter_to_prop iter);
              assert false
          | Sil.Aeq (Exp.Var id1, e1) when Ident.equal pid id1 ->
              split pairs_unpid ((id1, e1):: pairs_pid) eqs_cur
          | Sil.Aeq (Exp.Var id1, e1) ->
              split ((id1, e1):: pairs_unpid) pairs_pid eqs_cur
          | _ ->
              assert false
        end in

  let rec get_eqs acc = function
    | [] | [_] ->
        IList.rev acc
    | (_, e1) :: (((_, e2) :: _) as pairs) ->
        get_eqs (Sil.Aeq(e1, e2):: acc) pairs in

  let sub_new, sub_use, eqs_add =
    let eqs = IList.map normalize (Sil.sub_to_list iter.pit_sub) in
    let pairs_unpid, pairs_pid = split [] [] eqs in
    match pairs_pid with
    | [] ->
        let sub_unpid = Sil.sub_of_list pairs_unpid in
        let pairs = (id, Exp.Var pid) :: pairs_unpid in
        sub_unpid, Sil.sub_of_list pairs, []
    | (id1, e1):: _ ->
        let sub_id1 = Sil.sub_of_list [(id1, e1)] in
        let pairs_unpid' =
          IList.map (fun (id', e') -> (id', Sil.exp_sub sub_id1 e')) pairs_unpid in
        let sub_unpid = Sil.sub_of_list pairs_unpid' in
        let pairs = (id, e1) :: pairs_unpid' in
        sub_unpid, Sil.sub_of_list pairs, get_eqs [] pairs_pid in
  let nsub_new = sub_normalize sub_new in

  { iter with
    pit_sub = nsub_new;
    pit_pi = pi_sub sub_use (iter.pit_pi @ eqs_add);
    pit_old = sigma_sub sub_use iter.pit_old;
    pit_curr = Sil.hpred_sub sub_use iter.pit_curr;
    pit_new = sigma_sub sub_use iter.pit_new }

let prop_iter_footprint_fav_add fav iter =
  sigma_fav_add fav iter.pit_sigma_fp;
  pi_fav_add fav iter.pit_pi_fp

(** Find fav of the footprint part of the iterator *)
let prop_iter_footprint_fav iter =
  Sil.fav_imperative_to_functional prop_iter_footprint_fav_add iter

let prop_iter_fav_add fav iter =
  Sil.sub_fav_add fav iter.pit_sub;
  pi_fav_add fav iter.pit_pi;
  pi_fav_add fav (IList.map snd iter.pit_newpi);
  sigma_fav_add fav iter.pit_old;
  sigma_fav_add fav iter.pit_new;
  Sil.hpred_fav_add fav iter.pit_curr;
  prop_iter_footprint_fav_add fav iter

(** Find fav of the iterator *)
let prop_iter_fav iter =
  Sil.fav_imperative_to_functional prop_iter_fav_add iter

(** Free vars of the iterator except the current hpred (and footprint). *)
let prop_iter_noncurr_fav_add fav iter =
  sigma_fav_add fav iter.pit_old;
  sigma_fav_add fav iter.pit_new;
  Sil.sub_fav_add fav iter.pit_sub;
  pi_fav_add fav iter.pit_pi

(** Extract the sigma part of the footprint *)
let prop_iter_get_footprint_sigma iter =
  iter.pit_sigma_fp

(** Replace the sigma part of the footprint *)
let prop_iter_replace_footprint_sigma iter sigma =
  { iter with pit_sigma_fp = sigma }

let prop_iter_noncurr_fav iter =
  Sil.fav_imperative_to_functional prop_iter_noncurr_fav_add iter

let rec strexp_gc_fields (fav: Sil.fav) se =
  match se with
  | Sil.Eexp _ ->
      Some se
  | Sil.Estruct (fsel, inst) ->
      let fselo = IList.map (fun (f, se) -> (f, strexp_gc_fields fav se)) fsel in
      let fsel' =
        let fselo' = IList.filter (function | (_, Some _) -> true | _ -> false) fselo in
        IList.map (function (f, seo) -> (f, unSome seo)) fselo' in
      if Sil.fld_strexp_list_compare fsel fsel' = 0 then Some se
      else Some (Sil.Estruct (fsel', inst))
  | Sil.Earray _ ->
      Some se

let hpred_gc_fields (fav: Sil.fav) hpred = match hpred with
  | Sil.Hpointsto (e, se, te) ->
      Sil.exp_fav_add fav e;
      Sil.exp_fav_add fav te;
      (match strexp_gc_fields fav se with
       | None -> hpred
       | Some se' ->
           if Sil.strexp_compare se se' = 0 then hpred
           else Sil.Hpointsto (e, se', te))
  | Sil.Hlseg _ | Sil.Hdllseg _ ->
      hpred

let rec prop_iter_map f iter =
  let hpred_curr = f iter in
  let iter' = { iter with pit_curr = hpred_curr } in
  match prop_iter_next iter' with
  | None -> iter'
  | Some iter'' -> prop_iter_map f iter''

(** Collect garbage fields. *)
let prop_iter_gc_fields iter =
  let f iter' =
    let fav = prop_iter_noncurr_fav iter' in
    hpred_gc_fields fav iter'.pit_curr in
  prop_iter_map f iter

let prop_case_split prop =
  let pi_sigma_list = Sil.sigma_to_sigma_ne prop.sigma in
  let f props_acc (pi, sigma) =
    let sigma' = sigma_normalize_prop prop sigma in
    let prop' =
      unsafe_cast_to_normal
        (set prop ~sigma:sigma') in
    (IList.fold_left prop_atom_and prop' pi):: props_acc in
  IList.fold_left f [] pi_sigma_list

let prop_expand prop =
  (*
  let _ = check_prop_normalized prop in
  *)
  prop_case_split prop

let mk_nondet il1 il2 loc =
  Sil.Stackop (Sil.Push, loc) :: (* save initial state *)
  il1 @ (* compute result1 *)
  [Sil.Stackop (Sil.Swap, loc)] @ (* save result1 and restore initial state *)
  il2 @ (* compute result2 *)
  [Sil.Stackop (Sil.Pop, loc)] (* combine result1 and result2 *)

(** translate a logical and/or operation taking care of the non-strict semantics for side effects *)
let trans_land_lor op ((idl1, stml1), e1) ((idl2, stml2), e2) loc =
  let no_side_effects stml =
    stml = [] in
  if no_side_effects stml2 then
    ((idl1@idl2, stml1@stml2), Exp.BinOp(op, e1, e2))
  else
    begin
      let id = Ident.create_fresh Ident.knormal in
      let prune_instr1, prune_res1, prune_instr2, prune_res2 =
        let cond1, cond2, res = match op with
          | Binop.LAnd -> e1, Exp.UnOp(Unop.LNot, e1, None), IntLit.zero
          | Binop.LOr -> Exp.UnOp(Unop.LNot, e1, None), e1, IntLit.one
          | _ -> assert false in
        let cond_res1 = Exp.BinOp(Binop.Eq, Exp.Var id, e2) in
        let cond_res2 = Exp.BinOp(Binop.Eq, Exp.Var id, Exp.int res) in
        let mk_prune cond =
          (* don't report always true/false *)
          Sil.Prune (cond, loc, true, Sil.Ik_land_lor) in
        mk_prune cond1, mk_prune cond_res1, mk_prune cond2, mk_prune cond_res2 in
      let instrs2 =
        mk_nondet (prune_instr1 :: stml2 @ [prune_res1]) ([prune_instr2; prune_res2]) loc in
      ((id:: idl1@idl2, stml1@instrs2), Exp.Var id)
    end

(** translate an if-then-else expression *)
let trans_if_then_else ((idl1, stml1), e1) ((idl2, stml2), e2) ((idl3, stml3), e3) loc =
  match sym_eval false e1 with
  | Exp.Const (Const.Cint i) when IntLit.iszero i -> (idl1@idl3, stml1@stml3), e3
  | Exp.Const (Const.Cint _) -> (idl1@idl2, stml1@stml2), e2
  | _ ->
      let e1not = Exp.UnOp(Unop.LNot, e1, None) in
      let id = Ident.create_fresh Ident.knormal in
      let mk_prune_res e =
        let mk_prune cond = Sil.Prune (cond, loc, true, Sil.Ik_land_lor)
        (* don't report always true/false *) in
        mk_prune (Exp.BinOp(Binop.Eq, Exp.Var id, e)) in
      let prune1 = Sil.Prune (e1, loc, true, Sil.Ik_bexp) in
      let prune1not = Sil.Prune (e1not, loc, false, Sil.Ik_bexp) in
      let stml' =
        mk_nondet
          (prune1 :: stml2 @ [mk_prune_res e2]) (prune1not :: stml3 @ [mk_prune_res e3]) loc in
      (id:: idl1@idl2@idl3, stml1@stml'), Exp.Var id

(*** START of module Metrics ***)
module Metrics : sig
  val prop_size : 'a t -> int
  val prop_chain_size : 'a t -> int
end = struct
  let ptsto_weight = 1
  and lseg_weight = 3
  and pi_weight = 1

  let rec hpara_size hpara = sigma_size hpara.Sil.body

  and hpara_dll_size hpara_dll = sigma_size hpara_dll.Sil.body_dll

  and hpred_size = function
    | Sil.Hpointsto _ -> ptsto_weight
    | Sil.Hlseg (_, hpara, _, _, _) -> lseg_weight * hpara_size hpara
    | Sil.Hdllseg (_, hpara_dll, _, _, _, _, _) -> lseg_weight * hpara_dll_size hpara_dll

  and sigma_size sigma =
    let size = ref 0 in
    IList.iter (fun hpred -> size := hpred_size hpred + !size) sigma; !size

  let pi_size pi = pi_weight * IList.length pi

  (** Compute a size value for the prop, which indicates its
      complexity *)
  let prop_size p =
    let size_current = sigma_size p.sigma in
    let size_footprint = sigma_size p.sigma_fp in
    max size_current size_footprint

  (** Approximate the size of the longest chain by counting the max
      number of |-> with the same type and whose lhs is primed or
      footprint *)
  let prop_chain_size p =
    let fp_size = pi_size p.pi_fp + sigma_size p.sigma_fp in
    pi_size p.pi + sigma_size p.sigma + fp_size
end
(*** END of module Metrics ***)

module CategorizePreconditions = struct
  type pre_category =
    (* no preconditions *)
    | NoPres

    (* the preconditions impose no restrictions *)
    | Empty
    (* the preconditions only demand that some pointers are allocated *)
    | OnlyAllocation

    (* the preconditions impose constraints on the values of variables and/or memory *)
    | DataConstraints

  (** categorize a list of preconditions *)
  let categorize preconditions =
    let lhs_is_lvar = function
      | Exp.Lvar _ -> true
      | _ -> false in
    let lhs_is_var_lvar = function
      | Exp.Var _ -> true
      | Exp.Lvar _ -> true
      | _ -> false in
    let rhs_is_var = function
      | Sil.Eexp (Exp.Var _, _) -> true
      | _ -> false in
    let rec rhs_only_vars = function
      | Sil.Eexp (Exp.Var _, _) -> true
      | Sil.Estruct (fsel, _) ->
          IList.for_all (fun (_, se) -> rhs_only_vars se) fsel
      | Sil.Earray _ -> true
      | _ -> false in
    let hpred_is_var = function (* stack variable with no constraints *)
      | Sil.Hpointsto (e, se, _) ->
          lhs_is_lvar e && rhs_is_var se
      | _ -> false in
    let hpred_only_allocation = function (* only constraint is allocation *)
      | Sil.Hpointsto (e, se, _) ->
          lhs_is_var_lvar e && rhs_only_vars se
      | _ -> false in
    let check_pre hpred_filter pre =
      let check_pi pi =
        pi = [] in
      let check_sigma sigma =
        IList.for_all hpred_filter sigma in
      check_pi pre.pi && check_sigma pre.sigma in
    let pres_no_constraints = IList.filter (check_pre hpred_is_var) preconditions in
    let pres_only_allocation = IList.filter (check_pre hpred_only_allocation) preconditions in
    match preconditions, pres_no_constraints, pres_only_allocation with
    | [], _, _ ->
        NoPres
    | _:: _, _:: _, _ ->
        Empty
    | _:: _, [], _:: _ ->
        OnlyAllocation
    | _:: _, [], [] ->
        DataConstraints
end
