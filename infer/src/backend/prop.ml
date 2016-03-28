(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

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
    foot_sigma : sigma;  (** abduced spatial part *)
    foot_pi: pi;  (** abduced pure part *)
  }

exception Cannot_star of ml_loc

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
  |> next sigma_compare p1.foot_sigma p2.foot_sigma
  |> next pi_compare p1.foot_pi p2.foot_pi

(** Check the equality of two propositions *)
let prop_equal p1 p2 =
  prop_compare p1 p2 = 0

(** {1 Functions for Pretty Printing} *)

(** Pretty print a footprint. *)
let pp_footprint _pe f fp =
  let pe = { _pe with pe_cmap_norm = _pe.pe_cmap_foot } in
  let pp_pi f () =
    if fp.foot_pi != [] then
      F.fprintf f "%a ;@\n" (pp_semicolon_seq_oneline pe (Sil.pp_atom pe)) fp.foot_pi in
  if fp.foot_pi != [] || fp.foot_sigma != [] then
    F.fprintf f "@\n[footprint@\n  @[%a%a@]  ]"
      pp_pi () (pp_semicolon_seq pe (Sil.pp_hpred pe)) fp.foot_sigma

let pp_texp_simple pe = match pe.pe_opt with
  | PP_SIM_DEFAULT -> Sil.pp_texp pe
  | PP_SIM_WITH_TYP -> Sil.pp_texp_full pe

(** Pretty print a pointsto representing a stack variable as an equality *)
let pp_hpred_stackvar pe0 f hpred =
  let pe, changed = Sil.color_pre_wrapper pe0 f hpred in
  begin match hpred with
    | Sil.Hpointsto (Sil.Lvar pvar, se, te) ->
        let pe' = match se with
          | Sil.Eexp (Sil.Var _, _) when not (Pvar.is_global pvar) ->
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
  let pi_sub = IList.map (fun (id, e) -> Sil.Aeq(Sil.Var id, e)) (Sil.sub_to_list sub) in
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
  if !Config.smt_output then pp_semicolon_seq pe (pp_sub_entry pe)
  else pp_semicolon_seq_oneline pe (pp_sub_entry pe)

(** Pretty print a pi. *)
let pp_pi pe =
  if !Config.smt_output then pp_semicolon_seq pe (Sil.pp_atom pe)
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
    | Sil.Hpointsto (Sil.Lvar pvar, _, _) -> not only_local_vars || Pvar.is_local pvar
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

(** Return the sub part of [prop]. *)
let get_sub (p: 'a t) : Sil.subst = p.sub

(** Return the pi part of [prop]. *)
let get_pi (p: 'a t) : pi = p.pi

let pi_of_subst sub =
  IList.map (fun (id1, e2) -> Sil.Aeq (Sil.Var id1, e2)) (Sil.sub_to_list sub)

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
let create_pvar_env (sigma: sigma) : (Sil.exp -> Sil.exp) =
  let env = ref [] in
  let filter = function
    | Sil.Hpointsto (Sil.Lvar pvar, Sil.Eexp (Sil.Var v, _), _) ->
        if not (Pvar.is_global pvar) then env := (Sil.Var v, Sil.Lvar pvar) :: !env
    | _ -> () in
  IList.iter filter sigma;
  let find e =
    try
      snd (IList.find (fun (e1, _) -> Sil.exp_equal e1 e) !env)
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
  if fp.foot_pi != [] || fp.foot_sigma != [] then
    F.fprintf f "@\n[footprint@\n   @[%a%a@]  ]"
      pp_pure fp.foot_pi
      (pp_sigma_simple pe env) fp.foot_sigma

(** Create a predicate environment for a prop *)
let prop_pred_env prop =
  let env = Sil.Predicates.empty_env () in
  IList.iter (Sil.Predicates.process_hpred env) prop.sigma;
  IList.iter (Sil.Predicates.process_hpred env) prop.foot_sigma;
  env

(** Pretty print a proposition. *)
let pp_prop pe0 f prop =
  let pe = prop_update_obj_sub pe0 prop in
  let latex = pe.pe_kind == PP_LATEX in
  let do_print f () =
    let subl = Sil.sub_to_list (get_sub prop) in
    (* since prop diff is based on physical equality, we need to extract the sub verbatim *)
    let pi = get_pi prop in
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
  if !Config.forcing_delayed_prints then (** print in html mode *)
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
  sigma_fav_add fav prop.foot_sigma;
  pi_fav_add fav prop.foot_pi

(** Find fav of the footprint part of the prop *)
let prop_footprint_fav prop =
  Sil.fav_imperative_to_functional prop_footprint_fav_add prop

let prop_fav_add fav prop =
  sigma_fav_add fav prop.sigma;
  sigma_fav_add fav prop.foot_sigma;
  Sil.sub_fav_add fav prop.sub;
  pi_fav_add fav prop.pi;
  pi_fav_add fav prop.foot_pi

let prop_fav p =
  Sil.fav_imperative_to_functional prop_fav_add p

(** free vars of the prop, excluding the pure part *)
let prop_fav_nonpure_add fav prop =
  sigma_fav_add fav prop.sigma;
  sigma_fav_add fav prop.foot_sigma

(** free vars, except pi and sub, of current and footprint parts *)
let prop_fav_nonpure =
  Sil.fav_imperative_to_functional prop_fav_nonpure_add

let hpred_fav_in_pvars_add fav = function
  | Sil.Hpointsto (Sil.Lvar _, sexp, _) -> Sil.strexp_fav_add fav sexp
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
  (pi_fpv prop.foot_pi) @
  (sigma_fpv prop.foot_sigma) @
  (sigma_fpv prop.sigma)

(** {2 Functions for Subsitition} *)

let pi_sub (subst: Sil.subst) pi =
  let f = Sil.atom_sub subst in
  IList.map f pi

let sigma_sub subst sigma =
  let f = Sil.hpred_sub subst in
  IList.map f sigma

(** {2 Functions for normalization} *)

(** This function assumes that if (x,Sil.Var(y)) in sub, then compare x y = 1 *)
let sub_normalize sub =
  let f (id, e) = (not (Ident.is_primed id)) && (not (Sil.ident_in_exp id e)) in
  let sub' = Sil.sub_filter_pair f sub in
  if Sil.sub_equal sub sub' then sub else sub'

let (--) = Sil.Int.sub
let (++) = Sil.Int.add

let iszero_int_float = function
  | Sil.Cint i -> Sil.Int.iszero i
  | Sil.Cfloat 0.0 -> true
  | _ -> false

let isone_int_float = function
  | Sil.Cint i -> Sil.Int.isone i
  | Sil.Cfloat 1.0 -> true
  | _ -> false

let isminusone_int_float = function
  | Sil.Cint i -> Sil.Int.isminusone i
  | Sil.Cfloat (-1.0) -> true
  | _ -> false

let sym_eval abs e =
  let rec eval e =
    (* L.d_str " ["; Sil.d_exp e; L.d_str"] "; *)
    match e with
    | Sil.Var _ ->
        e
    | Sil.Const (Sil.Cclosure c) ->
        let captured_vars =
          IList.map (fun (exp, pvar, typ) -> (eval exp, pvar, typ)) c.captured_vars in
        Sil.Const (Sil.Cclosure { c with captured_vars; })
    | Sil.Const _ ->
        e
    | Sil.Sizeof (Sil.Tarray (Sil.Tint ik, e), _)
      when Sil.ikind_is_char ik && !Config.curr_language <> Config.Java ->
        eval e
    | Sil.Sizeof _ ->
        e
    | Sil.Cast (_, e1) ->
        eval e1
    | Sil.UnOp (Sil.LNot, e1, topt) ->
        begin
          match eval e1 with
          | Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
              Sil.exp_one
          | Sil.Const (Sil.Cint _) ->
              Sil.exp_zero
          | Sil.UnOp(Sil.LNot, e1', _) ->
              e1'
          | e1' ->
              if abs then Sil.exp_get_undefined false else Sil.UnOp(Sil.LNot, e1', topt)
        end
    | Sil.UnOp (Sil.Neg, e1, topt) ->
        begin
          match eval e1 with
          | Sil.UnOp (Sil.Neg, e2', _) ->
              e2'
          | Sil.Const (Sil.Cint i) ->
              Sil.exp_int (Sil.Int.neg i)
          | Sil.Const (Sil.Cfloat v) ->
              Sil.exp_float (-. v)
          | Sil.Var id ->
              Sil.UnOp (Sil.Neg, Sil.Var id, topt)
          | e1' ->
              if abs then Sil.exp_get_undefined false else Sil.UnOp (Sil.Neg, e1', topt)
        end
    | Sil.UnOp (Sil.BNot, e1, topt) ->
        begin
          match eval e1 with
          | Sil.UnOp(Sil.BNot, e2', _) ->
              e2'
          | Sil.Const (Sil.Cint i) ->
              Sil.exp_int (Sil.Int.lognot i)
          | e1' ->
              if abs then Sil.exp_get_undefined false else Sil.UnOp (Sil.BNot, e1', topt)
        end
    | Sil.BinOp (Sil.Le, e1, e2) ->
        begin
          match eval e1, eval e2 with
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_bool (Sil.Int.leq n m)
          | Sil.Const (Sil.Cfloat v), Sil.Const (Sil.Cfloat w) ->
              Sil.exp_bool (v <= w)
          | Sil.BinOp (Sil.PlusA, e3, Sil.Const (Sil.Cint n)), Sil.Const (Sil.Cint m) ->
              Sil.BinOp (Sil.Le, e3, Sil.exp_int (m -- n))
          | e1', e2' ->
              Sil.exp_le e1' e2'
        end
    | Sil.BinOp (Sil.Lt, e1, e2) ->
        begin
          match eval e1, eval e2 with
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_bool (Sil.Int.lt n m)
          | Sil.Const (Sil.Cfloat v), Sil.Const (Sil.Cfloat w) ->
              Sil.exp_bool (v < w)
          | Sil.Const (Sil.Cint n), Sil.BinOp (Sil.MinusA, f1, f2) ->
              Sil.BinOp
                (Sil.Le, Sil.BinOp (Sil.MinusA, f2, f1), Sil.exp_int (Sil.Int.minus_one -- n))
          | Sil.BinOp(Sil.MinusA, f1 , f2), Sil.Const(Sil.Cint n) ->
              Sil.exp_le (Sil.BinOp(Sil.MinusA, f1 , f2)) (Sil.exp_int (n -- Sil.Int.one))
          | Sil.BinOp (Sil.PlusA, e3, Sil.Const (Sil.Cint n)), Sil.Const (Sil.Cint m) ->
              Sil.BinOp (Sil.Lt, e3, Sil.exp_int (m -- n))
          | e1', e2' ->
              Sil.exp_lt e1' e2'
        end
    | Sil.BinOp (Sil.Ge, e1, e2) ->
        eval (Sil.exp_le e2 e1)
    | Sil.BinOp (Sil.Gt, e1, e2) ->
        eval (Sil.exp_lt e2 e1)
    | Sil.BinOp (Sil.Eq, e1, e2) ->
        begin
          match eval e1, eval e2 with
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_bool (Sil.Int.eq n m)
          | Sil.Const (Sil.Cfloat v), Sil.Const (Sil.Cfloat w) ->
              Sil.exp_bool (v = w)
          | e1', e2' ->
              Sil.exp_eq e1' e2'
        end
    | Sil.BinOp (Sil.Ne, e1, e2) ->
        begin
          match eval e1, eval e2 with
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_bool (Sil.Int.neq n m)
          | Sil.Const (Sil.Cfloat v), Sil.Const (Sil.Cfloat w) ->
              Sil.exp_bool (v <> w)
          | e1', e2' ->
              Sil.exp_ne e1' e2'
        end
    | Sil.BinOp (Sil.LAnd, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin match e1', e2' with
          | Sil.Const (Sil.Cint i), _ when Sil.Int.iszero i ->
              e1'
          | Sil.Const (Sil.Cint _), _ ->
              e2'
          | _, Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
              e2'
          | _, Sil.Const (Sil.Cint _) ->
              e1'
          | _ ->
              Sil.BinOp (Sil.LAnd, e1', e2')
        end
    | Sil.BinOp (Sil.LOr, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e1', e2' with
          | Sil.Const (Sil.Cint i), _ when Sil.Int.iszero i ->
              e2'
          | Sil.Const (Sil.Cint _), _ ->
              e1'
          | _, Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
              e1'
          | _, Sil.Const (Sil.Cint _) ->
              e2'
          | _ ->
              Sil.BinOp (Sil.LOr, e1', e2')
        end
    | Sil.BinOp(Sil.PlusPI, Sil.Lindex (ep, e1), e2) -> (* array access with pointer arithmetic *)
        let e' = Sil.BinOp (Sil.PlusA, e1, e2) in
        eval (Sil.Lindex (ep, e'))
    | Sil.BinOp (Sil.PlusPI, (Sil.BinOp (Sil.PlusPI, e11, e12)), e2) ->
        (* take care of pattern ((ptr + off1) + off2) *)
        (* progress: convert inner +I to +A *)
        let e2' = Sil.BinOp (Sil.PlusA, e12, e2) in
        eval (Sil.BinOp (Sil.PlusPI, e11, e2'))
    | Sil.BinOp
        (Sil.PlusA,
         (Sil.Sizeof (Sil.Tstruct struct_typ, _) as e1),
         e2) ->
        (* pattern for extensible structs given a struct declatead as struct s { ... t arr[n] ... },
           allocation pattern malloc(sizeof(struct s) + k * siezof(t)) turn it into
           struct s { ... t arr[n + k] ... } *)
        let e1' = eval e1 in
        let e2' = eval e2 in
        let instance_fields = struct_typ.Sil.instance_fields in
        (match IList.rev instance_fields, e2' with
           (fname, Sil.Tarray (typ, size), _) :: ltfa,
           Sil.BinOp(Sil.Mult, num_elem, Sil.Sizeof (texp, st))
           when instance_fields != [] && Sil.typ_equal typ texp ->
             let size' = Sil.BinOp(Sil.PlusA, size, num_elem) in
             let ltfa' = (fname, Sil.Tarray(typ, size'), Sil.item_annotation_empty) :: ltfa in
             let struct_typ' =
               { struct_typ with Sil.instance_fields = ltfa' } in
             Sil.Sizeof (Sil.Tstruct struct_typ', st)
         | _ -> Sil.BinOp(Sil.PlusA, e1', e2'))
    | Sil.BinOp (Sil.PlusA as oplus, e1, e2)
    | Sil.BinOp (Sil.PlusPI as oplus, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        let isPlusA = oplus = Sil.PlusA in
        let ominus = if isPlusA then Sil.MinusA else Sil.MinusPI in
        let (+++) x y = match y with
          | Sil.Const (Sil.Cint i) when Sil.Int.iszero i -> x
          | _ -> Sil.BinOp (oplus, x, y) in
        let (---) x y = match y with
          | Sil.Const (Sil.Cint i) when Sil.Int.iszero i -> x
          | _ -> Sil.BinOp (ominus, x, y) in
        begin
          match e1', e2' with
          | Sil.Const c, _ when iszero_int_float c ->
              e2'
          | _, Sil.Const c when iszero_int_float c ->
              e1'
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_int (n ++ m)
          | Sil.Const (Sil.Cfloat v), Sil.Const (Sil.Cfloat w) ->
              Sil.exp_float (v +. w)
          | Sil.UnOp(Sil.Neg, f1, _), f2
          | f2, Sil.UnOp(Sil.Neg, f1, _) ->
              Sil.BinOp (ominus, f2, f1)
          | Sil.BinOp (Sil.PlusA, e, Sil.Const (Sil.Cint n1)), Sil.Const (Sil.Cint n2)
          | Sil.BinOp (Sil.PlusPI, e, Sil.Const (Sil.Cint n1)), Sil.Const (Sil.Cint n2)
          | Sil.Const (Sil.Cint n2), Sil.BinOp (Sil.PlusA, e, Sil.Const (Sil.Cint n1))
          | Sil.Const (Sil.Cint n2), Sil.BinOp (Sil.PlusPI, e, Sil.Const (Sil.Cint n1)) ->
              e +++ (Sil.exp_int (n1 ++ n2))
          | Sil.BinOp (Sil.MinusA, Sil.Const (Sil.Cint n1), e), Sil.Const (Sil.Cint n2)
          | Sil.Const (Sil.Cint n2), Sil.BinOp (Sil.MinusA, Sil.Const (Sil.Cint n1), e) ->
              Sil.exp_int (n1 ++ n2) --- e
          | Sil.BinOp (Sil.MinusA, e1, e2), e3 -> (* (e1-e2)+e3 --> e1 + (e3-e2) *)
              (* progress: brings + to the outside *)
              eval (e1 +++ (e3 --- e2))
          | _, Sil.Const _ ->
              e1' +++ e2'
          | Sil.Const _, _ ->
              if isPlusA then e2' +++ e1' else e1' +++ e2'
          | Sil.Var _, Sil.Var _ ->
              e1' +++ e2'
          | _ ->
              if abs && isPlusA then Sil.exp_get_undefined false else
              if abs && not isPlusA then e1' +++ (Sil.exp_get_undefined false)
              else e1' +++ e2'
        end
    | Sil.BinOp (Sil.MinusA as ominus, e1, e2)
    | Sil.BinOp (Sil.MinusPI as ominus, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        let isMinusA = ominus = Sil.MinusA in
        let oplus = if isMinusA then Sil.PlusA else Sil.PlusPI in
        let (+++) x y = Sil.BinOp (oplus, x, y) in
        let (---) x y = Sil.BinOp (ominus, x, y) in
        if Sil.exp_equal e1' e2' then Sil.exp_zero
        else begin
          match e1', e2' with
          | Sil.Const c, _ when iszero_int_float c ->
              eval (Sil.UnOp(Sil.Neg, e2', None))
          | _, Sil.Const c when iszero_int_float c ->
              e1'
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_int (n -- m)
          | Sil.Const (Sil.Cfloat v), Sil.Const (Sil.Cfloat w) ->
              Sil.exp_float (v -. w)
          | _, Sil.UnOp (Sil.Neg, f2, _) ->
              eval (e1 +++ f2)
          | _ , Sil.Const(Sil.Cint n) ->
              eval (e1' +++ (Sil.exp_int (Sil.Int.neg n)))
          | Sil.Const _, _ ->
              e1' --- e2'
          | Sil.Var _, Sil.Var _ ->
              e1' --- e2'
          | _, _ ->
              if abs then Sil.exp_get_undefined false else e1' --- e2'
        end
    | Sil.BinOp (Sil.MinusPP, e1, e2) ->
        if abs then Sil.exp_get_undefined false
        else Sil.BinOp (Sil.MinusPP, eval e1, eval e2)
    | Sil.BinOp (Sil.Mult, esize, Sil.Sizeof (t, st))
    | Sil.BinOp(Sil.Mult, Sil.Sizeof (t, st), esize) ->
        begin
          match eval esize, eval (Sil.Sizeof (t, st)) with
          | Sil.Const (Sil.Cint i), e' when Sil.Int.isone i -> e'
          | esize', e' -> Sil.BinOp(Sil.Mult, esize', e')
        end
    | Sil.BinOp (Sil.Mult, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e1', e2' with
          | Sil.Const c, _ when iszero_int_float c ->
              Sil.exp_zero
          | Sil.Const c, _ when isone_int_float c ->
              e2'
          | Sil.Const c, _ when isminusone_int_float c ->
              eval (Sil.UnOp (Sil.Neg, e2', None))
          | _, Sil.Const c when iszero_int_float c ->
              Sil.exp_zero
          | _, Sil.Const c when isone_int_float c ->
              e1'
          | _, Sil.Const c when isminusone_int_float c ->
              eval (Sil.UnOp (Sil.Neg, e1', None))
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_int (Sil.Int.mul n m)
          | Sil.Const (Sil.Cfloat v), Sil.Const (Sil.Cfloat w) ->
              Sil.exp_float (v *. w)
          | Sil.Var _, Sil.Var _ ->
              Sil.BinOp(Sil.Mult, e1', e2')
          | _, _ ->
              if abs then Sil.exp_get_undefined false else Sil.BinOp(Sil.Mult, e1', e2')
        end
    | Sil.BinOp (Sil.Div, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e1', e2' with
          | _, Sil.Const c when iszero_int_float c ->
              Sil.exp_get_undefined false
          | Sil.Const c, _ when iszero_int_float c ->
              e1'
          | _, Sil.Const c when isone_int_float c ->
              e1'
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_int (Sil.Int.div n m)
          | Sil.Const (Sil.Cfloat v), Sil.Const (Sil.Cfloat w) ->
              Sil.exp_float (v /.w)
          | Sil.Sizeof(Sil.Tarray(typ, size), _), Sil.Sizeof(_typ, _)
            (* pattern: sizeof(arr) / sizeof(arr[0]) = size of arr *)
            when Sil.typ_equal _typ typ ->
              size
          | _ ->
              if abs then Sil.exp_get_undefined false else Sil.BinOp (Sil.Div, e1', e2')
        end
    | Sil.BinOp (Sil.Mod, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e1', e2' with
          | _, Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
              Sil.exp_get_undefined false
          | Sil.Const (Sil.Cint i), _ when Sil.Int.iszero i ->
              e1'
          | _, Sil.Const (Sil.Cint i) when Sil.Int.isone i ->
              Sil.exp_zero
          | Sil.Const (Sil.Cint n), Sil.Const (Sil.Cint m) ->
              Sil.exp_int (Sil.Int.rem n m)
          | _ ->
              if abs then Sil.exp_get_undefined false else Sil.BinOp (Sil.Mod, e1', e2')
        end
    | Sil.BinOp (Sil.Shiftlt, e1, e2) ->
        if abs then Sil.exp_get_undefined false else Sil.BinOp (Sil.Shiftlt, eval e1, eval e2)
    | Sil.BinOp (Sil.Shiftrt, e1, e2) ->
        if abs then Sil.exp_get_undefined false else Sil.BinOp (Sil.Shiftrt, eval e1, eval e2)
    | Sil.BinOp (Sil.BAnd, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin match e1', e2' with
          | Sil.Const (Sil.Cint i), _ when Sil.Int.iszero i ->
              e1'
          | _, Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
              e2'
          | Sil.Const (Sil.Cint i1), Sil.Const(Sil.Cint i2) ->
              Sil.exp_int (Sil.Int.logand i1 i2)
          | _ ->
              if abs then Sil.exp_get_undefined false else Sil.BinOp (Sil.BAnd, e1', e2')
        end
    | Sil.BinOp (Sil.BOr, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin match e1', e2' with
          | Sil.Const (Sil.Cint i), _ when Sil.Int.iszero i ->
              e2'
          | _, Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
              e1'
          | Sil.Const (Sil.Cint i1), Sil.Const(Sil.Cint i2) ->
              Sil.exp_int (Sil.Int.logor i1 i2)
          | _ ->
              if abs then Sil.exp_get_undefined false else Sil.BinOp (Sil.BOr, e1', e2')
        end
    | Sil.BinOp (Sil.BXor, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin match e1', e2' with
          | Sil.Const (Sil.Cint i), _ when Sil.Int.iszero i ->
              e2'
          | _, Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
              e1'
          | Sil.Const (Sil.Cint i1), Sil.Const(Sil.Cint i2) ->
              Sil.exp_int (Sil.Int.logxor i1 i2)
          | _ ->
              if abs then Sil.exp_get_undefined false else Sil.BinOp (Sil.BXor, e1', e2')
        end
    | Sil.BinOp (Sil.PtrFld, e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        begin
          match e2' with
          | Sil.Const (Sil.Cptr_to_fld (fn, typ)) ->
              eval (Sil.Lfield(e1', fn, typ))
          | Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
              Sil.exp_zero (* cause a NULL dereference *)
          | _ -> Sil.BinOp (Sil.PtrFld, e1', e2')
        end
    | Sil.Lvar _ ->
        e
    | Sil.Lfield (e1, fld, typ) ->
        let e1' = eval e1 in
        Sil.Lfield (e1', fld, typ)
    | Sil.Lindex(Sil.Lvar pv, e2) when false
      (* removed: it interferes with re-arrangement and error messages *)
      -> (* &x[n]  -->  &x + n *)
        eval (Sil.BinOp (Sil.PlusPI, Sil.Lvar pv, e2))
    | Sil.Lindex (Sil.BinOp(Sil.PlusPI, ep, e1), e2) -> (* array access with pointer arithmetic *)
        let e' = Sil.BinOp (Sil.PlusA, e1, e2) in
        eval (Sil.Lindex (ep, e'))
    | Sil.Lindex (e1, e2) ->
        let e1' = eval e1 in
        let e2' = eval e2 in
        Sil.Lindex(e1', e2') in
  let e' = eval e in
  (* L.d_str "sym_eval "; Sil.d_exp e; L.d_str" --> "; Sil.d_exp e'; L.d_ln (); *)
  e'

let exp_normalize sub exp =
  let exp' = Sil.exp_sub sub exp in
  if !Config.abs_val >= 1 then sym_eval true exp'
  else sym_eval false exp'

let rec texp_normalize sub exp = match exp with
  | Sil.Sizeof (typ, st) -> Sil.Sizeof (typ_normalize sub typ, st)
  | _ -> exp_normalize sub exp

and typ_normalize sub typ = match typ with
  | Sil.Tvar _
  | Sil.Tint _
  | Sil.Tfloat _
  | Sil.Tvoid
  | Sil.Tfun _ ->
      typ
  | Sil.Tptr (t', pk) ->
      Sil.Tptr (typ_normalize sub t', pk)
  | Sil.Tstruct struct_typ ->
      let fld_norm = IList.map (fun (f, t, a) -> (f, typ_normalize sub t, a)) in
      let instance_fields = fld_norm struct_typ.Sil.instance_fields in
      let static_fields = fld_norm struct_typ.Sil.static_fields in
      Sil.Tstruct
        { struct_typ with
          Sil.instance_fields;
          static_fields;
        }
  | Sil.Tarray (t, e) ->
      Sil.Tarray (typ_normalize sub t, exp_normalize sub e)

let run_with_abs_val_eq_zero f =
  let abs_val_old = !Config.abs_val in
  Config.abs_val := 0;
  let res = f () in
  Config.abs_val := abs_val_old;
  res

let exp_normalize_noabs sub exp =
  run_with_abs_val_eq_zero
    (fun () -> exp_normalize sub exp)

(** Return [true] if the atom is an inequality *)
let atom_is_inequality = function
  | Sil.Aeq (Sil.BinOp (Sil.Le, _, _), Sil.Const (Sil.Cint i)) when Sil.Int.isone i -> true
  | Sil.Aeq (Sil.BinOp (Sil.Lt, _, _), Sil.Const (Sil.Cint i)) when Sil.Int.isone i -> true
  | _ -> false

(** If the atom is [e<=n] return [e,n] *)
let atom_exp_le_const = function
  | Sil.Aeq(Sil.BinOp (Sil.Le, e1, Sil.Const (Sil.Cint n)), Sil.Const (Sil.Cint i))
    when Sil.Int.isone i ->
      Some (e1, n)
  | _ -> None

(** If the atom is [n<e] return [n,e] *)
let atom_const_lt_exp = function
  | Sil.Aeq(Sil.BinOp (Sil.Lt, Sil.Const (Sil.Cint n), e1), Sil.Const (Sil.Cint i))
    when Sil.Int.isone i ->
      Some (n, e1)
  | _ -> None

(** Turn an inequality expression into an atom *)
let mk_inequality e =
  match e with
  | Sil.BinOp (Sil.Le, base, Sil.Const (Sil.Cint n)) ->
      (* base <= n case *)
      let nbase = exp_normalize_noabs Sil.sub_empty base in
      (match nbase with
       | Sil.BinOp(Sil.PlusA, base', Sil.Const (Sil.Cint n')) ->
           let new_offset = Sil.exp_int (n -- n') in
           let new_e = Sil.BinOp (Sil.Le, base', new_offset) in
           Sil.Aeq (new_e, Sil.exp_one)
       | Sil.BinOp(Sil.PlusA, Sil.Const (Sil.Cint n'), base') ->
           let new_offset = Sil.exp_int (n -- n') in
           let new_e = Sil.BinOp (Sil.Le, base', new_offset) in
           Sil.Aeq (new_e, Sil.exp_one)
       | Sil.BinOp(Sil.MinusA, base', Sil.Const (Sil.Cint n')) ->
           let new_offset = Sil.exp_int (n ++ n') in
           let new_e = Sil.BinOp (Sil.Le, base', new_offset) in
           Sil.Aeq (new_e, Sil.exp_one)
       | Sil.BinOp(Sil.MinusA, Sil.Const (Sil.Cint n'), base') ->
           let new_offset = Sil.exp_int (n' -- n -- Sil.Int.one) in
           let new_e = Sil.BinOp (Sil.Lt, new_offset, base') in
           Sil.Aeq (new_e, Sil.exp_one)
       | Sil.UnOp(Sil.Neg, new_base, _) ->
           (* In this case, base = -new_base. Construct -n-1 < new_base. *)
           let new_offset = Sil.exp_int (Sil.Int.zero -- n -- Sil.Int.one) in
           let new_e = Sil.BinOp (Sil.Lt, new_offset, new_base) in
           Sil.Aeq (new_e, Sil.exp_one)
       | _ -> Sil.Aeq (e, Sil.exp_one))
  | Sil.BinOp (Sil.Lt, Sil.Const (Sil.Cint n), base) ->
      (* n < base case *)
      let nbase = exp_normalize_noabs Sil.sub_empty base in
      (match nbase with
       | Sil.BinOp(Sil.PlusA, base', Sil.Const (Sil.Cint n')) ->
           let new_offset = Sil.exp_int (n -- n') in
           let new_e = Sil.BinOp (Sil.Lt, new_offset, base') in
           Sil.Aeq (new_e, Sil.exp_one)
       | Sil.BinOp(Sil.PlusA, Sil.Const (Sil.Cint n'), base') ->
           let new_offset = Sil.exp_int (n -- n') in
           let new_e = Sil.BinOp (Sil.Lt, new_offset, base') in
           Sil.Aeq (new_e, Sil.exp_one)
       | Sil.BinOp(Sil.MinusA, base', Sil.Const (Sil.Cint n')) ->
           let new_offset = Sil.exp_int (n ++ n') in
           let new_e = Sil.BinOp (Sil.Lt, new_offset, base') in
           Sil.Aeq (new_e, Sil.exp_one)
       | Sil.BinOp(Sil.MinusA, Sil.Const (Sil.Cint n'), base') ->
           let new_offset = Sil.exp_int (n' -- n -- Sil.Int.one) in
           let new_e = Sil.BinOp (Sil.Le, base', new_offset) in
           Sil.Aeq (new_e, Sil.exp_one)
       | Sil.UnOp(Sil.Neg, new_base, _) ->
           (* In this case, base = -new_base. Construct new_base <= -n-1 *)
           let new_offset = Sil.exp_int (Sil.Int.zero -- n -- Sil.Int.one) in
           let new_e = Sil.BinOp (Sil.Le, new_base, new_offset) in
           Sil.Aeq (new_e, Sil.exp_one)
       | _ -> Sil.Aeq (e, Sil.exp_one))
  | _ -> Sil.Aeq (e, Sil.exp_one)

(** Normalize an inequality *)
let inequality_normalize a =
  (** turn an expression into a triple (pos,neg,off) of positive and negative occurrences,
      and integer offset *)
  (** representing inequality [sum(pos) - sum(neg) + off <= 0] *)
  let rec exp_to_posnegoff e = match e with
    | Sil.Const (Sil.Cint n) -> [],[], n
    | Sil.BinOp(Sil.PlusA, e1, e2) | Sil.BinOp(Sil.PlusPI, e1, e2) ->
        let pos1, neg1, n1 = exp_to_posnegoff e1 in
        let pos2, neg2, n2 = exp_to_posnegoff e2 in
        (pos1@pos2, neg1@neg2, n1 ++ n2)
    | Sil.BinOp(Sil.MinusA, e1, e2)
    | Sil.BinOp(Sil.MinusPI, e1, e2)
    | Sil.BinOp(Sil.MinusPP, e1, e2) ->
        let pos1, neg1, n1 = exp_to_posnegoff e1 in
        let pos2, neg2, n2 = exp_to_posnegoff e2 in
        (pos1@neg2, neg1@pos2, n1 -- n2)
    | Sil.UnOp(Sil.Neg, e1, _) ->
        let pos1, neg1, n1 = exp_to_posnegoff e1 in
        (neg1, pos1, Sil.Int.zero -- n1)
    | _ -> [e],[], Sil.Int.zero in
  (** sort and filter out expressions appearing in both the positive and negative part *)
  let normalize_posnegoff (pos, neg, off) =
    let pos' = IList.sort Sil.exp_compare pos in
    let neg' = IList.sort Sil.exp_compare neg in
    let rec combine pacc nacc = function
      | x:: ps, y:: ng ->
          (match Sil.exp_compare x y with
           | n when n < 0 -> combine (x:: pacc) nacc (ps, y :: ng)
           | 0 -> combine pacc nacc (ps, ng)
           | _ -> combine pacc (y:: nacc) (x :: ps, ng))
      | ps, ng -> (IList.rev pacc) @ ps, (IList.rev nacc) @ ng in
    let pos'', neg'' = combine [] [] (pos', neg') in
    (pos'', neg'', off) in
  (** turn a non-empty list of expressions into a sum expression *)
  let rec exp_list_to_sum = function
    | [] -> assert false
    | [e] -> e
    | e:: el -> Sil.BinOp(Sil.PlusA, e, exp_list_to_sum el) in
  let norm_from_exp e =
    match normalize_posnegoff (exp_to_posnegoff e) with
    | [],[], n -> Sil.BinOp(Sil.Le, Sil.exp_int n, Sil.exp_zero)
    | [], neg, n -> Sil.BinOp(Sil.Lt, Sil.exp_int (n -- Sil.Int.one), exp_list_to_sum neg)
    | pos, [], n -> Sil.BinOp(Sil.Le, exp_list_to_sum pos, Sil.exp_int (Sil.Int.zero -- n))
    | pos, neg, n ->
        let lhs_e = Sil.BinOp(Sil.MinusA, exp_list_to_sum pos, exp_list_to_sum neg) in
        Sil.BinOp(Sil.Le, lhs_e, Sil.exp_int (Sil.Int.zero -- n)) in
  let ineq = match a with
    | Sil.Aeq (ineq, Sil.Const (Sil.Cint i)) when Sil.Int.isone i ->
        ineq
    | _ -> assert false in
  match ineq with
  | Sil.BinOp(Sil.Le, e1, e2) ->
      let e = Sil.BinOp(Sil.MinusA, e1, e2) in
      mk_inequality (norm_from_exp e)
  | Sil.BinOp(Sil.Lt, e1, e2) ->
      let e = Sil.BinOp(Sil.MinusA, Sil.BinOp(Sil.MinusA, e1, e2), Sil.exp_minus_one) in
      mk_inequality (norm_from_exp e)
  | _ -> a

let exp_reorder e1 e2 = if Sil.exp_compare e1 e2 <= 0 then (e1, e2) else (e2, e1)

(** Normalize an atom.
    We keep the convention that inequalities with constants
    are only of the form [e <= n] and [n < e]. *)
let atom_normalize sub a0 =
  let a = Sil.atom_sub sub a0 in
  let rec normalize_eq eq = match eq with
    | Sil.BinOp(Sil.PlusA, e1, Sil.Const (Sil.Cint n1)), Sil.Const (Sil.Cint n2)
    (* e1+n1==n2 ---> e1==n2-n1 *)
    | Sil.BinOp(Sil.PlusPI, e1, Sil.Const (Sil.Cint n1)), Sil.Const (Sil.Cint n2) ->
        (e1, Sil.exp_int (n2 -- n1))
    | Sil.BinOp(Sil.MinusA, e1, Sil.Const (Sil.Cint n1)), Sil.Const (Sil.Cint n2)
    (* e1-n1==n2 ---> e1==n1+n2 *)
    | Sil.BinOp(Sil.MinusPI, e1, Sil.Const (Sil.Cint n1)), Sil.Const (Sil.Cint n2) ->
        (e1, Sil.exp_int (n1 ++ n2))
    | Sil.BinOp(Sil.MinusA, Sil.Const (Sil.Cint n1), e1), Sil.Const (Sil.Cint n2) ->
        (* n1-e1 == n2 -> e1==n1-n2 *)
        (e1, Sil.exp_int (n1 -- n2))
    | Sil.Lfield (e1', fld1, _), Sil.Lfield (e2', fld2, _) ->
        if Sil.fld_equal fld1 fld2
        then normalize_eq (e1', e2')
        else eq
    | Sil.Lindex (e1', idx1), Sil.Lindex (e2', idx2) ->
        if Sil.exp_equal idx1 idx2 then normalize_eq (e1', e2')
        else if Sil.exp_equal e1' e2' then normalize_eq (idx1, idx2)
        else eq
    | _ -> eq in
  let handle_unary_negation e1 e2 =
    match e1, e2 with
    | Sil.UnOp (Sil.LNot, e1', _), Sil.Const (Sil.Cint i)
    | Sil.Const (Sil.Cint i), Sil.UnOp (Sil.LNot, e1', _) when Sil.Int.iszero i ->
        (e1', Sil.exp_zero, true)
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
        handle_boolean_operation false e1 e2 in
  if atom_is_inequality a' then inequality_normalize a' else a'

(** Negate an atom *)
let atom_negate = function
  | Sil.Aeq (Sil.BinOp (Sil.Le, e1, e2), Sil.Const (Sil.Cint i)) when Sil.Int.isone i ->
      mk_inequality (Sil.exp_lt e2 e1)
  | Sil.Aeq (Sil.BinOp (Sil.Lt, e1, e2), Sil.Const (Sil.Cint i)) when Sil.Int.isone i ->
      mk_inequality (Sil.exp_le e2 e1)
  | Sil.Aeq (e1, e2) -> Sil.Aneq (e1, e2)
  | Sil.Aneq (e1, e2) -> Sil.Aeq (e1, e2)

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
  | Sil.Earray (size, idx_cnts, inst) ->
      begin
        let size' = exp_normalize_noabs sub size in
        match idx_cnts with
        | [] ->
            if Sil.exp_equal size size' then se else Sil.Earray (size', idx_cnts, inst)
        | _ ->
            let idx_cnts' =
              IList.map (fun (idx, cnt) ->
                  let idx' = exp_normalize sub idx in
                  idx', strexp_normalize sub cnt) idx_cnts in
            let idx_cnts'' =
              IList.sort Sil.exp_strexp_compare idx_cnts' in
            Sil.Earray (size', idx_cnts'', inst)
      end

(** create a strexp of the given type, populating the structures if [expand_structs] is true *)
let rec create_strexp_of_type tenvo struct_init_mode typ inst =
  let init_value () =
    let create_fresh_var () =
      let fresh_id =
        (Ident.create_fresh (if !Config.footprint then Ident.kfootprint else Ident.kprimed)) in
      Sil.Var fresh_id in
    if !Config.curr_language = Config.Java && inst = Sil.Ialloc
    then
      match typ with
      | Sil.Tfloat _ -> Sil.Const (Sil.Cfloat 0.0)
      | _ -> Sil.exp_zero
    else
      create_fresh_var () in
  match typ with
  | Sil.Tint _ | Sil.Tfloat _ | Sil.Tvoid | Sil.Tfun _ | Sil.Tptr _ ->
      Sil.Eexp (init_value (), inst)
  | Sil.Tstruct { Sil.instance_fields } ->
      begin
        match struct_init_mode with
        | No_init -> Sil.Estruct ([], inst)
        | Fld_init ->
            let f (fld, t, a) =
              if Sil.is_objc_ref_counter_field (fld, t, a) then
                (fld, Sil.Eexp (Sil.exp_one, inst))
              else
                (fld, create_strexp_of_type tenvo struct_init_mode t inst) in
            Sil.Estruct (IList.map f instance_fields, inst)
      end
  | Sil.Tarray (_, size) ->
      Sil.Earray (size, [], inst)
  | Sil.Tvar _ ->
      assert false

(** Sil.Construct a pointsto. *)
let mk_ptsto lexp sexp te =
  let nsexp = strexp_normalize Sil.sub_empty sexp in
  Sil.Hpointsto(lexp, nsexp, te)

(** Construct a points-to predicate for an expression using
    either the provided expression [name] as
    base for fresh identifiers. If [expand_structs] is true,
    initialize the fields of structs with fresh variables. *)
let mk_ptsto_exp tenvo struct_init_mode (exp, te, expo) inst : Sil.hpred =
  let default_strexp () = match te with
    | Sil.Sizeof (typ, _) ->
        create_strexp_of_type tenvo struct_init_mode typ inst
    | Sil.Var _ ->
        Sil.Estruct ([], inst)
    | te ->
        L.err "trying to create ptsto with type: %a@\n@." (Sil.pp_texp_full pe_text) te;
        assert false in
  let strexp = match expo with
    | Some e -> Sil.Eexp (e, inst)
    | None -> default_strexp () in
  mk_ptsto exp strexp te

let replace_array_contents hpred esel = match hpred with
  | Sil.Hpointsto (root, Sil.Earray (size, [], inst), te) ->
      Sil.Hpointsto (root, Sil.Earray (size, esel, inst), te)
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
        | Sil.Earray (Sil.Sizeof (t, st1), [], inst), Sil.Sizeof (Sil.Tarray _, _) ->
            (* check for an empty array whose size expression is (Sizeof type), and turn the array
               into a strexp of the given type *)
            let hpred' = mk_ptsto_exp None Fld_init (root, Sil.Sizeof (t, st1), None) inst in
            replace_hpred hpred'
        | Sil.Earray (Sil.BinOp(Sil.Mult, Sil.Sizeof (t, st1), x), esel, inst),
          Sil.Sizeof (Sil.Tarray _, _)
        | Sil.Earray (Sil.BinOp(Sil.Mult, x, Sil.Sizeof (t, st1)), esel, inst),
          Sil.Sizeof (Sil.Tarray _, _) ->
            (* check for an array whose size expression is n * (Sizeof type), and turn the array
               into a strexp of the given type *)
            let hpred' =
              mk_ptsto_exp None Fld_init (root, Sil.Sizeof (Sil.Tarray(t, x), st1), None) inst in
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
      | Sil.Aneq(Sil.Const (Sil.Cint n), e) | Sil.Aneq(e, Sil.Const (Sil.Cint n)) -> (e, n):: acc
      | _ -> acc in
    IList.fold_left get_disequality_info [] nonineq_list in
  let is_neq e n =
    IList.exists (fun (e', n') -> Sil.exp_equal e e' && Sil.Int.eq n n') diseq_list in
  let le_list_tightened =
    let get_le_inequality_info acc a =
      match atom_exp_le_const a with
      | Some (e, n) -> (e, n):: acc
      | _ -> acc in
    let rec le_tighten le_list_done = function
      | [] -> IList.rev le_list_done
      | (e, n):: le_list_todo -> (* e <= n *)
          if is_neq e n then le_tighten le_list_done ((e, n -- Sil.Int.one):: le_list_todo)
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
          let n_plus_one = n ++ Sil.Int.one in
          if is_neq e n_plus_one then lt_tighten lt_list_done ((n ++ Sil.Int.one, e):: lt_list_todo)
          else lt_tighten ((n, e):: lt_list_done) (lt_list_todo) in
    let lt_list = IList.rev (IList.fold_left get_lt_inequality_info [] ineq_list) in
    lt_tighten [] lt_list in
  let ineq_list' =
    let le_ineq_list =
      IList.map
        (fun (e, n) -> mk_inequality (Sil.BinOp(Sil.Le, e, Sil.exp_int n)))
        le_list_tightened in
    let lt_ineq_list =
      IList.map
        (fun (n, e) -> mk_inequality (Sil.BinOp(Sil.Lt, Sil.exp_int n, e)))
        lt_list_tightened in
    le_ineq_list @ lt_ineq_list in
  let nonineq_list' =
    IList.filter
      (function
        | Sil.Aneq(Sil.Const (Sil.Cint n), e)
        | Sil.Aneq(e, Sil.Const (Sil.Cint n)) ->
            (not (IList.exists
                    (fun (e', n') -> Sil.exp_equal e e' && Sil.Int.lt n' n)
                    le_list_tightened)) &&
            (not (IList.exists
                    (fun (n', e') -> Sil.exp_equal e e' && Sil.Int.leq n n')
                    lt_list_tightened))
        | _ -> true)
      nonineq_list in
  (ineq_list', nonineq_list')

(** remove duplicate atoms and redundant inequalities from a sorted pi *)
let rec pi_sorted_remove_redundant = function
  | (Sil.Aeq(Sil.BinOp (Sil.Le, e1, Sil.Const (Sil.Cint n1)), Sil.Const (Sil.Cint i1)) as a1) ::
    Sil.Aeq(Sil.BinOp (Sil.Le, e2, Sil.Const (Sil.Cint n2)), Sil.Const (Sil.Cint i2)) :: rest
    when Sil.Int.isone i1 && Sil.Int.isone i2 && Sil.exp_equal e1 e2 && Sil.Int.lt n1 n2 ->
      (* second inequality redundant *)
      pi_sorted_remove_redundant (a1 :: rest)
  | Sil.Aeq(Sil.BinOp (Sil.Lt, Sil.Const (Sil.Cint n1), e1), Sil.Const (Sil.Cint i1)) ::
    (Sil.Aeq(Sil.BinOp (Sil.Lt, Sil.Const (Sil.Cint n2), e2), Sil.Const (Sil.Cint i2)) as a2) ::
    rest
    when Sil.Int.isone i1 && Sil.Int.isone i2 && Sil.exp_equal e1 e2 && Sil.Int.lt n1 n2 ->
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
    | Sil.Hpointsto(_, Sil.Eexp(e, _), Sil.Sizeof (Sil.Tint ik, _)) when Sil.ikind_is_unsigned ik ->
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
    | Sil.BinOp(op1, e1, Sil.Const(c1)), Sil.BinOp(op2, e2, Sil.Const(c2))
      when Sil.exp_equal e1 e2 ->
        Sil.binop_equal op1 op2 && Sil.binop_injective op1 && not (Sil.const_equal c1 c2)
    | e1, Sil.BinOp(op2, e2, Sil.Const(c2))
      when Sil.exp_equal e1 e2 ->
        Sil.binop_injective op2 &&
        Sil.binop_is_zero_runit op2 &&
        not (Sil.const_equal (Sil.Cint Sil.Int.zero) c2)
    | Sil.BinOp(op1, e1, Sil.Const(c1)), e2
      when Sil.exp_equal e1 e2 ->
        Sil.binop_injective op1 &&
        Sil.binop_is_zero_runit op1 &&
        not (Sil.const_equal (Sil.Cint Sil.Int.zero) c1)
    | _ -> false in
  let filter_useful_atom =
    let unsigned_exps = lazy (sigma_get_unsigned_exps sigma) in
    function
    | Sil.Aneq ((Sil.Var _) as e, Sil.Const (Sil.Cint n)) when Sil.Int.isnegative n ->
        not (IList.exists (Sil.exp_equal e) (Lazy.force unsigned_exps))
    | Sil.Aneq(e1, e2) ->
        not (syntactically_different (e1, e2))
    | Sil.Aeq(Sil.Const c1, Sil.Const c2) ->
        not (Sil.const_equal c1 c2)
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
  let nsigma = sigma_normalize Sil.sub_empty prop.foot_sigma in
  let npi = pi_normalize Sil.sub_empty nsigma prop.foot_pi in
  let fp_vars =
    let fav = pi_fav npi in
    sigma_fav_add fav nsigma;
    fav in
  (* TODO (t4893479): make this check less angelic *)
  if Sil.fav_exists fp_vars Ident.is_normal && not !Config.angelic_execution then
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
        Sil.sub_of_list (IList.map (fun (id1, id2) -> (id1, Sil.Var id2)) ids_footprint) in
      let nsigma' = sigma_normalize Sil.sub_empty (sigma_sub ren_sub nsigma) in
      let npi' = pi_normalize Sil.sub_empty nsigma' (pi_sub ren_sub npi) in
      (npi', nsigma') in
  { prop with foot_pi = npi'; foot_sigma = nsigma' }

let exp_normalize_prop prop exp =
  run_with_abs_val_eq_zero
    (fun () -> exp_normalize prop.sub exp)

let lexp_normalize_prop p lexp =
  let root = Sil.root_of_lexp lexp in
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
    | Sil.Tint _ | Sil.Tfloat _ | Sil.Tstruct _ | Sil.Tvoid | Sil.Tfun _ -> true
    | _ -> false in
  let typ_is_one_step_from_base =
    match typ with
    | Sil.Tptr (t, _) | Sil.Tarray (t, _) -> typ_is_base t
    | _ -> false in
  let rec exp_remove e0 =
    match e0 with
    | Sil.Lindex(Sil.Lindex(base, e1), e2) ->
        let e0' = Sil.Lindex(base, Sil.BinOp(Sil.PlusA, e1, e2)) in
        exp_remove e0'
    | _ -> e0 in
  begin
    if typ_is_one_step_from_base then exp_remove exp else exp
  end

let atom_normalize_prop prop atom =
  run_with_abs_val_eq_zero
    (fun () -> atom_normalize prop.sub atom)

let strexp_normalize_prop prop strexp =
  run_with_abs_val_eq_zero
    (fun () -> strexp_normalize prop.sub strexp)

let hpred_normalize_prop prop hpred =
  run_with_abs_val_eq_zero
    (fun () -> hpred_normalize prop.sub hpred)

let sigma_normalize_prop prop sigma =
  run_with_abs_val_eq_zero
    (fun () -> sigma_normalize prop.sub sigma)

let pi_normalize_prop prop pi =
  run_with_abs_val_eq_zero
    (fun () -> pi_normalize prop.sub prop.sigma pi)

(** {2 Compaction} *)
(** Return a compact representation of the prop *)
let prop_compact sh prop =
  let sigma' = IList.map (Sil.hpred_compact sh) prop.sigma in
  { prop with sigma = sigma'}

(** {2 Function for replacing occurrences of expressions.} *)

let replace_pi pi eprop =
  { eprop with pi = pi }

let replace_sigma sigma eprop =
  { eprop with sigma = sigma }

let replace_sigma_footprint sigma (prop : 'a t) : exposed t =
  { prop with foot_sigma = sigma }

let replace_pi_footprint pi (prop : 'a t) : exposed t =
  { prop with foot_pi = pi }

let sigma_replace_exp epairs sigma =
  let sigma' = IList.map (Sil.hpred_replace_exp epairs) sigma in
  sigma_normalize Sil.sub_empty sigma'

let sigma_map prop f =
  let sigma' = IList.map f prop.sigma in
  { prop with sigma = sigma' }

(** {2 Query about Proposition} *)

(** Check if the sigma part of the proposition is emp *)
let prop_is_emp p = match p.sigma with
  | [] -> true
  | _ -> false

(** {2 Functions for changing and generating propositions} *)

(** Sil.Construct a disequality. *)
let mk_neq e1 e2 =
  run_with_abs_val_eq_zero
    (fun () ->
       let ne1 = exp_normalize Sil.sub_empty e1 in
       let ne2 = exp_normalize Sil.sub_empty e2 in
       atom_normalize Sil.sub_empty (Sil.Aneq (ne1, ne2)))

(** Sil.Construct an equality. *)
let mk_eq e1 e2 =
  run_with_abs_val_eq_zero
    (fun () ->
       let ne1 = exp_normalize Sil.sub_empty e1 in
       let ne2 = exp_normalize Sil.sub_empty e2 in
       atom_normalize Sil.sub_empty (Sil.Aeq (ne1, ne2)))

(** Construct a points-to predicate for a single program variable.
    If [expand_structs] is true, initialize the fields of structs with fresh variables. *)
let mk_ptsto_lvar tenv expand_structs inst ((pvar: Pvar.t), texp, expo) : Sil.hpred =
  mk_ptsto_exp tenv expand_structs (Sil.Lvar pvar, texp, expo) inst

(** Sil.Construct a lseg predicate *)
let mk_lseg k para e_start e_end es_shared =
  let npara = hpara_normalize para in
  Sil.Hlseg (k, npara, e_start, e_end, es_shared)

(** Sil.Construct a dllseg predicate *)
let mk_dllseg k para exp_iF exp_oB exp_oF exp_iB exps_shared =
  let npara = hpara_dll_normalize para in
  Sil.Hdllseg (k, npara, exp_iF, exp_oB , exp_oF, exp_iB, exps_shared)

(** Sil.Construct a hpara *)
let mk_hpara root next svars evars body =
  let para =
    { Sil.root = root;
      next = next;
      svars = svars;
      evars = evars;
      body = body } in
  hpara_normalize para

(** Sil.Construct a dll_hpara *)
let mk_dll_hpara iF oB oF svars evars body =
  let para =
    { Sil.cell = iF;
      blink = oB;
      flink = oF;
      svars_dll = svars;
      evars_dll = evars;
      body_dll = body } in
  hpara_dll_normalize para

(** Proposition [true /\ emp]. *)
let prop_emp : normal t =
  {
    sub = Sil.sub_empty;
    pi = [];
    sigma = [];
    foot_pi = [];
    foot_sigma = [];
  }

(** Conjoin a heap predicate by separating conjunction. *)
let prop_hpred_star (p : 'a t) (h : Sil.hpred) : exposed t =
  let sigma' = h:: p.sigma in
  { p with sigma = sigma'}

let prop_sigma_star (p : 'a t) (sigma : sigma) : exposed t =
  let sigma' = sigma @ p.sigma in
  { p with sigma = sigma' }

(** return the set of subexpressions of [strexp] *)
let strexp_get_exps strexp =
  let rec strexp_get_exps_rec exps = function
    | Sil.Eexp (Sil.Const (Sil.Cexn e), _) -> Sil.ExpSet.add e exps
    | Sil.Eexp (e, _) -> Sil.ExpSet.add e exps
    | Sil.Estruct (flds, _) ->
        IList.fold_left (fun exps (_, strexp) -> strexp_get_exps_rec exps strexp) exps flds
    | Sil.Earray (_, elems, _) ->
        IList.fold_left (fun exps (_, strexp) -> strexp_get_exps_rec exps strexp) exps elems in
  strexp_get_exps_rec Sil.ExpSet.empty strexp

(** get the set of expressions on the righthand side of [hpred] *)
let hpred_get_targets = function
  | Sil.Hpointsto (_, rhs, _) -> strexp_get_exps rhs
  | Sil.Hlseg (_, _, _, e, el) ->
      IList.fold_left (fun exps e -> Sil.ExpSet.add e exps) Sil.ExpSet.empty (e :: el)
  | Sil.Hdllseg (_, _, _, oB, oF, iB, el) ->
      (* only one direction supported for now *)
      IList.fold_left (fun exps e -> Sil.ExpSet.add e exps) Sil.ExpSet.empty (oB :: oF :: iB :: el)

(** return the set of hpred's and exp's in [sigma] that are reachable from an expression in
    [exps] *)
let compute_reachable_hpreds sigma exps =
  let rec compute_reachable_hpreds_rec sigma (reach, exps) =
    let add_hpred_if_reachable (reach, exps) = function
      | Sil.Hpointsto (lhs, _, _) as hpred when Sil.ExpSet.mem lhs exps->
          let reach' = Sil.HpredSet.add hpred reach in
          let reach_exps = hpred_get_targets hpred in
          (reach', Sil.ExpSet.union exps reach_exps)
      | _ -> reach, exps in
    let reach', exps' = IList.fold_left add_hpred_if_reachable (reach, exps) sigma in
    if (Sil.HpredSet.cardinal reach) = (Sil.HpredSet.cardinal reach') then (reach, exps)
    else compute_reachable_hpreds_rec sigma (reach', exps') in
  compute_reachable_hpreds_rec sigma (Sil.HpredSet.empty, exps)

(** if possible, produce a (fieldname, typ) path from one of the [src_exps] to [snk_exp] using
    [reachable_hpreds]. *)
let get_fld_typ_path_opt src_exps snk_exp_ reachable_hpreds_ =
  let strexp_matches target_exp = function
    | (_, Sil.Eexp (e, _)) -> Sil.exp_equal target_exp e
    | _ -> false in
  let extend_path hpred (snk_exp, path, reachable_hpreds) = match hpred with
    | Sil.Hpointsto (lhs, Sil.Estruct (flds, _), Sil.Sizeof (typ, _)) ->
        (try
           let fld, _ = IList.find (fun fld -> strexp_matches snk_exp fld) flds in
           let reachable_hpreds' = Sil.HpredSet.remove hpred reachable_hpreds in
           (lhs, (Some fld, typ) :: path, reachable_hpreds')
         with Not_found -> (snk_exp, path, reachable_hpreds))
    | Sil.Hpointsto (lhs, Sil.Earray (_, elems, _), Sil.Sizeof (typ, _)) ->
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
    if Sil.ExpSet.mem snk_exp' src_exps
    then Some path'
    else
    if Sil.HpredSet.cardinal reachable_hpreds' >= Sil.HpredSet.cardinal reachable_hpreds
    then None (* can't find a path from [src_exps] to [snk_exp] *)
    else get_fld_typ_path snk_exp' path' reachable_hpreds' in
  get_fld_typ_path snk_exp_ [] reachable_hpreds_

(** filter [pi] by removing the pure atoms that do not contain an expression in [exps] *)
let compute_reachable_atoms pi exps =
  let rec exp_contains = function
    | exp when Sil.ExpSet.mem exp exps -> true
    | Sil.UnOp (_, e, _) | Sil.Cast (_, e) | Sil.Lfield (e, _, _) -> exp_contains e
    | Sil.BinOp (_, e0, e1) | Sil.Lindex (e0, e1) -> exp_contains e0 || exp_contains e1
    | _ -> false in
  IList.filter
    (function
      | Sil.Aeq (lhs, rhs) | Sil.Aneq (lhs, rhs) -> exp_contains lhs || exp_contains rhs)
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
          f_alloc (Sil.ExpSet.add e set) sigma'
      | Sil.Hdllseg (Sil.Lseg_NE, _, iF, _, _, iB, _) :: sigma' ->
          f_alloc (Sil.ExpSet.add iF (Sil.ExpSet.add iB set)) sigma'
      | _ :: sigma' ->
          f_alloc set sigma' in
    f_alloc Sil.ExpSet.empty sigma
  in
  let rec f eqs_zero sigma_passed = function
    | [] ->
        (IList.rev eqs_zero, IList.rev sigma_passed)
    | Sil.Hpointsto _ as hpred :: sigma' ->
        f eqs_zero (hpred :: sigma_passed) sigma'
    | Sil.Hlseg (Sil.Lseg_PE, _, e1, e2, _) :: sigma'
      when (Sil.exp_equal e1 Sil.exp_zero) || (Sil.ExpSet.mem e1 alloc_set) ->
        f (Sil.Aeq(e1, e2) :: eqs_zero) sigma_passed sigma'
    | Sil.Hlseg _ as hpred :: sigma' ->
        f eqs_zero (hpred :: sigma_passed) sigma'
    | Sil.Hdllseg (Sil.Lseg_PE, _, iF, oB, oF, iB, _) :: sigma'
      when (Sil.exp_equal iF Sil.exp_zero) || (Sil.ExpSet.mem iF alloc_set)
           || (Sil.exp_equal iB Sil.exp_zero) || (Sil.ExpSet.mem iB alloc_set) ->
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
      when (Sil.exp_equal e1 f1 && Sil.exp_equal e2 f2)
        || (Sil.exp_equal e2 f1 && Sil.exp_equal e1 f2) ->
        f (Sil.Hlseg (Sil.Lseg_NE, para, f1, f2, shared) :: sigma_passed) sigma'
    | Sil.Hlseg _ as hpred :: sigma' ->
        f (hpred :: sigma_passed) sigma'
    | Sil.Hdllseg (Sil.Lseg_PE, para, iF, oB, oF, iB, shared) :: sigma'
      when (Sil.exp_equal e1 iF && Sil.exp_equal e2 oF)
        || (Sil.exp_equal e2 iF && Sil.exp_equal e1 oF)
        || (Sil.exp_equal e1 iB && Sil.exp_equal e2 oB)
        || (Sil.exp_equal e2 iB && Sil.exp_equal e1 oB) ->
        f (Sil.Hdllseg (Sil.Lseg_NE, para, iF, oB, oF, iB, shared) :: sigma_passed) sigma'
    | Sil.Hdllseg _ as hpred :: sigma' ->
        f (hpred :: sigma_passed) sigma'
  in
  f [] sigma

let normalize_and_strengthen_atom (p : normal t) (a : Sil.atom) : Sil.atom =
  let a' = atom_normalize p.sub a in
  match a' with
  | Sil.Aeq (Sil.BinOp (Sil.Le, Sil.Var id, Sil.Const (Sil.Cint n)), Sil.Const (Sil.Cint i))
    when Sil.Int.isone i ->
      let lower = Sil.exp_int (n -- Sil.Int.one) in
      let a_lower = Sil.Aeq (Sil.BinOp (Sil.Lt, lower, Sil.Var id), Sil.exp_one) in
      if not (IList.mem Sil.atom_equal a_lower p.pi) then a'
      else Sil.Aeq (Sil.Var id, Sil.exp_int n)
  | Sil.Aeq (Sil.BinOp (Sil.Lt, Sil.Const (Sil.Cint n), Sil.Var id), Sil.Const (Sil.Cint i))
    when Sil.Int.isone i ->
      let upper = Sil.exp_int (n ++ Sil.Int.one) in
      let a_upper = Sil.Aeq (Sil.BinOp (Sil.Le, Sil.Var id, upper), Sil.exp_one) in
      if not (IList.mem Sil.atom_equal a_upper p.pi) then a'
      else Sil.Aeq (Sil.Var id, upper)
  | Sil.Aeq (Sil.BinOp (Sil.Ne, e1, e2), Sil.Const (Sil.Cint i)) when Sil.Int.isone i ->
      Sil.Aneq (e1, e2)
  | _ -> a'

(** Conjoin a pure atomic predicate by normal conjunction. *)
let rec prop_atom_and ?(footprint=false) (p : normal t) a : normal t =
  let a' = normalize_and_strengthen_atom p a in
  if IList.mem Sil.atom_equal a' p.pi then p
  else begin
    let p' =
      match a' with
      | Sil.Aeq (Sil.Var i, e) when Sil.ident_in_exp i e -> p
      | Sil.Aeq (Sil.Var i, e) ->
          let sub_list = [(i, e)] in
          let mysub = Sil.sub_of_list sub_list in
          let p_sub = Sil.sub_filter (fun i' -> not (Ident.equal i i')) p.sub in
          let sub' = Sil.sub_join mysub (Sil.sub_range_map (Sil.exp_sub mysub) p_sub) in
          let (nsub', npi', nsigma') =
            let nsigma' = sigma_normalize sub' p.sigma in
            (sub_normalize sub', pi_normalize sub' nsigma' p.pi, nsigma') in
          let (eqs_zero, nsigma'') = sigma_remove_emptylseg nsigma' in
          let p' = { p with sub = nsub'; pi = npi'; sigma = nsigma''} in
          IList.fold_left (prop_atom_and ~footprint) p' eqs_zero
      | Sil.Aeq (e1, e2) when (Sil.exp_compare e1 e2 = 0) ->
          p
      | Sil.Aneq (e1, e2) ->
          let sigma' = sigma_intro_nonemptylseg e1 e2 p.sigma in
          let pi' = pi_normalize p.sub sigma' (a':: p.pi) in
          { p with pi = pi'; sigma = sigma'}
      | _ ->
          let pi' = pi_normalize p.sub p.sigma (a':: p.pi) in
          { p with pi = pi'} in
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
          | Sil.Aeq (Sil.Var i, e) when not (Sil.ident_in_exp i e) ->
              let mysub = Sil.sub_of_list [(i, e)] in
              let foot_sigma' = sigma_normalize mysub p'.foot_sigma in
              let foot_pi' = a' :: pi_normalize mysub foot_sigma' p'.foot_pi in
              footprint_normalize { p' with foot_pi = foot_pi'; foot_sigma = foot_sigma' }
          | _ ->
              footprint_normalize { p' with foot_pi = a' :: p'.foot_pi } in
      if predicate_warning then (L.d_warning "dropping non-footprint "; Sil.d_atom a'; L.d_ln ());
      p''
    end
  end

(** Conjoin [exp1]=[exp2] with a symbolic heap [prop]. *)
let conjoin_eq ?(footprint = false) exp1 exp2 prop =
  prop_atom_and ~footprint prop (Sil.Aeq(exp1, exp2))

(** Conjoin [exp1!=exp2] with a symbolic heap [prop]. *)
let conjoin_neq ?(footprint = false) exp1 exp2 prop =
  prop_atom_and ~footprint prop (Sil.Aneq (exp1, exp2))

(** Return the spatial part *)
let get_sigma (p: 'a t) : sigma = p.sigma

(** Return the pure part of the footprint *)
let get_pi_footprint p =
  p.foot_pi

(** Return the spatial part of the footprint *)
let get_sigma_footprint p =
  p.foot_sigma

(** Reset every inst in the prop using the given map *)
let prop_reset_inst inst_map prop =
  let sigma' = IList.map (Sil.hpred_instmap inst_map) (get_sigma prop) in
  let sigma_fp' = IList.map (Sil.hpred_instmap inst_map) (get_sigma_footprint prop) in
  replace_sigma_footprint sigma_fp' (replace_sigma sigma' prop)

(** {2 Attributes} *)

(** Return the exp and attribute marked in the atom if any, and return None otherwise *)
let atom_get_exp_attribute = function
  | Sil.Aneq (Sil.Const (Sil.Cattribute att), e)
  | Sil.Aneq (e, Sil.Const (Sil.Cattribute att)) -> Some (e, att)
  | _ -> None

(** Check whether an atom is used to mark an attribute *)
let atom_is_attribute a =
  atom_get_exp_attribute a <> None

(** Get the attribute associated to the expression, if any *)
let get_exp_attributes prop exp =
  let nexp = exp_normalize_prop prop exp in
  let atom_get_attr attributes atom =
    match atom with
    | Sil.Aneq (e, Sil.Const (Sil.Cattribute att))
    | Sil.Aneq (Sil.Const (Sil.Cattribute att), e) when Sil.exp_equal e nexp -> att:: attributes
    | _ -> attributes in
  IList.fold_left atom_get_attr [] prop.pi

let attributes_in_same_category attr1 attr2 =
  let cat1 = Sil.attribute_to_category attr1 in
  let cat2 = Sil.attribute_to_category attr2 in
  Sil.attribute_category_equal cat1 cat2

let get_attribute prop exp category =
  let atts = get_exp_attributes prop exp in
  try Some (IList.find
              (fun att ->
                 Sil.attribute_category_equal
                   (Sil.attribute_to_category att) category)
              atts)
  with Not_found -> None

let get_undef_attribute prop exp =
  get_attribute prop exp Sil.ACundef

let get_resource_attribute prop exp =
  get_attribute prop exp Sil.ACresource

let get_taint_attribute prop exp =
  get_attribute prop exp Sil.ACtaint

let get_autorelease_attribute prop exp =
  get_attribute prop exp Sil.ACautorelease

let get_objc_null_attribute prop exp =
  get_attribute prop exp Sil.ACobjc_null

let get_div0_attribute prop exp =
  get_attribute prop exp Sil.ACdiv0

let get_observer_attribute prop exp =
  get_attribute prop exp Sil.ACobserver

let has_dangling_uninit_attribute prop exp =
  let la = get_exp_attributes prop exp in
  IList.exists (fun a -> Sil.attribute_equal a (Sil.Adangling (Sil.DAuninit))) la

(** Get all the attributes of the prop *)
let get_all_attributes prop =
  let res = ref [] in
  let do_atom a = match atom_get_exp_attribute a with
    | Some (e, att) -> res := (e, att) :: !res
    | None -> () in
  IList.iter do_atom prop.pi;
  IList.rev !res

(** Set an attribute associated to the expression *)
let set_exp_attribute prop exp att =
  let exp_att = Sil.Const (Sil.Cattribute att) in
  conjoin_neq exp exp_att prop

(** Replace an attribute associated to the expression *)
let add_or_replace_exp_attribute_check_changed check_attribute_change prop exp att =
  let nexp = exp_normalize_prop prop exp in
  let found = ref false in
  let atom_map a = match a with
    | Sil.Aneq (e, Sil.Const (Sil.Cattribute att_old))
    | Sil.Aneq (Sil.Const (Sil.Cattribute att_old), e) ->
        if Sil.exp_equal nexp e && (attributes_in_same_category att_old att) then
          begin
            found := true;
            check_attribute_change att_old att;
            let e1, e2 = exp_reorder e (Sil.Const (Sil.Cattribute att)) in
            Sil.Aneq (e1, e2)
          end
        else a
    | _ -> a in
  let pi' = IList.map atom_map (get_pi prop) in
  if !found then replace_pi pi' prop
  else set_exp_attribute prop nexp att

let add_or_replace_exp_attribute prop exp att =
  (* wrapper for the most common case: do nothing *)
  let check_attr_changed = (fun _ _ -> ()) in
  add_or_replace_exp_attribute_check_changed check_attr_changed prop exp att

(** mark Sil.Var's or Sil.Lvar's as undefined *)
let mark_vars_as_undefined prop vars_to_mark callee_pname loc path_pos =
  let att_undef = Sil.Aundef (callee_pname, loc, path_pos) in
  let mark_var_as_undefined exp prop =
    match exp with
    | Sil.Var _ | Sil.Lvar _ -> add_or_replace_exp_attribute prop exp att_undef
    | _ -> prop in
  IList.fold_left (fun prop id -> mark_var_as_undefined id prop) prop vars_to_mark

(** Remove an attribute from all the atoms in the heap *)
let remove_attribute att prop =
  let atom_remove atom pi = match atom with
    | Sil.Aneq (_, Sil.Const (Sil.Cattribute att_old))
    | Sil.Aneq (Sil.Const (Sil.Cattribute att_old), _) ->
        if Sil.attribute_equal att_old att then
          pi
        else atom:: pi
    | _ -> atom:: pi in
  let pi' = IList.fold_right atom_remove (get_pi prop) [] in
  replace_pi pi' prop

let remove_attribute_from_exp att prop exp =
  let nexp = exp_normalize_prop prop exp in
  let atom_remove atom pi = match atom with
    | Sil.Aneq (e, Sil.Const (Sil.Cattribute att_old))
    | Sil.Aneq (Sil.Const (Sil.Cattribute att_old), e) ->
        if Sil.attribute_equal att_old att && Sil.exp_equal nexp e then
          pi
        else atom:: pi
    | _ -> atom:: pi in
  let pi' = IList.fold_right atom_remove (get_pi prop) [] in
  replace_pi pi' prop

(* Replace an attribute OBJC_NULL($n1) with OBJC_NULL(var) when var = $n1, and also sets $n1 = 0 *)
let replace_objc_null prop lhs_exp rhs_exp =
  match get_objc_null_attribute prop rhs_exp, rhs_exp with
  | Some att, Sil.Var _ ->
      let prop = remove_attribute_from_exp att prop rhs_exp in
      let prop = conjoin_eq rhs_exp Sil.exp_zero prop in
      add_or_replace_exp_attribute prop lhs_exp att
  | _ -> prop

let rec nullify_exp_with_objc_null prop exp =
  match exp with
  | Sil.BinOp (_, exp1, exp2) ->
      let prop' = nullify_exp_with_objc_null prop exp1 in
      nullify_exp_with_objc_null prop' exp2
  | Sil.UnOp (_, exp, _) ->
      nullify_exp_with_objc_null prop exp
  | Sil.Var _ ->
      (match get_objc_null_attribute prop exp with
       | Some att ->
           let prop' = remove_attribute_from_exp att prop exp in
           conjoin_eq exp Sil.exp_zero prop'
       | _ -> prop)
  | _ -> prop

(** Get all the attributes of the prop *)
let get_atoms_with_attribute att prop =
  let atom_remove atom autoreleased_atoms = match atom with
    | Sil.Aneq (e, Sil.Const (Sil.Cattribute att_old))
    | Sil.Aneq (Sil.Const (Sil.Cattribute att_old), e) ->
        if Sil.attribute_equal att_old att then
          e:: autoreleased_atoms
        else autoreleased_atoms
    | _ -> autoreleased_atoms in
  IList.fold_right atom_remove (get_pi prop) []

(** Apply f to every resource attribute in the prop *)
let attribute_map_resource prop f =
  let pi = get_pi prop in
  let attribute_map e = function
    | Sil.Aresource ra ->
        Sil.Aresource (f e ra)
    | att -> att in
  let atom_map a = match a with
    | Sil.Aneq (e, Sil.Const (Sil.Cattribute att))
    | Sil.Aneq (Sil.Const (Sil.Cattribute att), e) ->
        let att' = attribute_map e att in
        let e1, e2 = exp_reorder e (Sil.Const (Sil.Cattribute att')) in
        Sil.Aneq (e1, e2)
    | _ -> a in
  let pi' = IList.map atom_map pi in
  replace_pi pi' prop

(** type for arithmetic problems *)
type arith_problem =
  (* division by zero *)
  | Div0 of Sil.exp

  (* unary minus of unsigned type applied to the given expression *)
  | UminusUnsigned of Sil.exp * Sil.typ

(** Look for an arithmetic problem in [exp] *)
let find_arithmetic_problem proc_node_session prop exp =
  let exps_divided = ref [] in
  let uminus_unsigned = ref [] in
  let res = ref prop in
  let check_zero e =
    match exp_normalize_prop prop e with
    | Sil.Const c when iszero_int_float c -> true
    | _ ->
        res := add_or_replace_exp_attribute !res e (Sil.Adiv0 proc_node_session);
        false in
  let rec walk = function
    | Sil.Var _ -> ()
    | Sil.UnOp (Sil.Neg, e, Some (
        (Sil.Tint
           (Sil.IUChar | Sil.IUInt | Sil.IUShort | Sil.IULong | Sil.IULongLong) as typ))) ->
        uminus_unsigned := (e, typ) :: !uminus_unsigned
    | Sil.UnOp(_, e, _) -> walk e
    | Sil.BinOp(op, e1, e2) ->
        if op = Sil.Div || op = Sil.Mod then exps_divided := e2 :: !exps_divided;
        walk e1; walk e2
    | Sil.Const _ -> ()
    | Sil.Cast (_, e) -> walk e
    | Sil.Lvar _ -> ()
    | Sil.Lfield (e, _, _) -> walk e
    | Sil.Lindex (e1, e2) -> walk e1; walk e2
    | Sil.Sizeof _ -> () in
  walk exp;
  try Some (Div0 (IList.find check_zero !exps_divided)), !res
  with Not_found ->
    (match !uminus_unsigned with
     | (e, t):: _ -> Some (UminusUnsigned (e, t)), !res
     | _ -> None, !res)

(** Deallocate the stack variables in [pvars], and replace them by normal variables.
    Return the list of stack variables whose address was still present after deallocation. *)
let deallocate_stack_vars p pvars =
  let filter = function
    | Sil.Hpointsto (Sil.Lvar v, _, _) ->
        IList.exists (Pvar.equal v) pvars
    | _ -> false in
  let sigma_stack, sigma_other = IList.partition filter p.sigma in
  let fresh_address_vars = ref [] in (* fresh vars substituted for the address of stack vars *)
  let stack_vars_address_in_post = ref [] in (* stack vars whose address is still present *)
  let exp_replace = IList.map (function
      | Sil.Hpointsto (Sil.Lvar v, _, _) ->
          let freshv = Ident.create_fresh Ident.kprimed in
          fresh_address_vars := (v, freshv) :: !fresh_address_vars;
          (Sil.Lvar v, Sil.Var freshv)
      | _ -> assert false) sigma_stack in
  let pi1 = IList.map (fun (id, e) -> Sil.Aeq (Sil.Var id, e)) (Sil.sub_to_list p.sub) in
  let pi = IList.map (Sil.atom_replace_exp exp_replace) (p.pi @ pi1) in
  let p' =
    { p with
      sub = Sil.sub_empty;
      pi = [];
      sigma = sigma_replace_exp exp_replace sigma_other } in
  let p'' =
    let res = ref p' in
    let p'_fav = prop_fav p' in
    let do_var (v, freshv) =
      if Sil.fav_mem p'_fav freshv then (* the address of a de-allocated stack var in in the post *)
        begin
          stack_vars_address_in_post := v :: !stack_vars_address_in_post;
          res :=
            add_or_replace_exp_attribute !res (Sil.Var freshv) (Sil.Adangling Sil.DAaddr_stack_var)
        end in
    IList.iter do_var !fresh_address_vars;
    !res in
  !stack_vars_address_in_post, IList.fold_left prop_atom_and p'' pi


(** {1 Functions for transforming footprints into propositions.} *)

(** The ones used for abstraction add/remove local stacks in order to
    stop the firing of some abstraction rules. The other usual
    transforation functions do not use this hack. *)

(** Extract the footprint and return it as a prop *)
let extract_footprint p =
  { prop_emp with pi = p.foot_pi; sigma = p.foot_sigma }

(** Extract the (footprint,current) pair *)
let extract_spec p =
  let pre = extract_footprint p in
  let post = { p with foot_pi = []; foot_sigma = [] } in
  (pre, post)

(** [prop_set_fooprint p p_foot] sets proposition [p_foot] as footprint of [p]. *)
let prop_set_footprint p p_foot =
  let pi =
    (IList.map
       (fun (i, e) -> Sil.Aeq(Sil.Var i, e))
       (Sil.sub_to_list p_foot.sub)) @ p_foot.pi in
  { p with foot_pi = pi; foot_sigma = p_foot.sigma }

(** {2 Functions for renaming primed variables by "canonical names"} *)

module ExpStack : sig
  val init : Sil.exp list -> unit
  val final : unit -> unit
  val is_empty : unit -> bool
  val push : Sil.exp -> unit
  val pop : unit -> Sil.exp
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
  let exp_compare_neg e1 e2 = - (Sil.exp_compare e1 e2) in
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
          | Sil.Hpointsto (e', se, _) when Sil.exp_equal e e' ->
              handle_strexp se;
              (hpred:: visited, IList.rev_append cur seen)
          | Sil.Hlseg (_, _, root, next, shared) when Sil.exp_equal e root ->
              IList.iter ExpStack.push (next:: shared);
              (hpred:: visited, IList.rev_append cur seen)
          | Sil.Hdllseg (_, _, iF, oB, oF, iB, shared)
            when Sil.exp_equal e iF || Sil.exp_equal e iB ->
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
  let sigma = get_sigma p in
  let sigma' = sigma_dfs_sort sigma in
  let sigma_fp = get_sigma_footprint p in
  let sigma_fp' = sigma_dfs_sort sigma_fp in
  let p' = { p with sigma = sigma'; foot_sigma = sigma_fp'} in
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
    let base_new = Sil.Var (Ident.create_fresh Ident.kprimed) in
    let offset_new = Sil.exp_int (Sil.Int.neg offset) in
    let exp_new = Sil.BinOp(Sil.PlusA, base_new, offset_new) in
    (id, exp_new) in
  let reindexing = IList.map transform list_passed in
  Sil.sub_of_list reindexing

let compute_reindexing_from_indices indices =
  let get_id_offset = function
    | Sil.BinOp (Sil.PlusA, Sil.Var id, Sil.Const(Sil.Cint offset)) ->
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
    let atoms = IList.map (fun (id, e) -> Sil.Aeq (Sil.Var id, exp_normalize subst e)) eqs in
    (sub_keep, atoms) in
  let p' = { prop with sub = nsub; pi = npi; sigma = nsigma } in
  IList.fold_left prop_atom_and p' atoms

let prop_rename_array_indices prop =
  if !Config.footprint then prop
  else begin
    let indices = sigma_get_array_indices prop.sigma in
    let not_same_base_lt_offsets e1 e2 =
      match e1, e2 with
      | Sil.BinOp(Sil.PlusA, e1', Sil.Const (Sil.Cint n1')),
        Sil.BinOp(Sil.PlusA, e2', Sil.Const (Sil.Cint n2')) ->
          not (Sil.exp_equal e1' e2' && Sil.Int.lt n1' n2')
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
  | Sil.Var id -> Sil.Var (ident_captured_ren ren id)
  | Sil.Const (Sil.Cexn e) -> Sil.Const (Sil.Cexn (exp_captured_ren ren e))
  | Sil.Const _ as e -> e
  | Sil.Sizeof (t, st) -> Sil.Sizeof (typ_captured_ren ren t, st)
  | Sil.Cast (t, e) -> Sil.Cast (t, exp_captured_ren ren e)
  | Sil.UnOp (op, e, topt) ->
      let topt' = match topt with
        | Some t -> Some (typ_captured_ren ren t)
        | None -> None in
      Sil.UnOp (op, exp_captured_ren ren e, topt')
  | Sil.BinOp (op, e1, e2) ->
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      Sil.BinOp (op, e1', e2')
  | Sil.Lvar id -> Sil.Lvar id
  | Sil.Lfield (e, fld, typ) -> Sil.Lfield (exp_captured_ren ren e, fld, typ_captured_ren ren typ)
  | Sil.Lindex (e1, e2) ->
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      Sil.Lindex(e1', e2')

(* Apply a renaming function to a type *)
and typ_captured_ren ren typ = match typ with
  | Sil.Tvar _
  | Sil.Tint _
  | Sil.Tfloat _
  | Sil.Tvoid
  | Sil.Tstruct _
  | Sil.Tfun _ ->
      typ
  | Sil.Tptr (t', pk) ->
      Sil.Tptr (typ_captured_ren ren t', pk)
  | Sil.Tarray (t, e) ->
      Sil.Tarray (typ_captured_ren ren t, exp_captured_ren ren e)

let atom_captured_ren ren = function
  | Sil.Aeq (e1, e2) ->
      Sil.Aeq (exp_captured_ren ren e1, exp_captured_ren ren e2)
  | Sil.Aneq (e1, e2) ->
      Sil.Aneq (exp_captured_ren ren e1, exp_captured_ren ren e2)

let rec strexp_captured_ren ren = function
  | Sil.Eexp (e, inst) ->
      Sil.Eexp (exp_captured_ren ren e, inst)
  | Sil.Estruct (fld_se_list, inst) ->
      let f (fld, se) = (fld, strexp_captured_ren ren se) in
      Sil.Estruct (IList.map f fld_se_list, inst)
  | Sil.Earray (size, idx_se_list, inst) ->
      let f (idx, se) =
        let idx' = exp_captured_ren ren idx in
        (idx', strexp_captured_ren ren se) in
      let size' = exp_captured_ren ren size in
      Sil.Earray (size', IList.map f idx_se_list, inst)

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
let prop_rename_primed_footprint_vars p =
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
  let foot_pi' = pi_captured_ren ren p.foot_pi in
  let foot_sigma' = sigma_captured_ren ren p.foot_sigma in

  let sub_for_normalize = Sil.sub_empty in
  (* It is fine to use the empty substituion during normalization
     because the renaming maintains that a substitution is normalized *)
  let nsub' = sub_normalize sub' in
  let nsigma' = sigma_normalize sub_for_normalize sigma' in
  let npi' = pi_normalize sub_for_normalize nsigma' pi' in
  let p' = footprint_normalize {
      sub = nsub';
      pi = npi';
      sigma = nsigma';
      foot_pi = foot_pi';
      foot_sigma = foot_sigma';
    } in
  p'

(** {2 Functions for changing and generating propositions} *)

let mem_idlist i l =
  IList.exists (fun id -> Ident.equal i id) l

let expose (p : normal t) : exposed t = Obj.magic p

(** normalize a prop *)
let normalize (eprop : 'a t) : normal t =
  let p0 = { prop_emp with sigma = sigma_normalize Sil.sub_empty eprop.sigma } in
  let nprop = IList.fold_left prop_atom_and p0 (get_pure eprop) in
  footprint_normalize { nprop with foot_pi = eprop.foot_pi; foot_sigma = eprop.foot_sigma }

(** Apply subsitution to prop. *)
let prop_sub subst (prop: 'a t) : exposed t =
  let pi = pi_sub subst (prop.pi @ pi_of_subst prop.sub) in
  let sigma = sigma_sub subst prop.sigma in
  let foot_pi = pi_sub subst prop.foot_pi in
  let foot_sigma = sigma_sub subst prop.foot_sigma in
  { prop_emp with pi; sigma; foot_pi; foot_sigma; }

(** Apply renaming substitution to a proposition. *)
let prop_ren_sub (ren_sub: Sil.subst) (prop: normal t) : normal t =
  normalize (prop_sub ren_sub prop)

(** Existentially quantify the [fav] in [prop].
    [fav] should not contain any primed variables. *)
let exist_quantify fav prop =
  let ids = Sil.fav_to_list fav in
  if IList.exists Ident.is_primed ids then assert false; (* sanity check *)
  if ids == [] then prop else
    let gen_fresh_id_sub id = (id, Sil.Var (Ident.create_fresh Ident.kprimed)) in
    let ren_sub = Sil.sub_of_list (IList.map gen_fresh_id_sub ids) in
    let prop' =
      (* throw away x=E if x becomes _x *)
      let sub = Sil.sub_filter (fun i -> not (mem_idlist i ids)) prop.sub in
      if Sil.sub_equal sub prop.sub then prop
      else { prop with sub = sub } in
    (*
    L.out "@[<2>.... Existential Quantification ....\n";
    L.out "SUB:%a\n" pp_sub prop'.sub;
    L.out "PI:%a\n" pp_pi prop'.pi;
    L.out "PROP:%a\n@." pp_prop prop';
    *)
    prop_ren_sub ren_sub prop'

(** Apply the substitution [fe] to all the expressions in the prop. *)
let prop_expmap (fe: Sil.exp -> Sil.exp) prop =
  let f (e, sil_opt) = (fe e, sil_opt) in
  let pi = IList.map (Sil.atom_expmap fe) prop.pi in
  let sigma = IList.map (Sil.hpred_expmap f) prop.sigma in
  let foot_pi = IList.map (Sil.atom_expmap fe) prop.foot_pi in
  let foot_sigma = IList.map (Sil.hpred_expmap f) prop.foot_sigma in
  { prop with pi; sigma; foot_pi; foot_sigma; }

(** convert identifiers in fav to kind [k] *)
let vars_make_unprimed fav prop =
  let ids = Sil.fav_to_list fav in
  let ren_sub =
    Sil.sub_of_list (IList.map
                       (fun i -> (i, Sil.Var (Ident.create_fresh Ident.knormal)))
                       ids) in
  prop_ren_sub ren_sub prop

(** convert the normal vars to primed vars. *)
let prop_normal_vars_to_primed_vars p =
  let fav = prop_fav p in
  Sil.fav_filter_ident fav Ident.is_normal;
  exist_quantify fav p

(** Rename all primed variables fresh *)
let prop_rename_primed_fresh (p : normal t) : normal t =
  let ids_primed =
    let fav = prop_fav p in
    let ids = Sil.fav_to_list fav in
    IList.filter Ident.is_primed ids in
  let ren_sub =
    let f i = (i, Sil.Var (Ident.create_fresh Ident.kprimed)) in
    Sil.sub_of_list (IList.map f ids_primed) in
  prop_ren_sub ren_sub p

(** convert the primed vars to normal vars. *)
let prop_primed_vars_to_normal_vars (p : normal t) : normal t =
  let fav = prop_fav p in
  Sil.fav_filter_ident fav Ident.is_primed;
  vars_make_unprimed fav p

let from_pi pi = { prop_emp with pi = pi }

let from_sigma sigma = { prop_emp with sigma = sigma }

let replace_sub sub eprop =
  { eprop with sub = sub }

(** Rename free variables in a prop replacing them with existentially quantified vars *)
let prop_rename_fav_with_existentials (p : normal t) : normal t =
  let fav = Sil.fav_new () in
  prop_fav_add fav p;
  let ids = Sil.fav_to_list fav in
  let ids' = IList.map (fun i -> (i, Ident.create_fresh Ident.kprimed)) ids in
  let ren_sub = Sil.sub_of_list (IList.map (fun (i, i') -> (i, Sil.Var i')) ids') in
  let p' = prop_sub ren_sub p in
  (*L.d_strln "Prop after renaming:"; d_prop p'; L.d_strln "";*)
  normalize p'

(** {2 Prop iterators} *)

(** Iterator state over sigma. *)
type 'a prop_iter =
  { pit_sub : Sil.subst; (** substitution for equalities *)
    pit_pi : pi;    (** pure part *)
    pit_newpi : (bool * Sil.atom) list;   (** newly added atoms. *)
    (** The first records !Config.footprint. *)
    pit_old : sigma; (** sigma already visited *)
    pit_curr : Sil.hpred;      (** current element *)
    pit_state : 'a; (** state of current element *)
    pit_new : sigma; (** sigma not yet visited *)
    pit_foot_pi : pi; (** pure part of the footprint *)
    pit_foot_sigma : sigma; (** sigma part of the footprint *)
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
                          pit_foot_pi = prop.foot_pi;
                          pit_foot_sigma = prop.foot_sigma }
  | _ -> None

(** Return the prop associated to the iterator. *)
let prop_iter_to_prop iter =
  let sigma = IList.rev_append iter.pit_old (iter.pit_curr:: iter.pit_new) in
  let prop =
    normalize
      { sub = iter.pit_sub;
        pi = iter.pit_pi;
        sigma = sigma;
        foot_pi = iter.pit_foot_pi;
        foot_sigma = iter.pit_foot_sigma } in
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
let prop_iter_remove_curr_then_to_prop iter =
  let sigma = IList.rev_append iter.pit_old iter.pit_new in
  let normalized_sigma = sigma_normalize iter.pit_sub sigma in
  { sub = iter.pit_sub;
    pi = iter.pit_pi;
    sigma = normalized_sigma;
    foot_pi = iter.pit_foot_pi;
    foot_sigma = iter.pit_foot_sigma }

(** Return the current hpred and state. *)
let prop_iter_current iter =
  let curr = hpred_normalize iter.pit_sub iter.pit_curr in
  let prop = { prop_emp with sigma = [curr] } in
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
  let sub_id = Sil.sub_of_list [(id, Sil.Var pid)] in

  let normalize (id, e) =
    let eq' = Sil.Aeq(Sil.exp_sub sub_id (Sil.Var id), Sil.exp_sub sub_id e) in
    atom_normalize Sil.sub_empty eq' in

  let rec split pairs_unpid pairs_pid = function
    | [] -> (IList.rev pairs_unpid, IList.rev pairs_pid)
    | eq:: eqs_cur ->
        begin
          match eq with
          | Sil.Aeq (Sil.Var id1, e1) when Sil.ident_in_exp id1 e1 ->
              L.out "@[<2>#### ERROR: an assumption of the analyzer broken ####@\n";
              L.out "Broken Assumption: id notin e for all (id,e) in sub@\n";
              L.out "(id,e) : (%a,%a)@\n" (Ident.pp pe_text) id1 (Sil.pp_exp pe_text) e1;
              L.out "PROP : %a@\n@." (pp_prop pe_text) (prop_iter_to_prop iter);
              assert false
          | Sil.Aeq (Sil.Var id1, e1) when Ident.equal pid id1 ->
              split pairs_unpid ((id1, e1):: pairs_pid) eqs_cur
          | Sil.Aeq (Sil.Var id1, e1) ->
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
        let pairs = (id, Sil.Var pid) :: pairs_unpid in
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
  sigma_fav_add fav iter.pit_foot_sigma;
  pi_fav_add fav iter.pit_foot_pi

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
  iter.pit_foot_sigma

(** Replace the sigma part of the footprint *)
let prop_iter_replace_footprint_sigma iter sigma =
  { iter with pit_foot_sigma = sigma }

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
    let prop' = { prop with sigma = sigma' } in
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
    ((idl1@idl2, stml1@stml2), Sil.BinOp(op, e1, e2))
  else
    begin
      let id = Ident.create_fresh Ident.knormal in
      let prune_instr1, prune_res1, prune_instr2, prune_res2 =
        let cond1, cond2, res = match op with
          | Sil.LAnd -> e1, Sil.UnOp(Sil.LNot, e1, None), Sil.Int.zero
          | Sil.LOr -> Sil.UnOp(Sil.LNot, e1, None), e1, Sil.Int.one
          | _ -> assert false in
        let cond_res1 = Sil.BinOp(Sil.Eq, Sil.Var id, e2) in
        let cond_res2 = Sil.BinOp(Sil.Eq, Sil.Var id, Sil.exp_int res) in
        let mk_prune cond =
          (* don't report always true/false *)
          Sil.Prune (cond, loc, true, Sil.Ik_land_lor) in
        mk_prune cond1, mk_prune cond_res1, mk_prune cond2, mk_prune cond_res2 in
      let instrs2 =
        mk_nondet (prune_instr1 :: stml2 @ [prune_res1]) ([prune_instr2; prune_res2]) loc in
      ((id:: idl1@idl2, stml1@instrs2), Sil.Var id)
    end

(** Input of this mehtod is an exp in a prop. Output is a formal variable or path from a
    formal variable that is equal to the expression,
    or the OBJC_NULL attribute of the expression. *)
let find_equal_formal_path e prop =
  let rec find_in_sigma e seen_hpreds =
    IList.fold_right (
      fun hpred res ->
        if IList.mem Sil.hpred_equal hpred seen_hpreds then None
        else
          let seen_hpreds = hpred :: seen_hpreds in
          match res with
          | Some _ -> res
          | None ->
              match hpred with
              | Sil.Hpointsto (Sil.Lvar pvar1, Sil.Eexp (exp2, Sil.Iformal(_, _) ), _)
                when Sil.exp_equal exp2 e &&
                     (Pvar.is_local pvar1 || Pvar.is_seed pvar1) ->
                  Some (Sil.Lvar pvar1)
              | Sil.Hpointsto (exp1, Sil.Estruct (fields, _), _) ->
                  IList.fold_right (fun (field, strexp) res ->
                      match res with
                      | Some _ -> res
                      | None ->
                          match strexp with
                          | Sil.Eexp (exp2, _) when Sil.exp_equal exp2 e ->
                              (match find_in_sigma exp1 seen_hpreds with
                               | Some exp' -> Some (Sil.Lfield (exp', field, Sil.Tvoid))
                               | None -> None)
                          | _ -> None) fields None
              | _ -> None) (get_sigma prop) None in
  match find_in_sigma e [] with
  | Some res -> Some res
  | None -> match get_objc_null_attribute prop e with
    | Some (Sil.Aobjc_null exp) -> Some exp
    | _ -> None

(** translate an if-then-else expression *)
let trans_if_then_else ((idl1, stml1), e1) ((idl2, stml2), e2) ((idl3, stml3), e3) loc =
  match sym_eval false e1 with
  | Sil.Const (Sil.Cint i) when Sil.Int.iszero i -> (idl1@idl3, stml1@stml3), e3
  | Sil.Const (Sil.Cint _) -> (idl1@idl2, stml1@stml2), e2
  | _ ->
      let e1not = Sil.UnOp(Sil.LNot, e1, None) in
      let id = Ident.create_fresh Ident.knormal in
      let mk_prune_res e =
        let mk_prune cond = Sil.Prune (cond, loc, true, Sil.Ik_land_lor)
        (* don't report always true/false *) in
        mk_prune (Sil.BinOp(Sil.Eq, Sil.Var id, e)) in
      let prune1 = Sil.Prune (e1, loc, true, Sil.Ik_bexp) in
      let prune1not = Sil.Prune (e1not, loc, false, Sil.Ik_bexp) in
      let stml' =
        mk_nondet
          (prune1 :: stml2 @ [mk_prune_res e2]) (prune1not :: stml3 @ [mk_prune_res e3]) loc in
      (id:: idl1@idl2@idl3, stml1@stml'), Sil.Var id

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
    let size_footprint = sigma_size p.foot_sigma in
    max size_current size_footprint

  (** Approximate the size of the longest chain by counting the max
      number of |-> with the same type and whose lhs is primed or
      footprint *)
  let prop_chain_size p =
    let fp_size = pi_size p.foot_pi + sigma_size p.foot_sigma in
    pi_size p.pi + sigma_size p.sigma + fp_size

(*
  (** Approximate the size of the longest chain by counting the max
      number of |-> with the same type and whose lhs is primed or
      footprint *)
  let sigma_chain_size sigma =
    let tbl = ref Sil.ExpMap.empty in
    let add t =
      try
        let count = Sil.ExpMap.find t !tbl in
        tbl := Sil.ExpMap.add t (count + 1) !tbl
      with
      | Not_found ->
          tbl := Sil.ExpMap.add t 1 !tbl in
    let process_hpred = function
      | Sil.Hpointsto (e, _, te) ->
          (match e with
           | Sil.Var id when Ident.is_primed id || Ident.is_footprint id -> add te
           | _ -> ())
      | Sil.Hlseg _ | Sil.Hdllseg _ -> () in
    IList.iter process_hpred sigma;
    let size = ref 0 in
    Sil.ExpMap.iter (fun t n -> size := max n !size) !tbl;
    !size
*)
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
      | Sil.Lvar _ -> true
      | _ -> false in
    let lhs_is_var_lvar = function
      | Sil.Var _ -> true
      | Sil.Lvar _ -> true
      | _ -> false in
    let rhs_is_var = function
      | Sil.Eexp (Sil.Var _, _) -> true
      | _ -> false in
    let rec rhs_only_vars = function
      | Sil.Eexp (Sil.Var _, _) -> true
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
      check_pi (get_pi pre) && check_sigma (get_sigma pre) in
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

(*
let pp_lseg_kind f = function
  | Sil.Lseg_NE -> F.fprintf f "ne"
  | Sil.Lseg_PE -> F.fprintf f ""

let pi_av_add fav pi =
  IList.iter (Sil.atom_av_add fav) pi

let sigma_av_add fav sigma =
  IList.iter (Sil.hpred_av_add fav) sigma

let prop_av_add fav prop =
  Sil.sub_av_add fav prop.sub;
  pi_av_add fav prop.pi;
  sigma_av_add fav prop.sigma;
  pi_av_add fav prop.foot_pi;
  sigma_av_add fav prop.foot_sigma

let prop_av =
  Sil.fav_imperative_to_functional prop_av_add

let rec remove_duplicates_from_sorted special_equal = function
  | [] -> []
  | [x] -> [x]
  | x:: y:: l ->
      if (special_equal x y)
      then remove_duplicates_from_sorted special_equal (y:: l)
      else x:: (remove_duplicates_from_sorted special_equal (y:: l))

(** Replace the sub part of [prop]. *)
let prop_replace_sub sub p =
  let nsub = sub_normalize sub in
  { p with sub = nsub }

let unstructured_type = function
  | Sil.Tstruct _ | Sil.Tarray _ -> false
  | _ -> true

let rec pp_ren pe f = function
  | [] -> ()
  | [(i, x)] -> F.fprintf f "%a->%a" (Ident.pp pe) i (Ident.pp pe) x
  | (i, x):: ren -> F.fprintf f "%a->%a, %a" (Ident.pp pe) i (Ident.pp pe) x (pp_ren pe) ren

let id_exp_compare (id1, e1) (id2, e2) =
  let n = Sil.exp_compare e1 e2 in
  if n <> 0 then n
  else Ident.compare id1 id2

(** Raise an exception if the prop is not normalized *)
let check_prop_normalized prop =
  let sigma' = sigma_normalize_prop prop prop.sigma in
  if sigma_equal prop.sigma sigma' == false then assert false
*)
