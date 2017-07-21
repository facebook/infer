(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Functions for Propositions (i.e., Symbolic Heaps) *)

module L = Logging
module F = Format

(** type to describe different strategies for initializing fields of a structure. [No_init] does not
    initialize any fields of the struct. [Fld_init] initializes the fields of the struct with fresh
    variables (C) or default values (Java). *)
type struct_init_mode = No_init | Fld_init

let unSome = function Some x -> x | _ -> assert false

(** kind for normal props, i.e. normalized *)
type normal

(** kind for exposed props *)
type exposed

type pi = Sil.atom list [@@deriving compare]

type sigma = Sil.hpred list [@@deriving compare]

let equal_pi = [%compare.equal : pi]

let equal_sigma = [%compare.equal : sigma]

module Core : sig
  (** the kind 'a should range over [normal] and [exposed] *)
  type 'a t = private
    { sigma: sigma  (** spatial part *)
    ; sub: Sil.exp_subst  (** substitution *)
    ; pi: pi  (** pure part *)
    ; sigma_fp: sigma  (** abduced spatial part *)
    ; pi_fp: pi  (** abduced pure part *) }
    [@@deriving compare]

  val prop_emp : normal t
  (** Proposition [true /\ emp]. *)

  val set :
    ?sub:Sil.exp_subst -> ?pi:pi -> ?sigma:sigma -> ?pi_fp:pi -> ?sigma_fp:sigma -> 'a t
    -> exposed t
  (** Set individual fields of the prop. *)

  val unsafe_cast_to_normal : exposed t -> normal t
  (** Cast an exposed prop to a normalized one by just changing the type *)
end = struct
  (** A proposition. The following invariants are mantained. [sub] is of
      the form id1 = e1 ... idn = en where: the id's are distinct and do not
      occur in the e's nor in [pi] or [sigma]; the id's are in sorted
      order; the id's are not existentials; if idn = yn (for yn not
      existential) then idn < yn in the order on ident's. [pi] is sorted
      and normalized, and does not contain x = e. [sigma] is sorted and
      normalized. *)
  type 'a t =
    { sigma: sigma  (** spatial part *)
    ; sub: Sil.exp_subst  (** substitution *)
    ; pi: pi  (** pure part *)
    ; sigma_fp: sigma  (** abduced spatial part *)
    ; pi_fp: pi  (** abduced pure part *) }
    [@@deriving compare]

  (** Proposition [true /\ emp]. *)
  let prop_emp : normal t = {sub= Sil.exp_sub_empty; pi= []; sigma= []; pi_fp= []; sigma_fp= []}

  let set ?sub ?pi ?sigma ?pi_fp ?sigma_fp p =
    let set_ p ?(sub= p.sub) ?(pi= p.pi) ?(sigma= p.sigma) ?(pi_fp= p.pi_fp)
        ?(sigma_fp= p.sigma_fp) () =
      {sub; pi; sigma; pi_fp; sigma_fp}
    in
    set_ p ?sub ?pi ?sigma ?pi_fp ?sigma_fp ()

  let unsafe_cast_to_normal (p: exposed t) : normal t = (p :> normal t)
end

include Core

(** {2 Basic Functions for Propositions} *)

(** {1 Functions for Comparison} *)

(** Comparison between propositions. Lexicographical order. *)
let compare_prop p1 p2 = compare (fun _ _ -> 0) p1 p2

(** Check the equality of two propositions *)
let equal_prop p1 p2 = Int.equal (compare_prop p1 p2) 0

(** {1 Functions for Pretty Printing} *)

(** Pretty print a footprint. *)
let pp_footprint _pe f fp =
  let pe = {_pe with Pp.cmap_norm= _pe.Pp.cmap_foot} in
  let pp_pi f () =
    if fp.pi_fp <> [] then
      F.fprintf f "%a ;@\n" (Pp.semicolon_seq_oneline pe (Sil.pp_atom pe)) fp.pi_fp
  in
  if fp.pi_fp <> [] || fp.sigma_fp <> [] then
    F.fprintf f "@\n[footprint@\n  @[%a%a@]  ]" pp_pi ()
      (Pp.semicolon_seq pe (Sil.pp_hpred pe))
      fp.sigma_fp

let pp_texp_simple pe =
  match pe.Pp.opt with SIM_DEFAULT -> Sil.pp_texp pe | SIM_WITH_TYP -> Sil.pp_texp_full pe

(** Pretty print a pointsto representing a stack variable as an equality *)
let pp_hpred_stackvar pe0 f (hpred: Sil.hpred) =
  let pe, changed = Sil.color_pre_wrapper pe0 f hpred in
  ( match hpred with
  | Hpointsto (Exp.Lvar pvar, se, te)
   -> (
      let pe' =
        match se with
        | Eexp (Exp.Var _, _) when not (Pvar.is_global pvar)
         -> {pe with obj_sub= None} (* dont use obj sub on the var defining it *)
        | _
         -> pe
      in
      match pe'.kind with
      | TEXT | HTML
       -> F.fprintf f "%a = %a:%a" (Pvar.pp_value pe') pvar (Sil.pp_sexp pe') se
            (pp_texp_simple pe') te
      | LATEX
       -> F.fprintf f "%a{=}%a" (Pvar.pp_value pe') pvar (Sil.pp_sexp pe') se )
  | Hpointsto _ | Hlseg _ | Hdllseg _
   -> assert false (* should not happen *) ) ;
  Sil.color_post_wrapper changed pe0 f

(** Pretty print a substitution. *)
let pp_sub pe f = function
  | `Exp sub
   -> let pi_sub = List.map ~f:(fun (id, e) -> Sil.Aeq (Var id, e)) (Sil.sub_to_list sub) in
      Pp.semicolon_seq_oneline pe (Sil.pp_atom pe) f pi_sub
  | `Typ _
   -> F.fprintf f "Printing typ_subst not implemented."

(** Dump a substitution. *)
let d_sub (sub: Sil.subst) = L.add_print_action (PTsub, Obj.repr sub)

let pp_sub_entry pe0 f entry =
  let pe, changed = Sil.color_pre_wrapper pe0 f entry in
  let x, e = entry in
  ( match pe.kind with
  | TEXT | HTML
   -> F.fprintf f "%a = %a" (Ident.pp pe) x (Sil.pp_exp_printenv pe) e
  | LATEX
   -> F.fprintf f "%a{=}%a" (Ident.pp pe) x (Sil.pp_exp_printenv pe) e ) ;
  Sil.color_post_wrapper changed pe0 f

(** Pretty print a substitution as a list of (ident,exp) pairs *)
let pp_subl pe =
  if Config.smt_output then Pp.semicolon_seq pe (pp_sub_entry pe)
  else Pp.semicolon_seq_oneline pe (pp_sub_entry pe)

(** Pretty print a pi. *)
let pp_pi pe =
  if Config.smt_output then Pp.semicolon_seq pe (Sil.pp_atom pe)
  else Pp.semicolon_seq_oneline pe (Sil.pp_atom pe)

(** Dump a pi. *)
let d_pi (pi: pi) = L.add_print_action (PTpi, Obj.repr pi)

(** Pretty print a sigma. *)
let pp_sigma pe = Pp.semicolon_seq pe (Sil.pp_hpred pe)

(** Split sigma into stack and nonstack parts.
    The boolean indicates whether the stack should only include local variales. *)
let sigma_get_stack_nonstack only_local_vars sigma =
  let hpred_is_stack_var = function
    | Sil.Hpointsto (Lvar pvar, _, _)
     -> not only_local_vars || Pvar.is_local pvar
    | _
     -> false
  in
  List.partition_tf ~f:hpred_is_stack_var sigma

(** Pretty print a sigma in simple mode. *)
let pp_sigma_simple pe env fmt sigma =
  let sigma_stack, sigma_nonstack = sigma_get_stack_nonstack false sigma in
  let pp_stack fmt _sg =
    let sg = List.sort ~cmp:Sil.compare_hpred _sg in
    if sg <> [] then Format.fprintf fmt "%a" (Pp.semicolon_seq pe (pp_hpred_stackvar pe)) sg
  in
  let pp_nl fmt doit =
    if doit then
      match pe.Pp.kind with
      | TEXT | HTML
       -> Format.fprintf fmt " ;@\n"
      | LATEX
       -> Format.fprintf fmt " ; \\\\@\n"
  in
  let pp_nonstack fmt = Pp.semicolon_seq pe (Sil.pp_hpred_env pe (Some env)) fmt in
  if sigma_stack <> [] || sigma_nonstack <> [] then
    Format.fprintf fmt "%a%a%a" pp_stack sigma_stack pp_nl
      (sigma_stack <> [] && sigma_nonstack <> [])
      pp_nonstack sigma_nonstack

(** Dump a sigma. *)
let d_sigma (sigma: sigma) = L.add_print_action (PTsigma, Obj.repr sigma)

(** Dump a pi and a sigma *)
let d_pi_sigma pi sigma =
  let d_separator () = if pi <> [] && sigma <> [] then L.d_strln " *" in
  d_pi pi ; d_separator () ; d_sigma sigma

let pi_of_subst sub = List.map ~f:(fun (id1, e2) -> Sil.Aeq (Var id1, e2)) (Sil.sub_to_list sub)

(** Return the pure part of [prop]. *)
let get_pure (p: 'a t) : pi = pi_of_subst p.sub @ p.pi

(* Same with get_pure, except that when we have both "x = t" and "y = t" where t is a primed ident, 
* we add "x = y" to the result. This is crucial for the normalizer, as it tend to drop "x = t" before 
* processing "y = t". If we don't explicitly preserve "x = y", the normalizer cannot pick it up *)
let get_pure_extended p =
  let base = get_pure p in
  let primed_atoms, _ =
    List.fold base ~init:([], Ident.IdentMap.empty) ~f:(fun (atoms, primed_map as acc) base_atom ->
        let extend_atoms id pid =
          try
            let old_id = Ident.IdentMap.find pid primed_map in
            let new_atom = Sil.Aeq (Var id, Var old_id) in
            (new_atom :: atoms, primed_map)
          with Not_found -> (atoms, Ident.IdentMap.add pid id primed_map)
        in
        match base_atom with
        | Sil.Aeq (Exp.Var id0, Exp.Var id1) when Ident.is_primed id0 && not (Ident.is_primed id1)
         -> extend_atoms id1 id0
        | Sil.Aeq (Exp.Var id0, Exp.Var id1) when Ident.is_primed id1 && not (Ident.is_primed id0)
         -> extend_atoms id0 id1
        | _
         -> acc )
  in
  primed_atoms @ base

(** Print existential quantification *)
let pp_evars pe f evars =
  if evars <> [] then
    match pe.Pp.kind with
    | TEXT | HTML
     -> F.fprintf f "exists [%a]. " (Pp.comma_seq (Ident.pp pe)) evars
    | LATEX
     -> F.fprintf f "\\exists %a. " (Pp.comma_seq (Ident.pp pe)) evars

(** Print an hpara in simple mode *)
let pp_hpara_simple _pe env n f pred =
  let pe = Pp.reset_obj_sub _pe in
  (* no free vars: disable object substitution *)
  match pe.kind with
  | TEXT | HTML
   -> F.fprintf f "P%d = %a%a" n (pp_evars pe) pred.Sil.evars
        (Pp.semicolon_seq pe (Sil.pp_hpred_env pe (Some env)))
        pred.Sil.body
  | LATEX
   -> F.fprintf f "P_{%d} = %a%a\\\\" n (pp_evars pe) pred.Sil.evars
        (Pp.semicolon_seq pe (Sil.pp_hpred_env pe (Some env)))
        pred.Sil.body

(** Print an hpara_dll in simple mode *)
let pp_hpara_dll_simple _pe env n f pred =
  let pe = Pp.reset_obj_sub _pe in
  (* no free vars: disable object substitution *)
  match pe.kind with
  | TEXT | HTML
   -> F.fprintf f "P%d = %a%a" n (pp_evars pe) pred.Sil.evars_dll
        (Pp.semicolon_seq pe (Sil.pp_hpred_env pe (Some env)))
        pred.Sil.body_dll
  | LATEX
   -> F.fprintf f "P_{%d} = %a%a" n (pp_evars pe) pred.Sil.evars_dll
        (Pp.semicolon_seq pe (Sil.pp_hpred_env pe (Some env)))
        pred.Sil.body_dll

(** Create an environment mapping (ident) expressions to the program variables containing them *)
let create_pvar_env (sigma: sigma) : Exp.t -> Exp.t =
  let env = ref [] in
  let filter = function
    | Sil.Hpointsto (Lvar pvar, Eexp (Var v, _), _)
     -> if not (Pvar.is_global pvar) then env := (Exp.Var v, Exp.Lvar pvar) :: !env
    | _
     -> ()
  in
  List.iter ~f:filter sigma ;
  let find e =
    List.find ~f:(fun (e1, _) -> Exp.equal e1 e) !env |> Option.map ~f:snd
    |> Option.value ~default:e
  in
  find

(** Update the object substitution given the stack variables in the prop *)
let prop_update_obj_sub pe prop =
  if !Config.pp_simple then Pp.set_obj_sub pe (create_pvar_env prop.sigma) else pe

(** Pretty print a footprint in simple mode. *)
let pp_footprint_simple _pe env f fp =
  let pe = {_pe with Pp.cmap_norm= _pe.Pp.cmap_foot} in
  let pp_pure f pi = if pi <> [] then F.fprintf f "%a *@\n" (pp_pi pe) pi in
  if fp.pi_fp <> [] || fp.sigma_fp <> [] then
    F.fprintf f "@\n[footprint@\n   @[%a%a@]  ]" pp_pure fp.pi_fp (pp_sigma_simple pe env)
      fp.sigma_fp

(** Create a predicate environment for a prop *)
let prop_pred_env prop =
  let env = Sil.Predicates.empty_env () in
  List.iter ~f:(Sil.Predicates.process_hpred env) prop.sigma ;
  List.iter ~f:(Sil.Predicates.process_hpred env) prop.sigma_fp ;
  env

(** Pretty print a proposition. *)
let pp_prop pe0 f prop =
  let pe = prop_update_obj_sub pe0 prop in
  let latex = Pp.equal_print_kind pe.Pp.kind Pp.LATEX in
  let do_print f () =
    let subl = Sil.sub_to_list prop.sub in
    (* since prop diff is based on physical equality, we need to extract the sub verbatim *)
    let pi = prop.pi in
    let pp_pure f () =
      if subl <> [] then F.fprintf f "%a ;@\n" (pp_subl pe) subl ;
      if pi <> [] then F.fprintf f "%a ;@\n" (pp_pi pe) pi
    in
    if !Config.pp_simple || latex then
      let env = prop_pred_env prop in
      let iter_f n hpara = F.fprintf f "@,@[<h>%a@]" (pp_hpara_simple pe env n) hpara in
      let iter_f_dll n hpara_dll =
        F.fprintf f "@,@[<h>%a@]" (pp_hpara_dll_simple pe env n) hpara_dll
      in
      let pp_predicates _ () =
        if Sil.Predicates.is_empty env then ()
        else if latex then (
          F.fprintf f "@\n\\\\\\textsf{where }" ; Sil.Predicates.iter env iter_f iter_f_dll )
        else ( F.fprintf f "@,where" ; Sil.Predicates.iter env iter_f iter_f_dll )
      in
      F.fprintf f "%a%a%a%a" pp_pure () (pp_sigma_simple pe env) prop.sigma
        (pp_footprint_simple pe env) prop pp_predicates ()
    else F.fprintf f "%a%a%a" pp_pure () (pp_sigma pe) prop.sigma (pp_footprint pe) prop
  in
  if !Config.forcing_delayed_prints then
    (* print in html mode *)
    F.fprintf f "%a%a%a" Io_infer.Html.pp_start_color Pp.Blue do_print ()
      Io_infer.Html.pp_end_color ()
  else do_print f () 
  (** print in text mode *)

let pp_prop_with_typ pe f p = pp_prop {pe with opt= SIM_WITH_TYP} f p

(** Dump a proposition. *)
let d_prop (prop: 'a t) = L.add_print_action (PTprop, Obj.repr prop)

(** Dump a proposition. *)
let d_prop_with_typ (prop: 'a t) = L.add_print_action (PTprop_with_typ, Obj.repr prop)

(** Print a list of propositions, prepending each one with the given string *)
let pp_proplist_with_typ pe f plist =
  let rec pp_seq_newline f = function
    | []
     -> ()
    | [x]
     -> F.fprintf f "@[%a@]" (pp_prop_with_typ pe) x
    | x :: l
     -> F.fprintf f "@[%a@]@\n(||)@\n%a" (pp_prop_with_typ pe) x pp_seq_newline l
  in
  F.fprintf f "@[<v>%a@]" pp_seq_newline plist

(** dump a proplist *)
let d_proplist_with_typ (pl: 'a t list) = L.add_print_action (PTprop_list_with_typ, Obj.repr pl)

(** {1 Functions for computing free non-program variables} *)

let pi_fav_add fav pi = List.iter ~f:(Sil.atom_fav_add fav) pi

let pi_fav = Sil.fav_imperative_to_functional pi_fav_add

let sigma_fav_add fav sigma = List.iter ~f:(Sil.hpred_fav_add fav) sigma

let sigma_fav = Sil.fav_imperative_to_functional sigma_fav_add

let prop_footprint_fav_add fav prop = sigma_fav_add fav prop.sigma_fp ; pi_fav_add fav prop.pi_fp

(** Find fav of the footprint part of the prop *)
let prop_footprint_fav prop = Sil.fav_imperative_to_functional prop_footprint_fav_add prop

let prop_fav_add fav prop =
  sigma_fav_add fav prop.sigma ;
  sigma_fav_add fav prop.sigma_fp ;
  Sil.sub_fav_add fav prop.sub ;
  pi_fav_add fav prop.pi ;
  pi_fav_add fav prop.pi_fp

let prop_fav p = Sil.fav_imperative_to_functional prop_fav_add p

(** free vars of the prop, excluding the pure part *)
let prop_fav_nonpure_add fav prop = sigma_fav_add fav prop.sigma ; sigma_fav_add fav prop.sigma_fp

(** free vars, except pi and sub, of current and footprint parts *)
let prop_fav_nonpure = Sil.fav_imperative_to_functional prop_fav_nonpure_add

let hpred_fav_in_pvars_add fav (hpred: Sil.hpred) =
  match hpred with
  | Hpointsto (Lvar _, sexp, _)
   -> Sil.strexp_fav_add fav sexp
  | Hpointsto _ | Hlseg _ | Hdllseg _
   -> ()

let sigma_fav_in_pvars_add fav sigma = List.iter ~f:(hpred_fav_in_pvars_add fav) sigma

(** {2 Functions for Subsitition} *)

let pi_sub (subst: Sil.subst) pi =
  let f = Sil.atom_sub subst in
  List.map ~f pi

let sigma_sub subst sigma =
  let f = Sil.hpred_sub subst in
  List.map ~f sigma

(** Return [true] if the atom is an inequality *)
let atom_is_inequality (atom: Sil.atom) =
  match atom with
  | Aeq (BinOp ((Le | Lt), _, _), Const Cint i) when IntLit.isone i
   -> true
  | _
   -> false

(** If the atom is [e<=n] return [e,n] *)
let atom_exp_le_const (atom: Sil.atom) =
  match atom with
  | Aeq (BinOp (Le, e1, Const Cint n), Const Cint i) when IntLit.isone i
   -> Some (e1, n)
  | _
   -> None

(** If the atom is [n<e] return [n,e] *)
let atom_const_lt_exp (atom: Sil.atom) =
  match atom with
  | Aeq (BinOp (Lt, Const Cint n, e1), Const Cint i) when IntLit.isone i
   -> Some (n, e1)
  | _
   -> None

let exp_reorder e1 e2 = if Exp.compare e1 e2 <= 0 then (e1, e2) else (e2, e1)

(** create a strexp of the given type, populating the structures if [expand_structs] is true *)
let rec create_strexp_of_type tenv struct_init_mode (typ: Typ.t) len inst : Sil.strexp =
  let init_value () =
    let create_fresh_var () =
      let fresh_id =
        Ident.create_fresh (if !Config.footprint then Ident.kfootprint else Ident.kprimed)
      in
      Exp.Var fresh_id
    in
    if Config.curr_language_is Config.Java && Sil.equal_inst inst Sil.Ialloc then
      match typ.desc with Tfloat _ -> Exp.Const (Cfloat 0.0) | _ -> Exp.zero
    else create_fresh_var ()
  in
  match (typ.desc, len) with
  | (Tint _ | Tfloat _ | Tvoid | Tfun _ | Tptr _ | TVar _), None
   -> Eexp (init_value (), inst)
  | Tstruct name, _ -> (
    match (struct_init_mode, Tenv.lookup tenv name) with
    | Fld_init, Some {fields}
     -> (* pass len as an accumulator, so that it is passed to create_strexp_of_type for the last
             field, but always return None so that only the last field receives len *)
        let f (fld, t, a) (flds, len) =
          if Typ.Struct.is_objc_ref_counter_field (fld, t, a) then
            ((fld, Sil.Eexp (Exp.one, inst)) :: flds, None)
          else ((fld, create_strexp_of_type tenv struct_init_mode t len inst) :: flds, None)
        in
        let flds, _ = List.fold_right ~f fields ~init:([], len) in
        Estruct (flds, inst)
    | _
     -> Estruct ([], inst) )
  | Tarray (_, len_opt, _), None
   -> let len =
        match len_opt with None -> Exp.get_undefined false | Some len -> Exp.Const (Cint len)
      in
      Earray (len, [], inst)
  | Tarray _, Some len
   -> Earray (len, [], inst)
  | (Tint _ | Tfloat _ | Tvoid | Tfun _ | Tptr _ | TVar _), Some _
   -> assert false

let replace_array_contents (hpred: Sil.hpred) esel : Sil.hpred =
  match hpred with
  | Hpointsto (root, Sil.Earray (len, [], inst), te)
   -> Hpointsto (root, Earray (len, esel, inst), te)
  | _
   -> assert false

(** remove duplicate atoms and redundant inequalities from a sorted pi *)
let rec pi_sorted_remove_redundant (pi: pi) =
  match pi with
  | (Aeq (BinOp (Le, e1, Const Cint n1), Const Cint i1) as a1)
    :: (Aeq (BinOp (Le, e2, Const Cint n2), Const Cint i2)) :: rest
    when IntLit.isone i1 && IntLit.isone i2 && Exp.equal e1 e2 && IntLit.lt n1 n2
   -> (* second inequality redundant *)
      pi_sorted_remove_redundant (a1 :: rest)
  | (Aeq (BinOp (Lt, Const Cint n1, e1), Const Cint i1))
    :: (Aeq (BinOp (Lt, Const Cint n2, e2), Const Cint i2) as a2) :: rest
    when IntLit.isone i1 && IntLit.isone i2 && Exp.equal e1 e2 && IntLit.lt n1 n2
   -> (* first inequality redundant *)
      pi_sorted_remove_redundant (a2 :: rest)
  | a1 :: a2 :: rest
   -> if Sil.equal_atom a1 a2 then pi_sorted_remove_redundant (a2 :: rest)
      else a1 :: pi_sorted_remove_redundant (a2 :: rest)
  | [a]
   -> [a]
  | []
   -> []

(** find the unsigned expressions in sigma (immediately inside a pointsto, for now) *)
let sigma_get_unsigned_exps sigma =
  let uexps = ref [] in
  let do_hpred (hpred: Sil.hpred) =
    match hpred with
    | Hpointsto (_, Eexp (e, _), Sizeof {typ= {desc= Tint ik}}) when Typ.ikind_is_unsigned ik
     -> uexps := e :: !uexps
    | _
     -> ()
  in
  List.iter ~f:do_hpred sigma ; !uexps

(** Collapse consecutive indices that should be added. For instance,
    this function reduces x[1][1] to x[2]. The [typ] argument is used
    to ensure the soundness of this collapsing. *)
let exp_collapse_consecutive_indices_prop (typ: Typ.t) exp =
  let typ_is_base (typ1: Typ.t) =
    match typ1.desc with Tint _ | Tfloat _ | Tstruct _ | Tvoid | Tfun _ -> true | _ -> false
  in
  let typ_is_one_step_from_base =
    match typ.desc with Tptr (t, _) | Tarray (t, _, _) -> typ_is_base t | _ -> false
  in
  let rec exp_remove (e0: Exp.t) =
    match e0 with
    | Lindex (Lindex (base, e1), e2)
     -> let e0' : Exp.t = Lindex (base, BinOp (PlusA, e1, e2)) in
        exp_remove e0'
    | _
     -> e0
  in
  if typ_is_one_step_from_base then exp_remove exp else exp

(** {2 Compaction} *)

(** Return a compact representation of the prop *)
let prop_compact sh (prop: normal t) : normal t =
  let sigma' = List.map ~f:(Sil.hpred_compact sh) prop.sigma in
  unsafe_cast_to_normal (set prop ~sigma:sigma')

(** {2 Query about Proposition} *)

(** Check if the sigma part of the proposition is emp *)
let prop_is_emp p = match p.sigma with [] -> true | _ -> false

(** {2 Functions for changing and generating propositions} *)

(** Conjoin a heap predicate by separating conjunction. *)
let prop_hpred_star (p: 'a t) (h: Sil.hpred) : exposed t =
  let sigma' = h :: p.sigma in
  set p ~sigma:sigma'

let prop_sigma_star (p: 'a t) (sigma: sigma) : exposed t =
  let sigma' = sigma @ p.sigma in
  set p ~sigma:sigma'

(** return the set of subexpressions of [strexp] *)
let strexp_get_exps strexp =
  let rec strexp_get_exps_rec exps (se: Sil.strexp) =
    match se with
    | Eexp (Exn e, _)
     -> Exp.Set.add e exps
    | Eexp (e, _)
     -> Exp.Set.add e exps
    | Estruct (flds, _)
     -> List.fold ~f:(fun exps (_, strexp) -> strexp_get_exps_rec exps strexp) ~init:exps flds
    | Earray (_, elems, _)
     -> List.fold ~f:(fun exps (_, strexp) -> strexp_get_exps_rec exps strexp) ~init:exps elems
  in
  strexp_get_exps_rec Exp.Set.empty strexp

(** get the set of expressions on the righthand side of [hpred] *)
let hpred_get_targets (hpred: Sil.hpred) =
  match hpred with
  | Hpointsto (_, rhs, _)
   -> strexp_get_exps rhs
  | Hlseg (_, _, _, e, el)
   -> List.fold ~f:(fun exps e -> Exp.Set.add e exps) ~init:Exp.Set.empty (e :: el)
  | Hdllseg (_, _, _, oB, oF, iB, el)
   -> (* only one direction supported for now *)
      List.fold ~f:(fun exps e -> Exp.Set.add e exps) ~init:Exp.Set.empty (oB :: oF :: iB :: el)

(** return the set of hpred's and exp's in [sigma] that are reachable from an expression in
    [exps] *)
let compute_reachable_hpreds sigma exps =
  let rec compute_reachable_hpreds_rec sigma (reach, exps) =
    let add_hpred_if_reachable (reach, exps) (hpred: Sil.hpred) =
      match hpred with
      | Hpointsto (lhs, _, _) as hpred when Exp.Set.mem lhs exps
       -> let reach' = Sil.HpredSet.add hpred reach in
          let reach_exps = hpred_get_targets hpred in
          (reach', Exp.Set.union exps reach_exps)
      | _
       -> (reach, exps)
    in
    let reach', exps' = List.fold ~f:add_hpred_if_reachable ~init:(reach, exps) sigma in
    if Int.equal (Sil.HpredSet.cardinal reach) (Sil.HpredSet.cardinal reach') then (reach, exps)
    else compute_reachable_hpreds_rec sigma (reach', exps')
  in
  compute_reachable_hpreds_rec sigma (Sil.HpredSet.empty, exps)

(* Module for normalization *)
module Normalize = struct
  (** Eliminates all empty lsegs from sigma, and collect equalities
      The empty lsegs include
      (a) "lseg_pe para 0 e elist",
      (b) "dllseg_pe para iF oB oF iB elist" with iF = 0 or iB = 0,
      (c) "lseg_pe para e1 e2 elist" and the rest of sigma contains the "cell" e1,
      (d) "dllseg_pe para iF oB oF iB elist" and the rest of sigma contains
      cell iF or iB. *)
  let sigma_remove_emptylseg sigma =
    let alloc_set =
      let rec f_alloc set (sigma1: sigma) =
        match sigma1 with
        | []
         -> set
        | (Hpointsto (e, _, _)) :: sigma' | (Hlseg (Sil.Lseg_NE, _, e, _, _)) :: sigma'
         -> f_alloc (Exp.Set.add e set) sigma'
        | (Hdllseg (Sil.Lseg_NE, _, iF, _, _, iB, _)) :: sigma'
         -> f_alloc (Exp.Set.add iF (Exp.Set.add iB set)) sigma'
        | _ :: sigma'
         -> f_alloc set sigma'
      in
      f_alloc Exp.Set.empty sigma
    in
    let rec f eqs_zero sigma_passed (sigma1: sigma) =
      match sigma1 with
      | []
       -> (List.rev eqs_zero, List.rev sigma_passed)
      | (Hpointsto _ as hpred) :: sigma'
       -> f eqs_zero (hpred :: sigma_passed) sigma'
      | (Hlseg (Lseg_PE, _, e1, e2, _)) :: sigma'
        when Exp.equal e1 Exp.zero || Exp.Set.mem e1 alloc_set
       -> f (Sil.Aeq (e1, e2) :: eqs_zero) sigma_passed sigma'
      | (Hlseg _ as hpred) :: sigma'
       -> f eqs_zero (hpred :: sigma_passed) sigma'
      | (Hdllseg (Lseg_PE, _, iF, oB, oF, iB, _)) :: sigma'
        when Exp.equal iF Exp.zero || Exp.Set.mem iF alloc_set || Exp.equal iB Exp.zero
             || Exp.Set.mem iB alloc_set
       -> f (Sil.Aeq (iF, oF) :: Sil.Aeq (iB, oB) :: eqs_zero) sigma_passed sigma'
      | (Hdllseg _ as hpred) :: sigma'
       -> f eqs_zero (hpred :: sigma_passed) sigma'
    in
    f [] [] sigma

  let sigma_intro_nonemptylseg e1 e2 sigma =
    let rec f sigma_passed (sigma1: sigma) =
      match sigma1 with
      | []
       -> List.rev sigma_passed
      | (Hpointsto _ as hpred) :: sigma'
       -> f (hpred :: sigma_passed) sigma'
      | (Hlseg (Lseg_PE, para, f1, f2, shared)) :: sigma'
        when Exp.equal e1 f1 && Exp.equal e2 f2 || Exp.equal e2 f1 && Exp.equal e1 f2
       -> f (Sil.Hlseg (Lseg_NE, para, f1, f2, shared) :: sigma_passed) sigma'
      | (Hlseg _ as hpred) :: sigma'
       -> f (hpred :: sigma_passed) sigma'
      | (Hdllseg (Lseg_PE, para, iF, oB, oF, iB, shared)) :: sigma'
        when Exp.equal e1 iF && Exp.equal e2 oF || Exp.equal e2 iF && Exp.equal e1 oF
             || Exp.equal e1 iB && Exp.equal e2 oB || Exp.equal e2 iB && Exp.equal e1 oB
       -> f (Sil.Hdllseg (Lseg_NE, para, iF, oB, oF, iB, shared) :: sigma_passed) sigma'
      | (Hdllseg _ as hpred) :: sigma'
       -> f (hpred :: sigma_passed) sigma'
    in
    f [] sigma

  let ( -- ) = IntLit.sub

  let ( ++ ) = IntLit.add

  let sym_eval tenv abs e =
    let lookup = Tenv.lookup tenv in
    let rec eval (e: Exp.t) : Exp.t =
      (* L.d_str " ["; Sil.d_exp e; L.d_str"] "; *)
      match e with
      | Var _
       -> e
      | Closure c
       -> let captured_vars =
            List.map ~f:(fun (exp, pvar, typ) -> (eval exp, pvar, typ)) c.captured_vars
          in
          Closure {c with captured_vars}
      | Const _
       -> e
      | Sizeof {typ= {desc= Tarray ({desc= Tint ik}, _, _)}; dynamic_length= Some l}
        when Typ.ikind_is_char ik && Config.curr_language_is Config.Clang
       -> eval l
      | Sizeof {typ= {desc= Tarray ({desc= Tint ik}, Some l, _)}}
        when Typ.ikind_is_char ik && Config.curr_language_is Config.Clang
       -> Const (Cint l)
      | Sizeof _
       -> e
      | Cast (_, e1)
       -> eval e1
      | UnOp (Unop.LNot, e1, topt) -> (
        match eval e1 with
        | Const Cint i when IntLit.iszero i
         -> Exp.one
        | Const Cint _
         -> Exp.zero
        | UnOp (LNot, e1', _)
         -> e1'
        | e1'
         -> if abs then Exp.get_undefined false else UnOp (LNot, e1', topt) )
      | UnOp (Neg, e1, topt) -> (
        match eval e1 with
        | UnOp (Neg, e2', _)
         -> e2'
        | Const Cint i
         -> Exp.int (IntLit.neg i)
        | Const Cfloat v
         -> Exp.float ~-.v
        | Var id
         -> UnOp (Neg, Var id, topt)
        | e1'
         -> if abs then Exp.get_undefined false else UnOp (Neg, e1', topt) )
      | UnOp (BNot, e1, topt) -> (
        match eval e1 with
        | UnOp (BNot, e2', _)
         -> e2'
        | Const Cint i
         -> Exp.int (IntLit.lognot i)
        | e1'
         -> if abs then Exp.get_undefined false else UnOp (BNot, e1', topt) )
      | BinOp (Le, e1, e2) -> (
        match (eval e1, eval e2) with
        | Const Cint n, Const Cint m
         -> Exp.bool (IntLit.leq n m)
        | Const Cfloat v, Const Cfloat w
         -> Exp.bool (v <= w)
        | BinOp (PlusA, e3, Const Cint n), Const Cint m
         -> BinOp (Le, e3, Exp.int (m -- n))
        | e1', e2'
         -> Exp.le e1' e2' )
      | BinOp (Lt, e1, e2) -> (
        match (eval e1, eval e2) with
        | Const Cint n, Const Cint m
         -> Exp.bool (IntLit.lt n m)
        | Const Cfloat v, Const Cfloat w
         -> Exp.bool (v < w)
        | Const Cint n, BinOp (MinusA, f1, f2)
         -> BinOp (Le, BinOp (MinusA, f2, f1), Exp.int (IntLit.minus_one -- n))
        | BinOp (MinusA, f1, f2), Const Cint n
         -> Exp.le (BinOp (MinusA, f1, f2)) (Exp.int (n -- IntLit.one))
        | BinOp (PlusA, e3, Const Cint n), Const Cint m
         -> BinOp (Lt, e3, Exp.int (m -- n))
        | e1', e2'
         -> Exp.lt e1' e2' )
      | BinOp (Ge, e1, e2)
       -> eval (Exp.le e2 e1)
      | BinOp (Gt, e1, e2)
       -> eval (Exp.lt e2 e1)
      | BinOp (Eq, e1, e2) -> (
        match (eval e1, eval e2) with
        | Const Cint n, Const Cint m
         -> Exp.bool (IntLit.eq n m)
        | Const Cfloat v, Const Cfloat w
         -> Exp.bool (Float.equal v w)
        | e1', e2'
         -> Exp.eq e1' e2' )
      | BinOp (Ne, e1, e2) -> (
        match (eval e1, eval e2) with
        | Const Cint n, Const Cint m
         -> Exp.bool (IntLit.neq n m)
        | Const Cfloat v, Const Cfloat w
         -> Exp.bool (v <> w)
        | e1', e2'
         -> Exp.ne e1' e2' )
      | BinOp (LAnd, e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const Cint i, _ when IntLit.iszero i
           -> e1'
          | Const Cint _, _
           -> e2'
          | _, Const Cint i when IntLit.iszero i
           -> e2'
          | _, Const Cint _
           -> e1'
          | _
           -> BinOp (LAnd, e1', e2') )
      | BinOp (LOr, e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const Cint i, _ when IntLit.iszero i
           -> e2'
          | Const Cint _, _
           -> e1'
          | _, Const Cint i when IntLit.iszero i
           -> e1'
          | _, Const Cint _
           -> e2'
          | _
           -> BinOp (LOr, e1', e2') )
      | BinOp (PlusPI, Lindex (ep, e1), e2)
       -> (* array access with pointer arithmetic *)
          let e' : Exp.t = BinOp (PlusA, e1, e2) in
          eval (Exp.Lindex (ep, e'))
      | BinOp (PlusPI, BinOp (PlusPI, e11, e12), e2)
       -> (* take care of pattern ((ptr + off1) + off2) *)
          (* progress: convert inner +I to +A *)
          let e2' : Exp.t = BinOp (PlusA, e12, e2) in
          eval (Exp.BinOp (PlusPI, e11, e2'))
      | BinOp ((PlusA as oplus), e1, e2) | BinOp ((PlusPI as oplus), e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          let isPlusA = Binop.equal oplus Binop.PlusA in
          let ominus = if isPlusA then Binop.MinusA else Binop.MinusPI in
          let ( +++ ) (x: Exp.t) (y: Exp.t) : Exp.t =
            match (x, y) with
            | _, Const Cint i when IntLit.iszero i
             -> x
            | Const Cint i, Const Cint j
             -> Const (Cint (IntLit.add i j))
            | _
             -> BinOp (oplus, x, y)
          in
          let ( --- ) (x: Exp.t) (y: Exp.t) : Exp.t =
            match (x, y) with
            | _, Const Cint i when IntLit.iszero i
             -> x
            | Const Cint i, Const Cint j
             -> Const (Cint (IntLit.sub i j))
            | _
             -> BinOp (ominus, x, y)
          in
          (* test if the extensible array at the end of [typ] has elements of type [elt] *)
          let extensible_array_element_typ_equal elt typ =
            Option.value_map ~f:(Typ.equal elt) ~default:false
              (Typ.Struct.get_extensible_array_element_typ ~lookup typ)
          in
          match (e1', e2') with
          (* pattern for arrays and extensible structs:
               sizeof(struct s {... t[l]}) + k * sizeof(t)) = sizeof(struct s {... t[l + k]}) *)
          | ( Sizeof ({typ; dynamic_length= len1_opt} as sizeof_data)
            , BinOp (Mult, len2, Sizeof {typ= elt; dynamic_length= None}) )
            when isPlusA && extensible_array_element_typ_equal elt typ
           -> let len = match len1_opt with Some len1 -> len1 +++ len2 | None -> len2 in
              Sizeof {sizeof_data with dynamic_length= Some len}
          | Const c, _ when Const.iszero_int_float c
           -> e2'
          | _, Const c when Const.iszero_int_float c
           -> e1'
          | Const Cint n, Const Cint m
           -> Exp.int (n ++ m)
          | Const Cfloat v, Const Cfloat w
           -> Exp.float (v +. w)
          | UnOp (Neg, f1, _), f2 | f2, UnOp (Neg, f1, _)
           -> BinOp (ominus, f2, f1)
          | BinOp (PlusA, e, Const Cint n1), Const Cint n2
          | BinOp (PlusPI, e, Const Cint n1), Const Cint n2
          | Const Cint n2, BinOp (PlusA, e, Const Cint n1)
          | Const Cint n2, BinOp (PlusPI, e, Const Cint n1)
           -> e +++ Exp.int (n1 ++ n2)
          | BinOp (MinusA, Const Cint n1, e), Const Cint n2
          | Const Cint n2, BinOp (MinusA, Const Cint n1, e)
           -> Exp.int (n1 ++ n2) --- e
          | BinOp (MinusA, e1, e2), e3
           -> (* (e1-e2)+e3 --> e1 + (e3-e2) *)
              (* progress: brings + to the outside *)
              eval (e1 +++ (e3 --- e2))
          | _, Const _
           -> e1' +++ e2'
          | Const _, _
           -> if isPlusA then e2' +++ e1' else e1' +++ e2'
          | Var _, Var _
           -> e1' +++ e2'
          | _
           -> if abs && isPlusA then Exp.get_undefined false
              else if abs && not isPlusA then e1' +++ Exp.get_undefined false
              else e1' +++ e2' )
      | BinOp ((MinusA as ominus), e1, e2) | BinOp ((MinusPI as ominus), e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          let isMinusA = Binop.equal ominus Binop.MinusA in
          let oplus = if isMinusA then Binop.PlusA else Binop.PlusPI in
          let ( +++ ) x y : Exp.t = BinOp (oplus, x, y) in
          let ( --- ) x y : Exp.t = BinOp (ominus, x, y) in
          if Exp.equal e1' e2' then Exp.zero
          else
            match (e1', e2') with
            | Const c, _ when Const.iszero_int_float c
             -> eval (Exp.UnOp (Neg, e2', None))
            | _, Const c when Const.iszero_int_float c
             -> e1'
            | Const Cint n, Const Cint m
             -> Exp.int (n -- m)
            | Const Cfloat v, Const Cfloat w
             -> Exp.float (v -. w)
            | _, UnOp (Neg, f2, _)
             -> eval (e1 +++ f2)
            | _, Const Cint n
             -> eval (e1' +++ Exp.int (IntLit.neg n))
            | Const _, _
             -> e1' --- e2'
            | Var _, Var _
             -> e1' --- e2'
            | _, _
             -> if abs then Exp.get_undefined false else e1' --- e2' )
      | BinOp (MinusPP, e1, e2)
       -> if abs then Exp.get_undefined false else BinOp (MinusPP, eval e1, eval e2)
      | BinOp (Mult, e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const c, _ when Const.iszero_int_float c
           -> Exp.zero
          | Const c, _ when Const.isone_int_float c
           -> e2'
          | Const c, _ when Const.isminusone_int_float c
           -> eval (Exp.UnOp (Neg, e2', None))
          | _, Const c when Const.iszero_int_float c
           -> Exp.zero
          | _, Const c when Const.isone_int_float c
           -> e1'
          | _, Const c when Const.isminusone_int_float c
           -> eval (Exp.UnOp (Neg, e1', None))
          | Const Cint n, Const Cint m
           -> Exp.int (IntLit.mul n m)
          | Const Cfloat v, Const Cfloat w
           -> Exp.float (v *. w)
          | Var _, Var _
           -> BinOp (Mult, e1', e2')
          | _, Sizeof _ | Sizeof _, _
           -> BinOp (Mult, e1', e2')
          | _, _
           -> if abs then Exp.get_undefined false else BinOp (Mult, e1', e2') )
      | BinOp (Div, e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | _, Const c when Const.iszero_int_float c
           -> Exp.get_undefined false
          | Const c, _ when Const.iszero_int_float c
           -> e1'
          | _, Const c when Const.isone_int_float c
           -> e1'
          | Const Cint n, Const Cint m
           -> Exp.int (IntLit.div n m)
          | Const Cfloat v, Const Cfloat w
           -> Exp.float (v /. w)
          | ( Sizeof {typ= {desc= Tarray (elt, _, _)}; dynamic_length= Some len}
            , Sizeof {typ= elt2; dynamic_length= None} )
          (* pattern: sizeof(elt[len]) / sizeof(elt) = len *)
            when Typ.equal elt elt2
           -> len
          | ( Sizeof {typ= {desc= Tarray (elt, Some len, _)}; dynamic_length= None}
            , Sizeof {typ= elt2; dynamic_length= None} )
          (* pattern: sizeof(elt[len]) / sizeof(elt) = len *)
            when Typ.equal elt elt2
           -> Const (Cint len)
          | _
           -> if abs then Exp.get_undefined false else BinOp (Div, e1', e2') )
      | BinOp (Mod, e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | _, Const Cint i when IntLit.iszero i
           -> Exp.get_undefined false
          | Const Cint i, _ when IntLit.iszero i
           -> e1'
          | _, Const Cint i when IntLit.isone i
           -> Exp.zero
          | Const Cint n, Const Cint m
           -> Exp.int (IntLit.rem n m)
          | _
           -> if abs then Exp.get_undefined false else BinOp (Mod, e1', e2') )
      | BinOp (Shiftlt, e1, e2)
       -> if abs then Exp.get_undefined false else BinOp (Shiftlt, eval e1, eval e2)
      | BinOp (Shiftrt, e1, e2)
       -> if abs then Exp.get_undefined false else BinOp (Shiftrt, eval e1, eval e2)
      | BinOp (BAnd, e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const Cint i, _ when IntLit.iszero i
           -> e1'
          | _, Const Cint i when IntLit.iszero i
           -> e2'
          | Const Cint i1, Const Cint i2
           -> Exp.int (IntLit.logand i1 i2)
          | _
           -> if abs then Exp.get_undefined false else BinOp (BAnd, e1', e2') )
      | BinOp (BOr, e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const Cint i, _ when IntLit.iszero i
           -> e2'
          | _, Const Cint i when IntLit.iszero i
           -> e1'
          | Const Cint i1, Const Cint i2
           -> Exp.int (IntLit.logor i1 i2)
          | _
           -> if abs then Exp.get_undefined false else BinOp (BOr, e1', e2') )
      | BinOp (BXor, e1, e2)
       -> (
          let e1' = eval e1 in
          let e2' = eval e2 in
          match (e1', e2') with
          | Const Cint i, _ when IntLit.iszero i
           -> e2'
          | _, Const Cint i when IntLit.iszero i
           -> e1'
          | Const Cint i1, Const Cint i2
           -> Exp.int (IntLit.logxor i1 i2)
          | _
           -> if abs then Exp.get_undefined false else BinOp (BXor, e1', e2') )
      | Exn _
       -> e
      | Lvar _
       -> e
      | Lfield (e1, fld, typ)
       -> let e1' = eval e1 in
          Lfield (e1', fld, typ)
      | Lindex (Lvar pv, e2)
        when false (* removed: it interferes with re-arrangement and error messages *)
       -> (* &x[n]  -->  &x + n *)
          eval (Exp.BinOp (PlusPI, Lvar pv, e2))
      | Lindex (BinOp (PlusPI, ep, e1), e2)
       -> (* array access with pointer arithmetic *)
          let e' : Exp.t = BinOp (PlusA, e1, e2) in
          eval (Exp.Lindex (ep, e'))
      | Lindex (e1, e2)
       -> let e1' = eval e1 in
          let e2' = eval e2 in
          Lindex (e1', e2')
    in
    let e' = eval e in
    (* L.d_str "sym_eval "; Sil.d_exp e; L.d_str" --> "; Sil.d_exp e'; L.d_ln (); *)
    e'

  let exp_normalize tenv sub exp =
    let exp' = Sil.exp_sub sub exp in
    if !Config.abs_val >= 1 then sym_eval tenv true exp' else sym_eval tenv false exp'

  let texp_normalize tenv sub (exp: Exp.t) : Exp.t =
    match exp with
    | Sizeof ({dynamic_length} as sizeof_data)
     -> Sizeof
          {sizeof_data with dynamic_length= Option.map ~f:(exp_normalize tenv sub) dynamic_length}
    | _
     -> exp_normalize tenv sub exp

  let exp_normalize_noabs tenv sub exp =
    Config.run_with_abs_val_equal_zero (exp_normalize tenv sub) exp

  (** Turn an inequality expression into an atom *)
  let mk_inequality tenv (e: Exp.t) : Sil.atom =
    match e with
    | BinOp (Le, base, Const Cint n)
     -> (
        (* base <= n case *)
        let nbase = exp_normalize_noabs tenv Sil.sub_empty base in
        match nbase with
        | BinOp (PlusA, base', Const Cint n')
         -> let new_offset = Exp.int (n -- n') in
            let new_e : Exp.t = BinOp (Le, base', new_offset) in
            Aeq (new_e, Exp.one)
        | BinOp (PlusA, Const Cint n', base')
         -> let new_offset = Exp.int (n -- n') in
            let new_e : Exp.t = BinOp (Le, base', new_offset) in
            Aeq (new_e, Exp.one)
        | BinOp (MinusA, base', Const Cint n')
         -> let new_offset = Exp.int (n ++ n') in
            let new_e : Exp.t = BinOp (Le, base', new_offset) in
            Aeq (new_e, Exp.one)
        | BinOp (MinusA, Const Cint n', base')
         -> let new_offset = Exp.int (n' -- n -- IntLit.one) in
            let new_e : Exp.t = BinOp (Lt, new_offset, base') in
            Aeq (new_e, Exp.one)
        | UnOp (Neg, new_base, _)
         -> (* In this case, base = -new_base. Construct -n-1 < new_base. *)
            let new_offset = Exp.int (IntLit.zero -- n -- IntLit.one) in
            let new_e : Exp.t = BinOp (Lt, new_offset, new_base) in
            Aeq (new_e, Exp.one)
        | _
         -> Aeq (e, Exp.one) )
    | BinOp (Lt, Const Cint n, base)
     -> (
        (* n < base case *)
        let nbase = exp_normalize_noabs tenv Sil.sub_empty base in
        match nbase with
        | BinOp (PlusA, base', Const Cint n')
         -> let new_offset = Exp.int (n -- n') in
            let new_e : Exp.t = BinOp (Lt, new_offset, base') in
            Aeq (new_e, Exp.one)
        | BinOp (PlusA, Const Const.Cint n', base')
         -> let new_offset = Exp.int (n -- n') in
            let new_e : Exp.t = BinOp (Lt, new_offset, base') in
            Aeq (new_e, Exp.one)
        | BinOp (MinusA, base', Const Cint n')
         -> let new_offset = Exp.int (n ++ n') in
            let new_e : Exp.t = BinOp (Lt, new_offset, base') in
            Aeq (new_e, Exp.one)
        | BinOp (MinusA, Const Cint n', base')
         -> let new_offset = Exp.int (n' -- n -- IntLit.one) in
            let new_e : Exp.t = BinOp (Le, base', new_offset) in
            Aeq (new_e, Exp.one)
        | UnOp (Neg, new_base, _)
         -> (* In this case, base = -new_base. Construct new_base <= -n-1 *)
            let new_offset = Exp.int (IntLit.zero -- n -- IntLit.one) in
            let new_e : Exp.t = BinOp (Le, new_base, new_offset) in
            Aeq (new_e, Exp.one)
        | _
         -> Aeq (e, Exp.one) )
    | _
     -> Aeq (e, Exp.one)

  (** Normalize an inequality *)
  let inequality_normalize tenv (a: Sil.atom) =
    (* turn an expression into a triple (pos,neg,off) of positive and negative occurrences, and
       integer offset representing inequality [sum(pos) - sum(neg) + off <= 0] *)
    let rec exp_to_posnegoff (e: Exp.t) =
      match e with
      | Const Cint n
       -> ([], [], n)
      | BinOp (PlusA, e1, e2) | BinOp (PlusPI, e1, e2)
       -> let pos1, neg1, n1 = exp_to_posnegoff e1 in
          let pos2, neg2, n2 = exp_to_posnegoff e2 in
          (pos1 @ pos2, neg1 @ neg2, n1 ++ n2)
      | BinOp (MinusA, e1, e2) | BinOp (MinusPI, e1, e2) | BinOp (MinusPP, e1, e2)
       -> let pos1, neg1, n1 = exp_to_posnegoff e1 in
          let pos2, neg2, n2 = exp_to_posnegoff e2 in
          (pos1 @ neg2, neg1 @ pos2, n1 -- n2)
      | UnOp (Neg, e1, _)
       -> let pos1, neg1, n1 = exp_to_posnegoff e1 in
          (neg1, pos1, IntLit.zero -- n1)
      | _
       -> ([e], [], IntLit.zero)
    in
    (* sort and filter out expressions appearing in both the positive and negative part *)
    let normalize_posnegoff (pos, neg, off) =
      let pos' = List.sort ~cmp:Exp.compare pos in
      let neg' = List.sort ~cmp:Exp.compare neg in
      let rec combine pacc nacc = function
        | x :: ps, y :: ng -> (
          match Exp.compare x y with
          | n when n < 0
           -> combine (x :: pacc) nacc (ps, y :: ng)
          | 0
           -> combine pacc nacc (ps, ng)
          | _
           -> combine pacc (y :: nacc) (x :: ps, ng) )
        | ps, ng
         -> (List.rev_append pacc ps, List.rev_append nacc ng)
      in
      let pos'', neg'' = combine [] [] (pos', neg') in
      (pos'', neg'', off)
    in
    (* turn a non-empty list of expressions into a sum expression *)
    let rec exp_list_to_sum : Exp.t list -> Exp.t = function
      | []
       -> assert false
      | [e]
       -> e
      | e :: el
       -> BinOp (PlusA, e, exp_list_to_sum el)
    in
    let norm_from_exp e : Exp.t =
      match normalize_posnegoff (exp_to_posnegoff e) with
      | [], [], n
       -> BinOp (Le, Exp.int n, Exp.zero)
      | [], neg, n
       -> BinOp (Lt, Exp.int (n -- IntLit.one), exp_list_to_sum neg)
      | pos, [], n
       -> BinOp (Le, exp_list_to_sum pos, Exp.int (IntLit.zero -- n))
      | pos, neg, n
       -> let lhs_e : Exp.t = BinOp (MinusA, exp_list_to_sum pos, exp_list_to_sum neg) in
          BinOp (Le, lhs_e, Exp.int (IntLit.zero -- n))
    in
    let ineq =
      match a with Aeq (ineq, Const Cint i) when IntLit.isone i -> ineq | _ -> assert false
    in
    match ineq with
    | BinOp (Le, e1, e2)
     -> let e : Exp.t = BinOp (MinusA, e1, e2) in
        mk_inequality tenv (norm_from_exp e)
    | BinOp (Lt, e1, e2)
     -> let e : Exp.t = BinOp (MinusA, BinOp (MinusA, e1, e2), Exp.minus_one) in
        mk_inequality tenv (norm_from_exp e)
    | _
     -> a

  (** Normalize an atom.
      We keep the convention that inequalities with constants
      are only of the form [e <= n] and [n < e]. *)
  let atom_normalize tenv sub a0 =
    let a = Sil.atom_sub sub a0 in
    let rec normalize_eq (eq: Exp.t * Exp.t) =
      match eq with
      | BinOp (PlusA, e1, Const Cint n1), Const Cint n2
      (* e1+n1==n2 ---> e1==n2-n1 *)
      | BinOp (PlusPI, e1, Const Cint n1), Const Cint n2
       -> (e1, Exp.int (n2 -- n1))
      | BinOp (MinusA, e1, Const Cint n1), Const Cint n2
      (* e1-n1==n2 ---> e1==n1+n2 *)
      | BinOp (MinusPI, e1, Const Cint n1), Const Cint n2
       -> (e1, Exp.int (n1 ++ n2))
      | BinOp (MinusA, Const Cint n1, e1), Const Cint n2
       -> (* n1-e1 == n2 -> e1==n1-n2 *)
          (e1, Exp.int (n1 -- n2))
      | Lfield (e1', fld1, _), Lfield (e2', fld2, _)
       -> if Typ.Fieldname.equal fld1 fld2 then normalize_eq (e1', e2') else eq
      | Lindex (e1', idx1), Lindex (e2', idx2)
       -> if Exp.equal idx1 idx2 then normalize_eq (e1', e2')
          else if Exp.equal e1' e2' then normalize_eq (idx1, idx2)
          else eq
      | _
       -> eq
    in
    let handle_unary_negation (e1: Exp.t) (e2: Exp.t) =
      match (e1, e2) with
      | UnOp (LNot, e1', _), Const Cint i
      | Const Cint i, UnOp (LNot, e1', _)
        when IntLit.iszero i
       -> (e1', Exp.zero, true)
      | _
       -> (e1, e2, false)
    in
    let handle_boolean_operation from_equality e1 e2 : Sil.atom =
      let ne1 = exp_normalize tenv sub e1 in
      let ne2 = exp_normalize tenv sub e2 in
      let ne1', ne2', op_negated = handle_unary_negation ne1 ne2 in
      let e1', e2' = normalize_eq (ne1', ne2') in
      let e1'', e2'' = exp_reorder e1' e2' in
      let use_equality = if op_negated then not from_equality else from_equality in
      if use_equality then Aeq (e1'', e2'') else Aneq (e1'', e2'')
    in
    let a' : Sil.atom =
      match a with
      | Aeq (e1, e2)
       -> handle_boolean_operation true e1 e2
      | Aneq (e1, e2)
       -> handle_boolean_operation false e1 e2
      | Apred (a, es)
       -> Apred (a, List.map ~f:(fun e -> exp_normalize tenv sub e) es)
      | Anpred (a, es)
       -> Anpred (a, List.map ~f:(fun e -> exp_normalize tenv sub e) es)
    in
    if atom_is_inequality a' then inequality_normalize tenv a' else a'

  let normalize_and_strengthen_atom tenv (p: normal t) (a: Sil.atom) : Sil.atom =
    let a' = atom_normalize tenv (`Exp p.sub) a in
    match a' with
    | Aeq (BinOp (Le, Var id, Const Cint n), Const Cint i) when IntLit.isone i
     -> let lower = Exp.int (n -- IntLit.one) in
        let a_lower : Sil.atom = Aeq (BinOp (Lt, lower, Var id), Exp.one) in
        if not (List.mem ~equal:Sil.equal_atom p.pi a_lower) then a' else Aeq (Var id, Exp.int n)
    | Aeq (BinOp (Lt, Const Cint n, Var id), Const Cint i) when IntLit.isone i
     -> let upper = Exp.int (n ++ IntLit.one) in
        let a_upper : Sil.atom = Aeq (BinOp (Le, Var id, upper), Exp.one) in
        if not (List.mem ~equal:Sil.equal_atom p.pi a_upper) then a' else Aeq (Var id, upper)
    | Aeq (BinOp (Ne, e1, e2), Const Cint i) when IntLit.isone i
     -> Aneq (e1, e2)
    | _
     -> a'

  let rec strexp_normalize tenv sub (se: Sil.strexp) : Sil.strexp =
    match se with
    | Eexp (e, inst)
     -> Eexp (exp_normalize tenv sub e, inst)
    | Estruct (fld_cnts, inst) -> (
      match fld_cnts with
      | []
       -> se
      | _
       -> let fld_cnts' =
            List.map ~f:(fun (fld, cnt) -> (fld, strexp_normalize tenv sub cnt)) fld_cnts
          in
          let fld_cnts'' = List.sort ~cmp:[%compare : Typ.Fieldname.t * Sil.strexp] fld_cnts' in
          Estruct (fld_cnts'', inst) )
    | Earray (len, idx_cnts, inst)
     -> let len' = exp_normalize_noabs tenv sub len in
        match idx_cnts with
        | []
         -> if Exp.equal len len' then se else Earray (len', idx_cnts, inst)
        | _
         -> let idx_cnts' =
              List.map
                ~f:(fun (idx, cnt) ->
                  let idx' = exp_normalize tenv sub idx in
                  (idx', strexp_normalize tenv sub cnt))
                idx_cnts
            in
            let idx_cnts'' = List.sort ~cmp:[%compare : Exp.t * Sil.strexp] idx_cnts' in
            Earray (len', idx_cnts'', inst)

  (** Exp.Construct a pointsto. *)
  let mk_ptsto tenv lexp sexp te : Sil.hpred =
    let nsexp = strexp_normalize tenv Sil.sub_empty sexp in
    Hpointsto (lexp, nsexp, te)

  (** Construct a points-to predicate for an expression using
      either the provided expression [name] as
      base for fresh identifiers. If [expand_structs] is true,
      initialize the fields of structs with fresh variables. *)
  let mk_ptsto_exp tenv struct_init_mode (exp, (te: Exp.t), expo) inst : Sil.hpred =
    let default_strexp () : Sil.strexp =
      match te with
      | Sizeof {typ; dynamic_length}
       -> create_strexp_of_type tenv struct_init_mode typ dynamic_length inst
      | Var _
       -> Estruct ([], inst)
      | te
       -> L.internal_error "trying to create ptsto with type: %a@." (Sil.pp_texp_full Pp.text) te ;
          assert false
    in
    let strexp : Sil.strexp =
      match expo with Some e -> Eexp (e, inst) | None -> default_strexp ()
    in
    mk_ptsto tenv exp strexp te

  let rec hpred_normalize tenv sub (hpred: Sil.hpred) : Sil.hpred =
    let replace_hpred hpred' =
      L.d_strln "found array with sizeof(..) size" ;
      L.d_str "converting original hpred: " ;
      Sil.d_hpred hpred ;
      L.d_ln () ;
      L.d_str "into the following: " ;
      Sil.d_hpred hpred' ;
      L.d_ln () ;
      hpred'
    in
    match hpred with
    | Hpointsto (root, cnt, te)
     -> (
        let normalized_root = exp_normalize tenv sub root in
        let normalized_cnt = strexp_normalize tenv sub cnt in
        let normalized_te = texp_normalize tenv sub te in
        match (normalized_cnt, normalized_te) with
        | Earray ((Exp.Sizeof _ as size), [], inst), Sizeof {typ= {desc= Tarray _}}
         -> (* check for an empty array whose size expression is (Sizeof type), and turn the array
                 into a strexp of the given type *)
            let hpred' = mk_ptsto_exp tenv Fld_init (root, size, None) inst in
            replace_hpred hpred'
        | ( Earray
              (BinOp (Mult, Sizeof {typ= t; dynamic_length= None; subtype= st1}, x), esel, inst)
          , Sizeof {typ= {desc= Tarray (elt, _, _)} as arr} )
          when Typ.equal t elt
         -> let dynamic_length = Some x in
            let sizeof_data = {Exp.typ= arr; nbytes= None; dynamic_length; subtype= st1} in
            let hpred' = mk_ptsto_exp tenv Fld_init (root, Sizeof sizeof_data, None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | ( Earray (BinOp (Mult, x, Sizeof {typ; dynamic_length= None; subtype}), esel, inst)
          , Sizeof {typ= {desc= Tarray (elt, _, _)} as arr} )
          when Typ.equal typ elt
         -> let sizeof_data = {Exp.typ= arr; nbytes= None; dynamic_length= Some x; subtype} in
            let hpred' = mk_ptsto_exp tenv Fld_init (root, Sizeof sizeof_data, None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | ( Earray (BinOp (Mult, Sizeof {typ; dynamic_length= Some len; subtype}, x), esel, inst)
          , Sizeof {typ= {desc= Tarray (elt, _, _)} as arr} )
          when Typ.equal typ elt
         -> let sizeof_data =
              {Exp.typ= arr; nbytes= None; dynamic_length= Some (Exp.BinOp (Mult, x, len)); subtype}
            in
            let hpred' = mk_ptsto_exp tenv Fld_init (root, Sizeof sizeof_data, None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | ( Earray (BinOp (Mult, x, Sizeof {typ; dynamic_length= Some len; subtype}), esel, inst)
          , Sizeof {typ= {desc= Tarray (elt, _, _)} as arr} )
          when Typ.equal typ elt
         -> let sizeof_data =
              {Exp.typ= arr; nbytes= None; dynamic_length= Some (Exp.BinOp (Mult, x, len)); subtype}
            in
            let hpred' = mk_ptsto_exp tenv Fld_init (root, Sizeof sizeof_data, None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | _
         -> Hpointsto (normalized_root, normalized_cnt, normalized_te) )
    | Hlseg (k, para, e1, e2, elist)
     -> let normalized_e1 = exp_normalize tenv sub e1 in
        let normalized_e2 = exp_normalize tenv sub e2 in
        let normalized_elist = List.map ~f:(exp_normalize tenv sub) elist in
        let normalized_para = hpara_normalize tenv para in
        Hlseg (k, normalized_para, normalized_e1, normalized_e2, normalized_elist)
    | Hdllseg (k, para, e1, e2, e3, e4, elist)
     -> let norm_e1 = exp_normalize tenv sub e1 in
        let norm_e2 = exp_normalize tenv sub e2 in
        let norm_e3 = exp_normalize tenv sub e3 in
        let norm_e4 = exp_normalize tenv sub e4 in
        let norm_elist = List.map ~f:(exp_normalize tenv sub) elist in
        let norm_para = hpara_dll_normalize tenv para in
        Hdllseg (k, norm_para, norm_e1, norm_e2, norm_e3, norm_e4, norm_elist)

  and hpara_normalize tenv (para: Sil.hpara) =
    let normalized_body = List.map ~f:(hpred_normalize tenv Sil.sub_empty) para.body in
    let sorted_body = List.sort ~cmp:Sil.compare_hpred normalized_body in
    {para with body= sorted_body}

  and hpara_dll_normalize tenv (para: Sil.hpara_dll) =
    let normalized_body = List.map ~f:(hpred_normalize tenv Sil.sub_empty) para.body_dll in
    let sorted_body = List.sort ~cmp:Sil.compare_hpred normalized_body in
    {para with body_dll= sorted_body}

  let sigma_normalize tenv sub sigma =
    let sigma' =
      List.stable_sort ~cmp:Sil.compare_hpred (List.map ~f:(hpred_normalize tenv sub) sigma)
    in
    if equal_sigma sigma sigma' then sigma else sigma'

  let pi_tighten_ineq tenv pi =
    let ineq_list, nonineq_list = List.partition_tf ~f:atom_is_inequality pi in
    let diseq_list =
      let get_disequality_info acc (a: Sil.atom) =
        match a with Aneq (Const Cint n, e) | Aneq (e, Const Cint n) -> (e, n) :: acc | _ -> acc
      in
      List.fold ~f:get_disequality_info ~init:[] nonineq_list
    in
    let is_neq e n =
      List.exists ~f:(fun (e', n') -> Exp.equal e e' && IntLit.eq n n') diseq_list
    in
    let le_list_tightened =
      let get_le_inequality_info acc a =
        match atom_exp_le_const a with Some (e, n) -> (e, n) :: acc | _ -> acc
      in
      let rec le_tighten le_list_done = function
        | []
         -> List.rev le_list_done
        | (e, n) :: le_list_todo
         -> (* e <= n *)
            if is_neq e n then le_tighten le_list_done ((e, n -- IntLit.one) :: le_list_todo)
            else le_tighten ((e, n) :: le_list_done) le_list_todo
      in
      let le_list = List.rev (List.fold ~f:get_le_inequality_info ~init:[] ineq_list) in
      le_tighten [] le_list
    in
    let lt_list_tightened =
      let get_lt_inequality_info acc a =
        match atom_const_lt_exp a with Some (n, e) -> (n, e) :: acc | _ -> acc
      in
      let rec lt_tighten lt_list_done = function
        | []
         -> List.rev lt_list_done
        | (n, e) :: lt_list_todo
         -> (* n < e *)
            let n_plus_one = n ++ IntLit.one in
            if is_neq e n_plus_one then
              lt_tighten lt_list_done ((n ++ IntLit.one, e) :: lt_list_todo)
            else lt_tighten ((n, e) :: lt_list_done) lt_list_todo
      in
      let lt_list = List.rev (List.fold ~f:get_lt_inequality_info ~init:[] ineq_list) in
      lt_tighten [] lt_list
    in
    let ineq_list' =
      let le_ineq_list =
        List.map ~f:(fun (e, n) -> mk_inequality tenv (BinOp (Le, e, Exp.int n))) le_list_tightened
      in
      let lt_ineq_list =
        List.map ~f:(fun (n, e) -> mk_inequality tenv (BinOp (Lt, Exp.int n, e))) lt_list_tightened
      in
      le_ineq_list @ lt_ineq_list
    in
    let nonineq_list' =
      List.filter
        ~f:(fun (a: Sil.atom) ->
          match a with
          | Aneq (Const Cint n, e) | Aneq (e, Const Cint n)
           -> not
                (List.exists
                   ~f:(fun (e', n') -> Exp.equal e e' && IntLit.lt n' n)
                   le_list_tightened)
              && not
                   (List.exists
                      ~f:(fun (n', e') -> Exp.equal e e' && IntLit.leq n n')
                      lt_list_tightened)
          | _
           -> true)
        nonineq_list
    in
    (ineq_list', nonineq_list')

  (** Normalization of pi.
      The normalization filters out obviously - true disequalities, such as e <> e + 1. *)
  let pi_normalize tenv sub sigma pi0 =
    let pi = List.map ~f:(atom_normalize tenv sub) pi0 in
    let ineq_list, nonineq_list = pi_tighten_ineq tenv pi in
    let syntactically_different : Exp.t * Exp.t -> bool = function
      | BinOp (op1, e1, Const c1), BinOp (op2, e2, Const c2) when Exp.equal e1 e2
       -> Binop.equal op1 op2 && Binop.injective op1 && not (Const.equal c1 c2)
      | e1, BinOp (op2, e2, Const c2) when Exp.equal e1 e2
       -> Binop.injective op2 && Binop.is_zero_runit op2 && not (Const.equal (Cint IntLit.zero) c2)
      | BinOp (op1, e1, Const c1), e2 when Exp.equal e1 e2
       -> Binop.injective op1 && Binop.is_zero_runit op1 && not (Const.equal (Cint IntLit.zero) c1)
      | _
       -> false
    in
    let filter_useful_atom : Sil.atom -> bool =
      let unsigned_exps = (lazy (sigma_get_unsigned_exps sigma)) in
      function
        | Aneq ((Var _ as e), Const Cint n) when IntLit.isnegative n
         -> not (List.exists ~f:(Exp.equal e) (Lazy.force unsigned_exps))
        | Aneq (e1, e2)
         -> not (syntactically_different (e1, e2))
        | Aeq (Const c1, Const c2)
         -> not (Const.equal c1 c2)
        | _
         -> true
    in
    let pi' =
      List.stable_sort ~cmp:Sil.compare_atom
        (List.filter ~f:filter_useful_atom nonineq_list @ ineq_list)
    in
    let pi'' = pi_sorted_remove_redundant pi' in
    if equal_pi pi0 pi'' then pi0 else pi''

  (** normalize the footprint part, and rename any primed vars
      in the footprint with fresh footprint vars *)
  let footprint_normalize tenv prop =
    let nsigma = sigma_normalize tenv Sil.sub_empty prop.sigma_fp in
    let npi = pi_normalize tenv Sil.sub_empty nsigma prop.pi_fp in
    let fp_vars =
      let fav = pi_fav npi in
      sigma_fav_add fav nsigma ; fav
    in
    (* TODO (t4893479): make this check less angelic *)
    if Sil.fav_exists fp_vars Ident.is_normal && not Config.angelic_execution then (
      L.d_strln "footprint part contains normal variables" ;
      d_pi npi ;
      L.d_ln () ;
      d_sigma nsigma ;
      L.d_ln () ;
      assert false ) ;
    Sil.fav_filter_ident fp_vars Ident.is_primed ;
    (* only keep primed vars *)
    let npi', nsigma' =
      if Sil.fav_is_empty fp_vars then (npi, nsigma)
      else
        (* replace primed vars by fresh footprint vars *)
        let ids_primed = Sil.fav_to_list fp_vars in
        let ids_footprint =
          List.map ~f:(fun id -> (id, Ident.create_fresh Ident.kfootprint)) ids_primed
        in
        let ren_sub =
          Sil.subst_of_list (List.map ~f:(fun (id1, id2) -> (id1, Exp.Var id2)) ids_footprint)
        in
        let nsigma' = sigma_normalize tenv Sil.sub_empty (sigma_sub ren_sub nsigma) in
        let npi' = pi_normalize tenv Sil.sub_empty nsigma' (pi_sub ren_sub npi) in
        (npi', nsigma')
    in
    set prop ~pi_fp:npi' ~sigma_fp:nsigma'

  (** This function assumes that if (x,Exp.Var(y)) in sub, then compare x y = 1 *)
  let sub_normalize sub =
    let f (id, e) = not (Ident.is_primed id) && not (Sil.ident_in_exp id e) in
    let sub' = Sil.sub_filter_pair ~f sub in
    if Sil.equal_exp_subst sub sub' then sub else sub'

  (** Conjoin a pure atomic predicate by normal conjunction. *)
  let rec prop_atom_and tenv ?(footprint= false) (p: normal t) a : normal t =
    let a' = normalize_and_strengthen_atom tenv p a in
    if List.mem ~equal:Sil.equal_atom p.pi a' then p
    else
      let p' =
        match a' with
        | Aeq (Var i, e) when Sil.ident_in_exp i e
         -> p
        | Aeq (Var i, e)
         -> let sub_list = [(i, e)] in
            let mysub = Sil.exp_subst_of_list sub_list in
            let p_sub = Sil.sub_filter (fun i' -> not (Ident.equal i i')) p.sub in
            let exp_sub' =
              Sil.sub_join mysub (Sil.sub_range_map (Sil.exp_sub (`Exp mysub)) p_sub)
            in
            let sub' = `Exp exp_sub' in
            let nsub', npi', nsigma' =
              let nsigma' = sigma_normalize tenv sub' p.sigma in
              (sub_normalize exp_sub', pi_normalize tenv sub' nsigma' p.pi, nsigma')
            in
            let eqs_zero, nsigma'' = sigma_remove_emptylseg nsigma' in
            let p' = unsafe_cast_to_normal (set p ~sub:nsub' ~pi:npi' ~sigma:nsigma'') in
            List.fold ~f:(prop_atom_and tenv ~footprint) ~init:p' eqs_zero
        | Aeq (e1, e2) when Exp.equal e1 e2
         -> p
        | Aneq (e1, e2)
         -> let sigma' = sigma_intro_nonemptylseg e1 e2 p.sigma in
            let pi' = pi_normalize tenv (`Exp p.sub) sigma' (a' :: p.pi) in
            unsafe_cast_to_normal (set p ~pi:pi' ~sigma:sigma')
        | _
         -> let pi' = pi_normalize tenv (`Exp p.sub) p.sigma (a' :: p.pi) in
            unsafe_cast_to_normal (set p ~pi:pi')
      in
      if not footprint then p'
      else
        let fav_a' = Sil.atom_fav a' in
        let fav_nofootprint_a' =
          Sil.fav_copy_filter_ident fav_a' (fun id -> not (Ident.is_footprint id))
        in
        let predicate_warning = not (Sil.fav_is_empty fav_nofootprint_a') in
        let p'' =
          if predicate_warning then footprint_normalize tenv p'
          else
            match a' with
            | Aeq (Exp.Var i, e) when not (Sil.ident_in_exp i e)
             -> let mysub = Sil.subst_of_list [(i, e)] in
                let sigma_fp' = sigma_normalize tenv mysub p'.sigma_fp in
                let pi_fp' = a' :: pi_normalize tenv mysub sigma_fp' p'.pi_fp in
                footprint_normalize tenv (set p' ~pi_fp:pi_fp' ~sigma_fp:sigma_fp')
            | _
             -> footprint_normalize tenv (set p' ~pi_fp:(a' :: p'.pi_fp))
        in
        if predicate_warning then (
          L.d_warning "dropping non-footprint " ; Sil.d_atom a' ; L.d_ln () ) ;
        unsafe_cast_to_normal p''

  (** normalize a prop *)
  let normalize tenv (eprop: 'a t) : normal t =
    let p0 =
      unsafe_cast_to_normal (set prop_emp ~sigma:(sigma_normalize tenv Sil.sub_empty eprop.sigma))
    in
    let nprop = List.fold ~f:(prop_atom_and tenv) ~init:p0 (get_pure_extended eprop) in
    unsafe_cast_to_normal
      (footprint_normalize tenv (set nprop ~pi_fp:eprop.pi_fp ~sigma_fp:eprop.sigma_fp))
end

(* End of module Normalize *)

let exp_normalize_prop tenv prop exp =
  Config.run_with_abs_val_equal_zero (Normalize.exp_normalize tenv (`Exp prop.sub)) exp

let lexp_normalize_prop tenv p lexp =
  let root = Exp.root_of_lexp lexp in
  let offsets = Sil.exp_get_offsets lexp in
  let nroot = exp_normalize_prop tenv p root in
  let noffsets =
    List.map
      ~f:(fun (n: Sil.offset) ->
        match n with Off_fld _ -> n | Off_index e -> Sil.Off_index (exp_normalize_prop tenv p e))
      offsets
  in
  Sil.exp_add_offsets nroot noffsets

let atom_normalize_prop tenv prop atom =
  Config.run_with_abs_val_equal_zero (Normalize.atom_normalize tenv (`Exp prop.sub)) atom

let strexp_normalize_prop tenv prop strexp =
  Config.run_with_abs_val_equal_zero (Normalize.strexp_normalize tenv (`Exp prop.sub)) strexp

let hpred_normalize_prop tenv prop hpred =
  Config.run_with_abs_val_equal_zero (Normalize.hpred_normalize tenv (`Exp prop.sub)) hpred

let sigma_normalize_prop tenv prop sigma =
  Config.run_with_abs_val_equal_zero (Normalize.sigma_normalize tenv (`Exp prop.sub)) sigma

let pi_normalize_prop tenv prop pi =
  Config.run_with_abs_val_equal_zero (Normalize.pi_normalize tenv (`Exp prop.sub) prop.sigma) pi

let sigma_replace_exp tenv epairs sigma =
  let sigma' = List.map ~f:(Sil.hpred_replace_exp epairs) sigma in
  Normalize.sigma_normalize tenv Sil.sub_empty sigma'

(** Construct an atom. *)
let mk_atom tenv atom =
  Config.run_with_abs_val_equal_zero
    (fun () -> Normalize.atom_normalize tenv Sil.sub_empty atom)
    ()

(** Exp.Construct a disequality. *)
let mk_neq tenv e1 e2 = mk_atom tenv (Aneq (e1, e2))

(** Exp.Construct an equality. *)
let mk_eq tenv e1 e2 = mk_atom tenv (Aeq (e1, e2))

(** Construct a pred. *)
let mk_pred tenv a es = mk_atom tenv (Apred (a, es))

(** Construct a negated pred. *)
let mk_npred tenv a es = mk_atom tenv (Anpred (a, es))

(** Exp.Construct a lseg predicate *)
let mk_lseg tenv k para e_start e_end es_shared : Sil.hpred =
  let npara = Normalize.hpara_normalize tenv para in
  Hlseg (k, npara, e_start, e_end, es_shared)

(** Exp.Construct a dllseg predicate *)
let mk_dllseg tenv k para exp_iF exp_oB exp_oF exp_iB exps_shared : Sil.hpred =
  let npara = Normalize.hpara_dll_normalize tenv para in
  Hdllseg (k, npara, exp_iF, exp_oB, exp_oF, exp_iB, exps_shared)

(** Exp.Construct a hpara *)
let mk_hpara tenv root next svars evars body =
  let para = {Sil.root= root; next; svars; evars; body} in
  Normalize.hpara_normalize tenv para

(** Exp.Construct a dll_hpara *)
let mk_dll_hpara tenv iF oB oF svars evars body =
  let para =
    {Sil.cell= iF; blink= oB; flink= oF; svars_dll= svars; evars_dll= evars; body_dll= body}
  in
  Normalize.hpara_dll_normalize tenv para

(** Construct a points-to predicate for a single program variable.
    If [expand_structs] is true, initialize the fields of structs with fresh variables. *)
let mk_ptsto_lvar tenv expand_structs inst ((pvar: Pvar.t), texp, expo) : Sil.hpred =
  Normalize.mk_ptsto_exp tenv expand_structs (Lvar pvar, texp, expo) inst

(** Conjoin [exp1]=[exp2] with a symbolic heap [prop]. *)
let conjoin_eq tenv ?(footprint= false) exp1 exp2 prop =
  Normalize.prop_atom_and tenv ~footprint prop (Aeq (exp1, exp2))

(** Conjoin [exp1!=exp2] with a symbolic heap [prop]. *)
let conjoin_neq tenv ?(footprint= false) exp1 exp2 prop =
  Normalize.prop_atom_and tenv ~footprint prop (Aneq (exp1, exp2))

(** Reset every inst in the prop using the given map *)
let prop_reset_inst inst_map prop =
  let sigma' = List.map ~f:(Sil.hpred_instmap inst_map) prop.sigma in
  let sigma_fp' = List.map ~f:(Sil.hpred_instmap inst_map) prop.sigma_fp in
  set prop ~sigma:sigma' ~sigma_fp:sigma_fp'

(** {1 Functions for transforming footprints into propositions.} *)

(** The ones used for abstraction add/remove local stacks in order to
    stop the firing of some abstraction rules. The other usual
    transforation functions do not use this hack. *)

(** Extract the footprint and return it as a prop *)
let extract_footprint p = set prop_emp ~pi:p.pi_fp ~sigma:p.sigma_fp

(** Extract the (footprint,current) pair *)
let extract_spec (p: normal t) : normal t * normal t =
  let pre = extract_footprint p in
  let post = set p ~pi_fp:[] ~sigma_fp:[] in
  (unsafe_cast_to_normal pre, unsafe_cast_to_normal post)

(** [prop_set_fooprint p p_foot] sets proposition [p_foot] as footprint of [p]. *)
let prop_set_footprint p p_foot =
  let pi =
    List.map ~f:(fun (i, e) -> Sil.Aeq (Var i, e)) (Sil.sub_to_list p_foot.sub) @ p_foot.pi
  in
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
    Stack.clear stack ;
    List.iter ~f:(fun e -> Stack.push stack e) (List.rev es)

  let final () = Stack.clear stack

  let is_empty () = Stack.is_empty stack

  let push e = Stack.push stack e

  let pop () = Stack.pop_exn stack
end

let sigma_get_start_lexps_sort sigma =
  let exp_compare_neg e1 e2 = -Exp.compare e1 e2 in
  let filter e = Sil.fav_for_all (Sil.exp_fav e) Ident.is_normal in
  let lexps = Sil.hpred_list_get_lexps filter sigma in
  List.sort ~cmp:exp_compare_neg lexps

let sigma_dfs_sort tenv sigma =
  let init () =
    let start_lexps = sigma_get_start_lexps_sort sigma in
    ExpStack.init start_lexps
  in
  let final () = ExpStack.final () in
  let rec handle_strexp (se: Sil.strexp) =
    match se with
    | Eexp (e, _)
     -> ExpStack.push e
    | Estruct (fld_se_list, _)
     -> List.iter ~f:(fun (_, se) -> handle_strexp se) fld_se_list
    | Earray (_, idx_se_list, _)
     -> List.iter ~f:(fun (_, se) -> handle_strexp se) idx_se_list
  in
  let rec handle_e visited seen e (sigma: sigma) =
    match sigma with
    | []
     -> (visited, List.rev seen)
    | hpred :: cur ->
      match hpred with
      | Hpointsto (e', se, _) when Exp.equal e e'
       -> handle_strexp se ; (hpred :: visited, List.rev_append cur seen)
      | Hlseg (_, _, root, next, shared) when Exp.equal e root
       -> List.iter ~f:ExpStack.push (next :: shared) ;
          (hpred :: visited, List.rev_append cur seen)
      | Hdllseg (_, _, iF, oB, oF, iB, shared) when Exp.equal e iF || Exp.equal e iB
       -> List.iter ~f:ExpStack.push (oB :: oF :: shared) ;
          (hpred :: visited, List.rev_append cur seen)
      | _
       -> handle_e visited (hpred :: seen) e cur
  in
  let rec handle_sigma visited = function
    | []
     -> List.rev visited
    | cur
     -> if ExpStack.is_empty () then
          let cur' = Normalize.sigma_normalize tenv Sil.sub_empty cur in
          List.rev_append cur' visited
        else
          let e = ExpStack.pop () in
          let visited', cur' = handle_e visited [] e cur in
          handle_sigma visited' cur'
  in
  init () ;
  let sigma' = handle_sigma [] sigma in
  final () ; sigma'

let prop_dfs_sort tenv p =
  let sigma = p.sigma in
  let sigma' = sigma_dfs_sort tenv sigma in
  let sigma_fp = p.sigma_fp in
  let sigma_fp' = sigma_dfs_sort tenv sigma_fp in
  let p' = set p ~sigma:sigma' ~sigma_fp:sigma_fp' in
  (* L.out "@[<2>P SORTED:@\n%a@\n@." pp_prop p'; *)
  p'

let prop_fav_add_dfs tenv fav prop = prop_fav_add fav (prop_dfs_sort tenv prop)

let rec strexp_get_array_indices acc (se: Sil.strexp) =
  match se with
  | Eexp _
   -> acc
  | Estruct (fsel, _)
   -> let se_list = List.map ~f:snd fsel in
      List.fold ~f:strexp_get_array_indices ~init:acc se_list
  | Earray (_, isel, _)
   -> let acc_new = List.fold ~f:(fun acc' (idx, _) -> idx :: acc') ~init:acc isel in
      let se_list = List.map ~f:snd isel in
      List.fold ~f:strexp_get_array_indices ~init:acc_new se_list

let hpred_get_array_indices acc (hpred: Sil.hpred) =
  match hpred with
  | Hpointsto (_, se, _)
   -> strexp_get_array_indices acc se
  | Hlseg _ | Hdllseg _
   -> acc

let sigma_get_array_indices sigma =
  let indices = List.fold ~f:hpred_get_array_indices ~init:[] sigma in
  List.rev indices

let compute_reindexing fav_add get_id_offset list =
  let rec select list_passed list_seen = function
    | []
     -> list_passed
    | x :: list_rest
     -> let id_offset_opt = get_id_offset x in
        let list_passed_new =
          match id_offset_opt with
          | None
           -> list_passed
          | Some (id, _)
           -> let fav = Sil.fav_new () in
              List.iter ~f:(fav_add fav) list_seen ;
              List.iter ~f:(fav_add fav) list_passed ;
              if Sil.fav_exists fav (Ident.equal id) then list_passed else x :: list_passed
        in
        let list_seen_new = x :: list_seen in
        select list_passed_new list_seen_new list_rest
  in
  let list_passed = select [] [] list in
  let transform x =
    let id, offset = match get_id_offset x with None -> assert false | Some io -> io in
    let base_new : Exp.t = Var (Ident.create_fresh Ident.kprimed) in
    let offset_new = Exp.int (IntLit.neg offset) in
    let exp_new : Exp.t = BinOp (PlusA, base_new, offset_new) in
    (id, exp_new)
  in
  let reindexing = List.map ~f:transform list_passed in
  Sil.exp_subst_of_list reindexing

let compute_reindexing_from_indices indices =
  let get_id_offset (e: Exp.t) =
    match e with
    | BinOp (PlusA, Var id, Const Cint offset)
     -> if Ident.is_primed id then Some (id, offset) else None
    | _
     -> None
  in
  let fav_add = Sil.exp_fav_add in
  compute_reindexing fav_add get_id_offset indices

let apply_reindexing tenv (exp_subst: Sil.exp_subst) prop =
  let subst = `Exp exp_subst in
  let nsigma = Normalize.sigma_normalize tenv subst prop.sigma in
  let npi = Normalize.pi_normalize tenv subst nsigma prop.pi in
  let nsub, atoms =
    let dom_subst = List.map ~f:fst (Sil.sub_to_list exp_subst) in
    let in_dom_subst id = List.exists ~f:(Ident.equal id) dom_subst in
    let sub' = Sil.sub_filter (fun id -> not (in_dom_subst id)) prop.sub in
    let contains_substituted_id e = Sil.fav_exists (Sil.exp_fav e) in_dom_subst in
    let sub_eqs, sub_keep = Sil.sub_range_partition contains_substituted_id sub' in
    let eqs = Sil.sub_to_list sub_eqs in
    let atoms =
      List.map ~f:(fun (id, e) -> Sil.Aeq (Var id, Normalize.exp_normalize tenv subst e)) eqs
    in
    (sub_keep, atoms)
  in
  let p' = unsafe_cast_to_normal (set prop ~sub:nsub ~pi:npi ~sigma:nsigma) in
  List.fold ~f:(Normalize.prop_atom_and tenv) ~init:p' atoms

let prop_rename_array_indices tenv prop =
  if !Config.footprint then prop
  else
    let indices = sigma_get_array_indices prop.sigma in
    let not_same_base_lt_offsets (e1: Exp.t) (e2: Exp.t) =
      match (e1, e2) with
      | BinOp (PlusA, e1', Const Cint n1'), BinOp (PlusA, e2', Const Cint n2')
       -> not (Exp.equal e1' e2' && IntLit.lt n1' n2')
      | _
       -> true
    in
    let rec select_minimal_indices indices_seen = function
      | []
       -> List.rev indices_seen
      | index :: indices_rest
       -> let indices_seen' = List.filter ~f:(not_same_base_lt_offsets index) indices_seen in
          let indices_seen_new = index :: indices_seen' in
          let indices_rest_new = List.filter ~f:(not_same_base_lt_offsets index) indices_rest in
          select_minimal_indices indices_seen_new indices_rest_new
    in
    let minimal_indices = select_minimal_indices [] indices in
    let subst = compute_reindexing_from_indices minimal_indices in
    apply_reindexing tenv subst prop

let compute_renaming fav =
  let ids = Sil.fav_to_list fav in
  let ids_primed, ids_nonprimed = List.partition_tf ~f:Ident.is_primed ids in
  let ids_footprint = List.filter ~f:Ident.is_footprint ids_nonprimed in
  let id_base_primed = Ident.create Ident.kprimed 0 in
  let id_base_footprint = Ident.create Ident.kfootprint 0 in
  let rec f id_base index ren_subst = function
    | []
     -> ren_subst
    | id :: ids
     -> let new_id = Ident.set_stamp id_base index in
        if Ident.equal id new_id then f id_base (index + 1) ren_subst ids
        else f id_base (index + 1) ((id, new_id) :: ren_subst) ids
  in
  let ren_primed = f id_base_primed 0 [] ids_primed in
  let ren_footprint = f id_base_footprint 0 [] ids_footprint in
  ren_primed @ ren_footprint

let rec idlist_assoc id = function
  | []
   -> raise Not_found
  | (i, x) :: l
   -> if Ident.equal i id then x else idlist_assoc id l

let ident_captured_ren ren id =
  try idlist_assoc id ren
  with Not_found -> id

(* If not defined in ren, id should be mapped to itself *)

let rec exp_captured_ren ren (e: Exp.t) : Exp.t =
  match e with
  | Var id
   -> Var (ident_captured_ren ren id)
  | Exn e
   -> Exn (exp_captured_ren ren e)
  | Closure _
   -> e (* TODO: why captured vars not renamed? *)
  | Const _
   -> e
  | Sizeof ({dynamic_length} as sizeof_data)
   -> Sizeof {sizeof_data with dynamic_length= Option.map ~f:(exp_captured_ren ren) dynamic_length}
  | Cast (t, e)
   -> Cast (t, exp_captured_ren ren e)
  | UnOp (op, e, topt)
   -> UnOp (op, exp_captured_ren ren e, topt)
  | BinOp (op, e1, e2)
   -> let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      BinOp (op, e1', e2')
  | Lvar id
   -> Lvar id
  | Lfield (e, fld, typ)
   -> Lfield (exp_captured_ren ren e, fld, typ)
  | Lindex (e1, e2)
   -> let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      Lindex (e1', e2')

let atom_captured_ren ren (a: Sil.atom) : Sil.atom =
  match a with
  | Aeq (e1, e2)
   -> Aeq (exp_captured_ren ren e1, exp_captured_ren ren e2)
  | Aneq (e1, e2)
   -> Aneq (exp_captured_ren ren e1, exp_captured_ren ren e2)
  | Apred (a, es)
   -> Apred (a, List.map ~f:(fun e -> exp_captured_ren ren e) es)
  | Anpred (a, es)
   -> Anpred (a, List.map ~f:(fun e -> exp_captured_ren ren e) es)

let rec strexp_captured_ren ren (se: Sil.strexp) : Sil.strexp =
  match se with
  | Eexp (e, inst)
   -> Eexp (exp_captured_ren ren e, inst)
  | Estruct (fld_se_list, inst)
   -> let f (fld, se) = (fld, strexp_captured_ren ren se) in
      Estruct (List.map ~f fld_se_list, inst)
  | Earray (len, idx_se_list, inst)
   -> let f (idx, se) =
        let idx' = exp_captured_ren ren idx in
        (idx', strexp_captured_ren ren se)
      in
      let len' = exp_captured_ren ren len in
      Earray (len', List.map ~f idx_se_list, inst)

and hpred_captured_ren ren (hpred: Sil.hpred) : Sil.hpred =
  match hpred with
  | Hpointsto (base, se, te)
   -> let base' = exp_captured_ren ren base in
      let se' = strexp_captured_ren ren se in
      let te' = exp_captured_ren ren te in
      Hpointsto (base', se', te')
  | Hlseg (k, para, e1, e2, elist)
   -> let para' = hpara_ren para in
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      let elist' = List.map ~f:(exp_captured_ren ren) elist in
      Hlseg (k, para', e1', e2', elist')
  | Hdllseg (k, para, e1, e2, e3, e4, elist)
   -> let para' = hpara_dll_ren para in
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      let e3' = exp_captured_ren ren e3 in
      let e4' = exp_captured_ren ren e4 in
      let elist' = List.map ~f:(exp_captured_ren ren) elist in
      Hdllseg (k, para', e1', e2', e3', e4', elist')

and hpara_ren (para: Sil.hpara) : Sil.hpara =
  let av = Sil.hpara_shallow_av para in
  let ren = compute_renaming av in
  let root = ident_captured_ren ren para.root in
  let next = ident_captured_ren ren para.next in
  let svars = List.map ~f:(ident_captured_ren ren) para.svars in
  let evars = List.map ~f:(ident_captured_ren ren) para.evars in
  let body = List.map ~f:(hpred_captured_ren ren) para.body in
  {root; next; svars; evars; body}

and hpara_dll_ren (para: Sil.hpara_dll) : Sil.hpara_dll =
  let av = Sil.hpara_dll_shallow_av para in
  let ren = compute_renaming av in
  let iF = ident_captured_ren ren para.cell in
  let oF = ident_captured_ren ren para.flink in
  let oB = ident_captured_ren ren para.blink in
  let svars' = List.map ~f:(ident_captured_ren ren) para.svars_dll in
  let evars' = List.map ~f:(ident_captured_ren ren) para.evars_dll in
  let body' = List.map ~f:(hpred_captured_ren ren) para.body_dll in
  {cell= iF; flink= oF; blink= oB; svars_dll= svars'; evars_dll= evars'; body_dll= body'}

let pi_captured_ren ren pi = List.map ~f:(atom_captured_ren ren) pi

let sigma_captured_ren ren sigma = List.map ~f:(hpred_captured_ren ren) sigma

let sub_captured_ren ren sub = Sil.sub_map (ident_captured_ren ren) (exp_captured_ren ren) sub

(** Canonicalize the names of primed variables and footprint vars. *)
let prop_rename_primed_footprint_vars tenv (p: normal t) : normal t =
  let p = prop_rename_array_indices tenv p in
  let bound_vars =
    let filter id = Ident.is_footprint id || Ident.is_primed id in
    let p_dfs = prop_dfs_sort tenv p in
    let fvars_in_p = prop_fav p_dfs in
    Sil.fav_filter_ident fvars_in_p filter ; fvars_in_p
  in
  let ren = compute_renaming bound_vars in
  let sub' = sub_captured_ren ren p.sub in
  let pi' = pi_captured_ren ren p.pi in
  let sigma' = sigma_captured_ren ren p.sigma in
  let pi_fp' = pi_captured_ren ren p.pi_fp in
  let sigma_fp' = sigma_captured_ren ren p.sigma_fp in
  let sub_for_normalize = Sil.sub_empty in
  (* It is fine to use the empty substituion during normalization
     because the renaming maintains that a substitution is normalized *)
  let nsub' = Normalize.sub_normalize sub' in
  let nsigma' = Normalize.sigma_normalize tenv sub_for_normalize sigma' in
  let npi' = Normalize.pi_normalize tenv sub_for_normalize nsigma' pi' in
  let p' =
    Normalize.footprint_normalize tenv
      (set prop_emp ~sub:nsub' ~pi:npi' ~sigma:nsigma' ~pi_fp:pi_fp' ~sigma_fp:sigma_fp')
  in
  unsafe_cast_to_normal p'

let expose (p: normal t) : exposed t = Obj.magic p

(** Apply subsitution to prop. *)
let prop_sub subst (prop: 'a t) : exposed t =
  let pi = pi_sub subst (prop.pi @ pi_of_subst prop.sub) in
  let sigma = sigma_sub subst prop.sigma in
  let pi_fp = pi_sub subst prop.pi_fp in
  let sigma_fp = sigma_sub subst prop.sigma_fp in
  set prop_emp ~pi ~sigma ~pi_fp ~sigma_fp

(** Apply renaming substitution to a proposition. *)
let prop_ren_sub tenv (ren_sub: Sil.exp_subst) (prop: normal t) : normal t =
  Normalize.normalize tenv (prop_sub (`Exp ren_sub) prop)

(** Existentially quantify the [fav] in [prop].
    [fav] should not contain any primed variables. *)
let exist_quantify tenv fav (prop: normal t) : normal t =
  let ids = Sil.fav_to_list fav in
  if List.exists ~f:Ident.is_primed ids then assert false ;
  (* sanity check *)
  if List.is_empty ids then prop
  else
    let gen_fresh_id_sub id = (id, Exp.Var (Ident.create_fresh Ident.kprimed)) in
    let ren_sub = Sil.exp_subst_of_list (List.map ~f:gen_fresh_id_sub ids) in
    let prop' =
      (* throw away x=E if x becomes _x *)
      let mem_idlist i = List.exists ~f:(fun id -> Ident.equal i id) in
      let sub = Sil.sub_filter (fun i -> not (mem_idlist i ids)) prop.sub in
      if Sil.equal_exp_subst sub prop.sub then prop else unsafe_cast_to_normal (set prop ~sub)
    in
    (*
    L.out "@[<2>.... Existential Quantification ....@\n";
    L.out "SUB:%a@\n" pp_sub prop'.sub;
    L.out "PI:%a@\n" pp_pi prop'.pi;
    L.out "PROP:%a@\n@." pp_prop prop';
    *)
    prop_ren_sub tenv ren_sub prop'

(** Apply the substitution [fe] to all the expressions in the prop. *)
let prop_expmap (fe: Exp.t -> Exp.t) prop =
  let f (e, sil_opt) = (fe e, sil_opt) in
  let pi = List.map ~f:(Sil.atom_expmap fe) prop.pi in
  let sigma = List.map ~f:(Sil.hpred_expmap f) prop.sigma in
  let pi_fp = List.map ~f:(Sil.atom_expmap fe) prop.pi_fp in
  let sigma_fp = List.map ~f:(Sil.hpred_expmap f) prop.sigma_fp in
  set prop ~pi ~sigma ~pi_fp ~sigma_fp

(** convert identifiers in fav to kind [k] *)
let vars_make_unprimed tenv fav prop =
  let ids = Sil.fav_to_list fav in
  let ren_sub =
    Sil.exp_subst_of_list
      (List.map ~f:(fun i -> (i, Exp.Var (Ident.create_fresh Ident.knormal))) ids)
  in
  prop_ren_sub tenv ren_sub prop

(** convert the normal vars to primed vars. *)
let prop_normal_vars_to_primed_vars tenv p =
  let fav = prop_fav p in
  Sil.fav_filter_ident fav Ident.is_normal ; exist_quantify tenv fav p

(** convert the primed vars to normal vars. *)
let prop_primed_vars_to_normal_vars tenv (p: normal t) : normal t =
  let fav = prop_fav p in
  Sil.fav_filter_ident fav Ident.is_primed ; vars_make_unprimed tenv fav p

let from_pi pi = set prop_emp ~pi

let from_sigma sigma = set prop_emp ~sigma

(** Rename free variables in a prop replacing them with existentially quantified vars *)
let prop_rename_fav_with_existentials tenv (p: normal t) : normal t =
  let fav = Sil.fav_new () in
  prop_fav_add fav p ;
  let ids = Sil.fav_to_list fav in
  let ids' = List.map ~f:(fun i -> (i, Ident.create_fresh Ident.kprimed)) ids in
  let ren_sub = Sil.subst_of_list (List.map ~f:(fun (i, i') -> (i, Exp.Var i')) ids') in
  let p' = prop_sub ren_sub p in
  (*L.d_strln "Prop after renaming:"; d_prop p'; L.d_strln "";*)
  Normalize.normalize tenv p'

(** Removes seeds variables from a prop corresponding to captured variables in an objc block *)
let remove_seed_captured_vars_block tenv captured_vars prop =
  let hpred_seed_captured = function
    | Sil.Hpointsto (Exp.Lvar pv, _, _)
     -> let pname = Pvar.get_name pv in
        Pvar.is_seed pv && List.mem ~equal:Mangled.equal captured_vars pname
    | _
     -> false
  in
  let sigma = prop.sigma in
  let sigma' = List.filter ~f:(fun hpred -> not (hpred_seed_captured hpred)) sigma in
  Normalize.normalize tenv (set prop ~sigma:sigma')

(** {2 Prop iterators} *)

(** Iterator state over sigma. *)
type 'a prop_iter =
  { pit_sub: Sil.exp_subst  (** substitution for equalities *)
  ; pit_pi: pi  (** pure part *)
  ; pit_newpi: (bool * Sil.atom) list  (** newly added atoms. *)
  ; (* The first records !Config.footprint. *)
  pit_old: sigma  (** sigma already visited *)
  ; pit_curr: Sil.hpred  (** current element *)
  ; pit_state: 'a  (** state of current element *)
  ; pit_new: sigma  (** sigma not yet visited *)
  ; pit_pi_fp: pi  (** pure part of the footprint *)
  ; pit_sigma_fp: sigma  (** sigma part of the footprint *) }

let prop_iter_create prop =
  match prop.sigma with
  | hpred :: sigma'
   -> Some
        { pit_sub= prop.sub
        ; pit_pi= prop.pi
        ; pit_newpi= []
        ; pit_old= []
        ; pit_curr= hpred
        ; pit_state= ()
        ; pit_new= sigma'
        ; pit_pi_fp= prop.pi_fp
        ; pit_sigma_fp= prop.sigma_fp }
  | _
   -> None

(** Return the prop associated to the iterator. *)
let prop_iter_to_prop tenv iter =
  let sigma = List.rev_append iter.pit_old (iter.pit_curr :: iter.pit_new) in
  let prop =
    Normalize.normalize tenv
      (set prop_emp ~sub:iter.pit_sub ~pi:iter.pit_pi ~sigma ~pi_fp:iter.pit_pi_fp
         ~sigma_fp:iter.pit_sigma_fp)
  in
  List.fold
    ~f:(fun p (footprint, atom) -> Normalize.prop_atom_and tenv ~footprint p atom)
    ~init:prop iter.pit_newpi

(** Add an atom to the pi part of prop iter. The
    first parameter records whether it is done
    during footprint or during re - execution. *)
let prop_iter_add_atom footprint iter atom =
  {iter with pit_newpi= (footprint, atom) :: iter.pit_newpi}

(** Remove the current element of the iterator, and return the prop
    associated to the resulting iterator *)
let prop_iter_remove_curr_then_to_prop tenv iter : normal t =
  let sigma = List.rev_append iter.pit_old iter.pit_new in
  let normalized_sigma = Normalize.sigma_normalize tenv (`Exp iter.pit_sub) sigma in
  let prop =
    set prop_emp ~sub:iter.pit_sub ~pi:iter.pit_pi ~sigma:normalized_sigma ~pi_fp:iter.pit_pi_fp
      ~sigma_fp:iter.pit_sigma_fp
  in
  unsafe_cast_to_normal prop

(** Return the current hpred and state. *)
let prop_iter_current tenv iter =
  let curr = Normalize.hpred_normalize tenv (`Exp iter.pit_sub) iter.pit_curr in
  let prop = unsafe_cast_to_normal (set prop_emp ~sigma:[curr]) in
  let prop' =
    List.fold
      ~f:(fun p (footprint, atom) -> Normalize.prop_atom_and tenv ~footprint p atom)
      ~init:prop iter.pit_newpi
  in
  match prop'.sigma with [curr'] -> (curr', iter.pit_state) | _ -> assert false

(** Update the current element of the iterator. *)
let prop_iter_update_current iter hpred = {iter with pit_curr= hpred}

(** Update the current element of the iterator by a nonempty list of elements. *)
let prop_iter_update_current_by_list iter = function
  | []
   -> assert false (* the list should be nonempty *)
  | hpred :: hpred_list
   -> let pit_new' = hpred_list @ iter.pit_new in
      {iter with pit_curr= hpred; pit_state= (); pit_new= pit_new'}

let prop_iter_next iter =
  match iter.pit_new with
  | []
   -> None
  | hpred' :: new'
   -> Some
        { iter with
          pit_old= iter.pit_curr :: iter.pit_old; pit_curr= hpred'; pit_state= (); pit_new= new' }

let prop_iter_remove_curr_then_next iter =
  match iter.pit_new with
  | []
   -> None
  | hpred' :: new'
   -> Some {iter with pit_old= iter.pit_old; pit_curr= hpred'; pit_state= (); pit_new= new'}

(** Insert before the current element of the iterator. *)
let prop_iter_prev_then_insert iter hpred =
  {iter with pit_new= iter.pit_curr :: iter.pit_new; pit_curr= hpred}

(** Scan sigma to find an [hpred] satisfying the filter function. *)
let rec prop_iter_find iter filter =
  match filter iter.pit_curr with
  | Some st
   -> Some {iter with pit_state= st}
  | None ->
    match prop_iter_next iter with None -> None | Some iter' -> prop_iter_find iter' filter

(** Set the state of the iterator *)
let prop_iter_set_state iter state = {iter with pit_state= state}

let prop_iter_make_id_primed tenv id iter =
  let pid = Ident.create_fresh Ident.kprimed in
  let sub_id = Sil.subst_of_list [(id, Exp.Var pid)] in
  let normalize (id, e) =
    let eq' : Sil.atom = Aeq (Sil.exp_sub sub_id (Var id), Sil.exp_sub sub_id e) in
    Normalize.atom_normalize tenv Sil.sub_empty eq'
  in
  let rec split pairs_unpid pairs_pid = function
    | []
     -> (List.rev pairs_unpid, List.rev pairs_pid)
    | (eq :: eqs_cur: pi) ->
      match eq with
      | Aeq (Var id1, e1) when Sil.ident_in_exp id1 e1
       -> L.internal_error "@[<2>#### ERROR: an assumption of the analyzer broken ####@\n" ;
          L.internal_error "Broken Assumption: id notin e for all (id,e) in sub@\n" ;
          L.internal_error "(id,e) : (%a,%a)@\n" (Ident.pp Pp.text) id1 Exp.pp e1 ;
          L.internal_error "PROP : %a@\n@." (pp_prop Pp.text) (prop_iter_to_prop tenv iter) ;
          assert false
      | Aeq (Var id1, e1) when Ident.equal pid id1
       -> split pairs_unpid ((id1, e1) :: pairs_pid) eqs_cur
      | Aeq (Var id1, e1)
       -> split ((id1, e1) :: pairs_unpid) pairs_pid eqs_cur
      | _
       -> assert false
  in
  let rec get_eqs acc = function
    | [] | [_]
     -> List.rev acc
    | (_, e1) :: ((_, e2) :: _ as pairs)
     -> get_eqs (Sil.Aeq (e1, e2) :: acc) pairs
  in
  let sub_new, sub_use, eqs_add =
    let eqs = List.map ~f:normalize (Sil.sub_to_list iter.pit_sub) in
    let pairs_unpid, pairs_pid = split [] [] eqs in
    match pairs_pid with
    | []
     -> let sub_unpid = Sil.exp_subst_of_list pairs_unpid in
        let pairs = (id, Exp.Var pid) :: pairs_unpid in
        (sub_unpid, Sil.subst_of_list pairs, [])
    | (id1, e1) :: _
     -> let sub_id1 = Sil.subst_of_list [(id1, e1)] in
        let pairs_unpid' =
          List.map ~f:(fun (id', e') -> (id', Sil.exp_sub sub_id1 e')) pairs_unpid
        in
        let sub_unpid = Sil.exp_subst_of_list pairs_unpid' in
        let pairs = (id, e1) :: pairs_unpid' in
        (sub_unpid, Sil.subst_of_list pairs, get_eqs [] pairs_pid)
  in
  let nsub_new = Normalize.sub_normalize sub_new in
  { iter with
    pit_sub= nsub_new
  ; pit_pi= pi_sub sub_use (iter.pit_pi @ eqs_add)
  ; pit_old= sigma_sub sub_use iter.pit_old
  ; pit_curr= Sil.hpred_sub sub_use iter.pit_curr
  ; pit_new= sigma_sub sub_use iter.pit_new }

let prop_iter_footprint_fav_add fav iter =
  sigma_fav_add fav iter.pit_sigma_fp ; pi_fav_add fav iter.pit_pi_fp

(** Find fav of the footprint part of the iterator *)
let prop_iter_footprint_fav iter =
  Sil.fav_imperative_to_functional prop_iter_footprint_fav_add iter

let prop_iter_fav_add fav iter =
  Sil.sub_fav_add fav iter.pit_sub ;
  pi_fav_add fav iter.pit_pi ;
  pi_fav_add fav (List.map ~f:snd iter.pit_newpi) ;
  sigma_fav_add fav iter.pit_old ;
  sigma_fav_add fav iter.pit_new ;
  Sil.hpred_fav_add fav iter.pit_curr ;
  prop_iter_footprint_fav_add fav iter

(** Find fav of the iterator *)
let prop_iter_fav iter = Sil.fav_imperative_to_functional prop_iter_fav_add iter

(** Free vars of the iterator except the current hpred (and footprint). *)
let prop_iter_noncurr_fav_add fav iter =
  sigma_fav_add fav iter.pit_old ;
  sigma_fav_add fav iter.pit_new ;
  Sil.sub_fav_add fav iter.pit_sub ;
  pi_fav_add fav iter.pit_pi

(** Extract the sigma part of the footprint *)
let prop_iter_get_footprint_sigma iter = iter.pit_sigma_fp

(** Replace the sigma part of the footprint *)
let prop_iter_replace_footprint_sigma iter sigma = {iter with pit_sigma_fp= sigma}

let prop_iter_noncurr_fav iter = Sil.fav_imperative_to_functional prop_iter_noncurr_fav_add iter

let rec strexp_gc_fields (fav: Sil.fav) (se: Sil.strexp) =
  match se with
  | Eexp _
   -> Some se
  | Estruct (fsel, inst)
   -> let fselo = List.map ~f:(fun (f, se) -> (f, strexp_gc_fields fav se)) fsel in
      let fsel' =
        let fselo' = List.filter ~f:(function _, Some _ -> true | _ -> false) fselo in
        List.map ~f:(function f, seo -> (f, unSome seo)) fselo'
      in
      if [%compare.equal : (Typ.Fieldname.t * Sil.strexp) list] fsel fsel' then Some se
      else Some (Sil.Estruct (fsel', inst))
  | Earray _
   -> Some se

let hpred_gc_fields (fav: Sil.fav) (hpred: Sil.hpred) : Sil.hpred =
  match hpred with
  | Hpointsto (e, se, te)
   -> (
      Sil.exp_fav_add fav e ;
      Sil.exp_fav_add fav te ;
      match strexp_gc_fields fav se with
      | None
       -> hpred
      | Some se'
       -> if Sil.equal_strexp se se' then hpred else Hpointsto (e, se', te) )
  | Hlseg _ | Hdllseg _
   -> hpred

let rec prop_iter_map f iter =
  let hpred_curr = f iter in
  let iter' = {iter with pit_curr= hpred_curr} in
  match prop_iter_next iter' with None -> iter' | Some iter'' -> prop_iter_map f iter''

(** Collect garbage fields. *)
let prop_iter_gc_fields iter =
  let f iter' =
    let fav = prop_iter_noncurr_fav iter' in
    hpred_gc_fields fav iter'.pit_curr
  in
  prop_iter_map f iter

let prop_case_split tenv prop =
  let pi_sigma_list = Sil.sigma_to_sigma_ne prop.sigma in
  let f props_acc (pi, sigma) =
    let sigma' = sigma_normalize_prop tenv prop sigma in
    let prop' = unsafe_cast_to_normal (set prop ~sigma:sigma') in
    List.fold ~f:(Normalize.prop_atom_and tenv) ~init:prop' pi :: props_acc
  in
  List.fold ~f ~init:[] pi_sigma_list

let prop_expand prop =
  (*
  let _ = check_prop_normalized prop in
  *)
  prop_case_split prop

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

  and hpred_size (hpred: Sil.hpred) =
    match hpred with
    | Hpointsto _
     -> ptsto_weight
    | Hlseg (_, hpara, _, _, _)
     -> lseg_weight * hpara_size hpara
    | Hdllseg (_, hpara_dll, _, _, _, _, _)
     -> lseg_weight * hpara_dll_size hpara_dll

  and sigma_size sigma =
    let size = ref 0 in
    List.iter ~f:(fun hpred -> size := hpred_size hpred + !size) sigma ;
    !size

  let pi_size pi = pi_weight * List.length pi

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
    let lhs_is_lvar : Exp.t -> bool = function Lvar _ -> true | _ -> false in
    let lhs_is_var_lvar : Exp.t -> bool = function Var _ -> true | Lvar _ -> true | _ -> false in
    let rhs_is_var : Sil.strexp -> bool = function Eexp (Var _, _) -> true | _ -> false in
    let rec rhs_only_vars : Sil.strexp -> bool = function
      | Eexp (Var _, _)
       -> true
      | Estruct (fsel, _)
       -> List.for_all ~f:(fun (_, se) -> rhs_only_vars se) fsel
      | Earray _
       -> true
      | _
       -> false
    in
    let hpred_is_var : Sil.hpred -> bool = function
      (* stack variable with no constraints *)
      | Hpointsto (e, se, _)
       -> lhs_is_lvar e && rhs_is_var se
      | _
       -> false
    in
    let hpred_only_allocation : Sil.hpred -> bool = function
      (* only constraint is allocation *)
      | Hpointsto (e, se, _)
       -> lhs_is_var_lvar e && rhs_only_vars se
      | _
       -> false
    in
    let check_pre hpred_filter pre =
      let check_pi pi = List.is_empty pi in
      let check_sigma sigma = List.for_all ~f:hpred_filter sigma in
      check_pi pre.pi && check_sigma pre.sigma
    in
    let pres_no_constraints = List.filter ~f:(check_pre hpred_is_var) preconditions in
    let pres_only_allocation = List.filter ~f:(check_pre hpred_only_allocation) preconditions in
    match (preconditions, pres_no_constraints, pres_only_allocation) with
    | [], _, _
     -> NoPres
    | _ :: _, _ :: _, _
     -> Empty
    | _ :: _, [], _ :: _
     -> OnlyAllocation
    | _ :: _, [], []
     -> DataConstraints
end

(* Export for interface *)
let exp_normalize_noabs = Normalize.exp_normalize_noabs

let mk_inequality = Normalize.mk_inequality

let mk_ptsto_exp = Normalize.mk_ptsto_exp

let mk_ptsto = Normalize.mk_ptsto

let normalize = Normalize.normalize

let prop_atom_and = Normalize.prop_atom_and
