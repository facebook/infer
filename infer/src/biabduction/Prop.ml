(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for Propositions (i.e., Symbolic Heaps) *)

module L = Logging
module F = Format

(** type to describe different strategies for initializing fields of a structure. [No_init] does not
    initialize any fields of the struct. [Fld_init] initializes the fields of the struct with fresh
    variables (C) or default values (Java). *)
type struct_init_mode = No_init | Fld_init

(** kind for normal props, i.e. normalized *)
type normal

(** kind for exposed props *)
type exposed

(** kind for sorted props *)
type sorted

type pi = Predicates.atom list [@@deriving compare, equal]

type sigma = Predicates.hpred list [@@deriving compare, equal]

module Core : sig
  (** the kind 'a should range over [normal] and [exposed] *)
  type 'a t = private
    { sigma: sigma  (** spatial part *)
    ; sub: Predicates.subst  (** substitution *)
    ; pi: pi  (** pure part *)
    ; sigma_fp: sigma  (** abduced spatial part *)
    ; pi_fp: pi  (** abduced pure part *) }
  [@@deriving compare]

  val has_footprint : 'a t -> bool

  val prop_emp : normal t
  (** Proposition [true /\ emp]. *)

  val set :
       ?sub:Predicates.subst
    -> ?pi:pi
    -> ?sigma:sigma
    -> ?pi_fp:pi
    -> ?sigma_fp:sigma
    -> 'a t
    -> exposed t
  (** Set individual fields of the prop. *)

  val unsafe_cast_to_normal : exposed t -> normal t
  (** Cast an exposed prop to a normalized one by just changing the type *)

  val unsafe_cast_to_sorted : exposed t -> sorted t
end = struct
  (** A proposition. The following invariants are mantained. [sub] is of the form id1 = e1 ... idn =
      en where: the id's are distinct and do not occur in the e's nor in [pi] or [sigma]; the id's
      are in sorted order; the id's are not existentials; if idn = yn (for yn not existential) then
      idn < yn in the order on ident's. [pi] is sorted and normalized, and does not contain x = e.
      [sigma] is sorted and normalized. *)
  type 'a t =
    { sigma: sigma  (** spatial part *)
    ; sub: Predicates.subst  (** substitution *)
    ; pi: pi  (** pure part *)
    ; sigma_fp: sigma  (** abduced spatial part *)
    ; pi_fp: pi  (** abduced pure part *) }
  [@@deriving compare]

  let has_footprint {sigma_fp; pi_fp} = not (List.is_empty sigma_fp && List.is_empty pi_fp)

  (** Proposition [true /\ emp]. *)
  let prop_emp : normal t = {sub= Predicates.sub_empty; pi= []; sigma= []; pi_fp= []; sigma_fp= []}

  let set ?sub ?pi ?sigma ?pi_fp ?sigma_fp p =
    let set_ p ?(sub = p.sub) ?(pi = p.pi) ?(sigma = p.sigma) ?(pi_fp = p.pi_fp)
        ?(sigma_fp = p.sigma_fp) () =
      {sub; pi; sigma; pi_fp; sigma_fp}
    in
    set_ p ?sub ?pi ?sigma ?pi_fp ?sigma_fp ()


  let unsafe_cast_to_normal (p : exposed t) : normal t = (p :> normal t)

  let unsafe_cast_to_sorted (p : exposed t) : sorted t = (p :> sorted t)
end

include Core

(** {2 Basic Functions for Propositions} *)

let expose (p : _ t) : exposed t = Obj.magic p

let expose_sorted (p : sorted t) : exposed t = Obj.magic p

(** {1 Functions for Comparison} *)

(** Comparison between propositions. Lexicographical order. *)
let compare_prop p1 p2 = compare (fun _ _ -> 0) p1 p2

(** {1 Functions for Pretty Printing} *)

let pp_texp_simple pe =
  match pe.Pp.opt with SIM_DEFAULT -> Exp.pp_texp pe | SIM_WITH_TYP -> Exp.pp_texp_full pe


(** Pretty print a pointsto representing a stack variable as an equality *)
let pp_hpred_stackvar =
  Pp.color_wrapper ~f:(fun pe f (hpred : Predicates.hpred) ->
      match hpred with
      | Hpointsto (Exp.Lvar pvar, se, te) ->
          let pe' =
            match se with
            | Eexp (Exp.Var _, _) when not (Pvar.is_global pvar) ->
                {pe with obj_sub= None} (* dont use obj sub on the var defining it *)
            | _ ->
                pe
          in
          F.fprintf f "%a = %a:%a" Pvar.pp_value pvar (Predicates.pp_sexp pe') se
            (pp_texp_simple pe') te
      | Hpointsto _ | Hlseg _ | Hdllseg _ ->
          assert false
      (* should not happen *) )


(** Pretty print a substitution. *)
let pp_sub pe f sub =
  let pi_sub =
    List.map ~f:(fun (id, e) -> Predicates.Aeq (Var id, e)) (Predicates.sub_to_list sub)
  in
  Pp.semicolon_seq ~print_env:{pe with break_lines= false} (Predicates.pp_atom pe) f pi_sub


(** Dump a substitution. *)
let d_sub (sub : Predicates.subst) = L.d_pp_with_pe pp_sub sub

let pp_sub_entry =
  Pp.color_wrapper ~f:(fun pe f entry ->
      let x, e = entry in
      F.fprintf f "%a = %a" Ident.pp x (Exp.pp_diff pe) e )


(** Pretty print a substitution as a list of (ident,exp) pairs *)
let pp_subl pe =
  if Config.smt_output then Pp.semicolon_seq ~print_env:pe (pp_sub_entry pe)
  else Pp.semicolon_seq ~print_env:{pe with break_lines= false} (pp_sub_entry pe)


(** Pretty print a pi. *)
let pp_pi pe =
  if Config.smt_output then Pp.semicolon_seq ~print_env:pe (Predicates.pp_atom pe)
  else Pp.semicolon_seq ~print_env:{pe with break_lines= false} (Predicates.pp_atom pe)


(** Dump a pi. *)
let d_pi (pi : pi) = L.d_pp_with_pe pp_pi pi

(** Pretty print a sigma. *)
let pp_sigma pe = Pp.semicolon_seq ~print_env:pe (Predicates.pp_hpred pe)

(** Split sigma into stack and nonstack parts. The boolean indicates whether the stack should only
    include local variales. *)
let sigma_get_stack_nonstack only_local_vars sigma =
  let hpred_is_stack_var = function
    | Predicates.Hpointsto (Lvar pvar, _, _) ->
        (not only_local_vars) || Pvar.is_local pvar
    | _ ->
        false
  in
  List.partition_tf ~f:hpred_is_stack_var sigma


(** Pretty print a sigma in simple mode. *)
let pp_sigma_simple pe env fmt sigma =
  let sigma_stack, sigma_nonstack = sigma_get_stack_nonstack false sigma in
  let pp_stack fmt sg_ =
    let sg = List.sort ~compare:Predicates.compare_hpred sg_ in
    if not (List.is_empty sg) then (Pp.semicolon_seq ~print_env:pe (pp_hpred_stackvar pe)) fmt sg
  in
  let pp_nl fmt doit = if doit then Format.fprintf fmt " ;@\n" in
  let pp_nonstack fmt =
    Pp.semicolon_seq ~print_env:pe (Predicates.pp_hpred_env pe (Some env)) fmt
  in
  if (not (List.is_empty sigma_stack)) || not (List.is_empty sigma_nonstack) then
    Format.fprintf fmt "%a%a%a" pp_stack sigma_stack pp_nl
      ((not (List.is_empty sigma_stack)) && not (List.is_empty sigma_nonstack))
      pp_nonstack sigma_nonstack


(** Dump a sigma. *)
let d_sigma (sigma : sigma) = L.d_pp_with_pe pp_sigma sigma

(** Dump a pi and a sigma *)
let d_pi_sigma pi sigma =
  let d_separator () =
    if (not (List.is_empty pi)) && not (List.is_empty sigma) then L.d_strln " *"
  in
  d_pi pi ;
  d_separator () ;
  d_sigma sigma


let pi_of_subst sub =
  List.map ~f:(fun (id1, e2) -> Predicates.Aeq (Var id1, e2)) (Predicates.sub_to_list sub)


(** Return the pure part of [prop]. *)
let get_pure (p : 'a t) : pi = pi_of_subst p.sub @ p.pi

(* Same with get_pure, except that when we have both "x = t" and "y = t" where t is a primed ident,
   * we add "x = y" to the result. This is crucial for the normalizer, as it tend to drop "x = t" before
   * processing "y = t". If we don't explicitly preserve "x = y", the normalizer cannot pick it up *)
let get_pure_extended p =
  let base = get_pure p in
  let primed_atoms, _ =
    List.fold base ~init:([], Ident.Map.empty) ~f:(fun ((atoms, primed_map) as acc) base_atom ->
        let extend_atoms id pid =
          try
            let old_id = Ident.Map.find pid primed_map in
            let new_atom = Predicates.Aeq (Var id, Var old_id) in
            (new_atom :: atoms, primed_map)
          with Caml.Not_found -> (atoms, Ident.Map.add pid id primed_map)
        in
        match base_atom with
        | Predicates.Aeq (Exp.Var id0, Exp.Var id1)
          when Ident.is_primed id0 && not (Ident.is_primed id1) ->
            extend_atoms id1 id0
        | Predicates.Aeq (Exp.Var id0, Exp.Var id1)
          when Ident.is_primed id1 && not (Ident.is_primed id0) ->
            extend_atoms id0 id1
        | _ ->
            acc )
  in
  primed_atoms @ base


(** Print existential quantification *)
let pp_evars f evars =
  if not (List.is_empty evars) then F.fprintf f "exists [%a]. " (Pp.comma_seq Ident.pp) evars


(** Print an hpara in simple mode *)
let pp_hpara_simple pe_ env n f pred =
  let pe = Pp.reset_obj_sub pe_ in
  (* no free vars: disable object substitution *)
  F.fprintf f "P%d = %a%a" n pp_evars pred.Predicates.evars
    (Pp.semicolon_seq ~print_env:pe (Predicates.pp_hpred_env pe (Some env)))
    pred.Predicates.body


(** Print an hpara_dll in simple mode *)
let pp_hpara_dll_simple pe_ env n f pred =
  let pe = Pp.reset_obj_sub pe_ in
  (* no free vars: disable object substitution *)
  F.fprintf f "P%d = %a%a" n pp_evars pred.Predicates.evars_dll
    (Pp.semicolon_seq ~print_env:pe (Predicates.pp_hpred_env pe (Some env)))
    pred.Predicates.body_dll


(** Create an environment mapping (ident) expressions to the program variables containing them *)
let create_pvar_env (sigma : sigma) : Exp.t -> Exp.t =
  let env = ref [] in
  let filter = function
    | Predicates.Hpointsto (Lvar pvar, Eexp (Var v, _), _) ->
        if not (Pvar.is_global pvar) then env := (Exp.Var v, Exp.Lvar pvar) :: !env
    | _ ->
        ()
  in
  List.iter ~f:filter sigma ;
  let find e =
    List.find ~f:(fun (e1, _) -> Exp.equal e1 e) !env
    |> Option.map ~f:snd |> Option.value ~default:e
  in
  find


(** Update the object substitution given the stack variables in the prop *)
let prop_update_obj_sub pe prop = Pp.set_obj_sub pe (create_pvar_env prop.sigma)

(** Pretty print a footprint in simple mode. *)
let pp_footprint_simple pe_ env f fp =
  let pe = {pe_ with Pp.cmap_norm= pe_.Pp.cmap_foot} in
  let pp_pure f pi = if not (List.is_empty pi) then F.fprintf f "%a *@\n" (pp_pi pe) pi in
  if (not (List.is_empty fp.pi_fp)) || not (List.is_empty fp.sigma_fp) then
    F.fprintf f "@\n[footprint@\n   @[%a%a@]  ]" pp_pure fp.pi_fp (pp_sigma_simple pe env)
      fp.sigma_fp


(** Create a predicate environment for a prop *)
let prop_pred_env prop =
  let env = Predicates.Env.mk_empty () in
  List.iter ~f:(Predicates.Env.process_hpred env) prop.sigma ;
  List.iter ~f:(Predicates.Env.process_hpred env) prop.sigma_fp ;
  env


(** Pretty print a proposition. *)
let pp_prop pe0 f prop =
  let pe = prop_update_obj_sub pe0 prop in
  let do_print f () =
    let subl = Predicates.sub_to_list prop.sub in
    (* since prop diff is based on physical equality, we need to extract the sub verbatim *)
    let pi = prop.pi in
    let pp_pure f () =
      if not (List.is_empty subl) then F.fprintf f "%a ;@\n" (pp_subl pe) subl ;
      if not (List.is_empty pi) then F.fprintf f "%a ;@\n" (pp_pi pe) pi
    in
    let env = prop_pred_env prop in
    let iter_f n hpara = F.fprintf f "@,@[<h>%a@]" (pp_hpara_simple pe env n) hpara in
    let iter_f_dll n hpara_dll =
      F.fprintf f "@,@[<h>%a@]" (pp_hpara_dll_simple pe env n) hpara_dll
    in
    let pp_predicates _ () =
      if not (Predicates.Env.is_empty env) then (
        F.fprintf f "@,where" ;
        Predicates.Env.iter env iter_f iter_f_dll )
    in
    F.fprintf f "%a%a%a%a" pp_pure () (pp_sigma_simple pe env) prop.sigma
      (pp_footprint_simple pe env) prop pp_predicates ()
  in
  match pe0.Pp.kind with HTML -> Pp.html_with_color Blue do_print f () | TEXT -> do_print f ()


let pp_prop_with_typ pe f p = pp_prop {pe with opt= SIM_WITH_TYP} f p

(** Dump a proposition. *)
let d_prop (prop : 'a t) = L.d_pp_with_pe pp_prop prop

(** Print a list of propositions, prepending each one with the given string *)
let pp_proplist_with_typ pe f plist =
  let rec pp_seq_newline f = function
    | [] ->
        ()
    | [x] ->
        F.fprintf f "@[%a@]" (pp_prop_with_typ pe) x
    | x :: l ->
        F.fprintf f "@[%a@]@\n(||)@\n%a" (pp_prop_with_typ pe) x pp_seq_newline l
  in
  F.fprintf f "@[<v>%a@]" pp_seq_newline plist


(** dump a proplist *)
let d_proplist_with_typ (pl : 'a t list) = L.d_pp_with_pe pp_proplist_with_typ pl

(** {1 Functions for computing free non-program variables} *)

let pi_gen_free_vars pi = ISequence.gen_sequence_list pi ~f:Predicates.atom_gen_free_vars

let pi_free_vars pi = Sequence.Generator.run (pi_gen_free_vars pi)

let sigma_gen_free_vars sigma = ISequence.gen_sequence_list sigma ~f:Predicates.hpred_gen_free_vars

let sigma_free_vars sigma = Sequence.Generator.run (sigma_gen_free_vars sigma)

let gen_free_vars {sigma; sigma_fp; sub; pi; pi_fp} =
  let open Sequence.Generator in
  sigma_gen_free_vars sigma
  >>= fun () ->
  sigma_gen_free_vars sigma_fp
  >>= fun () ->
  Predicates.subst_gen_free_vars sub
  >>= fun () -> pi_gen_free_vars pi >>= fun () -> pi_gen_free_vars pi_fp


let free_vars prop = Sequence.Generator.run (gen_free_vars prop)

let seq_max_stamp predicate seq =
  seq |> Sequence.filter ~f:predicate |> Sequence.map ~f:Ident.get_stamp
  |> Sequence.max_elt ~compare:Int.compare
  |> Option.value ~default:0


let all_true _ = true

let max_stamp ?(f = all_true) prop = seq_max_stamp f (free_vars prop)

let exposed_gen_free_vars prop = gen_free_vars (unsafe_cast_to_normal prop)

let sorted_gen_free_vars prop = exposed_gen_free_vars (expose_sorted prop)

let sorted_free_vars prop = Sequence.Generator.run (sorted_gen_free_vars prop)

(** free vars of the prop, excluding the pure part *)
let non_pure_gen_free_vars {sigma; sigma_fp} =
  Sequence.Generator.(sigma_gen_free_vars sigma >>= fun () -> sigma_gen_free_vars sigma_fp)


let non_pure_free_vars prop = Sequence.Generator.run (non_pure_gen_free_vars prop)

(** {2 Functions for Subsitition} *)

let pi_sub (subst : Predicates.subst) pi =
  let f = Predicates.atom_sub subst in
  List.map ~f pi


let sigma_sub subst sigma =
  let f = Predicates.hpred_sub subst in
  List.map ~f sigma


(** Return [true] if the atom is an inequality *)
let atom_is_inequality (atom : Predicates.atom) =
  match atom with
  | Aeq (BinOp ((Le | Lt), _, _), Const (Cint i)) when IntLit.isone i ->
      true
  | _ ->
      false


(** If the atom is [e<=n] return [e,n] *)
let atom_exp_le_const (atom : Predicates.atom) =
  match atom with
  | Aeq (BinOp (Le, e1, Const (Cint n)), Const (Cint i)) when IntLit.isone i ->
      Some (e1, n)
  | _ ->
      None


(** If the atom is [n<e] return [n,e] *)
let atom_const_lt_exp (atom : Predicates.atom) =
  match atom with
  | Aeq (BinOp (Lt, Const (Cint n), e1), Const (Cint i)) when IntLit.isone i ->
      Some (n, e1)
  | _ ->
      None


let exp_reorder e1 e2 = if Exp.compare e1 e2 <= 0 then (e1, e2) else (e2, e1)

let rec pp_path f = function
  | [] ->
      ()
  | (name, fld) :: path ->
      F.fprintf f "%a.%a: " Typ.Name.pp name Fieldname.pp fld ;
      pp_path f path


(** create a strexp of the given type, populating the structures if [struct_init_mode] is [Fld_init] *)
let rec create_strexp_of_type ~path tenv struct_init_mode (typ : Typ.t) len inst : Predicates.strexp
    =
  let init_value () =
    let create_fresh_var () =
      let fresh_id =
        Ident.create_fresh (if !BiabductionConfig.footprint then Ident.kfootprint else Ident.kprimed)
      in
      Exp.Var fresh_id
    in
    if Language.curr_language_is Java && Predicates.equal_inst inst Predicates.Ialloc then
      match typ.desc with Tfloat _ -> Exp.Const (Cfloat 0.0) | _ -> Exp.zero
    else create_fresh_var ()
  in
  match (typ.desc, len) with
  | (Tint _ | Tfloat _ | Tvoid | Tfun | Tptr _ | TVar _), None ->
      Eexp (init_value (), inst)
  | Tstruct name, _ -> (
      if List.exists ~f:(fun (n, _) -> Typ.Name.equal n name) path then
        L.die InternalError
          "Ill-founded recursion in [create_strexp_of_type]: a sub-element of struct %a is also of \
           type struct %a: %a:%a"
          Typ.Name.pp name Typ.Name.pp name pp_path (List.rev path) Typ.Name.pp name ;
      match (struct_init_mode, Tenv.lookup tenv name) with
      | Fld_init, Some {fields} ->
          (* pass len as an accumulator, so that it is passed to create_strexp_of_type for the last
             field, but always return None so that only the last field receives len *)
          let f {Struct.name= fld; typ= t} (flds, len) =
            ( ( fld
              , create_strexp_of_type ~path:((name, fld) :: path) tenv struct_init_mode t len inst
              )
              :: flds
            , None )
          in
          let flds, _ = List.fold_right ~f fields ~init:([], len) in
          Estruct (flds, inst)
      | _ ->
          Estruct ([], inst) )
  | Tarray {length= len_opt}, None ->
      let len =
        match len_opt with None -> Exp.get_undefined false | Some len -> Exp.Const (Cint len)
      in
      Earray (len, [], inst)
  | Tarray _, Some len ->
      Earray (len, [], inst)
  | (Tint _ | Tfloat _ | Tvoid | Tfun | Tptr _ | TVar _), Some _ ->
      assert false


let create_strexp_of_type tenv struct_init_mode (typ : Typ.t) len inst : Predicates.strexp =
  create_strexp_of_type ~path:[] tenv struct_init_mode (typ : Typ.t) len inst


let replace_array_contents (hpred : Predicates.hpred) esel : Predicates.hpred =
  match hpred with
  | Hpointsto (root, Predicates.Earray (len, [], inst), te) ->
      Hpointsto (root, Earray (len, esel, inst), te)
  | _ ->
      assert false


(** remove duplicate atoms and redundant inequalities from a sorted pi *)
let rec pi_sorted_remove_redundant (pi : pi) =
  match pi with
  | (Aeq (BinOp (Le, e1, Const (Cint n1)), Const (Cint i1)) as a1)
    :: Aeq (BinOp (Le, e2, Const (Cint n2)), Const (Cint i2))
    :: rest
    when IntLit.isone i1 && IntLit.isone i2 && Exp.equal e1 e2 && IntLit.lt n1 n2 ->
      (* second inequality redundant *)
      pi_sorted_remove_redundant (a1 :: rest)
  | Aeq (BinOp (Lt, Const (Cint n1), e1), Const (Cint i1))
    :: (Aeq (BinOp (Lt, Const (Cint n2), e2), Const (Cint i2)) as a2)
    :: rest
    when IntLit.isone i1 && IntLit.isone i2 && Exp.equal e1 e2 && IntLit.lt n1 n2 ->
      (* first inequality redundant *)
      pi_sorted_remove_redundant (a2 :: rest)
  | a1 :: a2 :: rest ->
      if Predicates.equal_atom a1 a2 then pi_sorted_remove_redundant (a2 :: rest)
      else a1 :: pi_sorted_remove_redundant (a2 :: rest)
  | [a] ->
      [a]
  | [] ->
      []


(** find the unsigned expressions in sigma (immediately inside a pointsto, for now) *)
let sigma_get_unsigned_exps sigma =
  let uexps = ref [] in
  let do_hpred (hpred : Predicates.hpred) =
    match hpred with
    | Hpointsto (_, Eexp (e, _), Sizeof {typ= {desc= Tint ik}}) when Typ.ikind_is_unsigned ik ->
        uexps := e :: !uexps
    | _ ->
        ()
  in
  List.iter ~f:do_hpred sigma ;
  !uexps


(** Collapse consecutive indices that should be added. For instance, this function reduces [x[1][1]]
    to [x[2]]. The [typ] argument is used to ensure the soundness of this collapsing. *)
let exp_collapse_consecutive_indices_prop (typ : Typ.t) exp =
  let typ_is_base (typ1 : Typ.t) =
    match typ1.desc with Tint _ | Tfloat _ | Tstruct _ | Tvoid | Tfun -> true | _ -> false
  in
  let typ_is_one_step_from_base =
    match typ.desc with Tptr (t, _) | Tarray {elt= t} -> typ_is_base t | _ -> false
  in
  let rec exp_remove (e0 : Exp.t) =
    match e0 with
    | Lindex (Lindex (base, e1), e2) ->
        let e0' : Exp.t = Lindex (base, BinOp (PlusA None, e1, e2)) in
        exp_remove e0'
    | _ ->
        e0
  in
  if typ_is_one_step_from_base then exp_remove exp else exp


(** {2 Compaction} *)

(** Return a compact representation of the prop *)
let prop_compact sh (prop : normal t) : normal t =
  let sigma' = List.map ~f:(Predicates.hpred_compact sh) prop.sigma in
  unsafe_cast_to_normal (set prop ~sigma:sigma')


(** {2 Query about Proposition} *)

(** Check if the sigma part of the proposition is emp *)
let prop_is_emp p = match p.sigma with [] -> true | _ -> false

(** {2 Functions for changing and generating propositions} *)

(** Conjoin a heap predicate by separating conjunction. *)
let prop_hpred_star (p : 'a t) (h : Predicates.hpred) : exposed t =
  let sigma' = h :: p.sigma in
  set p ~sigma:sigma'


let prop_sigma_star (p : 'a t) (sigma : sigma) : exposed t =
  let sigma' = sigma @ p.sigma in
  set p ~sigma:sigma'


(* Module for normalization *)
module Normalize = struct
  (** Eliminates all empty lsegs from sigma, and collect equalities The empty lsegs include

      - "lseg_pe para 0 e elist",
      - "dllseg_pe para iF oB oF iB elist" with iF = 0 or iB = 0,
      - "lseg_pe para e1 e2 elist" and the rest of sigma contains the "cell" e1,
      - "dllseg_pe para iF oB oF iB elist" and the rest of sigma contains cell iF or iB. *)
  let sigma_remove_emptylseg sigma =
    let alloc_set =
      let rec f_alloc set (sigma1 : sigma) =
        match sigma1 with
        | [] ->
            set
        | Hpointsto (e, _, _) :: sigma' | Hlseg (Predicates.Lseg_NE, _, e, _, _) :: sigma' ->
            f_alloc (Exp.Set.add e set) sigma'
        | Hdllseg (Predicates.Lseg_NE, _, iF, _, _, iB, _) :: sigma' ->
            f_alloc (Exp.Set.add iF (Exp.Set.add iB set)) sigma'
        | _ :: sigma' ->
            f_alloc set sigma'
      in
      f_alloc Exp.Set.empty sigma
    in
    let rec f eqs_zero sigma_passed (sigma1 : sigma) =
      match sigma1 with
      | [] ->
          (List.rev eqs_zero, List.rev sigma_passed)
      | (Hpointsto _ as hpred) :: sigma' ->
          f eqs_zero (hpred :: sigma_passed) sigma'
      | Hlseg (Lseg_PE, _, e1, e2, _) :: sigma'
        when Exp.equal e1 Exp.zero || Exp.Set.mem e1 alloc_set ->
          f (Predicates.Aeq (e1, e2) :: eqs_zero) sigma_passed sigma'
      | (Hlseg _ as hpred) :: sigma' ->
          f eqs_zero (hpred :: sigma_passed) sigma'
      | Hdllseg (Lseg_PE, _, iF, oB, oF, iB, _) :: sigma'
        when Exp.equal iF Exp.zero || Exp.Set.mem iF alloc_set || Exp.equal iB Exp.zero
             || Exp.Set.mem iB alloc_set ->
          f (Predicates.Aeq (iF, oF) :: Predicates.Aeq (iB, oB) :: eqs_zero) sigma_passed sigma'
      | (Hdllseg _ as hpred) :: sigma' ->
          f eqs_zero (hpred :: sigma_passed) sigma'
    in
    f [] [] sigma


  let sigma_intro_nonemptylseg e1 e2 sigma =
    let rec f sigma_passed (sigma1 : sigma) =
      match sigma1 with
      | [] ->
          List.rev sigma_passed
      | (Hpointsto _ as hpred) :: sigma' ->
          f (hpred :: sigma_passed) sigma'
      | Hlseg (Lseg_PE, para, f1, f2, shared) :: sigma'
        when (Exp.equal e1 f1 && Exp.equal e2 f2) || (Exp.equal e2 f1 && Exp.equal e1 f2) ->
          f (Predicates.Hlseg (Lseg_NE, para, f1, f2, shared) :: sigma_passed) sigma'
      | (Hlseg _ as hpred) :: sigma' ->
          f (hpred :: sigma_passed) sigma'
      | Hdllseg (Lseg_PE, para, iF, oB, oF, iB, shared) :: sigma'
        when (Exp.equal e1 iF && Exp.equal e2 oF)
             || (Exp.equal e2 iF && Exp.equal e1 oF)
             || (Exp.equal e1 iB && Exp.equal e2 oB)
             || (Exp.equal e2 iB && Exp.equal e1 oB) ->
          f (Predicates.Hdllseg (Lseg_NE, para, iF, oB, oF, iB, shared) :: sigma_passed) sigma'
      | (Hdllseg _ as hpred) :: sigma' ->
          f (hpred :: sigma_passed) sigma'
    in
    f [] sigma


  let ( -- ) = IntLit.sub

  let ( ++ ) = IntLit.add

  let sym_eval ?(destructive = false) tenv abs e =
    let lookup = Tenv.lookup tenv in
    let rec eval (e : Exp.t) : Exp.t =
      (* L.d_str " ["; Predicates.d_exp e; L.d_str"] "; *)
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
      | Sizeof {nbytes= Some n} when destructive ->
          Exp.Const (Const.Cint (IntLit.of_int n))
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
            if abs then Exp.get_undefined false else UnOp (LNot, e1', topt) )
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
            if abs then Exp.get_undefined false else UnOp (Neg, e1', topt) )
      | UnOp (BNot, e1, topt) -> (
        match eval e1 with
        | UnOp (BNot, e2', _) ->
            e2'
        | Const (Cint i) ->
            Exp.int (IntLit.lognot i)
        | e1' ->
            if abs then Exp.get_undefined false else UnOp (BNot, e1', topt) )
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
              if abs && isPlusA then Exp.get_undefined false
              else if abs && not isPlusA then e1' +++ Exp.get_undefined false
              else e1' +++ e2' )
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
                if abs then Exp.get_undefined false else e1' --- e2' )
      | BinOp (MinusPP, e1, e2) ->
          if abs then Exp.get_undefined false else BinOp (MinusPP, eval e1, eval e2)
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
              if abs then Exp.get_undefined false else BinOp (omult, e1', e2') )
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
              if abs then Exp.get_undefined false else BinOp (div, e1', e2') )
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
              if abs then Exp.get_undefined false else BinOp (Mod, e1', e2') )
      | BinOp (Shiftlt, e1, e2) -> (
          if abs then Exp.get_undefined false
          else
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
          if abs then Exp.get_undefined false
          else
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
              if abs then Exp.get_undefined false else BinOp (BAnd, e1', e2') )
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
              if abs then Exp.get_undefined false else BinOp (BOr, e1', e2') )
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
              if abs then Exp.get_undefined false else BinOp (BXor, e1', e2') )
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
    (* L.d_str "sym_eval "; Predicates.d_exp e; L.d_str" --> "; Predicates.d_exp e'; L.d_ln (); *)
    if Exp.equal e e' then e else e'


  let exp_normalize ?destructive tenv sub exp =
    let exp' = Predicates.exp_sub sub exp in
    let abstract_expressions = !BiabductionConfig.abs_val >= 1 in
    sym_eval ?destructive tenv abstract_expressions exp'


  let texp_normalize tenv sub (exp : Exp.t) : Exp.t =
    match exp with
    | Sizeof {dynamic_length= None} ->
        exp
    | Sizeof ({dynamic_length= Some dyn_len} as sizeof_data) ->
        let dyn_len' = exp_normalize tenv sub dyn_len in
        if phys_equal dyn_len dyn_len' then exp
        else Sizeof {sizeof_data with dynamic_length= Some dyn_len'}
    | _ ->
        exp_normalize tenv sub exp


  let exp_normalize_noabs tenv sub exp =
    BiabductionConfig.run_with_abs_val_equal_zero (exp_normalize tenv sub) exp


  (** Turn an inequality expression into an atom *)
  let mk_inequality tenv (e : Exp.t) : Predicates.atom =
    match e with
    | BinOp (Le, base, Const (Cint n)) -> (
        (* base <= n case *)
        let nbase = exp_normalize_noabs tenv Predicates.sub_empty base in
        match nbase with
        | BinOp (PlusA _, base', Const (Cint n')) ->
            let new_offset = Exp.int (n -- n') in
            let new_e : Exp.t = BinOp (Le, base', new_offset) in
            Aeq (new_e, Exp.one)
        | BinOp (PlusA _, Const (Cint n'), base') ->
            let new_offset = Exp.int (n -- n') in
            let new_e : Exp.t = BinOp (Le, base', new_offset) in
            Aeq (new_e, Exp.one)
        | BinOp (MinusA _, base', Const (Cint n')) ->
            let new_offset = Exp.int (n ++ n') in
            let new_e : Exp.t = BinOp (Le, base', new_offset) in
            Aeq (new_e, Exp.one)
        | BinOp (MinusA _, Const (Cint n'), base') ->
            let new_offset = Exp.int (n' -- n -- IntLit.one) in
            let new_e : Exp.t = BinOp (Lt, new_offset, base') in
            Aeq (new_e, Exp.one)
        | UnOp (Neg, new_base, _) ->
            (* In this case, base = -new_base. Construct -n-1 < new_base. *)
            let new_offset = Exp.int (IntLit.zero -- n -- IntLit.one) in
            let new_e : Exp.t = BinOp (Lt, new_offset, new_base) in
            Aeq (new_e, Exp.one)
        | _ ->
            Aeq (e, Exp.one) )
    | BinOp (Lt, Const (Cint n), base) -> (
        (* n < base case *)
        let nbase = exp_normalize_noabs tenv Predicates.sub_empty base in
        match nbase with
        | BinOp (PlusA _, base', Const (Cint n')) ->
            let new_offset = Exp.int (n -- n') in
            let new_e : Exp.t = BinOp (Lt, new_offset, base') in
            Aeq (new_e, Exp.one)
        | BinOp (PlusA _, Const (Const.Cint n'), base') ->
            let new_offset = Exp.int (n -- n') in
            let new_e : Exp.t = BinOp (Lt, new_offset, base') in
            Aeq (new_e, Exp.one)
        | BinOp (MinusA _, base', Const (Cint n')) ->
            let new_offset = Exp.int (n ++ n') in
            let new_e : Exp.t = BinOp (Lt, new_offset, base') in
            Aeq (new_e, Exp.one)
        | BinOp (MinusA _, Const (Cint n'), base') ->
            let new_offset = Exp.int (n' -- n -- IntLit.one) in
            let new_e : Exp.t = BinOp (Le, base', new_offset) in
            Aeq (new_e, Exp.one)
        | UnOp (Neg, new_base, _) ->
            (* In this case, base = -new_base. Construct new_base <= -n-1 *)
            let new_offset = Exp.int (IntLit.zero -- n -- IntLit.one) in
            let new_e : Exp.t = BinOp (Le, new_base, new_offset) in
            Aeq (new_e, Exp.one)
        | _ ->
            Aeq (e, Exp.one) )
    | _ ->
        Aeq (e, Exp.one)


  (** Normalize an inequality *)
  let inequality_normalize tenv (a : Predicates.atom) =
    (* turn an expression into a triple (pos,neg,off) of positive and negative occurrences, and
       integer offset representing inequality [sum(pos) - sum(neg) + off <= 0] *)
    let rec exp_to_posnegoff (e : Exp.t) =
      match e with
      | Const (Cint n) ->
          ([], [], n)
      | BinOp (PlusA _, e1, e2) | BinOp (PlusPI, e1, e2) ->
          let pos1, neg1, n1 = exp_to_posnegoff e1 in
          let pos2, neg2, n2 = exp_to_posnegoff e2 in
          (pos1 @ pos2, neg1 @ neg2, n1 ++ n2)
      | BinOp (MinusA _, e1, e2) | BinOp (MinusPI, e1, e2) | BinOp (MinusPP, e1, e2) ->
          let pos1, neg1, n1 = exp_to_posnegoff e1 in
          let pos2, neg2, n2 = exp_to_posnegoff e2 in
          (pos1 @ neg2, neg1 @ pos2, n1 -- n2)
      | UnOp (Neg, e1, _) ->
          let pos1, neg1, n1 = exp_to_posnegoff e1 in
          (neg1, pos1, IntLit.zero -- n1)
      | _ ->
          ([e], [], IntLit.zero)
    in
    (* sort and filter out expressions appearing in both the positive and negative part *)
    let normalize_posnegoff (pos, neg, off) =
      let pos' = List.sort ~compare:Exp.compare pos in
      let neg' = List.sort ~compare:Exp.compare neg in
      let rec combine pacc nacc = function
        | x :: ps, y :: ng -> (
          match Exp.compare x y with
          | n when n < 0 ->
              combine (x :: pacc) nacc (ps, y :: ng)
          | 0 ->
              combine pacc nacc (ps, ng)
          | _ ->
              combine pacc (y :: nacc) (x :: ps, ng) )
        | ps, ng ->
            (List.rev_append pacc ps, List.rev_append nacc ng)
      in
      let pos'', neg'' = combine [] [] (pos', neg') in
      (pos'', neg'', off)
    in
    (* turn a non-empty list of expressions into a sum expression *)
    let rec exp_list_to_sum : Exp.t list -> Exp.t = function
      | [] ->
          assert false
      | [e] ->
          e
      | e :: el ->
          BinOp (PlusA None, e, exp_list_to_sum el)
    in
    let norm_from_exp e : Exp.t =
      match normalize_posnegoff (exp_to_posnegoff e) with
      | [], [], n ->
          BinOp (Le, Exp.int n, Exp.zero)
      | [], neg, n ->
          BinOp (Lt, Exp.int (n -- IntLit.one), exp_list_to_sum neg)
      | pos, [], n ->
          BinOp (Le, exp_list_to_sum pos, Exp.int (IntLit.zero -- n))
      | pos, neg, n ->
          let lhs_e : Exp.t = BinOp (MinusA None, exp_list_to_sum pos, exp_list_to_sum neg) in
          BinOp (Le, lhs_e, Exp.int (IntLit.zero -- n))
    in
    let ineq =
      match a with Aeq (ineq, Const (Cint i)) when IntLit.isone i -> ineq | _ -> assert false
    in
    match ineq with
    | BinOp (Le, e1, e2) ->
        let e : Exp.t = BinOp (MinusA None, e1, e2) in
        mk_inequality tenv (norm_from_exp e)
    | BinOp (Lt, e1, e2) ->
        let e : Exp.t = BinOp (MinusA None, BinOp (MinusA None, e1, e2), Exp.minus_one) in
        mk_inequality tenv (norm_from_exp e)
    | _ ->
        a


  (** Normalize an atom. We keep the convention that inequalities with constants are only of the
      form [e <= n] and [n < e]. *)
  let atom_normalize tenv sub a0 =
    let a = Predicates.atom_sub sub a0 in
    let rec normalize_eq (eq : Exp.t * Exp.t) =
      match eq with
      | BinOp (PlusA _, e1, Const (Cint n1)), Const (Cint n2)
      (* e1+n1==n2 ---> e1==n2-n1 *)
      | BinOp (PlusPI, e1, Const (Cint n1)), Const (Cint n2) ->
          (e1, Exp.int (n2 -- n1))
      | BinOp (MinusA _, e1, Const (Cint n1)), Const (Cint n2)
      (* e1-n1==n2 ---> e1==n1+n2 *)
      | BinOp (MinusPI, e1, Const (Cint n1)), Const (Cint n2) ->
          (e1, Exp.int (n1 ++ n2))
      | BinOp (MinusA _, Const (Cint n1), e1), Const (Cint n2) ->
          (* n1-e1 == n2 -> e1==n1-n2 *)
          (e1, Exp.int (n1 -- n2))
      | Lfield (e1', fld1, _), Lfield (e2', fld2, _) ->
          if Fieldname.equal fld1 fld2 then normalize_eq (e1', e2') else eq
      | Lindex (e1', idx1), Lindex (e2', idx2) ->
          if Exp.equal idx1 idx2 then normalize_eq (e1', e2')
          else if Exp.equal e1' e2' then normalize_eq (idx1, idx2)
          else eq
      | BinOp ((PlusA _ | PlusPI | MinusA _ | MinusPI), e1, e2), e1' when Exp.equal e1 e1' ->
          (e2, Exp.int IntLit.zero)
      | BinOp ((PlusA _ | PlusPI), e2, e1), e1' when Exp.equal e1 e1' ->
          (e2, Exp.int IntLit.zero)
      | e1', BinOp ((PlusA _ | PlusPI | MinusA _ | MinusPI), e1, e2) when Exp.equal e1 e1' ->
          (e2, Exp.int IntLit.zero)
      | e1', BinOp ((PlusA _ | PlusPI), e2, e1) when Exp.equal e1 e1' ->
          (e2, Exp.int IntLit.zero)
      | _ ->
          eq
    in
    let handle_unary_negation (e1 : Exp.t) (e2 : Exp.t) =
      match (e1, e2) with
      | (UnOp (LNot, e1', _), Const (Cint i) | Const (Cint i), UnOp (LNot, e1', _))
        when IntLit.iszero i ->
          (e1', Exp.zero, true)
      | _ ->
          (e1, e2, false)
    in
    let handle_boolean_operation orig_a from_equality e1 e2 : Predicates.atom =
      let ne1 = exp_normalize tenv sub e1 in
      let ne2 = exp_normalize tenv sub e2 in
      let ne1', ne2', op_negated = handle_unary_negation ne1 ne2 in
      let e1', e2' = normalize_eq (ne1', ne2') in
      let e1'', e2'' = exp_reorder e1' e2' in
      let use_equality = if op_negated then not from_equality else from_equality in
      if Bool.equal use_equality from_equality && phys_equal e1 e1'' && phys_equal e2 e2'' then
        orig_a
      else if use_equality then Aeq (e1'', e2'')
      else Aneq (e1'', e2'')
    in
    let a' : Predicates.atom =
      match a with
      | Aeq (e1, e2) ->
          handle_boolean_operation a true e1 e2
      | Aneq (e1, e2) ->
          handle_boolean_operation a false e1 e2
      | Apred (tag, es) ->
          let es' = IList.map_changed es ~equal:Exp.equal ~f:(fun e -> exp_normalize tenv sub e) in
          if phys_equal es es' then a else Apred (tag, es')
      | Anpred (tag, es) ->
          let es' = IList.map_changed es ~equal:Exp.equal ~f:(fun e -> exp_normalize tenv sub e) in
          if phys_equal es es' then a else Anpred (tag, es')
    in
    if atom_is_inequality a' then inequality_normalize tenv a' else a'


  let normalize_and_strengthen_atom tenv (p : normal t) (a : Predicates.atom) : Predicates.atom =
    let a' = atom_normalize tenv p.sub a in
    match a' with
    | Aeq (BinOp (Le, Var id, Const (Cint n)), Const (Cint i)) when IntLit.isone i ->
        let lower = Exp.int (n -- IntLit.one) in
        let a_lower : Predicates.atom = Aeq (BinOp (Lt, lower, Var id), Exp.one) in
        if not (List.mem ~equal:Predicates.equal_atom p.pi a_lower) then a'
        else Aeq (Var id, Exp.int n)
    | Aeq (BinOp (Lt, Const (Cint n), Var id), Const (Cint i)) when IntLit.isone i ->
        let upper = Exp.int (n ++ IntLit.one) in
        let a_upper : Predicates.atom = Aeq (BinOp (Le, Var id, upper), Exp.one) in
        if not (List.mem ~equal:Predicates.equal_atom p.pi a_upper) then a' else Aeq (Var id, upper)
    | Aeq (BinOp (Ne, e1, e2), Const (Cint i)) when IntLit.isone i ->
        Aneq (e1, e2)
    | _ ->
        a'


  let rec strexp_normalize tenv sub (se : Predicates.strexp) : Predicates.strexp =
    match se with
    | Eexp (e, inst) ->
        let e' = exp_normalize tenv sub e in
        if phys_equal e e' then se else Eexp (e', inst)
    | Estruct (fld_cnts, inst) -> (
      match fld_cnts with
      | [] ->
          se
      | _ :: _ ->
          let fld_cnts' =
            IList.map_changed fld_cnts ~equal:[%equal: Fieldname.t * Predicates.strexp]
              ~f:(fun ((fld, cnt) as x) ->
                let cnt' = strexp_normalize tenv sub cnt in
                if phys_equal cnt cnt' then x else (fld, cnt') )
          in
          if
            phys_equal fld_cnts fld_cnts'
            && List.is_sorted ~compare:[%compare: Fieldname.t * Predicates.strexp] fld_cnts
          then se
          else
            let fld_cnts'' =
              List.sort ~compare:[%compare: Fieldname.t * Predicates.strexp] fld_cnts'
            in
            Estruct (fld_cnts'', inst) )
    | Earray (len, idx_cnts, inst) -> (
        let len' = exp_normalize_noabs tenv sub len in
        match idx_cnts with
        | [] ->
            if Exp.equal len len' then se else Earray (len', idx_cnts, inst)
        | _ :: _ ->
            let idx_cnts' =
              IList.map_changed idx_cnts ~equal:[%equal: Exp.t * Predicates.strexp]
                ~f:(fun ((idx, cnt) as x) ->
                  let idx' = exp_normalize tenv sub idx in
                  let cnt' = strexp_normalize tenv sub cnt in
                  if phys_equal idx idx' && phys_equal cnt cnt' then x else (idx', cnt') )
            in
            if
              phys_equal idx_cnts idx_cnts'
              && List.is_sorted ~compare:[%compare: Exp.t * Predicates.strexp] idx_cnts
            then se
            else
              let idx_cnts'' = List.sort ~compare:[%compare: Exp.t * Predicates.strexp] idx_cnts' in
              Earray (len', idx_cnts'', inst) )


  (** Exp.Construct a pointsto. *)
  let mk_ptsto tenv lexp sexp te : Predicates.hpred =
    let nsexp = strexp_normalize tenv Predicates.sub_empty sexp in
    Hpointsto (lexp, nsexp, te)


  (** Construct a points-to predicate for an expression using either the provided expression [name]
      as base for fresh identifiers. If [struct_init_mode] is [Fld_init], initialize the fields of
      structs with fresh variables. *)
  let mk_ptsto_exp tenv struct_init_mode (exp, (te : Exp.t), expo) inst : Predicates.hpred =
    let default_strexp () : Predicates.strexp =
      match te with
      | Sizeof {typ; dynamic_length} ->
          create_strexp_of_type tenv struct_init_mode typ dynamic_length inst
      | Var _ ->
          Estruct ([], inst)
      | te ->
          L.internal_error "trying to create ptsto with type: %a@." (Exp.pp_texp_full Pp.text) te ;
          assert false
    in
    let strexp : Predicates.strexp =
      match expo with Some e -> Eexp (e, inst) | None -> default_strexp ()
    in
    mk_ptsto tenv exp strexp te


  (** Captured variables in the closures consist of expressions and variables, with the implicit
      assumption that these two values are in the relation &var -> id. However, after bi-abduction,
      etc. the constraint may not hold anymore, so this function ensures that it is always kept. In
      particular, we have &var -> id iff we also have the pair (id, var) as part of captured
      variables. *)
  let make_captured_in_closures_consistent sigma =
    let find_correct_captured captured =
      let find_captured_variable_in_the_heap captured' hpred =
        match (hpred : Predicates.hpred) with
        | Hpointsto (Exp.Lvar var, Eexp (Exp.Var id, _), _) ->
            IList.map_changed ~equal:phys_equal
              ~f:(fun ((e_captured, var_captured, t, mode) as captured_item) ->
                match e_captured with
                | Exp.Var id_captured ->
                    if Ident.equal id id_captured && Pvar.equal var var_captured then captured_item
                    else if Ident.equal id id_captured then (e_captured, var, t, mode)
                    else if Pvar.equal var var_captured then (Exp.Var id, var_captured, t, mode)
                    else captured_item
                | _ ->
                    captured_item )
              captured'
        | _ ->
            captured'
      in
      List.fold_left ~f:find_captured_variable_in_the_heap ~init:captured sigma
    in
    let process_closures exp =
      match exp with
      | Exp.Closure {name; captured_vars} ->
          let correct_captured = find_correct_captured captured_vars in
          if phys_equal captured_vars correct_captured then exp
          else Exp.Closure {name; captured_vars= correct_captured}
      | _ ->
          exp
    in
    let rec process_closures_in_se (se : Predicates.strexp) : Predicates.strexp =
      match se with
      | Eexp (exp, inst) ->
          let new_exp = process_closures exp in
          if phys_equal exp new_exp then se else Eexp (new_exp, inst)
      | Estruct (fields, inst) ->
          let new_fields =
            IList.map_changed ~equal:phys_equal
              ~f:(fun ((field, se) as fse) ->
                let se' = process_closures_in_se se in
                if phys_equal se se' then fse else (field, se') )
              fields
          in
          if phys_equal fields new_fields then se else Estruct (new_fields, inst)
      | _ ->
          se
    in
    let process_closures_in_the_heap (hpred : Predicates.hpred) : Predicates.hpred =
      match hpred with
      | Hpointsto (e, se, inst) ->
          let new_se = process_closures_in_se se in
          if phys_equal new_se se then hpred else Hpointsto (e, new_se, inst)
      | _ ->
          hpred
    in
    List.map ~f:process_closures_in_the_heap sigma


  let rec hpred_normalize tenv sub (hpred : Predicates.hpred) : Predicates.hpred =
    let replace_hpred hpred' =
      L.d_strln "found array with sizeof(..) size" ;
      L.d_str "converting original hpred: " ;
      Predicates.d_hpred hpred ;
      L.d_ln () ;
      L.d_str "into the following: " ;
      Predicates.d_hpred hpred' ;
      L.d_ln () ;
      hpred'
    in
    match hpred with
    | Hpointsto (root, cnt, te) -> (
        let normalized_root = exp_normalize tenv sub root in
        let normalized_cnt = strexp_normalize tenv sub cnt in
        let normalized_te = texp_normalize tenv sub te in
        match (normalized_cnt, normalized_te) with
        | Earray ((Exp.Sizeof _ as size), [], inst), Sizeof {typ= {desc= Tarray _}} ->
            (* check for an empty array whose size expression is (Sizeof type), and turn the array
               into a strexp of the given type *)
            let hpred' = mk_ptsto_exp tenv Fld_init (root, size, None) inst in
            replace_hpred hpred'
        | ( Earray
              (BinOp (Mult _, Sizeof {typ= t; dynamic_length= None; subtype= st1}, x), esel, inst)
          , Sizeof {typ= {desc= Tarray {elt}} as arr} )
          when Typ.equal t elt ->
            let dynamic_length = Some x in
            let sizeof_data =
              {Exp.typ= arr; nbytes= None; dynamic_length; subtype= st1; nullable= false}
            in
            let hpred' = mk_ptsto_exp tenv Fld_init (root, Sizeof sizeof_data, None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | ( Earray (BinOp (Mult _, x, Sizeof {typ; dynamic_length= None; subtype}), esel, inst)
          , Sizeof {typ= {desc= Tarray {elt}} as arr} )
          when Typ.equal typ elt ->
            let sizeof_data =
              {Exp.typ= arr; nbytes= None; dynamic_length= Some x; subtype; nullable= false}
            in
            let hpred' = mk_ptsto_exp tenv Fld_init (root, Sizeof sizeof_data, None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | ( Earray
              ( BinOp ((Mult _ as omult), Sizeof {typ; dynamic_length= Some len; subtype}, x)
              , esel
              , inst )
          , Sizeof {typ= {desc= Tarray {elt}} as arr} )
          when Typ.equal typ elt ->
            let sizeof_data =
              { Exp.typ= arr
              ; nbytes= None
              ; dynamic_length= Some (Exp.BinOp (omult, x, len))
              ; subtype
              ; nullable= false }
            in
            let hpred' = mk_ptsto_exp tenv Fld_init (root, Sizeof sizeof_data, None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | ( Earray
              ( BinOp ((Mult _ as omult), x, Sizeof {typ; dynamic_length= Some len; subtype})
              , esel
              , inst )
          , Sizeof {typ= {desc= Tarray {elt}} as arr} )
          when Typ.equal typ elt ->
            let sizeof_data =
              { Exp.typ= arr
              ; nbytes= None
              ; dynamic_length= Some (Exp.BinOp (omult, x, len))
              ; subtype
              ; nullable= false }
            in
            let hpred' = mk_ptsto_exp tenv Fld_init (root, Sizeof sizeof_data, None) inst in
            replace_hpred (replace_array_contents hpred' esel)
        | _ ->
            Hpointsto (normalized_root, normalized_cnt, normalized_te) )
    | Hlseg (k, para, e1, e2, elist) ->
        let normalized_e1 = exp_normalize tenv sub e1 in
        let normalized_e2 = exp_normalize tenv sub e2 in
        let normalized_elist = List.map ~f:(exp_normalize tenv sub) elist in
        let normalized_para = hpara_normalize tenv para in
        Hlseg (k, normalized_para, normalized_e1, normalized_e2, normalized_elist)
    | Hdllseg (k, para, e1, e2, e3, e4, elist) ->
        let norm_e1 = exp_normalize tenv sub e1 in
        let norm_e2 = exp_normalize tenv sub e2 in
        let norm_e3 = exp_normalize tenv sub e3 in
        let norm_e4 = exp_normalize tenv sub e4 in
        let norm_elist = List.map ~f:(exp_normalize tenv sub) elist in
        let norm_para = hpara_dll_normalize tenv para in
        Hdllseg (k, norm_para, norm_e1, norm_e2, norm_e3, norm_e4, norm_elist)


  and hpara_normalize tenv (para : Predicates.hpara) =
    let normalized_body = List.map ~f:(hpred_normalize tenv Predicates.sub_empty) para.body in
    let sorted_body = List.sort ~compare:Predicates.compare_hpred normalized_body in
    {para with body= sorted_body}


  and hpara_dll_normalize tenv (para : Predicates.hpara_dll) =
    let normalized_body = List.map ~f:(hpred_normalize tenv Predicates.sub_empty) para.body_dll in
    let sorted_body = List.sort ~compare:Predicates.compare_hpred normalized_body in
    {para with body_dll= sorted_body}


  let sigma_normalize tenv sub sigma =
    let sigma' =
      List.map ~f:(hpred_normalize tenv sub) sigma
      |> make_captured_in_closures_consistent
      |> List.stable_sort ~compare:Predicates.compare_hpred
    in
    if equal_sigma sigma sigma' then sigma else sigma'


  let pi_tighten_ineq tenv pi =
    let ineq_list, nonineq_list = List.partition_tf ~f:atom_is_inequality pi in
    let diseq_list =
      let get_disequality_info acc (a : Predicates.atom) =
        match a with
        | Aneq (Const (Cint n), e) | Aneq (e, Const (Cint n)) ->
            (e, n) :: acc
        | _ ->
            acc
      in
      List.fold ~f:get_disequality_info ~init:[] nonineq_list
    in
    let is_neq e n = List.exists ~f:(fun (e', n') -> Exp.equal e e' && IntLit.eq n n') diseq_list in
    let le_list_tightened =
      let get_le_inequality_info acc a =
        match atom_exp_le_const a with Some (e, n) -> (e, n) :: acc | _ -> acc
      in
      let rec le_tighten le_list_done = function
        | [] ->
            List.rev le_list_done
        | (e, n) :: le_list_todo ->
            (* e <= n *)
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
        | [] ->
            List.rev lt_list_done
        | (n, e) :: lt_list_todo ->
            (* n < e *)
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
        ~f:(fun (a : Predicates.atom) ->
          match a with
          | Aneq (Const (Cint n), e) | Aneq (e, Const (Cint n)) ->
              (not
                 (List.exists
                    ~f:(fun (e', n') -> Exp.equal e e' && IntLit.lt n' n)
                    le_list_tightened ) )
              && not
                   (List.exists
                      ~f:(fun (n', e') -> Exp.equal e e' && IntLit.leq n n')
                      lt_list_tightened )
          | _ ->
              true )
        nonineq_list
    in
    (ineq_list', nonineq_list')


  (** Normalization of pi. The normalization filters out obviously - true disequalities, such as e
      <> e + 1. *)
  let pi_normalize tenv sub sigma pi0 =
    let pi = List.map ~f:(atom_normalize tenv sub) pi0 in
    let ineq_list, nonineq_list = pi_tighten_ineq tenv pi in
    let syntactically_different : Exp.t * Exp.t -> bool = function
      | BinOp (op1, e1, Const c1), BinOp (op2, e2, Const c2) when Exp.equal e1 e2 ->
          Binop.equal op1 op2 && Binop.injective op1 && not (Const.equal c1 c2)
      | e1, BinOp (op2, e2, Const c2) when Exp.equal e1 e2 ->
          Binop.injective op2 && Binop.is_zero_runit op2 && not (Const.equal (Cint IntLit.zero) c2)
      | BinOp (op1, e1, Const c1), e2 when Exp.equal e1 e2 ->
          Binop.injective op1 && Binop.is_zero_runit op1 && not (Const.equal (Cint IntLit.zero) c1)
      | _ ->
          false
    in
    let filter_useful_atom : Predicates.atom -> bool =
      let unsigned_exps = lazy (sigma_get_unsigned_exps sigma) in
      function
      | Aneq ((Var _ as e), Const (Cint n)) when IntLit.isnegative n ->
          not (List.exists ~f:(Exp.equal e) (Lazy.force unsigned_exps))
      | Aneq (e1, e2) ->
          not (syntactically_different (e1, e2))
      | Aeq (Const c1, Const c2) ->
          not (Const.equal c1 c2)
      | _ ->
          true
    in
    let pi' =
      List.stable_sort ~compare:Predicates.compare_atom
        (List.filter ~f:filter_useful_atom nonineq_list @ ineq_list)
    in
    let pi'' = pi_sorted_remove_redundant pi' in
    if equal_pi pi0 pi'' then pi0 else pi''


  (** normalize the footprint part, and rename any primed vars in the footprint with fresh footprint
      vars *)
  let footprint_normalize tenv prop =
    let nsigma = sigma_normalize tenv Predicates.sub_empty prop.sigma_fp in
    let npi = pi_normalize tenv Predicates.sub_empty nsigma prop.pi_fp in
    let ids_primed =
      let fav =
        pi_free_vars npi |> Sequence.filter ~f:Ident.is_primed |> Ident.hashqueue_of_sequence
      in
      sigma_free_vars nsigma
      |> Sequence.filter ~f:Ident.is_primed
      |> Ident.hashqueue_of_sequence ~init:fav
      |> Ident.HashQueue.keys
    in
    (* only keep primed vars *)
    let npi', nsigma' =
      if List.is_empty ids_primed then (npi, nsigma)
      else
        (* replace primed vars by fresh footprint vars *)
        let ids_footprint =
          List.map ~f:(fun id -> (id, Ident.create_fresh Ident.kfootprint)) ids_primed
        in
        let ren_sub =
          Predicates.subst_of_list
            (List.map ~f:(fun (id1, id2) -> (id1, Exp.Var id2)) ids_footprint)
        in
        let nsigma' = sigma_normalize tenv Predicates.sub_empty (sigma_sub ren_sub nsigma) in
        let npi' = pi_normalize tenv Predicates.sub_empty nsigma' (pi_sub ren_sub npi) in
        (npi', nsigma')
    in
    set prop ~pi_fp:npi' ~sigma_fp:nsigma'


  (** This function assumes that if (x,Exp.Var(y)) in sub, then compare x y = 1 *)
  let sub_normalize sub =
    let f (id, e) = (not (Ident.is_primed id)) && not (Exp.ident_mem e id) in
    let sub' = Predicates.sub_filter_pair ~f sub in
    if Predicates.equal_subst sub sub' then sub else sub'


  (** Conjoin a pure atomic predicate by normal conjunction. *)
  let rec prop_atom_and tenv ?(footprint = false) (p : normal t) a : normal t =
    let a' = normalize_and_strengthen_atom tenv p a in
    if List.mem ~equal:Predicates.equal_atom p.pi a' then p
    else
      let p' =
        match a' with
        | Aeq (Var i, e) when Exp.ident_mem e i ->
            p
        | Aeq (Var i, e) ->
            let sub_list = [(i, e)] in
            let mysub = Predicates.subst_of_list sub_list in
            let p_sub = Predicates.sub_filter (fun i' -> not (Ident.equal i i')) p.sub in
            let sub' =
              Predicates.sub_join mysub (Predicates.sub_range_map (Predicates.exp_sub mysub) p_sub)
            in
            let nsub', npi', nsigma' =
              let nsigma' = sigma_normalize tenv sub' p.sigma in
              (sub_normalize sub', pi_normalize tenv sub' nsigma' p.pi, nsigma')
            in
            let eqs_zero, nsigma'' = sigma_remove_emptylseg nsigma' in
            let p' = unsafe_cast_to_normal (set p ~sub:nsub' ~pi:npi' ~sigma:nsigma'') in
            List.fold ~f:(prop_atom_and tenv ~footprint) ~init:p' eqs_zero
        | Aeq (e1, e2) when Exp.equal e1 e2 ->
            p
        | Aneq (e1, e2) ->
            let sigma' = sigma_intro_nonemptylseg e1 e2 p.sigma in
            let pi' = pi_normalize tenv p.sub sigma' (a' :: p.pi) in
            unsafe_cast_to_normal (set p ~pi:pi' ~sigma:sigma')
        | _ ->
            let pi' = pi_normalize tenv p.sub p.sigma (a' :: p.pi) in
            unsafe_cast_to_normal (set p ~pi:pi')
      in
      if not footprint then p'
      else
        let predicate_warning =
          not (Predicates.atom_free_vars a' |> Sequence.for_all ~f:Ident.is_footprint)
        in
        let p'' =
          if predicate_warning then footprint_normalize tenv p'
          else
            match a' with
            | Aeq (Exp.Var i, e) when not (Exp.ident_mem e i) ->
                let mysub = Predicates.subst_of_list [(i, e)] in
                let sigma_fp' = sigma_normalize tenv mysub p'.sigma_fp in
                let pi_fp' = a' :: pi_normalize tenv mysub sigma_fp' p'.pi_fp in
                footprint_normalize tenv (set p' ~pi_fp:pi_fp' ~sigma_fp:sigma_fp')
            | _ ->
                footprint_normalize tenv (set p' ~pi_fp:(a' :: p'.pi_fp))
        in
        if predicate_warning then (
          L.d_warning "dropping non-footprint " ;
          Predicates.d_atom a' ;
          L.d_ln () ) ;
        unsafe_cast_to_normal p''


  (** normalize a prop *)
  let normalize tenv (eprop : 'a t) : normal t =
    let p0 =
      unsafe_cast_to_normal
        (set prop_emp ~sigma:(sigma_normalize tenv Predicates.sub_empty eprop.sigma))
    in
    let nprop = List.fold ~f:(prop_atom_and tenv) ~init:p0 (get_pure_extended eprop) in
    unsafe_cast_to_normal
      (footprint_normalize tenv (set nprop ~pi_fp:eprop.pi_fp ~sigma_fp:eprop.sigma_fp))
end

(* End of module Normalize *)

let exp_normalize_prop ?destructive tenv prop exp =
  BiabductionConfig.run_with_abs_val_equal_zero
    (Normalize.exp_normalize ?destructive tenv prop.sub)
    exp


let lexp_normalize_prop tenv p lexp =
  let root = Exp.root_of_lexp lexp in
  let offsets = Predicates.exp_get_offsets lexp in
  let nroot = exp_normalize_prop tenv p root in
  let noffsets =
    List.map
      ~f:(fun (n : Predicates.offset) ->
        match n with
        | Off_fld _ ->
            n
        | Off_index e ->
            Predicates.Off_index (exp_normalize_prop tenv p e) )
      offsets
  in
  Predicates.exp_add_offsets nroot noffsets


let atom_normalize_prop tenv prop atom =
  BiabductionConfig.run_with_abs_val_equal_zero (Normalize.atom_normalize tenv prop.sub) atom


let sigma_normalize_prop tenv prop sigma =
  BiabductionConfig.run_with_abs_val_equal_zero (Normalize.sigma_normalize tenv prop.sub) sigma


let sigma_replace_exp tenv epairs sigma =
  let sigma' = List.map ~f:(Predicates.hpred_replace_exp epairs) sigma in
  Normalize.sigma_normalize tenv Predicates.sub_empty sigma'


(** Construct an atom. *)
let mk_atom tenv atom =
  BiabductionConfig.run_with_abs_val_equal_zero
    (fun () -> Normalize.atom_normalize tenv Predicates.sub_empty atom)
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
let mk_lseg tenv k para e_start e_end es_shared : Predicates.hpred =
  let npara = Normalize.hpara_normalize tenv para in
  Hlseg (k, npara, e_start, e_end, es_shared)


(** Exp.Construct a dllseg predicate *)
let mk_dllseg tenv k para exp_iF exp_oB exp_oF exp_iB exps_shared : Predicates.hpred =
  let npara = Normalize.hpara_dll_normalize tenv para in
  Hdllseg (k, npara, exp_iF, exp_oB, exp_oF, exp_iB, exps_shared)


(** Construct a points-to predicate for a single program variable. If [expand_structs] is
    [Fld_init], initialize the fields of structs with fresh variables. *)
let mk_ptsto_lvar tenv expand_structs inst ((pvar : Pvar.t), texp, expo) : Predicates.hpred =
  Normalize.mk_ptsto_exp tenv expand_structs (Lvar pvar, texp, expo) inst


(** Conjoin [exp1]=[exp2] with a symbolic heap [prop]. *)
let conjoin_eq tenv ?(footprint = false) exp1 exp2 prop =
  Normalize.prop_atom_and tenv ~footprint prop (Aeq (exp1, exp2))


(** Conjoin [exp1!=exp2] with a symbolic heap [prop]. *)
let conjoin_neq tenv ?(footprint = false) exp1 exp2 prop =
  Normalize.prop_atom_and tenv ~footprint prop (Aneq (exp1, exp2))


(** Reset every inst in the prop using the given map *)
let prop_reset_inst inst_map prop =
  let sigma' = List.map ~f:(Predicates.hpred_instmap inst_map) prop.sigma in
  let sigma_fp' = List.map ~f:(Predicates.hpred_instmap inst_map) prop.sigma_fp in
  set prop ~sigma:sigma' ~sigma_fp:sigma_fp'


(** {1 Functions for transforming footprints into propositions.} *)

(** The ones used for abstraction add/remove local stacks in order to stop the firing of some
    abstraction rules. The other usual transforation functions do not use this hack. *)

(** Extract the footprint and return it as a prop *)
let extract_footprint p = set prop_emp ~pi:p.pi_fp ~sigma:p.sigma_fp

(** Extract the (footprint,current) pair *)
let extract_spec (p : normal t) : normal t * normal t =
  let pre = extract_footprint p in
  let post = set p ~pi_fp:[] ~sigma_fp:[] in
  (unsafe_cast_to_normal pre, unsafe_cast_to_normal post)


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
  let filter e = Exp.free_vars e |> Sequence.for_all ~f:Ident.is_normal in
  let lexps = Predicates.hpred_list_get_lexps filter sigma in
  List.sort ~compare:exp_compare_neg lexps


let sigma_dfs_sort tenv sigma =
  let init () =
    let start_lexps = sigma_get_start_lexps_sort sigma in
    ExpStack.init start_lexps
  in
  let final () = ExpStack.final () in
  let rec handle_strexp (se : Predicates.strexp) =
    match se with
    | Eexp (e, _) ->
        ExpStack.push e
    | Estruct (fld_se_list, _) ->
        List.iter ~f:(fun (_, se) -> handle_strexp se) fld_se_list
    | Earray (_, idx_se_list, _) ->
        List.iter ~f:(fun (_, se) -> handle_strexp se) idx_se_list
  in
  let rec handle_e visited seen e (sigma : sigma) =
    match sigma with
    | [] ->
        (visited, List.rev seen)
    | hpred :: cur -> (
      match hpred with
      | Hpointsto (e', se, _) when Exp.equal e e' ->
          handle_strexp se ;
          (hpred :: visited, List.rev_append cur seen)
      | Hlseg (_, _, root, next, shared) when Exp.equal e root ->
          List.iter ~f:ExpStack.push (next :: shared) ;
          (hpred :: visited, List.rev_append cur seen)
      | Hdllseg (_, _, iF, oB, oF, iB, shared) when Exp.equal e iF || Exp.equal e iB ->
          List.iter ~f:ExpStack.push (oB :: oF :: shared) ;
          (hpred :: visited, List.rev_append cur seen)
      | _ ->
          handle_e visited (hpred :: seen) e cur )
  in
  let rec handle_sigma visited = function
    | [] ->
        List.rev visited
    | cur ->
        if ExpStack.is_empty () then
          let cur' = Normalize.sigma_normalize tenv Predicates.sub_empty cur in
          List.rev_append cur' visited
        else
          let e = ExpStack.pop () in
          let visited', cur' = handle_e visited [] e cur in
          handle_sigma visited' cur'
  in
  init () ;
  let sigma' = handle_sigma [] sigma in
  final () ;
  sigma'


let dfs_sort tenv p : sorted t =
  let sigma = p.sigma in
  let sigma' = sigma_dfs_sort tenv sigma in
  let sigma_fp = p.sigma_fp in
  let sigma_fp' = sigma_dfs_sort tenv sigma_fp in
  let p' = set p ~sigma:sigma' ~sigma_fp:sigma_fp' in
  (* L.out "@[<2>P SORTED:@\n%a@\n@." pp_prop p'; *)
  unsafe_cast_to_sorted p'


let rec strexp_get_array_indices acc (se : Predicates.strexp) =
  match se with
  | Eexp _ ->
      acc
  | Estruct (fsel, _) ->
      let se_list = List.map ~f:snd fsel in
      List.fold ~f:strexp_get_array_indices ~init:acc se_list
  | Earray (_, isel, _) ->
      let acc_new = List.fold ~f:(fun acc' (idx, _) -> idx :: acc') ~init:acc isel in
      let se_list = List.map ~f:snd isel in
      List.fold ~f:strexp_get_array_indices ~init:acc_new se_list


let hpred_get_array_indices acc (hpred : Predicates.hpred) =
  match hpred with
  | Hpointsto (_, se, _) ->
      strexp_get_array_indices acc se
  | Hlseg _ | Hdllseg _ ->
      acc


let sigma_get_array_indices sigma =
  let indices = List.fold ~f:hpred_get_array_indices ~init:[] sigma in
  List.rev indices


let compute_reindexing_from_indices list =
  let get_id_offset (e : Exp.t) =
    match e with
    | BinOp (PlusA _, Var id, Const (Cint offset)) ->
        if Ident.is_primed id then Some (id, offset) else None
    | _ ->
        None
  in
  let rec select list_passed list_seen = function
    | [] ->
        list_passed
    | x :: list_rest ->
        let id_offset_opt = get_id_offset x in
        let list_passed_new =
          match id_offset_opt with
          | None ->
              list_passed
          | Some (id, _) ->
              let find_id_in_list l = List.exists l ~f:(fun e -> Exp.ident_mem e id) in
              if find_id_in_list list_seen || find_id_in_list list_passed then list_passed
              else x :: list_passed
        in
        let list_seen_new = x :: list_seen in
        select list_passed_new list_seen_new list_rest
  in
  let list_passed = select [] [] list in
  let transform x =
    let id, offset = match get_id_offset x with None -> assert false | Some io -> io in
    let base_new : Exp.t = Var (Ident.create_fresh Ident.kprimed) in
    let offset_new = Exp.int (IntLit.neg offset) in
    let exp_new : Exp.t = BinOp (PlusA None, base_new, offset_new) in
    (id, exp_new)
  in
  let reindexing = List.map ~f:transform list_passed in
  Predicates.subst_of_list reindexing


let apply_reindexing tenv (subst : Predicates.subst) prop =
  let nsigma = Normalize.sigma_normalize tenv subst prop.sigma in
  let npi = Normalize.pi_normalize tenv subst nsigma prop.pi in
  let nsub, atoms =
    let dom_subst = List.map ~f:fst (Predicates.sub_to_list subst) in
    let in_dom_subst id = List.exists ~f:(Ident.equal id) dom_subst in
    let sub' = Predicates.sub_filter (fun id -> not (in_dom_subst id)) prop.sub in
    let contains_substituted_id e = Exp.free_vars e |> Sequence.exists ~f:in_dom_subst in
    let sub_eqs, sub_keep = Predicates.sub_range_partition contains_substituted_id sub' in
    let eqs = Predicates.sub_to_list sub_eqs in
    let atoms =
      List.map ~f:(fun (id, e) -> Predicates.Aeq (Var id, Normalize.exp_normalize tenv subst e)) eqs
    in
    (sub_keep, atoms)
  in
  let p' = unsafe_cast_to_normal (set prop ~sub:nsub ~pi:npi ~sigma:nsigma) in
  List.fold ~f:(Normalize.prop_atom_and tenv) ~init:p' atoms


let prop_rename_array_indices tenv prop =
  if !BiabductionConfig.footprint then prop
  else
    let indices = sigma_get_array_indices prop.sigma in
    let not_same_base_lt_offsets (e1 : Exp.t) (e2 : Exp.t) =
      match (e1, e2) with
      | BinOp (PlusA _, e1', Const (Cint n1')), BinOp (PlusA _, e2', Const (Cint n2')) ->
          not (Exp.equal e1' e2' && IntLit.lt n1' n2')
      | _ ->
          true
    in
    let rec select_minimal_indices indices_seen = function
      | [] ->
          List.rev indices_seen
      | index :: indices_rest ->
          let indices_seen' = List.filter ~f:(not_same_base_lt_offsets index) indices_seen in
          let indices_seen_new = index :: indices_seen' in
          let indices_rest_new = List.filter ~f:(not_same_base_lt_offsets index) indices_rest in
          select_minimal_indices indices_seen_new indices_rest_new
    in
    let minimal_indices = select_minimal_indices [] indices in
    let subst = compute_reindexing_from_indices minimal_indices in
    apply_reindexing tenv subst prop


let compute_renaming free_vars =
  let ids_primed, ids_nonprimed = List.partition_tf ~f:Ident.is_primed free_vars in
  let ids_footprint = List.filter ~f:Ident.is_footprint ids_nonprimed in
  let id_base_primed = Ident.create Ident.kprimed 0 in
  let id_base_footprint = Ident.create Ident.kfootprint 0 in
  let rec f id_base index ren_subst = function
    | [] ->
        ren_subst
    | id :: ids ->
        let new_id = Ident.set_stamp id_base index in
        if Ident.equal id new_id then f id_base (index + 1) ren_subst ids
        else f id_base (index + 1) ((id, new_id) :: ren_subst) ids
  in
  let ren_primed = f id_base_primed 0 [] ids_primed in
  let ren_footprint = f id_base_footprint 0 [] ids_footprint in
  ren_primed @ ren_footprint


let rec idlist_assoc id = function
  | [] ->
      raise Caml.Not_found
  | (i, x) :: l ->
      if Ident.equal i id then x else idlist_assoc id l


let ident_captured_ren ren id =
  (* If not defined in ren, id should be mapped to itself *)
  try idlist_assoc id ren with Caml.Not_found -> id


let rec exp_captured_ren ren (e : Exp.t) : Exp.t =
  match e with
  | Var id ->
      Var (ident_captured_ren ren id)
  | Exn e ->
      Exn (exp_captured_ren ren e)
  | Closure {name; captured_vars} ->
      let captured_vars' =
        List.map ~f:(fun (e, v, t, m) -> (exp_captured_ren ren e, v, t, m)) captured_vars
      in
      Closure {name; captured_vars= captured_vars'}
  | Const _ ->
      e
  | Sizeof ({dynamic_length} as sizeof_data) ->
      Sizeof {sizeof_data with dynamic_length= Option.map ~f:(exp_captured_ren ren) dynamic_length}
  | Cast (t, e) ->
      Cast (t, exp_captured_ren ren e)
  | UnOp (op, e, topt) ->
      UnOp (op, exp_captured_ren ren e, topt)
  | BinOp (op, e1, e2) ->
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      BinOp (op, e1', e2')
  | Lvar id ->
      Lvar id
  | Lfield (e, fld, typ) ->
      Lfield (exp_captured_ren ren e, fld, typ)
  | Lindex (e1, e2) ->
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      Lindex (e1', e2')


let atom_captured_ren ren (a : Predicates.atom) : Predicates.atom =
  match a with
  | Aeq (e1, e2) ->
      Aeq (exp_captured_ren ren e1, exp_captured_ren ren e2)
  | Aneq (e1, e2) ->
      Aneq (exp_captured_ren ren e1, exp_captured_ren ren e2)
  | Apred (a, es) ->
      Apred (a, List.map ~f:(fun e -> exp_captured_ren ren e) es)
  | Anpred (a, es) ->
      Anpred (a, List.map ~f:(fun e -> exp_captured_ren ren e) es)


let rec strexp_captured_ren ren (se : Predicates.strexp) : Predicates.strexp =
  match se with
  | Eexp (e, inst) ->
      Eexp (exp_captured_ren ren e, inst)
  | Estruct (fld_se_list, inst) ->
      let f (fld, se) = (fld, strexp_captured_ren ren se) in
      Estruct (List.map ~f fld_se_list, inst)
  | Earray (len, idx_se_list, inst) ->
      let f (idx, se) =
        let idx' = exp_captured_ren ren idx in
        (idx', strexp_captured_ren ren se)
      in
      let len' = exp_captured_ren ren len in
      Earray (len', List.map ~f idx_se_list, inst)


and hpred_captured_ren ren (hpred : Predicates.hpred) : Predicates.hpred =
  match hpred with
  | Hpointsto (base, se, te) ->
      let base' = exp_captured_ren ren base in
      let se' = strexp_captured_ren ren se in
      let te' = exp_captured_ren ren te in
      Hpointsto (base', se', te')
  | Hlseg (k, para, e1, e2, elist) ->
      let para' = hpara_ren para in
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      let elist' = List.map ~f:(exp_captured_ren ren) elist in
      Hlseg (k, para', e1', e2', elist')
  | Hdllseg (k, para, e1, e2, e3, e4, elist) ->
      let para' = hpara_dll_ren para in
      let e1' = exp_captured_ren ren e1 in
      let e2' = exp_captured_ren ren e2 in
      let e3' = exp_captured_ren ren e3 in
      let e4' = exp_captured_ren ren e4 in
      let elist' = List.map ~f:(exp_captured_ren ren) elist in
      Hdllseg (k, para', e1', e2', e3', e4', elist')


and hpara_ren (para : Predicates.hpara) : Predicates.hpara =
  let av =
    Predicates.hpara_shallow_free_vars para |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys
  in
  let ren = compute_renaming av in
  let root = ident_captured_ren ren para.root in
  let next = ident_captured_ren ren para.next in
  let svars = List.map ~f:(ident_captured_ren ren) para.svars in
  let evars = List.map ~f:(ident_captured_ren ren) para.evars in
  let body = List.map ~f:(hpred_captured_ren ren) para.body in
  {root; next; svars; evars; body}


and hpara_dll_ren (para : Predicates.hpara_dll) : Predicates.hpara_dll =
  let av =
    Predicates.hpara_dll_shallow_free_vars para
    |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys
  in
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

let sub_captured_ren ren sub =
  Predicates.sub_map (ident_captured_ren ren) (exp_captured_ren ren) sub


(** Canonicalize the names of primed variables and footprint vars. *)
let prop_rename_primed_footprint_vars tenv (p : normal t) : normal t =
  let p = prop_rename_array_indices tenv p in
  let bound_vars =
    let filter id = Ident.is_footprint id || Ident.is_primed id in
    dfs_sort tenv p |> sorted_free_vars |> Sequence.filter ~f:filter |> Ident.hashqueue_of_sequence
    |> Ident.HashQueue.keys
  in
  let ren = compute_renaming bound_vars in
  let sub' = sub_captured_ren ren p.sub in
  let pi' = pi_captured_ren ren p.pi in
  let sigma' = sigma_captured_ren ren p.sigma in
  let pi_fp' = pi_captured_ren ren p.pi_fp in
  let sigma_fp' = sigma_captured_ren ren p.sigma_fp in
  let sub_for_normalize = Predicates.sub_empty in
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


(** Apply subsitution to prop. *)
let prop_sub subst (prop : 'a t) : exposed t =
  let pi = pi_sub subst (prop.pi @ pi_of_subst prop.sub) in
  let sigma = sigma_sub subst prop.sigma in
  let pi_fp = pi_sub subst prop.pi_fp in
  let sigma_fp = sigma_sub subst prop.sigma_fp in
  set prop_emp ~pi ~sigma ~pi_fp ~sigma_fp


(** Apply renaming substitution to a proposition. *)
let prop_ren_sub tenv (ren_sub : Predicates.subst) (prop : normal t) : normal t =
  Normalize.normalize tenv (prop_sub ren_sub prop)


(** Existentially quantify the [ids] in [prop]. [ids] should not contain any primed variables. If
    [ids_queue] is passed then the function uses it instead of [ids] for membership tests. *)
let exist_quantify tenv ?ids_queue ids (prop : normal t) : normal t =
  assert (not (List.exists ~f:Ident.is_primed ids)) ;
  (* sanity check *)
  if List.is_empty ids then prop
  else
    let gen_fresh_id_sub id = (id, Exp.Var (Ident.create_fresh Ident.kprimed)) in
    let ren_sub = Predicates.subst_of_list (List.map ~f:gen_fresh_id_sub ids) in
    let prop' =
      (* throw away x=E if x becomes x_ *)
      let filter =
        match ids_queue with
        | Some q ->
            (* this is more efficient than a linear scan of [ids] *)
            fun id -> not (Ident.HashQueue.mem q id)
        | None ->
            fun id -> not (List.mem ~equal:Ident.equal ids id)
      in
      let sub = Predicates.sub_filter filter prop.sub in
      if Predicates.equal_subst sub prop.sub then prop else unsafe_cast_to_normal (set prop ~sub)
    in
    (*
    L.out "@[<2>.... Existential Quantification ....@\n";
    L.out "SUB:%a@\n" pp_sub prop'.sub;
    L.out "PI:%a@\n" pp_pi prop'.pi;
    L.out "PROP:%a@\n@." pp_prop prop';
    *)
    prop_ren_sub tenv ren_sub prop'


(** Apply the substitution [fe] to all the expressions in the prop. *)
let prop_expmap (fe : Exp.t -> Exp.t) prop =
  let f (e, sil_opt) = (fe e, sil_opt) in
  let pi = List.map ~f:(Predicates.atom_expmap fe) prop.pi in
  let sigma = List.map ~f:(Predicates.hpred_expmap f) prop.sigma in
  let pi_fp = List.map ~f:(Predicates.atom_expmap fe) prop.pi_fp in
  let sigma_fp = List.map ~f:(Predicates.hpred_expmap f) prop.sigma_fp in
  set prop ~pi ~sigma ~pi_fp ~sigma_fp


(** convert the normal vars to primed vars *)
let prop_normal_vars_to_primed_vars tenv p =
  let ids_queue =
    free_vars p |> Sequence.filter ~f:Ident.is_normal |> Ident.hashqueue_of_sequence
  in
  exist_quantify tenv ~ids_queue (Ident.HashQueue.keys ids_queue) p


(** convert the primed vars to normal vars. *)
let prop_primed_vars_to_normal_vars tenv (prop : normal t) : normal t =
  let ids =
    free_vars prop
    |> Sequence.filter ~f:Ident.is_primed
    |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys
  in
  let ren_sub =
    Predicates.subst_of_list
      (List.map ~f:(fun i -> (i, Exp.Var (Ident.create_fresh Ident.knormal))) ids)
  in
  prop_ren_sub tenv ren_sub prop


let from_pi pi = set prop_emp ~pi

let from_sigma sigma = set prop_emp ~sigma

(** {2 Prop iterators} *)

(** Iterator state over sigma. *)
type 'a prop_iter =
  { pit_sub: Predicates.subst  (** substitution for equalities *)
  ; pit_pi: pi  (** pure part *)
  ; pit_newpi: (bool * Predicates.atom) list  (** newly added atoms. *)
  ; (* The first records !BiabductionConfig.footprint. *)
    pit_old: sigma  (** sigma already visited *)
  ; pit_curr: Predicates.hpred  (** current element *)
  ; pit_state: 'a  (** state of current element *)
  ; pit_new: sigma  (** sigma not yet visited *)
  ; pit_pi_fp: pi  (** pure part of the footprint *)
  ; pit_sigma_fp: sigma  (** sigma part of the footprint *) }

let prop_iter_create prop =
  match prop.sigma with
  | hpred :: sigma' ->
      Some
        { pit_sub= prop.sub
        ; pit_pi= prop.pi
        ; pit_newpi= []
        ; pit_old= []
        ; pit_curr= hpred
        ; pit_state= ()
        ; pit_new= sigma'
        ; pit_pi_fp= prop.pi_fp
        ; pit_sigma_fp= prop.sigma_fp }
  | _ ->
      None


(** Return the prop associated to the iterator. *)
let prop_iter_to_prop tenv iter =
  let sigma = List.rev_append iter.pit_old (iter.pit_curr :: iter.pit_new) in
  let prop =
    Normalize.normalize tenv
      (set prop_emp ~sub:iter.pit_sub ~pi:iter.pit_pi ~sigma ~pi_fp:iter.pit_pi_fp
         ~sigma_fp:iter.pit_sigma_fp )
  in
  List.fold
    ~f:(fun p (footprint, atom) -> Normalize.prop_atom_and tenv ~footprint p atom)
    ~init:prop iter.pit_newpi


(** Add an atom to the pi part of prop iter. The first parameter records whether it is done during
    footprint or during re - execution. *)
let prop_iter_add_atom footprint iter atom =
  {iter with pit_newpi= (footprint, atom) :: iter.pit_newpi}


(** Remove the current element of the iterator, and return the prop associated to the resulting
    iterator *)
let prop_iter_remove_curr_then_to_prop tenv iter : normal t =
  let sigma = List.rev_append iter.pit_old iter.pit_new in
  let normalized_sigma = Normalize.sigma_normalize tenv iter.pit_sub sigma in
  let prop =
    set prop_emp ~sub:iter.pit_sub ~pi:iter.pit_pi ~sigma:normalized_sigma ~pi_fp:iter.pit_pi_fp
      ~sigma_fp:iter.pit_sigma_fp
  in
  unsafe_cast_to_normal prop


(** Return the current hpred and state. *)
let prop_iter_current tenv iter =
  let curr = Normalize.hpred_normalize tenv iter.pit_sub iter.pit_curr in
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
  | [] ->
      assert false (* the list should be nonempty *)
  | hpred :: hpred_list ->
      let pit_new' = hpred_list @ iter.pit_new in
      {iter with pit_curr= hpred; pit_state= (); pit_new= pit_new'}


let prop_iter_next iter =
  match iter.pit_new with
  | [] ->
      None
  | hpred' :: new' ->
      Some
        { iter with
          pit_old= iter.pit_curr :: iter.pit_old
        ; pit_curr= hpred'
        ; pit_state= ()
        ; pit_new= new' }


(** Insert before the current element of the iterator. *)
let prop_iter_prev_then_insert iter hpred =
  {iter with pit_new= iter.pit_curr :: iter.pit_new; pit_curr= hpred}


(** Scan sigma to find an [hpred] satisfying the filter function. *)
let rec prop_iter_find iter filter =
  match filter iter.pit_curr with
  | Some st ->
      Some {iter with pit_state= st}
  | None -> (
    match prop_iter_next iter with None -> None | Some iter' -> prop_iter_find iter' filter )


(** Set the state of the iterator *)
let prop_iter_set_state iter state = {iter with pit_state= state}

let prop_iter_make_id_primed tenv id iter =
  let pid = Ident.create_fresh Ident.kprimed in
  let sub_id = Predicates.subst_of_list [(id, Exp.Var pid)] in
  let normalize (id, e) =
    let eq' : Predicates.atom =
      Aeq (Predicates.exp_sub sub_id (Var id), Predicates.exp_sub sub_id e)
    in
    Normalize.atom_normalize tenv Predicates.sub_empty eq'
  in
  let rec split pairs_unpid pairs_pid = function
    | [] ->
        (List.rev pairs_unpid, List.rev pairs_pid)
    | (eq :: eqs_cur : pi) -> (
      match eq with
      | Aeq (Var id1, e1) when Exp.ident_mem e1 id1 ->
          L.internal_error "@[<2>#### ERROR: an assumption of the analyzer broken ####@\n" ;
          L.internal_error "Broken Assumption: id notin e for all (id,e) in sub@\n" ;
          L.internal_error "(id,e) : (%a,%a)@\n" Ident.pp id1 Exp.pp e1 ;
          L.internal_error "PROP : %a@\n@." (pp_prop Pp.text) (prop_iter_to_prop tenv iter) ;
          assert false
      | Aeq (Var id1, e1) when Ident.equal pid id1 ->
          split pairs_unpid ((id1, e1) :: pairs_pid) eqs_cur
      | Aeq (Var id1, e1) ->
          split ((id1, e1) :: pairs_unpid) pairs_pid eqs_cur
      | _ ->
          assert false )
  in
  let rec get_eqs acc = function
    | [] | [_] ->
        List.rev acc
    | (_, e1) :: ((_, e2) :: _ as pairs) ->
        get_eqs (Predicates.Aeq (e1, e2) :: acc) pairs
  in
  let sub_new, sub_use, eqs_add =
    let eqs = List.map ~f:normalize (Predicates.sub_to_list iter.pit_sub) in
    let pairs_unpid, pairs_pid = split [] [] eqs in
    match pairs_pid with
    | [] ->
        let sub_unpid = Predicates.subst_of_list pairs_unpid in
        let pairs = (id, Exp.Var pid) :: pairs_unpid in
        (sub_unpid, Predicates.subst_of_list pairs, [])
    | (id1, e1) :: _ ->
        let sub_id1 = Predicates.subst_of_list [(id1, e1)] in
        let pairs_unpid' =
          List.map ~f:(fun (id', e') -> (id', Predicates.exp_sub sub_id1 e')) pairs_unpid
        in
        let sub_unpid = Predicates.subst_of_list pairs_unpid' in
        let pairs = (id, e1) :: pairs_unpid' in
        (sub_unpid, Predicates.subst_of_list pairs, get_eqs [] pairs_pid)
  in
  let nsub_new = Normalize.sub_normalize sub_new in
  { iter with
    pit_sub= nsub_new
  ; pit_pi= pi_sub sub_use (iter.pit_pi @ eqs_add)
  ; pit_old= sigma_sub sub_use iter.pit_old
  ; pit_curr= Predicates.hpred_sub sub_use iter.pit_curr
  ; pit_new= sigma_sub sub_use iter.pit_new }


(** Find fav of the footprint part of the iterator *)
let prop_iter_footprint_gen_free_vars {pit_sigma_fp; pit_pi_fp} =
  Sequence.Generator.(sigma_gen_free_vars pit_sigma_fp >>= fun () -> pi_gen_free_vars pit_pi_fp)


(** Find fav of the iterator *)
let prop_iter_gen_free_vars ({pit_sub; pit_pi; pit_newpi; pit_old; pit_new; pit_curr} as iter) =
  let open Sequence.Generator in
  Predicates.subst_gen_free_vars pit_sub
  >>= fun () ->
  pi_gen_free_vars pit_pi
  >>= fun () ->
  pi_gen_free_vars (List.map ~f:snd pit_newpi)
  >>= fun () ->
  sigma_gen_free_vars pit_old
  >>= fun () ->
  sigma_gen_free_vars pit_new
  >>= fun () ->
  Predicates.hpred_gen_free_vars pit_curr >>= fun () -> prop_iter_footprint_gen_free_vars iter


let prop_iter_free_vars iter = Sequence.Generator.run (prop_iter_gen_free_vars iter)

let prop_iter_max_stamp ?(f = all_true) iter = seq_max_stamp f (prop_iter_free_vars iter)

(** Extract the sigma part of the footprint *)
let prop_iter_get_footprint_sigma iter = iter.pit_sigma_fp

(** Replace the sigma part of the footprint *)
let prop_iter_replace_footprint_sigma iter sigma = {iter with pit_sigma_fp= sigma}

let rec strexp_gc_fields (se : Predicates.strexp) =
  match se with
  | Eexp _ ->
      Some se
  | Estruct (fsel, inst) ->
      let fselo = List.map ~f:(fun (f, se) -> (f, strexp_gc_fields se)) fsel in
      let fsel' =
        let fselo' = List.filter ~f:(function _, Some _ -> true | _ -> false) fselo in
        List.map ~f:(function f, seo -> (f, Option.value_exn seo)) fselo'
      in
      if [%equal: (Fieldname.t * Predicates.strexp) list] fsel fsel' then Some se
      else Some (Predicates.Estruct (fsel', inst))
  | Earray _ ->
      Some se


let hpred_gc_fields (hpred : Predicates.hpred) : Predicates.hpred =
  match hpred with
  | Hpointsto (e, se, te) -> (
    match strexp_gc_fields se with
    | None ->
        hpred
    | Some se' ->
        if Predicates.equal_strexp se se' then hpred else Hpointsto (e, se', te) )
  | Hlseg _ | Hdllseg _ ->
      hpred


let rec prop_iter_map f iter =
  let hpred_curr = f iter in
  let iter' = {iter with pit_curr= hpred_curr} in
  match prop_iter_next iter' with None -> iter' | Some iter'' -> prop_iter_map f iter''


(** Collect garbage fields. *)
let prop_iter_gc_fields iter =
  let f iter' = hpred_gc_fields iter'.pit_curr in
  prop_iter_map f iter


let prop_expand tenv prop =
  let pi_sigma_list = Predicates.sigma_to_sigma_ne prop.sigma in
  let f props_acc (pi, sigma) =
    let sigma' = sigma_normalize_prop tenv prop sigma in
    let prop' = unsafe_cast_to_normal (set prop ~sigma:sigma') in
    List.fold ~f:(Normalize.prop_atom_and tenv) ~init:prop' pi :: props_acc
  in
  List.fold ~f ~init:[] pi_sigma_list


(*** START of module Metrics ***)
module Metrics : sig
  val prop_size : 'a t -> int
end = struct
  let ptsto_weight = 1

  and lseg_weight = 3

  let rec hpara_size hpara = sigma_size hpara.Predicates.body

  and hpara_dll_size hpara_dll = sigma_size hpara_dll.Predicates.body_dll

  and hpred_size (hpred : Predicates.hpred) =
    match hpred with
    | Hpointsto _ ->
        ptsto_weight
    | Hlseg (_, hpara, _, _, _) ->
        lseg_weight * hpara_size hpara
    | Hdllseg (_, hpara_dll, _, _, _, _, _) ->
        lseg_weight * hpara_dll_size hpara_dll


  and sigma_size sigma =
    let size = ref 0 in
    List.iter ~f:(fun hpred -> size := hpred_size hpred + !size) sigma ;
    !size


  (** Compute a size value for the prop, which indicates its complexity *)
  let prop_size p =
    let size_current = sigma_size p.sigma in
    let size_footprint = sigma_size p.sigma_fp in
    max size_current size_footprint
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
    let rhs_is_var : Predicates.strexp -> bool = function Eexp (Var _, _) -> true | _ -> false in
    let rec rhs_only_vars : Predicates.strexp -> bool = function
      | Eexp (Var _, _) ->
          true
      | Estruct (fsel, _) ->
          List.for_all ~f:(fun (_, se) -> rhs_only_vars se) fsel
      | Earray _ ->
          true
      | _ ->
          false
    in
    let hpred_is_var : Predicates.hpred -> bool = function
      (* stack variable with no constraints *)
      | Hpointsto (e, se, _) ->
          lhs_is_lvar e && rhs_is_var se
      | _ ->
          false
    in
    let hpred_only_allocation : Predicates.hpred -> bool = function
      (* only constraint is allocation *)
      | Hpointsto (e, se, _) ->
          lhs_is_var_lvar e && rhs_only_vars se
      | _ ->
          false
    in
    let check_pre hpred_filter pre =
      let check_pi pi = List.is_empty pi in
      let check_sigma sigma = List.for_all ~f:hpred_filter sigma in
      check_pi pre.pi && check_sigma pre.sigma
    in
    let pres_no_constraints = List.filter ~f:(check_pre hpred_is_var) preconditions in
    let pres_only_allocation = List.filter ~f:(check_pre hpred_only_allocation) preconditions in
    match (preconditions, pres_no_constraints, pres_only_allocation) with
    | [], _, _ ->
        NoPres
    | _ :: _, _ :: _, _ ->
        Empty
    | _ :: _, [], _ :: _ ->
        OnlyAllocation
    | _ :: _, [], [] ->
        DataConstraints
end

(* Export for interface *)
let exp_normalize_noabs = Normalize.exp_normalize_noabs

let mk_inequality = Normalize.mk_inequality

let mk_ptsto_exp = Normalize.mk_ptsto_exp

let mk_ptsto = Normalize.mk_ptsto

let normalize tenv prop = Normalize.normalize tenv prop

let prop_atom_and tenv ?footprint prop atom = Normalize.prop_atom_and tenv ?footprint prop atom
