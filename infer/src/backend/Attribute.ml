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

(** Attribute manipulation in Propositions (i.e., Symbolic Heaps) *)

module L = Logging
module F = Format


(** Check whether an atom is used to mark an attribute *)
let is_pred atom =
  match atom with
  | Sil.Apred _ | Anpred _ -> true
  | _ -> false

(** Add an attribute associated to the argument expressions *)
let add ?(footprint = false) ?(polarity = true) prop attr args =
  Prop.prop_atom_and ~footprint prop
    (if polarity then Sil.Apred (attr, args) else Sil.Anpred (attr, args))

let attributes_in_same_category attr1 attr2 =
  let cat1 = PredSymb.to_category attr1 in
  let cat2 = PredSymb.to_category attr2 in
  PredSymb.category_equal cat1 cat2

(** Replace an attribute associated to the expression *)
let add_or_replace_check_changed check_attribute_change prop atom0 =
  match atom0 with
  | Sil.Apred (att0, ((_ :: _) as exps0)) | Anpred (att0, ((_ :: _) as exps0)) ->
      let nexps = IList.map (fun e -> Prop.exp_normalize_prop prop e) exps0 in
      let nexp = IList.hd nexps in (* len nexps = len exps0 > 0 by match *)
      let natom = Sil.atom_replace_exp (IList.combine exps0 nexps) atom0 in
      let atom_map = function
        | Sil.Apred (att, exp :: _) | Anpred (att, exp :: _)
          when Exp.equal nexp exp && attributes_in_same_category att att0 ->
            check_attribute_change att att0;
            natom
        | atom ->
            atom in
      let pi = Prop.get_pi prop in
      let pi' = IList.map_changed atom_map pi in
      if pi == pi'
      then Prop.prop_atom_and prop natom
      else Prop.normalize (Prop.replace_pi pi' prop)
  | _ ->
      prop

let add_or_replace prop atom =
  (* wrapper for the most common case: do nothing *)
  let check_attr_changed = (fun _ _ -> ()) in
  add_or_replace_check_changed check_attr_changed prop atom

(** Get all the attributes of the prop *)
let get_all (prop: 'a Prop.t) =
  let res = ref [] in
  let do_atom a = if is_pred a then res := a :: !res in
  IList.iter do_atom (Prop.get_pi prop);
  IList.rev !res

(** Get all the attributes of the prop *)
let get_for_symb prop att =
  IList.filter (function
      | Sil.Apred (att', _) | Anpred (att', _) -> PredSymb.equal att' att
      | _ -> false
    ) (Prop.get_pi prop)

(** Get the attribute associated to the expression, if any *)
let get_for_exp (prop: 'a Prop.t) exp =
  let nexp = Prop.exp_normalize_prop prop exp in
  let atom_get_attr attributes atom =
    match atom with
    | Sil.Apred (_, es) | Anpred (_, es) when IList.mem Exp.equal nexp es -> atom :: attributes
    | _ -> attributes in
  IList.fold_left atom_get_attr [] (Prop.get_pi prop)

let get prop exp category =
  let atts = get_for_exp prop exp in
  try
    Some
      (IList.find (function
           | Sil.Apred (att, _) | Anpred (att, _) ->
               PredSymb.category_equal (PredSymb.to_category att) category
           | _ -> false
         ) atts)
  with Not_found -> None

let get_undef prop exp =
  get prop exp ACundef

let get_resource prop exp =
  get prop exp ACresource

let get_taint prop exp =
  get prop exp ACtaint

let get_autorelease prop exp =
  get prop exp ACautorelease

let get_objc_null prop exp =
  get prop exp ACobjc_null

let get_div0 prop exp =
  get prop exp ACdiv0

let get_observer prop exp =
  get prop exp ACobserver

let get_retval prop exp =
  get prop exp ACretval

let has_dangling_uninit prop exp =
  let la = get_for_exp prop exp in
  IList.exists (function
      | Sil.Apred (a, _) -> PredSymb.equal a (Adangling DAuninit)
      | _ -> false
    ) la

let filter_atoms ~f prop =
  let pi0 = Prop.get_pi prop in
  let pi1 = IList.filter_changed f pi0 in
  if pi1 == pi0 then
    prop
  else
    Prop.normalize (Prop.replace_pi pi1 prop)

let remove prop atom =
  if is_pred atom then
    let natom = Prop.atom_normalize_prop prop atom in
    let f a = not (Sil.atom_equal natom a) in
    filter_atoms ~f prop
  else
    prop

(** Remove an attribute from all the atoms in the heap *)
let remove_for_attr prop att0 =
  let f = function
    | Sil.Apred (att, _) | Anpred (att, _) -> not (PredSymb.equal att0 att)
    | _ -> true in
  filter_atoms ~f prop

let remove_resource ra_kind ra_res =
  let f = function
    | Sil.Apred (Aresource res_action, _) ->
        PredSymb.res_act_kind_compare res_action.ra_kind ra_kind <> 0
        || PredSymb.resource_compare res_action.ra_res ra_res <> 0
    | _ -> true in
  filter_atoms ~f

(** Apply f to every resource attribute in the prop *)
let map_resource prop f =
  let attribute_map e = function
    | PredSymb.Aresource ra -> PredSymb.Aresource (f e ra)
    | att -> att in
  let atom_map = function
    | Sil.Apred (att, ([e] as es)) -> Sil.Apred (attribute_map e att, es)
    | Sil.Anpred (att, ([e] as es)) -> Sil.Anpred (attribute_map e att, es)
    | atom -> atom in
  let pi0 = Prop.get_pi prop in
  let pi1 = IList.map_changed atom_map pi0 in
  if pi1 == pi0 then
    prop
  else
    Prop.normalize (Prop.replace_pi pi1 prop)

(* Replace an attribute OBJC_NULL($n1) with OBJC_NULL(var) when var = $n1, and also sets $n1 =
   0 *)
let replace_objc_null prop lhs_exp rhs_exp =
  match get_objc_null prop rhs_exp, rhs_exp with
  | Some atom, Exp.Var _ ->
      let prop = remove prop atom in
      let prop = Prop.conjoin_eq rhs_exp Exp.zero prop in
      let natom = Sil.atom_replace_exp [(rhs_exp, lhs_exp)] atom in
      add_or_replace prop natom
  | _ -> prop

let rec nullify_exp_with_objc_null prop exp =
  match exp with
  | Exp.BinOp (_, exp1, exp2) ->
      let prop' = nullify_exp_with_objc_null prop exp1 in
      nullify_exp_with_objc_null prop' exp2
  | Exp.UnOp (_, exp, _) ->
      nullify_exp_with_objc_null prop exp
  | Exp.Var _ ->
      (match get_objc_null prop exp with
       | Some atom ->
           let prop' = remove prop atom in
           Prop.conjoin_eq exp Exp.zero prop'
       | _ -> prop)
  | _ -> prop

(** mark Exp.Var's or Exp.Lvar's as undefined *)
let mark_vars_as_undefined prop vars_to_mark callee_pname ret_annots loc path_pos =
  let att_undef = PredSymb.Aundef (callee_pname, ret_annots, loc, path_pos) in
  let mark_var_as_undefined exp prop =
    match exp with
    | Exp.Var _ | Lvar _ -> add_or_replace prop (Apred (att_undef, [exp]))
    | _ -> prop in
  IList.fold_left (fun prop id -> mark_var_as_undefined id prop) prop vars_to_mark

(** type for arithmetic problems *)
type arith_problem =
  (* division by zero *)
  | Div0 of Exp.t

  (* unary minus of unsigned type applied to the given expression *)
  | UminusUnsigned of Exp.t * Typ.t

(** Look for an arithmetic problem in [exp] *)
let find_arithmetic_problem proc_node_session prop exp =
  let exps_divided = ref [] in
  let uminus_unsigned = ref [] in
  let res = ref prop in
  let check_zero e =
    match Prop.exp_normalize_prop prop e with
    | Exp.Const c when Const.iszero_int_float c -> true
    | _ ->
        res := add_or_replace !res (Apred (Adiv0 proc_node_session, [e]));
        false in
  let rec walk = function
    | Exp.Var _ -> ()
    | Exp.UnOp (Unop.Neg, e, Some (
        (Typ.Tint
           (Typ.IUChar | Typ.IUInt | Typ.IUShort | Typ.IULong | Typ.IULongLong) as typ))) ->
        uminus_unsigned := (e, typ) :: !uminus_unsigned
    | Exp.UnOp(_, e, _) -> walk e
    | Exp.BinOp(op, e1, e2) ->
        if op = Binop.Div || op = Binop.Mod then exps_divided := e2 :: !exps_divided;
        walk e1; walk e2
    | Exp.Exn _ -> ()
    | Exp.Closure _ -> ()
    | Exp.Const _ -> ()
    | Exp.Cast (_, e) -> walk e
    | Exp.Lvar _ -> ()
    | Exp.Lfield (e, _, _) -> walk e
    | Exp.Lindex (e1, e2) -> walk e1; walk e2
    | Exp.Sizeof (_, None, _) -> ()
    | Exp.Sizeof (_, Some len, _) -> walk len in
  walk exp;
  try Some (Div0 (IList.find check_zero !exps_divided)), !res
  with Not_found ->
    (match !uminus_unsigned with
     | (e, t):: _ -> Some (UminusUnsigned (e, t)), !res
     | _ -> None, !res)

(** Deallocate the stack variables in [pvars], and replace them by normal variables.
    Return the list of stack variables whose address was still present after deallocation. *)
let deallocate_stack_vars (p: 'a Prop.t) pvars =
  let filter = function
    | Sil.Hpointsto (Exp.Lvar v, _, _) ->
        IList.exists (Pvar.equal v) pvars
    | _ -> false in
  let sigma_stack, sigma_other = IList.partition filter (Prop.get_sigma p) in
  let fresh_address_vars = ref [] in (* fresh vars substituted for the address of stack vars *)
  let stack_vars_address_in_post = ref [] in (* stack vars whose address is still present *)
  let exp_replace = IList.map (function
      | Sil.Hpointsto (Exp.Lvar v, _, _) ->
          let freshv = Ident.create_fresh Ident.kprimed in
          fresh_address_vars := (v, freshv) :: !fresh_address_vars;
          (Exp.Lvar v, Exp.Var freshv)
      | _ -> assert false) sigma_stack in
  let pi1 = IList.map (fun (id, e) -> Sil.Aeq (Exp.Var id, e)) (Sil.sub_to_list (Prop.get_sub p)) in
  let pi = IList.map (Sil.atom_replace_exp exp_replace) ((Prop.get_pi p) @ pi1) in
  let p' =
    Prop.normalize
      (Prop.replace_sub Sil.sub_empty
         (Prop.replace_sigma (Prop.sigma_replace_exp exp_replace sigma_other) p)) in
  let p'' =
    let res = ref p' in
    let p'_fav = Prop.prop_fav p' in
    let do_var (v, freshv) =
      if Sil.fav_mem p'_fav freshv then (* the address of a de-allocated stack var in in the post *)
        begin
          stack_vars_address_in_post := v :: !stack_vars_address_in_post;
          let pred = Sil.Apred (Adangling DAaddr_stack_var, [Exp.Var freshv]) in
          res := add_or_replace !res pred
        end in
    IList.iter do_var !fresh_address_vars;
    !res in
  !stack_vars_address_in_post, IList.fold_left Prop.prop_atom_and p'' pi

(** Input of this method is an exp in a prop. Output is a formal variable or path from a
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
              | Sil.Hpointsto (Exp.Lvar pvar1, Sil.Eexp (exp2, Sil.Iformal(_, _) ), _)
                when Exp.equal exp2 e &&
                     (Pvar.is_local pvar1 || Pvar.is_seed pvar1) ->
                  Some (Exp.Lvar pvar1)
              | Sil.Hpointsto (exp1, Sil.Estruct (fields, _), _) ->
                  IList.fold_right (fun (field, strexp) res ->
                      match res with
                      | Some _ -> res
                      | None ->
                          match strexp with
                          | Sil.Eexp (exp2, _) when Exp.equal exp2 e ->
                              (match find_in_sigma exp1 seen_hpreds with
                               | Some vfs -> Some (Exp.Lfield (vfs, field, Typ.Tvoid))
                               | None -> None)
                          | _ -> None) fields None
              | _ -> None) (Prop.get_sigma prop) None in
  match find_in_sigma e [] with
  | Some vfs -> Some vfs
  | None ->
      match get_objc_null prop e with
      | Some (Apred (Aobjc_null, [_; vfs])) -> Some vfs
      | _ -> None
