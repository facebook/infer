(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Transformation from llvm to llair *)

open HolKernel boolLib bossLib Parse;
open arithmeticTheory pred_setTheory;
open settingsTheory llvmTheory llairTheory;

new_theory "llvm_to_llair";

numLib.prefer_num ();

Definition the_def:
  (the None x = x) ∧
  (the (Some x) _ = x)
End

Definition find_name_def:
  find_name used new suff =
    let n = new ++ (toString suff) in
      if n ∉ used ∨ ¬finite used then
        n
      else
        find_name used new (suff + 1n)
Termination
  WF_REL_TAC `measure (λ(u,new,s). card { str | ?n. str = new++toString n ∧ str ∈ u ∧ s ≤ n })` >> rw [] >>
  qmatch_abbrev_tac `card s1 < card s2` >>
  `s2 ⊆ used` by rw [Abbr `s1`, Abbr `s2`, SUBSET_DEF] >>
  `s1 ⊆ s2` by (rw [Abbr `s1`, Abbr `s2`, SUBSET_DEF] >> qexists_tac `n` >> rw []) >>
  `s1 ≠ s2`
  by (
    rw [Abbr `s1`, Abbr `s2`, EXTENSION] >> qexists_tac `new ++ toString suff` >> rw []) >>
  metis_tac [CARD_SUBSET, SUBSET_FINITE, SUBSET_EQ_CARD, LESS_OR_EQ]
End

Definition gen_name_def:
  gen_name used new =
    if new ∈ used then
      find_name used new 0
    else
      new
End

Definition gen_names_def:
  (gen_names used [] = (used, [])) ∧
  (gen_names used (n::ns) =
    let n = gen_name used n in
      let (used, names) = gen_names ({n} ∪ used) ns in
        (used, n::names))
End

Definition translate_size_def:
  (translate_size llvm$W1 = 1) ∧
  (translate_size W8 = 8) ∧
  (translate_size W32 = 32) ∧
  (translate_size W64 = 64)
End

Definition translate_ty_def:
  (translate_ty (FunT t ts) = FunctionT (translate_ty t) (map translate_ty ts)) ∧
  (translate_ty (IntT s) = IntegerT (translate_size s)) ∧
  (translate_ty (PtrT t) = PointerT (translate_ty t)) ∧
  (translate_ty (ArrT n t) = ArrayT (translate_ty t) n) ∧
  (translate_ty (StrT ts) = TupleT (map translate_ty ts))
Termination
  WF_REL_TAC `measure ty_size` >> rw [] >>
  Induct_on `ts` >> rw [ty_size_def] >>
  res_tac >> decide_tac
End

Definition translate_glob_var_def:
  translate_glob_var (Glob_var g) t = Var_name g (translate_ty t)
End

Definition translate_reg_def:
  translate_reg (Reg r) t = Var_name r (translate_ty t)
End

Definition translate_label_def:
  translate_label f (Lab l) = Lab_name f l
End

Definition translate_const_def:
  (translate_const (IntC s i) = Integer i (IntegerT (translate_size s))) ∧
  (translate_const (StrC tcs) =
    Record (map (λ(ty, c). translate_const c) tcs)) ∧
  (translate_const (ArrC tcs) =
    Record (map (λ(ty, c). translate_const c) tcs)) ∧
  (* TODO *)
  (translate_const (GlobalC g) = Var (translate_glob_var g ARB)) ∧
  (* TODO *)
  (translate_const (GepC _ _ _ _) = ARB) ∧
  (translate_const UndefC = Nondet)
Termination
  WF_REL_TAC `measure const_size` >>
  Induct_on `tcs` >> rw [] >> rw [const_size_def] >>
  first_x_assum drule >> decide_tac
End

Definition translate_arg_def:
  (translate_arg emap (Constant c) = translate_const c) ∧
  (translate_arg emap (Variable r) =
    case flookup emap r of
    (* With the current strategy of threading the emap through the whole
     * function, we should never get a None here.
     *)
    | None => Var (translate_reg r (IntT W64))
    | Some e => e)
End

Definition translate_updatevalue_def:
  (translate_updatevalue a v [] = v) ∧
  (translate_updatevalue a v (c::cs) =
    let c' = translate_const c in
      Update a c' (translate_updatevalue (Select a c') v cs))
End

(* TODO *)
Definition translate_instr_to_exp_def:
  (translate_instr_to_exp emap (llvm$Sub _ _ _ ty a1 a2) =
    llair$Sub (translate_ty ty) (translate_arg emap a1) (translate_arg emap a2)) ∧
  (translate_instr_to_exp emap (Extractvalue _ (t, a) cs) =
    foldl (λe c. Select e (translate_const c)) (translate_arg emap a) cs) ∧
  (translate_instr_to_exp emap (Insertvalue _ (t1, a1) (t2, a2) cs) =
    translate_updatevalue (translate_arg emap a1) (translate_arg emap a2) cs)
End

(* This translation of insertvalue to update and select is quadratic in the
 * number of indices, but we haven't observed clang-generated code with multiple
 * indices.
 *
 * Insertvalue a v [c1; c2; c3] becomes
 *
 * Up a c1 (Up (Sel a c1) c2 (Up (Sel (Sel a c1) c2) c3 v))
 *
 * We could store each of the selections and get a linear size list of
 * instructions instead of a single expression.
 *
 * Examples:
 * EVAL ``translate_instr_to_exp fempty (Extractvalue r (t,a) [c1; c2; c3; c4; c5])``
   ⊢ translate_instr_to_exp fempty (Extractvalue r (t,a) [c1; c2; c3; c4; c5]) =
     Select
       (Select
          (Select
             (Select (Select (translate_arg fempty a) (translate_const c1))
                (translate_const c2)) (translate_const c3))
          (translate_const c4)) (translate_const c5): thm
 *
 * EVAL ``translate_instr_to_exp fempty (Insertvalue r (t,a) (t,v) [c1; c2; c3; c4; c5])``
   ⊢ translate_instr_to_exp fempty (Insertvalue r (t,a) (t,v) [c1; c2; c3; c4; c5]) =
     Update (translate_arg fempty a) (translate_const c1)
       (Update (Select (translate_arg fempty a) (translate_const c1))
          (translate_const c2)
          (Update
             (Select (Select (translate_arg fempty a) (translate_const c1))
                (translate_const c2)) (translate_const c3)
             (Update
                (Select
                   (Select
                      (Select (translate_arg fempty a) (translate_const c1))
                      (translate_const c2)) (translate_const c3))
                (translate_const c4)
                (Update
                   (Select
                      (Select
                         (Select
                            (Select (translate_arg fempty a)
                               (translate_const c1)) (translate_const c2))
                         (translate_const c3)) (translate_const c4))
                   (translate_const c5) (translate_arg fempty v))))): thm
 *
 * *)


(* TODO *)
Definition translate_instr_to_inst_def:
  (translate_instr_to_inst emap (llvm$Store (t1, a1) (t2, a2)) =
    llair$Store (translate_arg emap a2) (translate_arg emap a1) (sizeof t1)) ∧
  (translate_instr_to_inst emap (Load r t (t1, a1)) =
    Load (translate_reg r t) (translate_arg emap a1) (sizeof t))
End

(* TODO *)
Definition translate_instr_to_term_def:
  (translate_instr_to_term f emap (Br a l1 l2) =
    Switch (translate_arg emap a) [(0, translate_label f l2)] (translate_label f l1)) ∧
  (translate_instr_to_term f emap (Exit a) =
    Exit (translate_arg emap a))
End

Datatype:
  instr_class =
  | Exp reg ty
  | Non_exp
  | Term
  | Call
End

(* Convert index lists as for GEP into number lists, for the purpose of
 * calculating types. Everything goes to 0 but for positive integer constants,
 * because those things can't be used to index anything but arrays, and the type
 * for the array contents doesn't depend on the index's value. *)
Definition idx_to_num_def:
  (idx_to_num (_, (Constant (IntC _ n))) = Num (ABS n)) ∧
  (idx_to_num (_, _) = 0)
End

Definition cidx_to_num_def:
  (cidx_to_num (IntC _ n) = Num (ABS n)) ∧
  (cidx_to_num _ = 0)
End

Definition classify_instr_def:
  (classify_instr (Call _ _ _ _) = Call) ∧
  (classify_instr (Ret _) = Term) ∧
  (classify_instr (Br _ _ _) = Term) ∧
  (classify_instr (Invoke _ _ _ _ _ _) = Term) ∧
  (classify_instr Unreachable = Term) ∧
  (classify_instr (Exit _) = Term) ∧
  (classify_instr (Load _ _ _) = Non_exp) ∧
  (classify_instr (Store _ _) = Non_exp) ∧
  (classify_instr (Cxa_throw _ _ _) = Non_exp) ∧
  (classify_instr Cxa_end_catch = Non_exp) ∧
  (classify_instr (Cxa_begin_catch _ _) = Non_exp) ∧
  (classify_instr (Sub r _ _ t _ _) = Exp r t) ∧
  (classify_instr (Extractvalue r (t, _) idx) =
    Exp r (THE (extract_type t (map cidx_to_num idx)))) ∧
  (classify_instr (Insertvalue r (t, _) _ idx) = Exp r t) ∧
  (classify_instr (Alloca r t _) = Exp r (PtrT t)) ∧
  (classify_instr (Gep r t _ idx) =
    Exp r (PtrT (THE (extract_type t (map idx_to_num idx))))) ∧
  (classify_instr (Ptrtoint r _ t) = Exp r t) ∧
  (classify_instr (Inttoptr r _ t) = Exp r t) ∧
  (classify_instr (Icmp r _ _ _ _) = Exp r (IntT W1)) ∧
  (* TODO *)
  (classify_instr (Cxa_allocate_exn r _) = Exp r ARB) ∧
  (classify_instr (Cxa_get_exception_ptr r _) = Exp r ARB)
End

(* Translate a list of instructions into an block. f is the name of the function
 * that the instructions are in, reg_to_keep is the set of variables that we
 * want to keep assignments to (e.g., because of sharing in the expression
 * structure.
 *
 * emap is a mapping of registers to expressions that compute the
 * value that should have been in the expression.
 *
 * This tries to build large expressions, and omits intermediate assignments
 * wherever possible.
 * For example:
 * r2 = sub r0 r1
 * r3 = sub r2 r1
 * r4 = sub r3 r0
 *
 * becomes
 * r4 = sub (sub (sub r0 r1) r1) r0
 *
 * if r4 is the only register listed as needing to be kept.
 *
 *)
Definition translate_instrs_def:
  (translate_instrs f emap reg_to_keep [] = (<| cmnd := []; term := Unreachable |>, emap)) ∧
  (translate_instrs f emap reg_to_keep (i :: is) =
    case classify_instr i of
    | Exp r t =>
      let x = translate_reg r t in
      let e = translate_instr_to_exp emap i in
        if r ∈ reg_to_keep then
          let (b, emap') = translate_instrs f (emap |+ (r, Var x)) reg_to_keep is in
            (b with cmnd := Move [(x, e)] :: b.cmnd, emap')
        else
          translate_instrs f (emap |+ (r, e)) reg_to_keep is
    | Non_exp =>
        let (b, emap') = translate_instrs f emap reg_to_keep is in
          (b with cmnd := translate_instr_to_inst emap i :: b.cmnd, emap')
    | Term =>
        (<| cmnd := []; term := translate_instr_to_term f emap i |>, emap)
    (* TODO *)
    | Call => ARB)
End

Definition dest_label_def:
  dest_label (Lab s) = s
End

Definition dest_phi_def:
  dest_phi (Phi r t largs) = (r, t, largs)
End

Definition translate_label_opt_def:
  (translate_label_opt f entry None = Lab_name f entry) ∧
  (translate_label_opt f entry (Some l) = translate_label f l)
End

Definition translate_header_def:
  (translate_header f entry Entry = []) ∧
  (translate_header f entry (Head phis _) =
    map
      (λ(r, t, largs).
        (translate_reg r t,
        (* TODO: shouldn't use fempty here *)
         map (λ(l, arg). (translate_label_opt f entry l, translate_arg fempty arg)) largs))
      (map dest_phi phis))
End

Definition translate_block_def:
  translate_block f entry_n emap regs_to_keep (l, b) =
    let (b', emap') = translate_instrs f emap regs_to_keep b.body in
      ((Lab_name f (the (option_map dest_label l) entry_n),
       (translate_header f entry_n b.h, b')),
       emap')
End

(* Given a label and phi node, get the assignment for that incoming label. It's
 * convenient to return a list, but we expect there to be exactly 1 element. *)
Definition build_move_for_lab_def:
  build_move_for_lab l (r, les) =
   let les = filter (λ(l', e). l = l') les in
     map (λ(l', e). (r,e)) les
End

(* Given a list of phis and a label, get the move corresponding to entering
 * the block targeted by l_to from block l_from *)
Definition generate_move_block_def:
  generate_move_block phis l_from l_to =
    let t = Iswitch (Integer 0 (IntegerT 1)) [l_to] in
      case alookup phis l_to of
      | None => <| cmnd := [Move []]; term := t |>
      | Some (phis, _) =>
        <| cmnd := [Move (flat (map (build_move_for_lab l_from) phis))];
           term := t |>
End

Definition label_name_def:
  label_to_name (Lab_name _ l) = l
End

(* Given association list of labels and phi-block pairs, and a particular block,
 * build the new move blocks for its terminator *)
Definition generate_move_blocks_def:
  generate_move_blocks f used_names bs (l_from, (_, body)) =
    case body.term of
    | Iswitch e ls =>
      let (used_names, new_names) = gen_names used_names (map label_to_name ls) in
      let mb = map2 (λl_to new. (Lab_name f new, generate_move_block bs l_from l_to)) ls new_names in
      (used_names, (l_from, body with term := Iswitch e (map fst mb)) :: mb)
End

Definition generate_move_blocks_list_def:
  (generate_move_blocks_list f used_names bs [] = (used_names, [])) ∧
  (generate_move_blocks_list f used_names bs (b::bs') =
    let (used_names, new_blocks) = generate_move_blocks f used_names bs b in
    let (used_names, new_blocks2) =
      generate_move_blocks_list f used_names bs bs'
    in
      (used_names, new_blocks :: new_blocks2))
End

(* Given an association list of labels and phi-block pairs, remove the phi's,
 * by generating an extra block along each control flow edge that does the move
 * corresponding to the relevant phis. *)
Definition remove_phis_def:
  remove_phis f used_names bs = flat (snd (generate_move_blocks_list f used_names bs bs))
End

Definition translate_param_def:
  translate_param (t, r) = translate_reg r t
End

Definition translate_def_def:
  translate_def f d =
    let used_names = ARB in
    let entry_name = gen_name used_names "entry" in
    (* TODO *)
    let regs_to_keep = UNIV in
    (* We thread a mapping from register names to expressions through. This
     * works assuming that the blocks are in a good ordering, which must exist
     * because the LLVM is in SSA form, and so each definition must dominate all
     * uses.
     * *)
    let (bs, emap) =
      foldl
        (λ(bs, emap) b.
          let (b', emap') = translate_block f entry_name emap regs_to_keep b in
            (b'::bs, emap'))
        ([], fempty) d.blocks
    in
      <| params := map translate_param d.params;
        (* TODO: calculate these from the produced llair, and not the llvm *)
        locals := ARB;
        entry := Lab_name f entry_name;
        cfg := remove_phis f used_names (reverse bs);
        freturn := ARB;
        fthrow := ARB |>
End

Definition dest_fn_def:
  dest_fn (Fn f) = f
End

Definition translate_prog_def:
  translate_prog p =
    <| glob_init := ARB;
       functions := map (\(fname, d). (dest_fn fname, translate_def (dest_fn fname) d)) p |>
End

export_theory ();
