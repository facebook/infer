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

(** Operators for the abstract domain. In particular, join and meet. *)

module L = Logging
module F = Format

let (++) = Sil.Int.add
let (--) = Sil.Int.sub

(** {2 Utility functions for ids} *)

let can_rename id =
  Ident.is_primed id || Ident.is_footprint id

(** {2 Utility functions for sigma} *)

let sigma_equal sigma1 sigma2 =
  let rec f sigma1_rest sigma2_rest =
    match (sigma1_rest, sigma2_rest) with
    | [], [] -> ()
    | [], _:: _ | _:: _, [] ->
        (L.d_strln "failure reason 1"; raise IList.Fail)
    | hpred1:: sigma1_rest', hpred2:: sigma2_rest' ->
        if Sil.hpred_equal hpred1 hpred2 then f sigma1_rest' sigma2_rest'
        else (L.d_strln "failure reason 2"; raise IList.Fail) in
  let sigma1_sorted = IList.sort Sil.hpred_compare sigma1 in
  let sigma2_sorted = IList.sort Sil.hpred_compare sigma2 in
  f sigma1_sorted sigma2_sorted

let sigma_get_start_lexps_sort sigma =
  let exp_compare_neg e1 e2 = - (Sil.exp_compare e1 e2) in
  let filter e = Sil.fav_for_all (Sil.exp_fav e) Ident.is_normal in
  let lexps = Sil.hpred_list_get_lexps filter sigma in
  IList.sort exp_compare_neg lexps

(** {2 Utility functions for side} *)

type side = Lhs | Rhs

let select side e1 e2 =
  match side with
  | Lhs -> e1
  | Rhs -> e2

let opposite side =
  match side with
  | Lhs -> Rhs
  | Rhs -> Lhs

let do_side side f e1 e2 =
  match side with
  | Lhs -> f e1 e2
  | Rhs -> f e2 e1

(** {2 Sets for expression pairs} *)

module EPset = Set.Make
    (struct
      type t = Sil.exp * Sil.exp
      let compare (e1, e1') (e2, e2') =
        match (Sil.exp_compare e1 e2) with
        | i when i <> 0 -> i
        | _ -> Sil.exp_compare e1' e2'
    end)

(** {2 Module for maintaining information about noninjectivity during join} *)

module NonInj : sig

  val init : unit -> unit
  val final : unit -> unit
  val add : side -> Sil.exp -> Sil.exp -> unit
  val check : side -> Sil.exp list -> bool

end = struct

  (* mappings from primed or footprint var exps to primed or footprint var exps *)
  let equiv_tbl1 = Hashtbl.create 32
  let equiv_tbl2 = Hashtbl.create 32

  (* mappings from primed or footprint var exps to normal var, lvar or const exps *)
  let const_tbl1 = Hashtbl.create 32
  let const_tbl2 = Hashtbl.create 32

  let reset () =
    Hashtbl.clear equiv_tbl1;
    Hashtbl.clear equiv_tbl2;
    Hashtbl.clear const_tbl1;
    Hashtbl.clear const_tbl2

  let init () = reset ()
  let final () = reset ()

  let lookup' tbl e default =
    match e with
    | Sil.Var _ ->
        begin
          try Hashtbl.find tbl e
          with Not_found -> (Hashtbl.replace tbl e default; default)
        end
    | _ -> assert false

  let lookup_equiv' tbl e =
    lookup' tbl e e
  let lookup_const' tbl e =
    lookup' tbl e Sil.ExpSet.empty

  let rec find' tbl e =
    let e' = lookup_equiv' tbl e in
    match e' with
    | Sil.Var _ ->
        if Sil.exp_equal e e' then e
        else
          begin
            let root = find' tbl e' in
            Hashtbl.replace tbl e root;
            root
          end
    | _ -> assert false

  let union' tbl const_tbl e1 e2 =
    let r1 = find' tbl e1 in
    let r2 = find' tbl e2 in
    let new_r, old_r =
      match (Sil.exp_compare r1 r2) with
      | i when i <= 0 -> r1, r2
      | _ -> r2, r1 in
    let new_c = lookup_const' const_tbl new_r in
    let old_c = lookup_const' const_tbl old_r in
    let res_c = Sil.ExpSet.union new_c old_c in
    if Sil.ExpSet.cardinal res_c > 1 then (L.d_strln "failure reason 3"; raise IList.Fail);
    Hashtbl.replace tbl old_r new_r;
    Hashtbl.replace const_tbl new_r res_c

  let replace_const' tbl const_tbl e c =
    let r = find' tbl e in
    let set = Sil.ExpSet.add c (lookup_const' const_tbl r) in
    if Sil.ExpSet.cardinal set > 1 then (L.d_strln "failure reason 4"; raise IList.Fail);
    Hashtbl.replace const_tbl r set

  let add side e e' =
    let tbl, const_tbl =
      match side with
      | Lhs -> equiv_tbl1, const_tbl1
      | Rhs -> equiv_tbl2, const_tbl2
    in
    match e, e' with
    | Sil.Var id, Sil.Var id' ->
        begin
          match can_rename id, can_rename id' with
          | true, true -> union' tbl const_tbl e e'
          | true, false -> replace_const' tbl const_tbl e e'
          | false, true -> replace_const' tbl const_tbl e' e
          | _ -> L.d_strln "failure reason 5"; raise IList.Fail
        end
    | Sil.Var id, Sil.Const _ | Sil.Var id, Sil.Lvar _ ->
        if (can_rename id) then replace_const' tbl const_tbl e e'
        else (L.d_strln "failure reason 6"; raise IList.Fail)
    | Sil.Const _, Sil.Var id' | Sil.Lvar _, Sil.Var id' ->
        if (can_rename id') then replace_const' tbl const_tbl e' e
        else (L.d_strln "failure reason 7"; raise IList.Fail)
    | _ ->
        if not (Sil.exp_equal e e') then (L.d_strln "failure reason 8"; raise IList.Fail) else ()

  let check side es =
    let f = function Sil.Var id -> can_rename id | _ -> false in
    let vars, nonvars = IList.partition f es in
    let tbl, const_tbl =
      match side with
      | Lhs -> equiv_tbl1, const_tbl1
      | Rhs -> equiv_tbl2, const_tbl2
    in
    if (IList.length nonvars > 1) then false
    else
      match vars, nonvars with
      | [], _ | [_], [] -> true
      | v:: vars', _ ->
          let r = find' tbl v in
          let set = lookup_const' const_tbl r in
          (IList.for_all (fun v' -> Sil.exp_equal (find' tbl v') r) vars') &&
          (IList.for_all (fun c -> Sil.ExpSet.mem c set) nonvars)

end

(** {2 Modules for checking whether join or meet loses too much info} *)

module type InfoLossCheckerSig =
sig
  val init : Prop.sigma -> Prop.sigma -> unit
  val final : unit -> unit
  val lost_little : side -> Sil.exp -> Sil.exp list -> bool
  val add : side -> Sil.exp -> Sil.exp -> unit
end

module Dangling : sig

  val init : Prop.sigma -> Prop.sigma -> unit
  val final : unit -> unit
  val check : side -> Sil.exp -> bool

end = struct

  let lexps1 = ref Sil.ExpSet.empty
  let lexps2 = ref Sil.ExpSet.empty

  let get_lexp_set' sigma =
    let lexp_lst = Sil.hpred_list_get_lexps (fun _ -> true) sigma in
    IList.fold_left (fun set e -> Sil.ExpSet.add e set) Sil.ExpSet.empty lexp_lst
  let init sigma1 sigma2 =
    lexps1 := get_lexp_set' sigma1;
    lexps2 := get_lexp_set' sigma2
  let final () =
    lexps1 := Sil.ExpSet.empty;
    lexps2 := Sil.ExpSet.empty

  (* conservatively checks whether e is dangling *)
  let check side e =
    let lexps =
      match side with
      | Lhs -> !lexps1
      | Rhs -> !lexps2
    in
    match e with
    | Sil.Var id -> can_rename id && not (Sil.ExpSet.mem e lexps)
    | Sil.Const _ -> not (Sil.ExpSet.mem e lexps)
    | Sil.BinOp _ -> not (Sil.ExpSet.mem e lexps)
    | _ -> false
end

module CheckJoinPre : InfoLossCheckerSig = struct

  let init sigma1 sigma2 =
    NonInj.init ();
    Dangling.init sigma1 sigma2

  let final () =
    NonInj.final ();
    Dangling.final ()

  let fail_case side e es =
    let side_op = opposite side in
    match e with
    | Sil.Lvar _ -> false
    | Sil.Var id when Ident.is_normal id -> IList.length es >= 1
    | Sil.Var _ ->
        if !Config.join_cond = 0 then
          IList.exists (Sil.exp_equal Sil.exp_zero) es
        else if Dangling.check side e then
          begin
            let r = IList.exists (fun e' -> not (Dangling.check side_op e')) es in
            if r then begin
              L.d_str ".... Dangling Check (dang e:"; Sil.d_exp e;
              L.d_str ") (? es:"; Sil.d_exp_list es; L.d_strln ") ....";
              L.d_ln ()
            end;
            r
          end
        else
          begin
            let r = IList.exists (Dangling.check side_op) es in
            if r then begin
              L.d_str ".... Dangling Check (notdang e:"; Sil.d_exp e;
              L.d_str ") (? es:"; Sil.d_exp_list es; L.d_strln ") ....";
              L.d_ln ()
            end;
            r
          end
    | _ -> false

  let lost_little side e es =
    let side_op = opposite side in
    let es = match e with Sil.Const _ -> [] | _ -> es in
    if (fail_case side e es) then false
    else
      match es with
      | [] | [_] -> true
      | _ -> (NonInj.check side_op es)

  let add = NonInj.add
end

module CheckJoinPost : InfoLossCheckerSig = struct

  let init _ _ =
    NonInj.init ()

  let final () =
    NonInj.final ()

  let fail_case _ e es =
    match e with
    | Sil.Lvar _ -> false
    | Sil.Var id when Ident.is_normal id -> IList.length es >= 1
    | Sil.Var _ -> false
    | _ -> false

  let lost_little side e es =
    let side_op = opposite side in
    let es = match e with Sil.Const _ -> [] | _ -> es in
    if (fail_case side e es) then false
    else
      match es with
      | [] | [_] -> true
      | _ -> NonInj.check side_op es

  let add = NonInj.add
end

module CheckJoin : sig

  val init : JoinState.mode -> Prop.sigma -> Prop.sigma -> unit
  val final : unit -> unit
  val lost_little : side -> Sil.exp -> Sil.exp list -> bool
  val add : side -> Sil.exp -> Sil.exp -> unit

end = struct

  let mode_ref : JoinState.mode ref = ref JoinState.Post

  let init mode sigma1 sigma2 =
    mode_ref := mode;
    match mode with
    | JoinState.Pre -> CheckJoinPre.init sigma1 sigma2
    | JoinState.Post -> CheckJoinPost.init sigma1 sigma2

  let final () =
    match !mode_ref with
    | JoinState.Pre -> CheckJoinPre.final (); mode_ref := JoinState.Post
    | JoinState.Post -> CheckJoinPost.final (); mode_ref := JoinState.Post

  let lost_little side e es =
    match !mode_ref with
    | JoinState.Pre -> CheckJoinPre.lost_little side e es
    | JoinState.Post -> CheckJoinPost.lost_little side e es

  let add side e1 e2 =
    match !mode_ref with
    | JoinState.Pre -> CheckJoinPre.add side e1 e2
    | JoinState.Post -> CheckJoinPost.add side e1 e2
end

module CheckMeet : InfoLossCheckerSig = struct

  let lexps1 = ref Sil.ExpSet.empty
  let lexps2 = ref Sil.ExpSet.empty

  let init sigma1 sigma2 =
    let lexps1_lst = Sil.hpred_list_get_lexps (fun _ -> true) sigma1 in
    let lexps2_lst = Sil.hpred_list_get_lexps (fun _ -> true) sigma2 in
    lexps1 := Sil.elist_to_eset lexps1_lst;
    lexps2 := Sil.elist_to_eset lexps2_lst

  let final () =
    lexps1 := Sil.ExpSet.empty;
    lexps2 := Sil.ExpSet.empty

  let lost_little side e es =
    let lexps = match side with
      | Lhs -> !lexps1
      | Rhs -> !lexps2
    in
    match es, e with
    | [], _ ->
        true
    | [Sil.Const _], Sil.Lvar _ ->
        false
    | [Sil.Const _], Sil.Var _ ->
        not (Sil.ExpSet.mem e lexps)
    | [Sil.Const _], _ ->
        assert false
    | [_], Sil.Lvar _ | [_], Sil.Var _ ->
        true
    | [_], _ ->
        assert false
    | _, Sil.Lvar _ | _, Sil.Var _ ->
        false
    | _, Sil.Const _ ->
        assert false
    | _ -> assert false

  let add = NonInj.add
end

(** {2 Module for worklist} *)

module Todo : sig

  exception Empty
  type t
  val init : unit -> unit
  val final : unit -> unit
  val reset : (Sil.exp * Sil.exp * Sil.exp) list -> unit
  val push : (Sil.exp * Sil.exp * Sil.exp) -> unit
  val pop : unit -> (Sil.exp * Sil.exp * Sil.exp)
  val set : t -> unit
  val take : unit -> t

end = struct

  exception Empty
  type t = (Sil.exp * Sil.exp * Sil.exp) list

  let tbl = ref []

  let init () = tbl := []
  let final () = tbl := []
  let reset todo = tbl := todo

  let push e =
    tbl := e :: !tbl
  let pop () =
    match !tbl with
    | h:: t -> tbl := t; h
    | _ -> raise Empty

  let set todo = tbl := todo
  let take () = let res = !tbl in tbl := []; res

end

(** {2 Module for introducing fresh variables} *)

module FreshVarExp : sig

  val init : unit -> unit
  val get_fresh_exp : Sil.exp -> Sil.exp -> Sil.exp
  val get_induced_pi : unit -> Prop.pi
  val final : unit -> unit

(*
  val lookup : side -> Sil.exp -> (Sil.exp * Sil.exp) option
*)
end = struct

  let t = ref []

  let init () = t := []
  let final () = t := []

  let entry_compare (e1, e2, _) (_, e2', _) =
    let n1 = Sil.exp_compare e1 e2 in
    if n1 <> 0 then n1 else Sil.exp_compare e2 e2'

  let get_fresh_exp e1 e2 =
    try
      let (_, _, e) = IList.find (fun (e1', e2', _) -> Sil.exp_equal e1 e1' && Sil.exp_equal e2 e2') !t in
      e
    with Not_found ->
      let e = Sil.exp_get_undefined (JoinState.get_footprint ()) in
      t := (e1, e2, e)::!t;
      e

  let get_induced_atom acc strict_lower upper e =
    let ineq_lower = Prop.mk_inequality (Sil.BinOp(Sil.Lt, strict_lower, e)) in
    let ineq_upper = Prop.mk_inequality (Sil.BinOp(Sil.Le, e, upper)) in
    ineq_lower:: ineq_upper:: acc

  let minus2_to_2 = IList.map Sil.Int.of_int [-2; -1; 0; 1; 2]

  let get_induced_pi () =
    let t_sorted = IList.sort entry_compare !t in

    let add_and_chk_eq e1 e1' n =
      match e1, e1' with
      | Sil.Const (Sil.Cint n1), Sil.Const (Sil.Cint n1') -> Sil.Int.eq (n1 ++ n) n1'
      | _ -> false in
    let add_and_gen_eq e e' n =
      let e_plus_n = Sil.BinOp(Sil.PlusA, e, Sil.exp_int n) in
      Prop.mk_eq e_plus_n e' in
    let rec f_eqs_entry ((e1, e2, e) as entry) eqs_acc t_seen = function
      | [] -> eqs_acc, t_seen
      | ((e1', e2', e') as entry'):: t_rest' ->
          try
            let n = IList.find (fun n -> add_and_chk_eq e1 e1' n && add_and_chk_eq e2 e2' n) minus2_to_2 in
            let eq = add_and_gen_eq e e' n in
            let eqs_acc' = eq:: eqs_acc in
            f_eqs_entry entry eqs_acc' t_seen t_rest'
          with Not_found ->
            let t_seen' = entry':: t_seen in
            f_eqs_entry entry eqs_acc t_seen' t_rest' in
    let rec f_eqs eqs_acc t_acc = function
      | [] -> (eqs_acc, t_acc)
      | entry:: t_rest ->
          let eqs_acc', t_rest' = f_eqs_entry entry eqs_acc [] t_rest in
          let t_acc' = entry:: t_acc in
          f_eqs eqs_acc' t_acc' t_rest' in
    let eqs, t_minimal = f_eqs [] [] t_sorted in

    let f_ineqs acc (e1, e2, e) =
      match e1, e2 with
      | Sil.Const (Sil.Cint n1), Sil.Const (Sil.Cint n2) ->
          let strict_lower1, upper1 = if Sil.Int.leq n1 n2 then (n1 -- Sil.Int.one, n2) else (n2 -- Sil.Int.one, n1) in
          let e_strict_lower1 = Sil.exp_int strict_lower1 in
          let e_upper1 = Sil.exp_int upper1 in
          get_induced_atom acc e_strict_lower1 e_upper1 e
      | _ -> acc in
    IList.fold_left f_ineqs eqs t_minimal

(*
  let lookup side e =
    try
      let (e1, e2, e) =
        IList.find (fun (e1', e2', _) -> Sil.exp_equal e (select side e1' e2')) !t in
      Some (e, select (opposite side) e1 e2)
    with Not_found ->
      None
*)
end

(** {2 Modules for renaming} *)

module Rename : sig

  type data_opt = ExtFresh | ExtDefault of Sil.exp

  val init : unit -> unit
  val final : unit -> unit
  val reset : unit -> (Sil.exp * Sil.exp * Sil.exp) list

  val extend : Sil.exp -> Sil.exp -> data_opt -> Sil.exp
  val check : (side -> Sil.exp -> Sil.exp list -> bool) -> bool

  val get_others : side -> Sil.exp -> (Sil.exp * Sil.exp) option
  val get_other_atoms : side -> Sil.atom -> (Sil.atom * Sil.atom) option

  val lookup : side -> Sil.exp -> Sil.exp
  val lookup_list : side -> Sil.exp list -> Sil.exp list
  val lookup_list_todo : side -> Sil.exp list -> Sil.exp list

  val to_subst_proj : side -> Sil.fav -> Sil.subst
  val to_subst_emb : side -> Sil.subst
(*
  val get : Sil.exp -> Sil.exp -> Sil.exp option
  val pp : printenv -> Format.formatter -> (Sil.exp * Sil.exp * Sil.exp) list -> unit
*)
end = struct

  type t = (Sil.exp * Sil.exp * Sil.exp) list

  let tbl : t ref = ref []

  let init () = tbl := []
  let final () = tbl := []
  let reset () =
    let f = function
      | Sil.Var id, e, _ | e, Sil.Var id, _ ->
          (Ident.is_footprint id) &&
          (Sil.fav_for_all (Sil.exp_fav e) (fun id -> not (Ident.is_primed id)))
      | _ -> false in
    let t' = IList.filter f !tbl in
    tbl := t';
    t'

  let push v = tbl := v :: !tbl

  let check lost_little =
    let f side e =
      let side_op = opposite side in
      let assoc_es =
        match e with
        | Sil.Const _ -> []
        | Sil.Lvar _ | Sil.Var _
        | Sil.BinOp (Sil.PlusA, Sil.Var _, _) ->
            let is_same_e (e1, e2, _) = Sil.exp_equal e (select side e1 e2) in
            let assoc = IList.filter is_same_e !tbl in
            IList.map (fun (e1, e2, _) -> select side_op e1 e2) assoc
        | _ ->
            L.d_str "no pattern match in check lost_little e: "; Sil.d_exp e; L.d_ln ();
            raise IList.Fail in
      lost_little side e assoc_es in
    let lhs_es = IList.map (fun (e1, _, _) -> e1) !tbl in
    let rhs_es = IList.map (fun (_, e2, _) -> e2) !tbl in
    (IList.for_all (f Rhs) rhs_es) && (IList.for_all (f Lhs) lhs_es)

  let lookup_side' side e =
    let f (e1, e2, _) = Sil.exp_equal e (select side e1 e2) in
    IList.filter f !tbl

  let lookup_side_induced' side e =
    let res = ref [] in
    let f v = match v, side with
      | (Sil.BinOp (Sil.PlusA, e1', Sil.Const (Sil.Cint i)), e2, e'), Lhs
        when Sil.exp_equal e e1' ->
          let c' = Sil.exp_int (Sil.Int.neg i) in
          let v' = (e1', Sil.BinOp(Sil.PlusA, e2, c'), Sil.BinOp (Sil.PlusA, e', c')) in
          res := v'::!res
      | (e1, Sil.BinOp (Sil.PlusA, e2', Sil.Const (Sil.Cint i)), e'), Rhs
        when Sil.exp_equal e e2' ->
          let c' = Sil.exp_int (Sil.Int.neg i) in
          let v' = (Sil.BinOp(Sil.PlusA, e1, c'), e2', Sil.BinOp (Sil.PlusA, e', c')) in
          res := v'::!res
      | _ -> () in
    begin
      IList.iter f !tbl;
      IList.rev !res
    end

  (* Return the triple whose side is [e], if it exists unique *)
  let lookup' todo side e : Sil.exp =
    match e with
    | Sil.Var id when can_rename id ->
        begin
          let r = lookup_side' side e in
          match r with
          | [(_, _, id) as t] -> if todo then Todo.push t; id
          | _ -> L.d_strln "failure reason 9"; raise IList.Fail
        end
    | Sil.Var _ | Sil.Const _ | Sil.Lvar _ -> if todo then Todo.push (e, e, e); e
    | _ -> L.d_strln "failure reason 10"; raise IList.Fail

  let lookup side e = lookup' false side e
  let lookup_todo side e = lookup' true side e
  let lookup_list side l = IList.map (lookup side) l
  let lookup_list_todo side l = IList.map (lookup_todo side) l

  let to_subst_proj (side: side) vars =
    let renaming_restricted =
      IList.filter (function (_, _, Sil.Var i) -> Sil.fav_mem vars i | _ -> assert false) !tbl in
    let sub_list_side =
      IList.map
        (function (e1, e2, Sil.Var i) -> (i, select side e1 e2) | _ -> assert false)
        renaming_restricted in
    let sub_list_side_sorted =
      IList.sort (fun (_, e) (_, e') -> Sil.exp_compare e e') sub_list_side in
    let rec find_duplicates =
      function
      | (_, e):: ((_, e'):: _ as t) -> Sil.exp_equal e e' || find_duplicates t
      | _ -> false in
    if find_duplicates sub_list_side_sorted then (L.d_strln "failure reason 11"; raise IList.Fail)
    else Sil.sub_of_list sub_list_side

  let to_subst_emb (side : side) =
    let renaming_restricted =
      let pick_id_case (e1, e2, _) =
        match select side e1 e2 with
        | Sil.Var i -> can_rename i
        | _ -> false in
      IList.filter pick_id_case !tbl in
    let sub_list =
      let project (e1, e2, e) =
        match select side e1 e2 with
        | Sil.Var i -> (i, e)
        | _ -> assert false in
      IList.map project renaming_restricted in
    let sub_list_sorted =
      let compare (i, _) (i', _) = Ident.compare i i' in
      IList.sort compare sub_list in
    let rec find_duplicates = function
      | (i, _):: ((i', _):: _ as t) -> Ident.equal i i' || find_duplicates t
      | _ -> false in
    if find_duplicates sub_list_sorted then (L.d_strln "failure reason 12"; raise IList.Fail)
    else Sil.sub_of_list sub_list_sorted

  let get_others' f_lookup side e =
    let side_op = opposite side in
    let r = f_lookup side e in
    match r with
    | [] -> None
    | [(e1, e2, e')] -> Some (e', select side_op e1 e2)
    | _ -> None
  let get_others = get_others' lookup_side'
  let get_others_direct_or_induced side e =
    let others = get_others side e in
    match others with
    | None -> get_others' lookup_side_induced' side e
    | Some _ -> others
  let get_others_deep side = function
    | Sil.BinOp(op, e, e') ->
        let others = get_others_direct_or_induced side e in
        let others' = get_others_direct_or_induced side e' in
        (match others, others' with
         | None, _ | _, None -> None
         | Some (e_res, e_op), Some(e_res', e_op') ->
             let e_res'' = Sil.BinOp(op, e_res, e_res') in
             let e_op'' = Sil.BinOp(op, e_op, e_op') in
             Some (e_res'', e_op''))
    | _ -> None

  let get_other_atoms side atom_in =
    let build_other_atoms construct side e =
      if !Config.trace_join then (L.d_str "build_other_atoms: "; Sil.d_exp e; L.d_ln ());
      let others1 = get_others_direct_or_induced side e in
      let others2 = match others1 with None -> get_others_deep side e | Some _ -> others1 in
      match others2 with
      | None -> None
      | Some (e_res, e_op) ->
          let a_res = construct e_res in
          let a_op = construct e_op in
          if !Config.trace_join then begin
            L.d_str "build_other_atoms (successful) ";
            Sil.d_atom a_res; L.d_str ", "; Sil.d_atom a_op; L.d_ln ()
          end;
          Some (a_res, a_op) in
    let exp_contains_only_normal_ids e =
      let fav = Sil.exp_fav e in
      Sil.fav_for_all fav Ident.is_normal in
    let atom_contains_only_normal_ids a =
      let fav = Sil.atom_fav a in
      Sil.fav_for_all fav Ident.is_normal in
    let normal_ids_only = atom_contains_only_normal_ids atom_in in
    if normal_ids_only then Some (atom_in, atom_in)
    else
      begin
        match atom_in with
        | Sil.Aneq((Sil.Var id as e), e') | Sil.Aneq(e', (Sil.Var id as e))
          when (exp_contains_only_normal_ids e' && not (Ident.is_normal id)) ->
            build_other_atoms (fun e0 -> Prop.mk_neq e0 e') side e

        | Sil.Aeq((Sil.Var id as e), e') | Sil.Aeq(e', (Sil.Var id as e))
          when (exp_contains_only_normal_ids e' && not (Ident.is_normal id)) ->
            build_other_atoms (fun e0 -> Prop.mk_eq e0 e') side e

        | Sil.Aeq(Sil.BinOp(Sil.Le, e, e'), Sil.Const (Sil.Cint i))
        | Sil.Aeq(Sil.Const (Sil.Cint i), Sil.BinOp(Sil.Le, e, e'))
          when Sil.Int.isone i && (exp_contains_only_normal_ids e') ->
            let construct e0 = Prop.mk_inequality (Sil.BinOp(Sil.Le, e0, e')) in
            build_other_atoms construct side e

        | Sil.Aeq(Sil.BinOp(Sil.Lt, e', e), Sil.Const (Sil.Cint i))
        | Sil.Aeq(Sil.Const (Sil.Cint i), Sil.BinOp(Sil.Lt, e', e))
          when Sil.Int.isone i && (exp_contains_only_normal_ids e') ->
            let construct e0 = Prop.mk_inequality (Sil.BinOp(Sil.Lt, e', e0)) in
            build_other_atoms construct side e

        | _ -> None
      end

  type data_opt = ExtFresh | ExtDefault of Sil.exp

  (* Extend the renaming relation. At least one of e1 and e2
   * should be a primed or footprint variable *)
  let extend e1 e2 default_op =
    try
      let eq_to_e (f1, f2, _) = Sil.exp_equal e1 f1 && Sil.exp_equal e2 f2 in
      let _, _, res = IList.find eq_to_e !tbl in
      res
    with Not_found ->
      let fav1 = Sil.exp_fav e1 in
      let fav2 = Sil.exp_fav e2 in
      let no_ren1 = not (Sil.fav_exists fav1 can_rename) in
      let no_ren2 = not (Sil.fav_exists fav2 can_rename) in
      let some_primed () = Sil.fav_exists fav1 Ident.is_primed || Sil.fav_exists fav2 Ident.is_primed in
      let e =
        if (no_ren1 && no_ren2) then
          if (Sil.exp_equal e1 e2) then e1 else (L.d_strln "failure reason 13"; raise IList.Fail)
        else
          match default_op with
          | ExtDefault e -> e
          | ExtFresh ->
              let kind = if JoinState.get_footprint () && not (some_primed ()) then Ident.kfootprint else Ident.kprimed in
              Sil.Var (Ident.create_fresh kind) in
      let entry = e1, e2, e in
      push entry;
      Todo.push entry;
      e
(*
  let get e1 e2 =
    let f (e1', e2', _) = Sil.exp_equal e1 e1' && Sil.exp_equal e2 e2' in
    match (IList.filter f !tbl) with
    | [] -> None
    | (_, _, e):: _ -> Some e

  let pp pe f renaming =
    let pp_triple f (e1, e2, e3) =
      F.fprintf f "(%a,%a,%a)" (Sil.pp_exp pe) e3 (Sil.pp_exp pe) e1 (Sil.pp_exp pe) e2 in
    (pp_seq pp_triple) f renaming
*)
end

(** {2 Functions for constructing fresh sil data types} *)

let extend_side' kind side e =
  match Rename.get_others side e with
  | None ->
      let e_op = Sil.Var (Ident.create_fresh kind) in
      let e_new = Sil.Var (Ident.create_fresh kind) in
      let e1, e2 =
        match side with
        | Lhs -> e, e_op
        | Rhs -> e_op, e in
      Rename.extend e1 e2 (Rename.ExtDefault (e_new))
  | Some (e', _) -> e'

let rec exp_construct_fresh side e =
  match e with
  | Sil.Var id ->
      if Ident.is_normal id then
        (Todo.push (e, e, e); e)
      else if Ident.is_footprint id then
        extend_side' Ident.kfootprint side e
      else
        extend_side' Ident.kprimed side e
  | Sil.Const _ -> e
  | Sil.Cast (t, e1) ->
      let e1' = exp_construct_fresh side e1 in
      Sil.Cast (t, e1')
  | Sil.UnOp(unop, e1, topt) ->
      let e1' = exp_construct_fresh side e1 in
      Sil.UnOp(unop, e1', topt)
  | Sil.BinOp(binop, e1, e2) ->
      let e1' = exp_construct_fresh side e1 in
      let e2' = exp_construct_fresh side e2 in
      Sil.BinOp(binop, e1', e2')
  | Sil.Lvar _ ->
      e
  | Sil.Lfield(e1, fld, typ) ->
      let e1' = exp_construct_fresh side e1 in
      Sil.Lfield(e1', fld, typ)
  | Sil.Lindex(e1, e2) ->
      let e1' = exp_construct_fresh side e1 in
      let e2' = exp_construct_fresh side e2 in
      Sil.Lindex(e1', e2')
  | Sil.Sizeof _ ->
      e

let strexp_construct_fresh side =
  let f (e, inst_opt) = (exp_construct_fresh side e, inst_opt) in
  Sil.strexp_expmap f

let hpred_construct_fresh side =
  let f (e, inst_opt) = (exp_construct_fresh side e, inst_opt) in
  Sil.hpred_expmap f

(** {2 Join and Meet for Ids} *)

let ident_same_kind_primed_footprint id1 id2 =
  (Ident.is_primed id1 && Ident.is_primed id2) ||
  (Ident.is_footprint id1 && Ident.is_footprint id2)

let ident_partial_join (id1: Ident.t) (id2: Ident.t) =
  match Ident.is_normal id1, Ident.is_normal id2 with
  | true, true ->
      if Ident.equal id1 id2 then Sil.Var id1 else (L.d_strln "failure reason 14"; raise IList.Fail)
  | true, _ | _, true ->
      Rename.extend (Sil.Var id1) (Sil.Var id2) Rename.ExtFresh
  | _ ->
      begin
        if not (ident_same_kind_primed_footprint id1 id2) then
          (L.d_strln "failure reason 15"; raise IList.Fail)
        else
          let e1 = Sil.Var id1 in
          let e2 = Sil.Var id2 in
          Rename.extend e1 e2 Rename.ExtFresh
      end

let ident_partial_meet (id1: Ident.t) (id2: Ident.t) =
  match Ident.is_normal id1, Ident.is_normal id2 with
  | true, true ->
      if Ident.equal id1 id2 then Sil.Var id1
      else (L.d_strln "failure reason 16"; raise IList.Fail)
  | true, _ ->
      let e1, e2 = Sil.Var id1, Sil.Var id2 in
      Rename.extend e1 e2 (Rename.ExtDefault(e1))
  | _, true ->
      let e1, e2 = Sil.Var id1, Sil.Var id2 in
      Rename.extend e1 e2 (Rename.ExtDefault(e2))
  | _ ->
      if Ident.is_primed id1 && Ident.is_primed id2 then
        Rename.extend (Sil.Var id1) (Sil.Var id2) Rename.ExtFresh
      else if Ident.is_footprint id1 && Ident.equal id1 id2 then
        let e = Sil.Var id1 in Rename.extend e e (Rename.ExtDefault(e))
      else
        (L.d_strln "failure reason 17"; raise IList.Fail)

(** {2 Join and Meet for Exps} *)

let const_partial_join c1 c2 =
  let is_int = function Sil.Cint _ -> true | _ -> false in
  if Sil.const_equal c1 c2 then Sil.Const c1
  else if Sil.const_kind_equal c1 c2 && not (is_int c1) then
    (L.d_strln "failure reason 18"; raise IList.Fail)
  else if !Config.abs_val >= 2 then
    FreshVarExp.get_fresh_exp (Sil.Const c1) (Sil.Const c2)
  else (L.d_strln "failure reason 19"; raise IList.Fail)

let rec exp_partial_join (e1: Sil.exp) (e2: Sil.exp) : Sil.exp =
  (* L.d_str "exp_partial_join "; Sil.d_exp e1; L.d_str " "; Sil.d_exp e2; L.d_ln (); *)
  match e1, e2 with
  | Sil.Var id1, Sil.Var id2 ->
      ident_partial_join id1 id2

  | Sil.Var id, Sil.Const _
  | Sil.Const _, Sil.Var id ->
      if Ident.is_normal id then
        (L.d_strln "failure reason 20"; raise IList.Fail)
      else
        Rename.extend e1 e2 Rename.ExtFresh
  | Sil.Const c1, Sil.Const c2 ->
      const_partial_join c1 c2

  | Sil.Var id, Sil.Lvar _
  | Sil.Lvar _, Sil.Var id ->
      if Ident.is_normal id then (L.d_strln "failure reason 21"; raise IList.Fail)
      else Rename.extend e1 e2 Rename.ExtFresh

  | Sil.BinOp(Sil.PlusA, Sil.Var id1, Sil.Const _), Sil.Var id2
  | Sil.Var id1, Sil.BinOp(Sil.PlusA, Sil.Var id2, Sil.Const _)
    when ident_same_kind_primed_footprint id1 id2 ->
      Rename.extend e1 e2 Rename.ExtFresh
  | Sil.BinOp(Sil.PlusA, Sil.Var id1, Sil.Const (Sil.Cint c1)), Sil.Const (Sil.Cint c2)
    when can_rename id1 ->
      let c2' = c2 -- c1 in
      let e_res = Rename.extend (Sil.Var id1) (Sil.exp_int c2') Rename.ExtFresh in
      Sil.BinOp(Sil.PlusA, e_res, Sil.exp_int c1)
  | Sil.Const (Sil.Cint c1), Sil.BinOp(Sil.PlusA, Sil.Var id2, Sil.Const (Sil.Cint c2))
    when can_rename id2 ->
      let c1' = c1 -- c2 in
      let e_res = Rename.extend (Sil.exp_int c1') (Sil.Var id2) Rename.ExtFresh in
      Sil.BinOp(Sil.PlusA, e_res, Sil.exp_int c2)
  | Sil.Cast(t1, e1), Sil.Cast(t2, e2) ->
      if not (Sil.typ_equal t1 t2) then (L.d_strln "failure reason 22"; raise IList.Fail)
      else
        let e1'' = exp_partial_join e1 e2 in
        Sil.Cast (t1, e1'')
  | Sil.UnOp(unop1, e1, topt1), Sil.UnOp(unop2, e2, _) ->
      if not (Sil.unop_equal unop1 unop2) then (L.d_strln "failure reason 23"; raise IList.Fail)
      else Sil.UnOp (unop1, exp_partial_join e1 e2, topt1) (* should be topt1 = topt2 *)
  | Sil.BinOp(Sil.PlusPI, e1, e1'), Sil.BinOp(Sil.PlusPI, e2, e2') ->
      let e1'' = exp_partial_join e1 e2 in
      let e2'' = match e1', e2' with
        | Sil.Const _, Sil.Const _ -> exp_partial_join e1' e2'
        | _ -> FreshVarExp.get_fresh_exp e1 e2 in
      Sil.BinOp(Sil.PlusPI, e1'', e2'')
  | Sil.BinOp(binop1, e1, e1'), Sil.BinOp(binop2, e2, e2') ->
      if not (Sil.binop_equal binop1 binop2) then (L.d_strln "failure reason 24"; raise IList.Fail)
      else
        let e1'' = exp_partial_join e1 e2 in
        let e2'' = exp_partial_join e1' e2' in
        Sil.BinOp(binop1, e1'', e2'')
  | Sil.Lvar(pvar1), Sil.Lvar(pvar2) ->
      if not (Pvar.equal pvar1 pvar2) then (L.d_strln "failure reason 25"; raise IList.Fail)
      else e1
  | Sil.Lfield(e1, f1, t1), Sil.Lfield(e2, f2, _) ->
      if not (Sil.fld_equal f1 f2) then (L.d_strln "failure reason 26"; raise IList.Fail)
      else Sil.Lfield(exp_partial_join e1 e2, f1, t1) (* should be t1 = t2 *)
  | Sil.Lindex(e1, e1'), Sil.Lindex(e2, e2') ->
      let e1'' = exp_partial_join e1 e2 in
      let e2'' = exp_partial_join e1' e2' in
      Sil.Lindex(e1'', e2'')
  | Sil.Sizeof (t1, st1), Sil.Sizeof (t2, st2) ->
      Sil.Sizeof (typ_partial_join t1 t2, Sil.Subtype.join st1 st2)
  | _ ->
      L.d_str "exp_partial_join no match "; Sil.d_exp e1; L.d_str " "; Sil.d_exp e2; L.d_ln ();
      raise IList.Fail

and size_partial_join size1 size2 = match size1, size2 with
  | Sil.BinOp(Sil.PlusA, e1, Sil.Const c1), Sil.BinOp(Sil.PlusA, e2, Sil.Const c2) ->
      let e' = exp_partial_join e1 e2 in
      let c' = exp_partial_join (Sil.Const c1) (Sil.Const c2) in
      Sil.BinOp (Sil.PlusA, e', c')
  | Sil.BinOp(Sil.PlusA, _, _), Sil.BinOp(Sil.PlusA, _, _) ->
      Rename.extend size1 size2 Rename.ExtFresh
  | Sil.Var id1, Sil.Var id2 when Ident.equal id1 id2 ->
      size1
  | _ -> exp_partial_join size1 size2

and typ_partial_join t1 t2 = match t1, t2 with
  | Sil.Tptr (t1, pk1), Sil.Tptr (t2, pk2) when Sil.ptr_kind_compare pk1 pk2 = 0 ->
      Sil.Tptr (typ_partial_join t1 t2, pk1)
  | Sil.Tarray (typ1, size1), Sil.Tarray(typ2, size2) ->
      let t = typ_partial_join typ1 typ2 in
      let size = size_partial_join size1 size2 in
      Sil.Tarray (t, size)
  | _ when Sil.typ_equal t1 t2 -> t1 (* common case *)
  | _ ->
      L.d_str "typ_partial_join no match "; Sil.d_typ_full t1; L.d_str " "; Sil.d_typ_full t2; L.d_ln ();
      raise IList.Fail

let rec exp_partial_meet (e1: Sil.exp) (e2: Sil.exp) : Sil.exp =
  match e1, e2 with
  | Sil.Var id1, Sil.Var id2 ->
      ident_partial_meet id1 id2
  | Sil.Var id, Sil.Const _ ->
      if not (Ident.is_normal id) then
        Rename.extend e1 e2 (Rename.ExtDefault(e2))
      else (L.d_strln "failure reason 27"; raise IList.Fail)
  | Sil.Const _, Sil.Var id ->
      if not (Ident.is_normal id) then
        Rename.extend e1 e2 (Rename.ExtDefault(e1))
      else (L.d_strln "failure reason 28"; raise IList.Fail)
  | Sil.Const c1, Sil.Const c2 ->
      if (Sil.const_equal c1 c2) then e1 else (L.d_strln "failure reason 29"; raise IList.Fail)
  | Sil.Cast(t1, e1), Sil.Cast(t2, e2) ->
      if not (Sil.typ_equal t1 t2) then (L.d_strln "failure reason 30"; raise IList.Fail)
      else
        let e1'' = exp_partial_meet e1 e2 in
        Sil.Cast (t1, e1'')
  | Sil.UnOp(unop1, e1, topt1), Sil.UnOp(unop2, e2, _) ->
      if not (Sil.unop_equal unop1 unop2) then (L.d_strln "failure reason 31"; raise IList.Fail)
      else Sil.UnOp (unop1, exp_partial_meet e1 e2, topt1) (* should be topt1 = topt2 *)
  | Sil.BinOp(binop1, e1, e1'), Sil.BinOp(binop2, e2, e2') ->
      if not (Sil.binop_equal binop1 binop2) then (L.d_strln "failure reason 32"; raise IList.Fail)
      else
        let e1'' = exp_partial_meet e1 e2 in
        let e2'' = exp_partial_meet e1' e2' in
        Sil.BinOp(binop1, e1'', e2'')
  | Sil.Var id, Sil.Lvar _ ->
      if not (Ident.is_normal id) then
        Rename.extend e1 e2 (Rename.ExtDefault(e2))
      else (L.d_strln "failure reason 33"; raise IList.Fail)
  | Sil.Lvar _, Sil.Var id ->
      if not (Ident.is_normal id) then
        Rename.extend e1 e2 (Rename.ExtDefault(e1))
      else (L.d_strln "failure reason 34"; raise IList.Fail)
  | Sil.Lvar(pvar1), Sil.Lvar(pvar2) ->
      if not (Pvar.equal pvar1 pvar2) then (L.d_strln "failure reason 35"; raise IList.Fail)
      else e1
  | Sil.Lfield(e1, f1, t1), Sil.Lfield(e2, f2, _) ->
      if not (Sil.fld_equal f1 f2) then (L.d_strln "failure reason 36"; raise IList.Fail)
      else Sil.Lfield(exp_partial_meet e1 e2, f1, t1) (* should be t1 = t2 *)
  | Sil.Lindex(e1, e1'), Sil.Lindex(e2, e2') ->
      let e1'' = exp_partial_meet e1 e2 in
      let e2'' = exp_partial_meet e1' e2' in
      Sil.Lindex(e1'', e2'')
  | _ -> (L.d_strln "failure reason 37"; raise IList.Fail)

let exp_list_partial_join = IList.map2 exp_partial_join

let exp_list_partial_meet = IList.map2 exp_partial_meet


(** {2 Join and Meet for Strexp} *)

let rec strexp_partial_join mode (strexp1: Sil.strexp) (strexp2: Sil.strexp) : Sil.strexp =

  let rec f_fld_se_list inst mode acc fld_se_list1 fld_se_list2 =
    match fld_se_list1, fld_se_list2 with
    | [], [] -> Sil.Estruct (IList.rev acc, inst)
    | [], _ | _, [] ->
        begin
          match mode with
          | JoinState.Pre -> (L.d_strln "failure reason 42"; raise IList.Fail)
          | JoinState.Post -> Sil.Estruct (IList.rev acc, inst)
        end
    | (fld1, se1):: fld_se_list1', (fld2, se2):: fld_se_list2' ->
        let comparison = Sil.fld_compare fld1 fld2 in
        if comparison = 0 then
          let strexp' = strexp_partial_join mode se1 se2 in
          let fld_se_list_new = (fld1, strexp') :: acc in
          f_fld_se_list inst mode fld_se_list_new fld_se_list1' fld_se_list2'
        else begin
          match mode with
          | JoinState.Pre ->
              (L.d_strln "failure reason 43"; raise IList.Fail)
          | JoinState.Post ->
              if comparison < 0 then begin
                f_fld_se_list inst mode acc fld_se_list1' fld_se_list2
              end
              else if comparison > 0 then begin
                f_fld_se_list inst mode acc fld_se_list1 fld_se_list2'
              end
              else
                assert false (* This case should not happen. *)
        end in

  let rec f_idx_se_list inst size idx_se_list_acc idx_se_list1 idx_se_list2 =
    match idx_se_list1, idx_se_list2 with
    | [], [] -> Sil.Earray (size, IList.rev idx_se_list_acc, inst)
    | [], _ | _, [] ->
        begin
          match mode with
          | JoinState.Pre -> (L.d_strln "failure reason 44"; raise IList.Fail)
          | JoinState.Post ->
              Sil.Earray (size, IList.rev idx_se_list_acc, inst)
        end
    | (idx1, se1):: idx_se_list1', (idx2, se2):: idx_se_list2' ->
        let idx = exp_partial_join idx1 idx2 in
        let strexp' = strexp_partial_join mode se1 se2 in
        let idx_se_list_new = (idx, strexp') :: idx_se_list_acc in
        f_idx_se_list inst size idx_se_list_new idx_se_list1' idx_se_list2' in

  match strexp1, strexp2 with
  | Sil.Eexp (e1, inst1), Sil.Eexp (e2, inst2) ->
      Sil.Eexp (exp_partial_join e1 e2, Sil.inst_partial_join inst1 inst2)
  | Sil.Estruct (fld_se_list1, inst1), Sil.Estruct (fld_se_list2, inst2) ->
      let inst = Sil.inst_partial_join inst1 inst2 in
      f_fld_se_list inst mode [] fld_se_list1 fld_se_list2
  | Sil.Earray (size1, idx_se_list1, inst1), Sil.Earray (size2, idx_se_list2, inst2) ->
      let size = size_partial_join size1 size2 in
      let inst = Sil.inst_partial_join inst1 inst2 in
      f_idx_se_list inst size [] idx_se_list1 idx_se_list2
  | _ -> L.d_strln "no match in strexp_partial_join"; raise IList.Fail

let rec strexp_partial_meet (strexp1: Sil.strexp) (strexp2: Sil.strexp) : Sil.strexp =

  let construct side rev_list ref_list =
    let construct_offset_se (off, se) = (off, strexp_construct_fresh side se) in
    let acc = IList.map construct_offset_se ref_list in
    IList.rev_with_acc acc rev_list in

  let rec f_fld_se_list inst acc fld_se_list1 fld_se_list2 =
    match fld_se_list1, fld_se_list2 with
    | [], [] ->
        Sil.Estruct (IList.rev acc, inst)
    | [], _ ->
        Sil.Estruct (construct Rhs acc fld_se_list2, inst)
    | _, [] ->
        Sil.Estruct (construct Lhs acc fld_se_list1, inst)
    | (fld1, se1):: fld_se_list1', (fld2, se2):: fld_se_list2' ->
        let comparison = Sil.fld_compare fld1 fld2 in
        if comparison < 0 then
          let se' = strexp_construct_fresh Lhs se1 in
          let acc_new = (fld1, se'):: acc in
          f_fld_se_list inst acc_new fld_se_list1' fld_se_list2
        else if comparison > 0 then
          let se' = strexp_construct_fresh Rhs se2 in
          let acc_new = (fld2, se'):: acc in
          f_fld_se_list inst acc_new fld_se_list1 fld_se_list2'
        else
          let strexp' = strexp_partial_meet se1 se2 in
          let acc_new = (fld1, strexp') :: acc in
          f_fld_se_list inst acc_new fld_se_list1' fld_se_list2' in

  let rec f_idx_se_list inst size acc idx_se_list1 idx_se_list2 =
    match idx_se_list1, idx_se_list2 with
    | [],[] ->
        Sil.Earray (size, IList.rev acc, inst)
    | [], _ ->
        Sil.Earray (size, construct Rhs acc idx_se_list2, inst)
    | _, [] ->
        Sil.Earray (size, construct Lhs acc idx_se_list1, inst)
    | (idx1, se1):: idx_se_list1', (idx2, se2):: idx_se_list2' ->
        let idx = exp_partial_meet idx1 idx2 in
        let se' = strexp_partial_meet se1 se2 in
        let acc_new = (idx, se') :: acc in
        f_idx_se_list inst size acc_new idx_se_list1' idx_se_list2' in

  match strexp1, strexp2 with
  | Sil.Eexp (e1, inst1), Sil.Eexp (e2, inst2) ->
      Sil.Eexp (exp_partial_meet e1 e2, Sil.inst_partial_meet inst1 inst2)
  | Sil.Estruct (fld_se_list1, inst1), Sil.Estruct (fld_se_list2, inst2) ->
      let inst = Sil.inst_partial_meet inst1 inst2 in
      f_fld_se_list inst [] fld_se_list1 fld_se_list2
  | Sil.Earray (size1, idx_se_list1, inst1), Sil.Earray (size2, idx_se_list2, inst2)
    when Sil.exp_equal size1 size2 ->
      let inst = Sil.inst_partial_meet inst1 inst2 in
      f_idx_se_list inst size1 [] idx_se_list1 idx_se_list2
  | _ -> (L.d_strln "failure reason 52"; raise IList.Fail)

(** {2 Join and Meet for kind, hpara, hpara_dll} *)

let kind_join k1 k2 = match k1, k2 with
  | Sil.Lseg_PE, _ -> Sil.Lseg_PE
  | _, Sil.Lseg_PE -> Sil.Lseg_PE
  | Sil.Lseg_NE, Sil.Lseg_NE -> Sil.Lseg_NE

let kind_meet k1 k2 = match k1, k2 with
  | Sil.Lseg_NE, _ -> Sil.Lseg_NE
  | _, Sil.Lseg_NE -> Sil.Lseg_NE
  | Sil.Lseg_PE, Sil.Lseg_PE -> Sil.Lseg_PE

let hpara_partial_join (hpara1: Sil.hpara) (hpara2: Sil.hpara) : Sil.hpara =
  if Match.hpara_match_with_impl true hpara2 hpara1 then
    hpara1
  else if Match.hpara_match_with_impl true hpara1 hpara2 then
    hpara2
  else
    (L.d_strln "failure reason 53"; raise IList.Fail)

let hpara_partial_meet (hpara1: Sil.hpara) (hpara2: Sil.hpara) : Sil.hpara =
  if Match.hpara_match_with_impl true hpara2 hpara1 then
    hpara2
  else if Match.hpara_match_with_impl true hpara1 hpara2 then
    hpara1
  else
    (L.d_strln "failure reason 54"; raise IList.Fail)

let hpara_dll_partial_join (hpara1: Sil.hpara_dll) (hpara2: Sil.hpara_dll) : Sil.hpara_dll =
  if Match.hpara_dll_match_with_impl true hpara2 hpara1 then
    hpara1
  else if Match.hpara_dll_match_with_impl true hpara1 hpara2 then
    hpara2
  else
    (L.d_strln "failure reason 55"; raise IList.Fail)

let hpara_dll_partial_meet (hpara1: Sil.hpara_dll) (hpara2: Sil.hpara_dll) : Sil.hpara_dll =
  if Match.hpara_dll_match_with_impl true hpara2 hpara1 then
    hpara2
  else if Match.hpara_dll_match_with_impl true hpara1 hpara2 then
    hpara1
  else
    (L.d_strln "failure reason 56"; raise IList.Fail)

(** {2 Join and Meet for hpred} *)

let hpred_partial_join mode (todo: Sil.exp * Sil.exp * Sil.exp) (hpred1: Sil.hpred) (hpred2: Sil.hpred) : Sil.hpred =
  let e1, e2, e = todo in
  match hpred1, hpred2 with
  | Sil.Hpointsto (_, se1, te1), Sil.Hpointsto (_, se2, te2) ->
      let te = exp_partial_join te1 te2 in
      Prop.mk_ptsto e (strexp_partial_join mode se1 se2) te
  | Sil.Hlseg (k1, hpara1, _, next1, shared1), Sil.Hlseg (k2, hpara2, _, next2, shared2) ->
      let hpara' = hpara_partial_join hpara1 hpara2 in
      let next' = exp_partial_join next1 next2 in
      let shared' = exp_list_partial_join shared1 shared2 in
      Prop.mk_lseg (kind_join k1 k2) hpara' e next' shared'
  | Sil.Hdllseg (k1, para1, iF1, oB1, oF1, iB1, shared1),
    Sil.Hdllseg (k2, para2, iF2, oB2, oF2, iB2, shared2) ->
      let fwd1 = Sil.exp_equal e1 iF1 in
      let fwd2 = Sil.exp_equal e2 iF2 in
      let hpara' = hpara_dll_partial_join para1 para2 in
      let iF', iB' =
        if (fwd1 && fwd2) then (e, exp_partial_join iB1 iB2)
        else if (not fwd1 && not fwd2) then (exp_partial_join iF1 iF2, e)
        else (L.d_strln "failure reason 57"; raise IList.Fail) in
      let oF' = exp_partial_join oF1 oF2 in
      let oB' = exp_partial_join oB1 oB2 in
      let shared' = exp_list_partial_join shared1 shared2 in
      Prop.mk_dllseg (kind_join k1 k2) hpara' iF' oB' oF' iB' shared'
  | _ ->
      assert false

let hpred_partial_meet (todo: Sil.exp * Sil.exp * Sil.exp) (hpred1: Sil.hpred) (hpred2: Sil.hpred) : Sil.hpred =
  let e1, e2, e = todo in
  match hpred1, hpred2 with
  | Sil.Hpointsto (_, se1, te1), Sil.Hpointsto (_, se2, te2) when Sil.exp_equal te1 te2 ->
      Prop.mk_ptsto e (strexp_partial_meet se1 se2) te1
  | Sil.Hpointsto _, _ | _, Sil.Hpointsto _ ->
      (L.d_strln "failure reason 58"; raise IList.Fail)
  | Sil.Hlseg (k1, hpara1, _, next1, shared1), Sil.Hlseg (k2, hpara2, _, next2, shared2) ->
      let hpara' = hpara_partial_meet hpara1 hpara2 in
      let next' = exp_partial_meet next1 next2 in
      let shared' = exp_list_partial_meet shared1 shared2 in
      Prop.mk_lseg (kind_meet k1 k2) hpara' e next' shared'
  | Sil.Hdllseg (k1, para1, iF1, oB1, oF1, iB1, shared1),
    Sil.Hdllseg (k2, para2, iF2, oB2, oF2, iB2, shared2) ->
      let fwd1 = Sil.exp_equal e1 iF1 in
      let fwd2 = Sil.exp_equal e2 iF2 in
      let hpara' = hpara_dll_partial_meet para1 para2 in
      let iF', iB' =
        if (fwd1 && fwd2) then (e, exp_partial_meet iB1 iB2)
        else if (not fwd1 && not fwd2) then (exp_partial_meet iF1 iF2, e)
        else (L.d_strln "failure reason 59"; raise IList.Fail) in
      let oF' = exp_partial_meet oF1 oF2 in
      let oB' = exp_partial_meet oB1 oB2 in
      let shared' = exp_list_partial_meet shared1 shared2 in
      Prop.mk_dllseg (kind_meet k1 k2) hpara' iF' oB' oF' iB' shared'
  | _ ->
      assert false

(** {2 Join and Meet for Sigma} *)

let find_hpred_by_address (e: Sil.exp) (sigma: Prop.sigma) : Sil.hpred option * Prop.sigma =
  let is_root_for_e e' =
    match (Prover.is_root Prop.prop_emp e' e) with
    | None -> false
    | Some _ -> true in
  let contains_e = function
    | Sil.Hpointsto (e', _, _) -> is_root_for_e e'
    | Sil.Hlseg (_, _, e', _, _) -> is_root_for_e e'
    | Sil.Hdllseg (_, _, iF, _, _, iB, _) -> is_root_for_e iF || is_root_for_e iB in
  let rec f sigma_acc = function
    | [] -> None, sigma
    | hpred:: sigma ->
        if contains_e hpred then
          Some hpred, (IList.rev sigma_acc) @ sigma
        else
          f (hpred:: sigma_acc) sigma in
  f [] sigma

let same_pred (hpred1: Sil.hpred) (hpred2: Sil.hpred) : bool =
  match hpred1, hpred2 with
  | Sil.Hpointsto _, Sil.Hpointsto _ -> true
  | Sil.Hlseg _, Sil.Hlseg _ -> true
  | Sil.Hdllseg _, Sil.Hdllseg _ -> true
  | _ -> false

(* check that applying renaming to the lhs / rhs of [sigma_new]
 * gives [sigma] and that the renaming is injective *)

let sigma_renaming_check (lhs: side) (sigma: Prop.sigma) (sigma_new: Prop.sigma) =
  (* apply the lhs / rhs of the renaming to sigma,
   * and check that the renaming of primed vars is injective *)
  let fav_sigma = Prop.sigma_fav sigma_new in
  let sub = Rename.to_subst_proj lhs fav_sigma in
  let sigma' = Prop.sigma_sub sub sigma_new in
  sigma_equal sigma sigma'

let sigma_renaming_check_lhs = sigma_renaming_check Lhs
let sigma_renaming_check_rhs = sigma_renaming_check Rhs

let rec sigma_partial_join' mode (sigma_acc: Prop.sigma)
    (sigma1_in: Prop.sigma) (sigma2_in: Prop.sigma) : (Prop.sigma * Prop.sigma * Prop.sigma) =

  let lookup_and_expand side e e' =
    match (Rename.get_others side e, side) with
    | None, _ -> (L.d_strln "failure reason 60"; raise IList.Fail)
    | Some(e_res, e_op), Lhs -> (e_res, exp_partial_join e' e_op)
    | Some(e_res, e_op), Rhs -> (e_res, exp_partial_join e_op e') in

  let join_list_and_non side root' hlseg e opposite =
    match hlseg with
    | Sil.Hlseg (_, hpara, root, next, shared) ->
        let next' = do_side side exp_partial_join next opposite in
        let shared' = Rename.lookup_list side shared in
        CheckJoin.add side root next;
        Sil.Hlseg (Sil.Lseg_PE, hpara, root', next', shared')

    | Sil.Hdllseg (_, hpara, iF, oB, oF, iB, shared)
      when Sil.exp_equal iF e ->
        let oF' = do_side side exp_partial_join oF opposite in
        let shared' = Rename.lookup_list side shared in
        let oB', iB' = lookup_and_expand side oB iB in
        (*
        let oB' = Rename.lookup side oB in
        let iB' = Rename.lookup side iB in
        *)
        CheckJoin.add side iF oF;
        CheckJoin.add side oB iB;
        Sil.Hdllseg (Sil.Lseg_PE, hpara, root', oB', oF', iB', shared')

    | Sil.Hdllseg (_, hpara, iF, oB, oF, iB, shared)
      when Sil.exp_equal iB e ->
        let oB' = do_side side exp_partial_join oB opposite in
        let shared' = Rename.lookup_list side shared in
        let oF', iF' = lookup_and_expand side oF iF in
        (*
        let oF' = Rename.lookup side oF in
        let iF' = Rename.lookup side iF in
        *)
        CheckJoin.add side iF oF;
        CheckJoin.add side oB iB;
        Sil.Hdllseg (Sil.Lseg_PE, hpara, iF', oB', oF', root', shared')

    | _ -> assert false in

  let update_list side lseg root' =
    match lseg with
    | Sil.Hlseg (k, hpara, _, next, shared) ->
        let next' = Rename.lookup side next
        and shared' = Rename.lookup_list_todo side shared in
        Sil.Hlseg (k, hpara, root', next', shared')
    | _ -> assert false in

  let update_dllseg side dllseg iF iB =
    match dllseg with
    | Sil.Hdllseg (k, hpara, _, oB, oF, _, shared) ->
        let oB' = Rename.lookup side oB
        and oF' = Rename.lookup side oF
        and shared' = Rename.lookup_list_todo side shared in
        Sil.Hdllseg (k, hpara, iF, oB', oF', iB, shared')
    | _ -> assert false in

  (* Drop the part of 'other' sigma corresponding to 'target' sigma if possible.
     'side' describes that target is Lhs or Rhs.
     'todo' describes the start point. *)

  let cut_sigma side todo (target: Prop.sigma) (other: Prop.sigma) =
    let list_is_empty l = if l != [] then (L.d_strln "failure reason 61"; raise IList.Fail) in
    let x = Todo.take () in
    Todo.push todo;
    let res =
      match side with
      | Lhs ->
          let res, target', other' = sigma_partial_join' mode [] target other in
          list_is_empty target';
          sigma_renaming_check_lhs target res;
          other'
      | Rhs ->
          let res, other', target' = sigma_partial_join' mode [] other target in
          list_is_empty target';
          sigma_renaming_check_rhs target res;
          other' in
    Todo.set x;
    res in

  let cut_lseg side todo lseg sigma =
    match lseg with
    | Sil.Hlseg (_, hpara, root, next, shared) ->
        let _, sigma_lseg = Sil.hpara_instantiate hpara root next shared in
        cut_sigma side todo sigma_lseg sigma
    | _ -> assert false in

  let cut_dllseg side todo root lseg sigma =
    match lseg with
    | Sil.Hdllseg (_, hpara, _, oB, oF, _, shared) ->
        let _, sigma_dllseg = Sil.hpara_dll_instantiate hpara root oB oF shared in
        cut_sigma side todo sigma_dllseg sigma
    | _ -> assert false in

  try
    let todo_curr = Todo.pop () in
    let e1, e2, e = todo_curr in
    if !Config.trace_join then begin
      L.d_strln ".... sigma_partial_join' ....";
      L.d_str "TODO: "; Sil.d_exp e1; L.d_str ","; Sil.d_exp e2; L.d_str ","; Sil.d_exp e; L.d_ln ();
      L.d_strln "SIGMA1 ="; Prop.d_sigma sigma1_in; L.d_ln ();
      L.d_strln "SIGMA2 ="; Prop.d_sigma sigma2_in; L.d_ln ();
      L.d_ln ()
    end;
    let hpred_opt1, sigma1 = find_hpred_by_address e1 sigma1_in in
    let hpred_opt2, sigma2 = find_hpred_by_address e2 sigma2_in in
    match hpred_opt1, hpred_opt2 with
    | None, None ->
        sigma_partial_join' mode sigma_acc sigma1 sigma2

    | Some (Sil.Hlseg (k, _, _, _, _) as lseg), None
    | Some (Sil.Hdllseg (k, _, _, _, _, _, _) as lseg), None ->
        if (not !Config.nelseg) || (Sil.lseg_kind_equal k Sil.Lseg_PE) then
          let sigma_acc' = join_list_and_non Lhs e lseg e1 e2 :: sigma_acc in
          sigma_partial_join' mode sigma_acc' sigma1 sigma2
        else
          (L.d_strln "failure reason 62"; raise IList.Fail)

    | None, Some (Sil.Hlseg (k, _, _, _, _) as lseg)
    | None, Some (Sil.Hdllseg (k, _, _, _, _, _, _) as lseg) ->
        if (not !Config.nelseg) || (Sil.lseg_kind_equal k Sil.Lseg_PE) then
          let sigma_acc' = join_list_and_non Rhs e lseg e2 e1 :: sigma_acc in
          sigma_partial_join' mode sigma_acc' sigma1 sigma2
        else
          (L.d_strln "failure reason 63"; raise IList.Fail)

    | None, _ | _, None -> (L.d_strln "failure reason 64"; raise IList.Fail)

    | Some (hpred1), Some (hpred2) when same_pred hpred1 hpred2 ->
        let hpred_res1 = hpred_partial_join mode todo_curr hpred1 hpred2 in
        sigma_partial_join' mode (hpred_res1:: sigma_acc) sigma1 sigma2

    | Some (Sil.Hlseg _ as lseg), Some (hpred2) ->
        let sigma2' = cut_lseg Lhs todo_curr lseg (hpred2:: sigma2) in
        let sigma_acc' = update_list Lhs lseg e :: sigma_acc in
        sigma_partial_join' mode sigma_acc' sigma1 sigma2'

    | Some (hpred1), Some (Sil.Hlseg _ as lseg) ->
        let sigma1' = cut_lseg Rhs todo_curr lseg (hpred1:: sigma1) in
        let sigma_acc' = update_list Rhs lseg e :: sigma_acc in
        sigma_partial_join' mode sigma_acc' sigma1' sigma2

    | Some (Sil.Hdllseg (_, _, iF1, _, _, iB1, _) as dllseg), Some (hpred2)
      when Sil.exp_equal e1 iF1 ->
        let iB_res = exp_partial_join iB1 e2 in
        let sigma2' = cut_dllseg Lhs todo_curr iF1 dllseg (hpred2:: sigma2) in
        let sigma_acc' = update_dllseg Lhs dllseg e iB_res :: sigma_acc in
        CheckJoin.add Lhs iF1 iB1; (* add equality iF1=iB1 *)
        sigma_partial_join' mode sigma_acc' sigma1 sigma2'

    | Some (Sil.Hdllseg (_, _, iF1, _, _, iB1, _) as dllseg), Some (hpred2)
    (* when Sil.exp_equal e1 iB1 *) ->
        let iF_res = exp_partial_join iF1 e2 in
        let sigma2' = cut_dllseg Lhs todo_curr iB1 dllseg (hpred2:: sigma2) in
        let sigma_acc' = update_dllseg Lhs dllseg iF_res e :: sigma_acc in
        CheckJoin.add Lhs iF1 iB1; (* add equality iF1=iB1 *)
        sigma_partial_join' mode sigma_acc' sigma1 sigma2'

    | Some (hpred1), Some (Sil.Hdllseg (_, _, iF2, _, _, iB2, _) as dllseg)
      when Sil.exp_equal e2 iF2 ->
        let iB_res = exp_partial_join e1 iB2 in
        let sigma1' = cut_dllseg Rhs todo_curr iF2 dllseg (hpred1:: sigma1) in
        let sigma_acc' = update_dllseg Rhs dllseg e iB_res :: sigma_acc in
        CheckJoin.add Rhs iF2 iB2; (* add equality iF2=iB2 *)
        sigma_partial_join' mode sigma_acc' sigma1' sigma2

    | Some (hpred1), Some (Sil.Hdllseg (_, _, iF2, _, _, iB2, _) as dllseg) ->
        let iF_res = exp_partial_join e1 iF2 in
        let sigma1' = cut_dllseg Rhs todo_curr iB2 dllseg (hpred1:: sigma1) in
        let sigma_acc' = update_dllseg Rhs dllseg iF_res e :: sigma_acc in
        CheckJoin.add Rhs iF2 iB2; (* add equality iF2=iB2 *)
        sigma_partial_join' mode sigma_acc' sigma1' sigma2

    | Some (Sil.Hpointsto _), Some (Sil.Hpointsto _) ->
        assert false (* Should be handled by a guarded case *)

  with Todo.Empty ->
    match sigma1_in, sigma2_in with
    | _:: _, _:: _ -> L.d_strln "todo is empty, but the sigmas are not"; raise IList.Fail
    | _ -> sigma_acc, sigma1_in, sigma2_in

let sigma_partial_join mode (sigma1: Prop.sigma) (sigma2: Prop.sigma)
  : (Prop.sigma * Prop.sigma * Prop.sigma) =
  CheckJoin.init mode sigma1 sigma2;
  let lost_little = CheckJoin.lost_little in
  let s1, s2, s3 = sigma_partial_join' mode [] sigma1 sigma2 in
  try
    if Rename.check lost_little then
      (CheckJoin.final (); (s1, s2, s3))
    else begin
      L.d_strln "failed Rename.check";
      CheckJoin.final ();
      raise IList.Fail
    end
  with
  | exn -> (CheckJoin.final (); raise exn)

let rec sigma_partial_meet' (sigma_acc: Prop.sigma) (sigma1_in: Prop.sigma) (sigma2_in: Prop.sigma)
  : Prop.sigma =
  try
    let todo_curr = Todo.pop () in
    let e1, e2, e = todo_curr in
    L.d_strln ".... sigma_partial_meet' ....";
    L.d_str "TODO: "; Sil.d_exp e1; L.d_str ","; Sil.d_exp e2; L.d_str ","; Sil.d_exp e; L.d_ln ();
    L.d_str "PROP1="; Prop.d_sigma sigma1_in; L.d_ln ();
    L.d_str "PROP2="; Prop.d_sigma sigma2_in; L.d_ln ();
    L.d_ln ();
    let hpred_opt1, sigma1 = find_hpred_by_address e1 sigma1_in in
    let hpred_opt2, sigma2 = find_hpred_by_address e2 sigma2_in in
    match hpred_opt1, hpred_opt2 with
    | None, None ->
        sigma_partial_meet' sigma_acc sigma1 sigma2

    | Some hpred, None ->
        let hpred' = hpred_construct_fresh Lhs hpred in
        let sigma_acc' = hpred' :: sigma_acc in
        sigma_partial_meet' sigma_acc' sigma1 sigma2

    | None, Some hpred ->
        let hpred' = hpred_construct_fresh Rhs hpred in
        let sigma_acc' = hpred' :: sigma_acc in
        sigma_partial_meet' sigma_acc' sigma1 sigma2

    | Some (hpred1), Some (hpred2) when same_pred hpred1 hpred2 ->
        let hpred' = hpred_partial_meet todo_curr hpred1 hpred2 in
        sigma_partial_meet' (hpred':: sigma_acc) sigma1 sigma2

    | Some _, Some _ ->
        (L.d_strln "failure reason 65"; raise IList.Fail)

  with Todo.Empty ->
    match sigma1_in, sigma2_in with
    | [], [] -> sigma_acc
    | _, _ -> L.d_strln "todo is empty, but the sigmas are not"; raise IList.Fail

let sigma_partial_meet (sigma1: Prop.sigma) (sigma2: Prop.sigma) : Prop.sigma =
  sigma_partial_meet' [] sigma1 sigma2

let widening_top = Sil.Int.of_int64 Int64.max_int -- Sil.Int.of_int 1000 (* nearly max_int but not so close to overflow *)
let widening_bottom = Sil.Int.of_int64 Int64.min_int ++ Sil.Int.of_int 1000 (* nearly min_int but not so close to underflow *)

(** {2 Join and Meet for Pi} *)
let pi_partial_join mode
    (ep1: Prop.exposed Prop.t) (ep2: Prop.exposed Prop.t)
    (pi1: Prop.pi) (pi2: Prop.pi) : Prop.pi
  =
  let exp_is_const = function
    (* | Sil.Var id -> is_normal id *)
    | Sil.Const _ -> true
    (* | Sil.Lvar _ -> true *)
    | _ -> false in
  let get_array_size prop =
    (* find some array size in the prop, to be used as heuritic for upper bound in widening *)
    let size_list = ref [] in
    let do_hpred = function
      | Sil.Hpointsto (_, Sil.Earray (Sil.Const (Sil.Cint n), _, _), _) ->
          (if Sil.Int.geq n Sil.Int.one then size_list := n::!size_list)
      | _ -> () in
    IList.iter do_hpred (Prop.get_sigma prop);
    !size_list in
  let bounds =
    let bounds1 = get_array_size ep1 in
    let bounds2 = get_array_size ep2 in
    let bounds_sorted = IList.sort Sil.Int.compare_value (bounds1@bounds2) in
    IList.rev (IList.remove_duplicates Sil.Int.compare_value bounds_sorted) in
  let widening_atom a =
    (* widening heuristic for upper bound: take the size of some array, -2 and -1 *)
    match Prop.atom_exp_le_const a, bounds with
    | Some (e, n), size:: _ ->
        let first_try = Sil.Int.sub size Sil.Int.one in
        let second_try = Sil.Int.sub size Sil.Int.two in
        let bound =
          if Sil.Int.leq n first_try then
            if Sil.Int.leq n second_try then second_try else first_try
          else widening_top in
        let a' = Prop.mk_inequality (Sil.BinOp(Sil.Le, e, Sil.exp_int bound)) in
        Some a'
    | Some (e, _), [] ->
        let bound = widening_top in
        let a' = Prop.mk_inequality (Sil.BinOp(Sil.Le, e, Sil.exp_int bound)) in
        Some a'
    | _ ->
        begin
          match Prop.atom_const_lt_exp a with
          | None -> None
          | Some (n, e) ->
              let bound = if Sil.Int.leq Sil.Int.minus_one n then Sil.Int.minus_one else widening_bottom in
              let a' = Prop.mk_inequality (Sil.BinOp(Sil.Lt, Sil.exp_int bound, e)) in
              Some a'
        end in
  let is_stronger_le e n a =
    match Prop.atom_exp_le_const a with
    | None -> false
    | Some (e', n') -> Sil.exp_equal e e' && Sil.Int.lt n' n in
  let is_stronger_lt n e a =
    match Prop.atom_const_lt_exp a with
    | None -> false
    | Some (n', e') -> Sil.exp_equal e e' && Sil.Int.lt n n' in
  let join_atom_check_pre p a =
    (* check for atoms in pre mode: fail if the negation is implied by the other side *)
    let not_a = Prop.atom_negate a in
    if (Prover.check_atom p not_a) then
      (L.d_str "join_atom_check failed on "; Sil.d_atom a; L.d_ln (); raise IList.Fail) in
  let join_atom_check_attribute p a =
    (* check for attribute: fail if the attribute is not in the other side *)
    if not (Prover.check_atom p a) then
      (L.d_str "join_atom_check_attribute failed on "; Sil.d_atom a; L.d_ln (); raise IList.Fail) in
  let join_atom side p_op pi_op a =
    (* try to find the atom corresponding to a on the other side, and check if it is implied *)
    match Rename.get_other_atoms side a with
    | None -> None
    | Some (a_res, a_op) ->
        if mode = JoinState.Pre then join_atom_check_pre p_op a_op;
        if Prop.atom_is_attribute a then join_atom_check_attribute p_op a_op;
        if not (Prover.check_atom p_op a_op) then None
        else begin
          match Prop.atom_exp_le_const a_op with
          | None ->
              begin
                match Prop.atom_const_lt_exp a_op with
                | None -> Some a_res
                | Some (n, e) -> if IList.exists (is_stronger_lt n e) pi_op then (widening_atom a_res) else Some a_res
              end
          | Some (e, n) ->
              if IList.exists (is_stronger_le e n) pi_op then (widening_atom a_res) else Some a_res
        end in
  let handle_atom_with_widening size p_op pi_op atom_list a =
    (* find a join for the atom, if it fails apply widening heuristing and try again *)
    match join_atom size p_op pi_op a with
    | None ->
        (match widening_atom a with
         | None -> atom_list
         | Some a' ->
             (match join_atom size p_op pi_op a' with
              | None -> atom_list
              | Some a' -> a' :: atom_list))
    | Some a' -> a' :: atom_list in
  let filter_atom = function
    | Sil.Aneq(e, e') | Sil.Aeq(e, e')
      when (exp_is_const e && exp_is_const e') ->
        true
    | Sil.Aneq(Sil.Var _, e') | Sil.Aneq(e', Sil.Var _)
    | Sil.Aeq(Sil.Var _, e') | Sil.Aeq(e', Sil.Var _)
      when (exp_is_const e') ->
        true
    | Sil.Aneq _ -> false
    | e -> Prop.atom_is_inequality e in
  begin
    if !Config.trace_join then begin
      L.d_str "pi1: "; Prop.d_pi pi1; L.d_ln ();
      L.d_str "pi2: "; Prop.d_pi pi2; L.d_ln ()
    end;
    let atom_list1 =
      let p2 = Prop.normalize ep2 in
      IList.fold_left (handle_atom_with_widening Lhs p2 pi2) [] pi1 in
    if !Config.trace_join then (L.d_str "atom_list1: "; Prop.d_pi atom_list1; L.d_ln ());
    let atom_list_combined =
      let p1 = Prop.normalize ep1 in
      IList.fold_left (handle_atom_with_widening Rhs p1 pi1) atom_list1 pi2 in
    if !Config.trace_join then (L.d_str "atom_list_combined: "; Prop.d_pi atom_list_combined; L.d_ln ());
    let atom_list_filtered =
      IList.filter filter_atom atom_list_combined in
    if !Config.trace_join then (L.d_str "atom_list_filtered: "; Prop.d_pi atom_list_filtered; L.d_ln ());
    let atom_list_res =
      IList.rev atom_list_filtered in
    atom_list_res
  end

let pi_partial_meet (p: Prop.normal Prop.t) (ep1: 'a Prop.t) (ep2: 'b Prop.t) : Prop.normal Prop.t =
  let sub1 = Rename.to_subst_emb Lhs in
  let sub2 = Rename.to_subst_emb Rhs in

  let dom1 = Ident.idlist_to_idset (Sil.sub_domain sub1) in
  let dom2 = Ident.idlist_to_idset (Sil.sub_domain sub2) in

  let handle_atom sub dom atom =
    let fav_list = Sil.fav_to_list (Sil.atom_fav atom) in
    if IList.for_all (fun id -> Ident.IdentSet.mem id dom) fav_list then
      Sil.atom_sub sub atom
    else (L.d_str "handle_atom failed on "; Sil.d_atom atom; L.d_ln (); raise IList.Fail) in
  let f1 p' atom =
    Prop.prop_atom_and p' (handle_atom sub1 dom1 atom) in
  let f2 p' atom =
    Prop.prop_atom_and p' (handle_atom sub2 dom2 atom) in

  let pi1 = Prop.get_pi ep1 in
  let pi2 = Prop.get_pi ep2 in

  let p_pi1 = IList.fold_left f1 p pi1 in
  let p_pi2 = IList.fold_left f2 p_pi1 pi2 in
  if (Prover.check_inconsistency_base p_pi2) then (L.d_strln "check_inconsistency_base failed"; raise IList.Fail)
  else p_pi2

(** {2 Join and Meet for Prop} *)

let eprop_partial_meet (ep1: 'a Prop.t) (ep2: 'b Prop.t) : 'c Prop.t =
  SymOp.pay(); (* pay one symop *)
  let sigma1 = Prop.get_sigma ep1 in
  let sigma2 = Prop.get_sigma ep2 in

  let es1 = sigma_get_start_lexps_sort sigma1 in
  let es2 = sigma_get_start_lexps_sort sigma2 in
  let es = IList.merge_sorted_nodup Sil.exp_compare [] es1 es2 in

  let sub_check _ =
    let sub1 = Prop.get_sub ep1 in
    let sub2 = Prop.get_sub ep2 in
    let range1 = Sil.sub_range sub1 in
    let f e = Sil.fav_for_all (Sil.exp_fav e) Ident.is_normal in
    Sil.sub_equal sub1 sub2 && IList.for_all f range1 in

  if not (sub_check ()) then
    (L.d_strln "sub_check() failed"; raise IList.Fail)
  else begin
    let todos = IList.map (fun x -> (x, x, x)) es in
    IList.iter Todo.push todos;
    let sigma_new = sigma_partial_meet sigma1 sigma2 in
    let ep = Prop.replace_sigma sigma_new ep1 in
    let ep' = Prop.replace_pi [] ep in
    let p' = Prop.normalize ep' in
    let p'' = pi_partial_meet p' ep1 ep2 in
    let res = Prop.prop_rename_primed_footprint_vars p'' in
    res
  end

let prop_partial_meet p1 p2 =
  Rename.init (); FreshVarExp.init (); Todo.init ();
  try
    let res = eprop_partial_meet p1 p2 in
    Rename.final (); FreshVarExp.final (); Todo.final ();
    Some res
  with exn ->
    begin
      Rename.final (); FreshVarExp.final (); Todo.final ();
      match exn with
      | IList.Fail -> None
      | _ -> raise exn
    end

let eprop_partial_join' mode (ep1: Prop.exposed Prop.t) (ep2: Prop.exposed Prop.t) : Prop.normal Prop.t =
  SymOp.pay(); (* pay one symop *)
  let sigma1 = Prop.get_sigma ep1 in
  let sigma2 = Prop.get_sigma ep2 in
  let es1 = sigma_get_start_lexps_sort sigma1 in
  let es2 = sigma_get_start_lexps_sort sigma2 in

  let simple_check = IList.length es1 = IList.length es2 in
  let rec expensive_check es1' es2' =
    match (es1', es2') with
    | [], [] -> true
    | [], _:: _ | _:: _, [] -> false
    | e1:: es1'', e2:: es2'' ->
        Sil.exp_equal e1 e2 && expensive_check es1'' es2'' in
  let sub_common, eqs_from_sub1, eqs_from_sub2 =
    let sub1 = Prop.get_sub ep1 in
    let sub2 = Prop.get_sub ep2 in
    let sub_common, sub1_only, sub2_only = Sil.sub_symmetric_difference sub1 sub2 in
    let sub_common_normal, sub_common_other =
      let f e = Sil.fav_for_all (Sil.exp_fav e) Ident.is_normal in
      Sil.sub_range_partition f sub_common in
    let eqs1, eqs2 =
      let sub_to_eqs sub = IList.map (fun (id, e) -> Sil.Aeq(Sil.Var id, e)) (Sil.sub_to_list sub) in
      let eqs1 = sub_to_eqs sub1_only @ sub_to_eqs sub_common_other in
      let eqs2 = sub_to_eqs sub2_only in
      (eqs1, eqs2) in
    (sub_common_normal, eqs1, eqs2) in

  if not (simple_check && expensive_check es1 es2) then
    begin
      if not simple_check then L.d_strln "simple_check failed"
      else L.d_strln "expensive_check failed";
      raise IList.Fail
    end;
  let todos = IList.map (fun x -> (x, x, x)) es1 in
  IList.iter Todo.push todos;
  match sigma_partial_join mode sigma1 sigma2 with
  | sigma_new, [], [] ->
      L.d_strln "sigma_partial_join succeeded";
      let ep_sub =
        let ep = Prop.replace_pi [] ep1 in
        Prop.replace_sub sub_common ep in
      let p_sub_sigma =
        Prop.normalize (Prop.replace_sigma sigma_new ep_sub) in
      let p_sub_sigma_pi =
        let pi1 = (Prop.get_pi ep1) @ eqs_from_sub1 in
        let pi2 = (Prop.get_pi ep2) @ eqs_from_sub2 in
        let pi' = pi_partial_join mode ep1 ep2 pi1 pi2 in
        L.d_strln "pi_partial_join succeeded";
        let pi_from_fresh_vars = FreshVarExp.get_induced_pi () in
        let pi_all = pi' @ pi_from_fresh_vars in
        IList.fold_left Prop.prop_atom_and p_sub_sigma pi_all in
      p_sub_sigma_pi
  | _ ->
      L.d_strln "leftovers not empty"; raise IList.Fail

let footprint_partial_join' (p1: Prop.normal Prop.t) (p2: Prop.normal Prop.t) : Prop.normal Prop.t * Prop.normal Prop.t =
  if not !Config.footprint then p1, p2
  else begin
    let fp1 = Prop.extract_footprint p1 in
    let fp2 = Prop.extract_footprint p2 in
    let efp = eprop_partial_join' JoinState.Pre fp1 fp2 in
    let fp_pi = (* Prop.get_pure efp in *)
      let fp_pi0 = Prop.get_pure efp in
      let f a = Sil.fav_for_all (Sil.atom_fav a) Ident.is_footprint in
      IList.filter f fp_pi0 in
    let fp_sigma = (* Prop.get_sigma efp in *)
      let fp_sigma0 = Prop.get_sigma efp in
      let f a = Sil.fav_exists (Sil.hpred_fav a) (fun a -> not (Ident.is_footprint a)) in
      if IList.exists f fp_sigma0 then (L.d_strln "failure reason 66"; raise IList.Fail);
      fp_sigma0 in
    let ep1' = Prop.replace_sigma_footprint fp_sigma (Prop.replace_pi_footprint fp_pi p1) in
    let ep2' = Prop.replace_sigma_footprint fp_sigma (Prop.replace_pi_footprint fp_pi p2) in
    Prop.normalize ep1', Prop.normalize ep2'
  end

let prop_partial_join pname tenv mode p1 p2 =
  let res_by_implication_only =
    if !Config.footprint then None
    else if Prover.check_implication pname tenv p1 (Prop.expose p2) then Some p2
    else if Prover.check_implication pname tenv p2 (Prop.expose p1) then Some p1
    else None in
  match res_by_implication_only with
  | None ->
      begin
        (if !Config.footprint then JoinState.set_footprint true);
        Rename.init (); FreshVarExp.init (); Todo.init ();
        try
          let p1', p2' = footprint_partial_join' p1 p2 in
          let rename_footprint = Rename.reset () in
          Todo.reset rename_footprint;
          let res = Some (eprop_partial_join' mode (Prop.expose p1') (Prop.expose p2')) in
          (if !Config.footprint then JoinState.set_footprint false);
          Rename.final (); FreshVarExp.final (); Todo.final ();
          res
        with exn ->
          begin
            Rename.final (); FreshVarExp.final (); Todo.final ();
            (if !Config.footprint then JoinState.set_footprint false);
            (match exn with IList.Fail -> None | _ -> raise exn)
          end
      end
  | Some _ -> res_by_implication_only

let eprop_partial_join mode (ep1: Prop.exposed Prop.t) (ep2: Prop.exposed Prop.t) : Prop.normal Prop.t =
  Rename.init (); FreshVarExp.init (); Todo.init ();
  try
    let res = eprop_partial_join' mode ep1 ep2 in
    Rename.final (); FreshVarExp.final (); Todo.final ();
    res
  with exn -> (Rename.final (); FreshVarExp.final (); Todo.final (); raise exn)

(** {2 Join and Meet for Propset} *)

let list_reduce name dd f list =
  let rec element_list_reduce acc (x, p1) = function
    | [] -> ((x, p1), IList.rev acc)
    | (y, p2):: ys -> begin
        L.d_strln ("COMBINE[" ^ name ^ "] ....");
        L.d_str "ENTRY1: "; L.d_ln (); dd x; L.d_ln ();
        L.d_str "ENTRY2: "; L.d_ln (); dd y; L.d_ln ();
        L.d_ln ();
        match f x y with
        | None ->
            L.d_strln_color Red (".... COMBINE[" ^ name ^ "] FAILED ...");
            element_list_reduce ((y, p2):: acc) (x, p1) ys
        | Some x' ->
            L.d_strln_color Green (".... COMBINE[" ^ name ^ "] SUCCEEDED ....");
            L.d_strln "RESULT:"; dd x'; L.d_ln ();
            element_list_reduce acc (x', p1) ys
      end in
  let rec reduce acc = function
    | [] -> IList.rev acc
    | x:: xs ->
        let (x', xs') = element_list_reduce [] x xs in
        reduce (x':: acc) xs' in
  reduce [] list

let pathset_collapse_impl pname tenv pset =
  let f x y =
    if Prover.check_implication pname tenv x (Prop.expose y) then Some y
    else if Prover.check_implication pname tenv y (Prop.expose x) then Some x
    else None in
  let plist = Paths.PathSet.elements pset in
  let plist' = list_reduce "JOIN_IMPL" Prop.d_prop f plist in
  Paths.PathSet.from_renamed_list plist'

let jprop_partial_join mode jp1 jp2 =
  let p1, p2 = Prop.expose (Specs.Jprop.to_prop jp1), Prop.expose (Specs.Jprop.to_prop jp2) in
  try
    let p = eprop_partial_join mode p1 p2 in
    let p_renamed = Prop.prop_rename_primed_footprint_vars p in
    Some (Specs.Jprop.Joined (0, p_renamed, jp1, jp2))
  with IList.Fail -> None

let jplist_collapse mode jplist =
  let f = jprop_partial_join mode in
  list_reduce "JOIN" Specs.Jprop.d_shallow f jplist


(** Add identifiers to a list of jprops *)
let jprop_list_add_ids jplist =
  let seq_number = ref 0 in
  let rec do_jprop = function
    | Specs.Jprop.Prop (_, p) -> incr seq_number; Specs.Jprop.Prop (!seq_number, p)
    | Specs.Jprop.Joined (_, p, jp1, jp2) ->
        let jp1' = do_jprop jp1 in
        let jp2' = do_jprop jp2 in
        incr seq_number;
        Specs.Jprop.Joined (!seq_number, p, jp1', jp2') in
  IList.map (fun (p, path) -> (do_jprop p, path)) jplist

let proplist_collapse mode plist =
  let jplist = IList.map (fun (p, path) -> (Specs.Jprop.Prop (0, p), path)) plist in
  let jplist_joined = jplist_collapse mode (jplist_collapse mode jplist) in
  jprop_list_add_ids jplist_joined

let proplist_collapse_pre plist =
  let plist' = IList.map (fun p -> (p, ())) plist in
  IList.map fst (proplist_collapse JoinState.Pre plist')

let pathset_collapse pset =
  let plist = Paths.PathSet.elements pset in
  let plist' = proplist_collapse JoinState.Post plist in
  Paths.PathSet.from_renamed_list (IList.map (fun (p, path) -> (Specs.Jprop.to_prop p, path)) plist')

let join_time = ref 0.0

let pathset_join
    pname tenv (pset1: Paths.PathSet.t) (pset2: Paths.PathSet.t)
  : Paths.PathSet.t * Paths.PathSet.t =
  let mode = JoinState.Post in
  let initial_time = Unix.gettimeofday () in
  let pset_to_plist pset =
    let f_list p pa acc = (p, pa) :: acc in
    Paths.PathSet.fold f_list pset [] in
  let ppalist1 = pset_to_plist pset1 in
  let ppalist2 = pset_to_plist pset2 in
  let rec join_proppath_plist ppalist2_acc ((p2, pa2) as ppa2) = function
    | [] -> (ppa2, IList.rev ppalist2_acc)
    | ((p2', pa2') as ppa2') :: ppalist2_rest -> begin
        L.d_strln ".... JOIN ....";
        L.d_strln "JOIN SYM HEAP1: "; Prop.d_prop p2; L.d_ln ();
        L.d_strln "JOIN SYM HEAP2: "; Prop.d_prop p2'; L.d_ln (); L.d_ln ();
        match prop_partial_join pname tenv mode p2 p2' with
        | None ->
            L.d_strln_color Red ".... JOIN FAILED ...."; L.d_ln ();
            join_proppath_plist (ppa2':: ppalist2_acc) ppa2 ppalist2_rest
        | Some p2'' ->
            L.d_strln_color Green ".... JOIN SUCCEEDED ....";
            L.d_strln "RESULT SYM HEAP:"; Prop.d_prop p2''; L.d_ln (); L.d_ln ();
            join_proppath_plist ppalist2_acc (p2'', Paths.Path.join pa2 pa2') ppalist2_rest
      end in
  let rec join ppalist1_cur ppalist2_acc = function
    | [] -> (ppalist1_cur, ppalist2_acc)
    | ppa2:: ppalist2_rest ->
        let (ppa2', ppalist2_acc') = join_proppath_plist [] ppa2 ppalist2_acc in
        let (ppa2'', ppalist2_rest') = join_proppath_plist [] ppa2' ppalist2_rest in
        let (ppa2_new, ppalist1_cur') = join_proppath_plist [] ppa2'' ppalist1_cur in
        join ppalist1_cur' (ppa2_new:: ppalist2_acc') ppalist2_rest' in
  let _ppalist1_res, _ppalist2_res = join ppalist1 [] ppalist2 in
  let ren l = IList.map (fun (p, x) -> (Prop.prop_rename_primed_footprint_vars p, x)) l in
  let ppalist1_res, ppalist2_res = ren _ppalist1_res, ren _ppalist2_res in
  let res = (Paths.PathSet.from_renamed_list ppalist1_res, Paths.PathSet.from_renamed_list ppalist2_res) in
  join_time := !join_time +. (Unix.gettimeofday () -. initial_time);
  res

(**
   The meet operator does two things:
   1) makes the result logically stronger (just like additive conjunction)
   2) makes the result spatially larger (just like multiplicative conjunction).
   Assuming that the meet operator forms a partial commutative monoid (soft assumption: it means
   that the results are more predictable), try to combine every element of plist with any other element.
   Return a list of the same lenght, with each element maximally combined. The algorithm is quadratic.
   The operation is dependent on the order in which elements are combined; there is a straightforward
   order - independent algorithm but it is exponential.
*)
let proplist_meet_generate plist =
  let props_done = ref Propset.empty in
  let combine p (porig, pcombined) =
    SymOp.pay (); (* pay one symop *)
    L.d_strln ".... MEET ....";
    L.d_strln "MEET SYM HEAP1: "; Prop.d_prop p; L.d_ln ();
    L.d_strln "MEET SYM HEAP2: "; Prop.d_prop pcombined; L.d_ln ();
    match prop_partial_meet p pcombined with
    | None ->
        L.d_strln_color Red ".... MEET FAILED ...."; L.d_ln ();
        (porig, pcombined)
    | Some pcombined' ->
        L.d_strln_color Green ".... MEET SUCCEEDED ....";
        L.d_strln "RESULT SYM HEAP:"; Prop.d_prop pcombined'; L.d_ln (); L.d_ln ();
        (porig, pcombined') in
  let rec proplist_meet = function
    | [] -> ()
    | (porig, pcombined) :: pplist ->
        (* use porig instead of pcombined because it might be combinable with more othe props *)
        (* e.g. porig might contain a global var to add to the ture branch of a conditional *)
        (* but pcombined might have been combined with the false branch already *)
        let pplist' = IList.map (combine porig) pplist in
        props_done := Propset.add pcombined !props_done;
        proplist_meet pplist' in
  proplist_meet (IList.map (fun p -> (p, p)) plist);
  !props_done


let propset_meet_generate_pre pset =
  let plist = Propset.to_proplist pset in
  if !Config.meet_level = 0 then plist
  else
    let pset1 = proplist_meet_generate plist in
    let pset_new = Propset.diff pset1 pset in
    let plist_old = Propset.to_proplist pset in
    let plist_new = Propset.to_proplist pset_new in
    plist_new @ plist_old
