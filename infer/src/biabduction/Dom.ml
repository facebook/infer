(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(** Operators for the abstract domain. In particular, join and meet. *)

module L = Logging

let ( ++ ) = IntLit.add

let ( -- ) = IntLit.sub

(** {2 Utility functions for ids} *)

let can_rename id = Ident.is_primed id || Ident.is_footprint id

(** {2 Utility functions for sigma} *)

let equal_sigma sigma1 sigma2 =
  let rec f sigma1_rest sigma2_rest =
    match (sigma1_rest, sigma2_rest) with
    | [], [] ->
        ()
    | [], _ :: _ | _ :: _, [] ->
        L.d_strln "failure reason 1" ; raise Sil.JoinFail
    | hpred1 :: sigma1_rest', hpred2 :: sigma2_rest' ->
        if Sil.equal_hpred hpred1 hpred2 then f sigma1_rest' sigma2_rest'
        else ( L.d_strln "failure reason 2" ; raise Sil.JoinFail )
  in
  let sigma1_sorted = List.sort ~compare:Sil.compare_hpred sigma1 in
  let sigma2_sorted = List.sort ~compare:Sil.compare_hpred sigma2 in
  f sigma1_sorted sigma2_sorted


let sigma_get_start_lexps_sort sigma =
  let exp_compare_neg e1 e2 = -Exp.compare e1 e2 in
  let filter e = Exp.free_vars e |> Sequence.for_all ~f:Ident.is_normal in
  let lexps = Sil.hpred_list_get_lexps filter sigma in
  List.sort ~compare:exp_compare_neg lexps


(** {2 Utility functions for side} *)

type side = Lhs | Rhs

let select side e1 e2 = match side with Lhs -> e1 | Rhs -> e2

let opposite side = match side with Lhs -> Rhs | Rhs -> Lhs

let do_side side f e1 e2 = match side with Lhs -> f e1 e2 | Rhs -> f e2 e1

(** {2 Module for maintaining information about noninjectivity during join} *)

module NonInj : sig
  val init : unit -> unit

  val final : unit -> unit

  val add : side -> Exp.t -> Exp.t -> unit

  val check : side -> Exp.t list -> bool
end = struct
  (* mappings from primed or footprint var exps to primed or footprint var exps *)
  let equiv_tbl1 = Hashtbl.create 32

  let equiv_tbl2 = Hashtbl.create 32

  (* mappings from primed or footprint var exps to normal var, lvar or const exps *)
  let const_tbl1 = Hashtbl.create 32

  let const_tbl2 = Hashtbl.create 32

  let reset () =
    Hashtbl.clear equiv_tbl1 ;
    Hashtbl.clear equiv_tbl2 ;
    Hashtbl.clear const_tbl1 ;
    Hashtbl.clear const_tbl2


  let init () = reset ()

  let final () = reset ()

  let lookup' tbl e default =
    match e with
    | Exp.Var _ -> (
      try Hashtbl.find tbl e with Caml.Not_found -> Hashtbl.replace tbl e default ; default )
    | _ ->
        assert false


  let lookup_equiv' tbl e = lookup' tbl e e

  let lookup_const' tbl e = lookup' tbl e Exp.Set.empty

  let rec find' tbl e =
    let e' = lookup_equiv' tbl e in
    match e' with
    | Exp.Var _ ->
        if Exp.equal e e' then e
        else
          let root = find' tbl e' in
          Hashtbl.replace tbl e root ; root
    | _ ->
        assert false


  let union' tbl const_tbl e1 e2 =
    let r1 = find' tbl e1 in
    let r2 = find' tbl e2 in
    let new_r, old_r = match Exp.compare r1 r2 with i when i <= 0 -> (r1, r2) | _ -> (r2, r1) in
    let new_c = lookup_const' const_tbl new_r in
    let old_c = lookup_const' const_tbl old_r in
    let res_c = Exp.Set.union new_c old_c in
    if Exp.Set.cardinal res_c > 1 then ( L.d_strln "failure reason 3" ; raise Sil.JoinFail ) ;
    Hashtbl.replace tbl old_r new_r ;
    Hashtbl.replace const_tbl new_r res_c


  let replace_const' tbl const_tbl e c =
    let r = find' tbl e in
    let set = Exp.Set.add c (lookup_const' const_tbl r) in
    if Exp.Set.cardinal set > 1 then ( L.d_strln "failure reason 4" ; raise Sil.JoinFail ) ;
    Hashtbl.replace const_tbl r set


  let add side e e' =
    let tbl, const_tbl =
      match side with Lhs -> (equiv_tbl1, const_tbl1) | Rhs -> (equiv_tbl2, const_tbl2)
    in
    match (e, e') with
    | Exp.Var id, Exp.Var id' -> (
      match (can_rename id, can_rename id') with
      | true, true ->
          union' tbl const_tbl e e'
      | true, false ->
          replace_const' tbl const_tbl e e'
      | false, true ->
          replace_const' tbl const_tbl e' e
      | _ ->
          L.d_strln "failure reason 5" ; raise Sil.JoinFail )
    | Exp.Var id, Exp.Const _ | Exp.Var id, Exp.Lvar _ ->
        if can_rename id then replace_const' tbl const_tbl e e'
        else ( L.d_strln "failure reason 6" ; raise Sil.JoinFail )
    | Exp.Const _, Exp.Var id' | Exp.Lvar _, Exp.Var id' ->
        if can_rename id' then replace_const' tbl const_tbl e' e
        else ( L.d_strln "failure reason 7" ; raise Sil.JoinFail )
    | _ ->
        if not (Exp.equal e e') then ( L.d_strln "failure reason 8" ; raise Sil.JoinFail ) else ()


  let check side es =
    let f = function Exp.Var id -> can_rename id | _ -> false in
    let vars, nonvars = List.partition_tf ~f es in
    let tbl, const_tbl =
      match side with Lhs -> (equiv_tbl1, const_tbl1) | Rhs -> (equiv_tbl2, const_tbl2)
    in
    if List.length nonvars > 1 then false
    else
      match (vars, nonvars) with
      | [], _ | [_], [] ->
          true
      | v :: vars', _ ->
          let r = find' tbl v in
          let set = lookup_const' const_tbl r in
          List.for_all ~f:(fun v' -> Exp.equal (find' tbl v') r) vars'
          && List.for_all ~f:(fun c -> Exp.Set.mem c set) nonvars
end

(** {2 Modules for checking whether join or meet loses too much info} *)

module type InfoLossCheckerSig = sig
  val init : Prop.sigma -> Prop.sigma -> unit

  val final : unit -> unit

  val lost_little : side -> Exp.t -> Exp.t list -> bool

  val add : side -> Exp.t -> Exp.t -> unit
end

module Dangling : sig
  val init : Prop.sigma -> Prop.sigma -> unit

  val final : unit -> unit

  val check : side -> Exp.t -> bool
end = struct
  let lexps1 = ref Exp.Set.empty

  let lexps2 = ref Exp.Set.empty

  let get_lexp_set' sigma =
    let lexp_lst = Sil.hpred_list_get_lexps (fun _ -> true) sigma in
    List.fold ~f:(fun set e -> Exp.Set.add e set) ~init:Exp.Set.empty lexp_lst


  let init sigma1 sigma2 =
    lexps1 := get_lexp_set' sigma1 ;
    lexps2 := get_lexp_set' sigma2


  let final () =
    lexps1 := Exp.Set.empty ;
    lexps2 := Exp.Set.empty


  (* conservatively checks whether e is dangling *)
  let check side e =
    let lexps = match side with Lhs -> !lexps1 | Rhs -> !lexps2 in
    match e with
    | Exp.Var id ->
        can_rename id && not (Exp.Set.mem e lexps)
    | Exp.Const _ ->
        not (Exp.Set.mem e lexps)
    | Exp.BinOp _ ->
        not (Exp.Set.mem e lexps)
    | _ ->
        false
end

module CheckJoinPre : InfoLossCheckerSig = struct
  let init sigma1 sigma2 = NonInj.init () ; Dangling.init sigma1 sigma2

  let final () = NonInj.final () ; Dangling.final ()

  let fail_case side e es =
    let side_op = opposite side in
    match e with
    | Exp.Lvar _ ->
        false
    | Exp.Var id when Ident.is_normal id ->
        List.length es >= 1
    | Exp.Var _ ->
        if Int.equal Config.join_cond 0 then List.exists ~f:(Exp.equal Exp.zero) es
        else if Dangling.check side e then (
          let r = List.exists ~f:(fun e' -> not (Dangling.check side_op e')) es in
          if r then (
            L.d_str ".... Dangling Check (dang e:" ;
            Sil.d_exp e ;
            L.d_str ") (? es:" ;
            Sil.d_exp_list es ;
            L.d_strln ") ...." ;
            L.d_ln () ) ;
          r )
        else
          let r = List.exists ~f:(Dangling.check side_op) es in
          if r then (
            L.d_str ".... Dangling Check (notdang e:" ;
            Sil.d_exp e ;
            L.d_str ") (? es:" ;
            Sil.d_exp_list es ;
            L.d_strln ") ...." ;
            L.d_ln () ) ;
          r
    | _ ->
        false


  let lost_little side e es =
    let side_op = opposite side in
    let es = match e with Exp.Const _ -> [] | _ -> es in
    if fail_case side e es then false
    else match es with [] | [_] -> true | _ -> NonInj.check side_op es


  let add = NonInj.add
end

module CheckJoinPost : InfoLossCheckerSig = struct
  let init _ _ = NonInj.init ()

  let final () = NonInj.final ()

  let fail_case _ e es =
    match e with
    | Exp.Lvar _ ->
        false
    | Exp.Var id when Ident.is_normal id ->
        List.length es >= 1
    | Exp.Var _ ->
        false
    | _ ->
        false


  let lost_little side e es =
    let side_op = opposite side in
    let es = match e with Exp.Const _ -> [] | _ -> es in
    if fail_case side e es then false
    else match es with [] | [_] -> true | _ -> NonInj.check side_op es


  let add = NonInj.add
end

module CheckJoin : sig
  val init : JoinState.mode -> Prop.sigma -> Prop.sigma -> unit

  val final : unit -> unit

  val lost_little : side -> Exp.t -> Exp.t list -> bool

  val add : side -> Exp.t -> Exp.t -> unit
end = struct
  let mode_ref : JoinState.mode ref = ref JoinState.Post

  let init mode sigma1 sigma2 =
    mode_ref := mode ;
    match mode with
    | JoinState.Pre ->
        CheckJoinPre.init sigma1 sigma2
    | JoinState.Post ->
        CheckJoinPost.init sigma1 sigma2


  let final () =
    match !mode_ref with
    | JoinState.Pre ->
        CheckJoinPre.final () ;
        mode_ref := JoinState.Post
    | JoinState.Post ->
        CheckJoinPost.final () ;
        mode_ref := JoinState.Post


  let lost_little side e es =
    match !mode_ref with
    | JoinState.Pre ->
        CheckJoinPre.lost_little side e es
    | JoinState.Post ->
        CheckJoinPost.lost_little side e es


  let add side e1 e2 =
    match !mode_ref with
    | JoinState.Pre ->
        CheckJoinPre.add side e1 e2
    | JoinState.Post ->
        CheckJoinPost.add side e1 e2
end

(** {2 Module for worklist} *)

module Todo : sig
  exception Empty

  type t

  val init : unit -> unit

  val final : unit -> unit

  val reset : (Exp.t * Exp.t * Exp.t) list -> unit

  val push : Exp.t * Exp.t * Exp.t -> unit

  val pop : unit -> Exp.t * Exp.t * Exp.t

  val set : t -> unit

  val take : unit -> t
end = struct
  exception Empty

  type t = (Exp.t * Exp.t * Exp.t) list

  let tbl = ref []

  let init () = tbl := []

  let final () = tbl := []

  let reset todo = tbl := todo

  let push e = tbl := e :: !tbl

  let pop () =
    match !tbl with
    | h :: t ->
        tbl := t ;
        h
    | _ ->
        raise Empty


  let set todo = tbl := todo

  let take () =
    let res = !tbl in
    tbl := [] ;
    res
end

(** {2 Module for introducing fresh variables} *)

module FreshVarExp : sig
  val init : unit -> unit

  val get_fresh_exp : Exp.t -> Exp.t -> Exp.t

  val get_induced_pi : Tenv.t -> unit -> Prop.pi

  val final : unit -> unit
  (*
  val lookup : side -> Exp.t -> (Exp.t * Exp.t) option
*)
end = struct
  let t = ref []

  let init () = t := []

  let final () = t := []

  let entry_compare (e1, e2, _) (_, e2', _) =
    let n1 = Exp.compare e1 e2 in
    if n1 <> 0 then n1 else Exp.compare e2 e2'


  let get_fresh_exp e1 e2 =
    match
      List.find ~f:(fun (e1', e2', _) -> Exp.equal e1 e1' && Exp.equal e2 e2') !t
      |> Option.map ~f:trd3
    with
    | Some res ->
        res
    | None ->
        let e = Exp.get_undefined (JoinState.get_footprint ()) in
        t := (e1, e2, e) :: !t ;
        e


  let get_induced_atom tenv acc strict_lower upper e =
    let ineq_lower = Prop.mk_inequality tenv (Exp.BinOp (Binop.Lt, strict_lower, e)) in
    let ineq_upper = Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, e, upper)) in
    ineq_lower :: ineq_upper :: acc


  let minus2_to_2 = List.map ~f:IntLit.of_int [-2; -1; 0; 1; 2]

  let get_induced_pi tenv () =
    let t_sorted = List.sort ~compare:entry_compare !t in
    let add_and_chk_eq e1 e1' n =
      match (e1, e1') with
      | Exp.Const (Const.Cint n1), Exp.Const (Const.Cint n1') ->
          IntLit.eq (n1 ++ n) n1'
      | _ ->
          false
    in
    let add_and_gen_eq e e' n =
      let e_plus_n = Exp.BinOp (Binop.PlusA None, e, Exp.int n) in
      Prop.mk_eq tenv e_plus_n e'
    in
    let rec f_eqs_entry ((e1, e2, e) as entry) eqs_acc t_seen = function
      | [] ->
          (eqs_acc, t_seen)
      | ((e1', e2', e') as entry') :: t_rest' -> (
        match
          List.find ~f:(fun n -> add_and_chk_eq e1 e1' n && add_and_chk_eq e2 e2' n) minus2_to_2
          |> Option.map ~f:(fun n ->
                 let eq = add_and_gen_eq e e' n in
                 let eqs_acc' = eq :: eqs_acc in
                 f_eqs_entry entry eqs_acc' t_seen t_rest' )
        with
        | Some res ->
            res
        | None ->
            let t_seen' = entry' :: t_seen in
            f_eqs_entry entry eqs_acc t_seen' t_rest' )
    in
    let rec f_eqs eqs_acc t_acc = function
      | [] ->
          (eqs_acc, t_acc)
      | entry :: t_rest ->
          let eqs_acc', t_rest' = f_eqs_entry entry eqs_acc [] t_rest in
          let t_acc' = entry :: t_acc in
          f_eqs eqs_acc' t_acc' t_rest'
    in
    let eqs, t_minimal = f_eqs [] [] t_sorted in
    let f_ineqs acc (e1, e2, e) =
      match (e1, e2) with
      | Exp.Const (Const.Cint n1), Exp.Const (Const.Cint n2) ->
          let strict_lower1, upper1 =
            if IntLit.leq n1 n2 then (n1 -- IntLit.one, n2) else (n2 -- IntLit.one, n1)
          in
          let e_strict_lower1 = Exp.int strict_lower1 in
          let e_upper1 = Exp.int upper1 in
          get_induced_atom tenv acc e_strict_lower1 e_upper1 e
      | _ ->
          acc
    in
    List.fold ~f:f_ineqs ~init:eqs t_minimal
end

(** {2 Modules for renaming} *)

module Rename : sig
  type data_opt = ExtFresh | ExtDefault of Exp.t

  val init : unit -> unit

  val final : unit -> unit

  val reset : unit -> (Exp.t * Exp.t * Exp.t) list

  val extend : Exp.t -> Exp.t -> data_opt -> Exp.t

  val check : (side -> Exp.t -> Exp.t list -> bool) -> bool

  val get_others : side -> Exp.t -> (Exp.t * Exp.t) option

  val get_other_atoms : Tenv.t -> side -> Sil.atom -> (Sil.atom * Sil.atom) option

  val lookup : side -> Exp.t -> Exp.t

  val lookup_list : side -> Exp.t list -> Exp.t list

  val lookup_list_todo : side -> Exp.t list -> Exp.t list

  val to_subst_proj : side -> unit Ident.HashQueue.t -> Sil.subst

  val to_subst_emb : side -> Sil.subst
  (*
  val get : Exp.t -> Exp.t -> Exp.t option
  val pp : printenv -> Format.formatter -> (Exp.t * Exp.t * Exp.t) list -> unit
*)
end = struct
  type t = (Exp.t * Exp.t * Exp.t) list

  let tbl : t ref = ref []

  let init () = tbl := []

  let final () = tbl := []

  let reset () =
    let f = function
      | Exp.Var id, e, _ | e, Exp.Var id, _ ->
          Ident.is_footprint id
          && Exp.free_vars e |> Sequence.for_all ~f:(fun id -> not (Ident.is_primed id))
      | _ ->
          false
    in
    let t' = List.filter ~f !tbl in
    tbl := t' ;
    t'


  let push v = tbl := v :: !tbl

  let check lost_little =
    let f side e =
      let side_op = opposite side in
      let assoc_es =
        match e with
        | Exp.Const _ ->
            []
        | Exp.Lvar _ | Exp.Var _ | Exp.BinOp (Binop.PlusA _, Exp.Var _, _) ->
            let is_same_e (e1, e2, _) = Exp.equal e (select side e1 e2) in
            let assoc = List.filter ~f:is_same_e !tbl in
            List.map ~f:(fun (e1, e2, _) -> select side_op e1 e2) assoc
        | _ ->
            L.d_str "no pattern match in check lost_little e: " ;
            Sil.d_exp e ;
            L.d_ln () ;
            raise Sil.JoinFail
      in
      lost_little side e assoc_es
    in
    let lhs_es = List.map ~f:(fun (e1, _, _) -> e1) !tbl in
    let rhs_es = List.map ~f:(fun (_, e2, _) -> e2) !tbl in
    List.for_all ~f:(f Rhs) rhs_es && List.for_all ~f:(f Lhs) lhs_es


  let lookup_side' side e =
    let f (e1, e2, _) = Exp.equal e (select side e1 e2) in
    List.filter ~f !tbl


  let lookup_side_induced' side e =
    let res = ref [] in
    let f v =
      match (v, side) with
      | (Exp.BinOp (Binop.PlusA _, e1', Exp.Const (Const.Cint i)), e2, e'), Lhs
        when Exp.equal e e1' ->
          let c' = Exp.int (IntLit.neg i) in
          let v' =
            (e1', Exp.BinOp (Binop.PlusA None, e2, c'), Exp.BinOp (Binop.PlusA None, e', c'))
          in
          res := v' :: !res
      | (e1, Exp.BinOp (Binop.PlusA _, e2', Exp.Const (Const.Cint i)), e'), Rhs
        when Exp.equal e e2' ->
          let c' = Exp.int (IntLit.neg i) in
          let v' =
            (Exp.BinOp (Binop.PlusA None, e1, c'), e2', Exp.BinOp (Binop.PlusA None, e', c'))
          in
          res := v' :: !res
      | _ ->
          ()
    in
    List.iter ~f !tbl ; List.rev !res


  (* Return the triple whose side is [e], if it exists unique *)
  let lookup' todo side e : Exp.t =
    match e with
    | Exp.Var id when can_rename id -> (
        let r = lookup_side' side e in
        match r with
        | [((_, _, id) as t)] ->
            if todo then Todo.push t ;
            id
        | _ ->
            L.d_strln "failure reason 9" ; raise Sil.JoinFail )
    | Exp.Var _ | Exp.Const _ | Exp.Lvar _ ->
        if todo then Todo.push (e, e, e) ;
        e
    | _ ->
        L.d_strln "failure reason 10" ; raise Sil.JoinFail


  let lookup side e = lookup' false side e

  let lookup_todo side e = lookup' true side e

  let lookup_list side l = List.map ~f:(lookup side) l

  let lookup_list_todo side l = List.map ~f:(lookup_todo side) l

  let to_subst_proj (side : side) vars =
    let renaming_restricted =
      List.filter
        ~f:(function _, _, Exp.Var i -> Ident.HashQueue.mem vars i | _ -> assert false)
        !tbl
    in
    let sub_list_side =
      List.map
        ~f:(function e1, e2, Exp.Var i -> (i, select side e1 e2) | _ -> assert false)
        renaming_restricted
    in
    let sub_list_side_sorted =
      List.sort ~compare:(fun (_, e) (_, e') -> Exp.compare e e') sub_list_side
    in
    let rec find_duplicates = function
      | (_, e) :: ((_, e') :: _ as t) ->
          Exp.equal e e' || find_duplicates t
      | _ ->
          false
    in
    if find_duplicates sub_list_side_sorted then (
      L.d_strln "failure reason 11" ; raise Sil.JoinFail )
    else Sil.subst_of_list sub_list_side


  let to_subst_emb (side : side) =
    let renaming_restricted =
      let pick_id_case (e1, e2, _) =
        match select side e1 e2 with Exp.Var i -> can_rename i | _ -> false
      in
      List.filter ~f:pick_id_case !tbl
    in
    let sub_list =
      let project (e1, e2, e) =
        match select side e1 e2 with Exp.Var i -> (i, e) | _ -> assert false
      in
      List.map ~f:project renaming_restricted
    in
    let sub_list_sorted =
      let compare (i, _) (i', _) = Ident.compare i i' in
      List.sort ~compare sub_list
    in
    let rec find_duplicates = function
      | (i, _) :: ((i', _) :: _ as t) ->
          Ident.equal i i' || find_duplicates t
      | _ ->
          false
    in
    if find_duplicates sub_list_sorted then ( L.d_strln "failure reason 12" ; raise Sil.JoinFail )
    else Sil.subst_of_list sub_list_sorted


  let get_others' f_lookup side e =
    let side_op = opposite side in
    let r = f_lookup side e in
    match r with [] -> None | [(e1, e2, e')] -> Some (e', select side_op e1 e2) | _ -> None


  let get_others = get_others' lookup_side'

  let get_others_direct_or_induced side e =
    let others = get_others side e in
    match others with None -> get_others' lookup_side_induced' side e | Some _ -> others


  let get_others_deep side = function
    | Exp.BinOp (op, e, e') -> (
        let others = get_others_direct_or_induced side e in
        let others' = get_others_direct_or_induced side e' in
        match (others, others') with
        | None, _ | _, None ->
            None
        | Some (e_res, e_op), Some (e_res', e_op') ->
            let e_res'' = Exp.BinOp (op, e_res, e_res') in
            let e_op'' = Exp.BinOp (op, e_op, e_op') in
            Some (e_res'', e_op'') )
    | _ ->
        None


  let get_other_atoms tenv side atom_in =
    let build_other_atoms construct side e =
      if Config.trace_join then ( L.d_str "build_other_atoms: " ; Sil.d_exp e ; L.d_ln () ) ;
      let others1 = get_others_direct_or_induced side e in
      let others2 = match others1 with None -> get_others_deep side e | Some _ -> others1 in
      match others2 with
      | None ->
          None
      | Some (e_res, e_op) ->
          let a_res = construct e_res in
          let a_op = construct e_op in
          if Config.trace_join then (
            L.d_str "build_other_atoms (successful) " ;
            Sil.d_atom a_res ;
            L.d_str ", " ;
            Sil.d_atom a_op ;
            L.d_ln () ) ;
          Some (a_res, a_op)
    in
    let exp_contains_only_normal_ids e = Exp.free_vars e |> Sequence.for_all ~f:Ident.is_normal in
    let atom_contains_only_normal_ids a =
      Sil.atom_free_vars a |> Sequence.for_all ~f:Ident.is_normal
    in
    let normal_ids_only = atom_contains_only_normal_ids atom_in in
    if normal_ids_only then Some (atom_in, atom_in)
    else
      match atom_in with
      | Sil.Aneq ((Exp.Var id as e), e')
        when exp_contains_only_normal_ids e' && not (Ident.is_normal id) ->
          (* e' cannot also be a normal id according to the guard so we can consider the two cases
               separately (this case and the next) *)
          build_other_atoms (fun e0 -> Prop.mk_neq tenv e0 e') side e
      | Sil.Aneq (e', (Exp.Var id as e))
        when exp_contains_only_normal_ids e' && not (Ident.is_normal id) ->
          build_other_atoms (fun e0 -> Prop.mk_neq tenv e0 e') side e
      | Sil.Apred (a, (Var id as e) :: es)
        when (not (Ident.is_normal id)) && List.for_all ~f:exp_contains_only_normal_ids es ->
          build_other_atoms (fun e0 -> Prop.mk_pred tenv a (e0 :: es)) side e
      | Sil.Anpred (a, (Var id as e) :: es)
        when (not (Ident.is_normal id)) && List.for_all ~f:exp_contains_only_normal_ids es ->
          build_other_atoms (fun e0 -> Prop.mk_npred tenv a (e0 :: es)) side e
      | Sil.Aeq ((Exp.Var id as e), e')
        when exp_contains_only_normal_ids e' && not (Ident.is_normal id) ->
          (* e' cannot also be a normal id according to the guard so we can consider the two cases
               separately (this case and the next) *)
          build_other_atoms (fun e0 -> Prop.mk_eq tenv e0 e') side e
      | Sil.Aeq (e', (Exp.Var id as e))
        when exp_contains_only_normal_ids e' && not (Ident.is_normal id) ->
          build_other_atoms (fun e0 -> Prop.mk_eq tenv e0 e') side e
      | Sil.Aeq (Exp.BinOp (Binop.Le, e, e'), Exp.Const (Const.Cint i))
      | Sil.Aeq (Exp.Const (Const.Cint i), Exp.BinOp (Binop.Le, e, e'))
        when IntLit.isone i && exp_contains_only_normal_ids e' ->
          let construct e0 = Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, e0, e')) in
          build_other_atoms construct side e
      | Sil.Aeq (Exp.BinOp (Binop.Lt, e', e), Exp.Const (Const.Cint i))
      | Sil.Aeq (Exp.Const (Const.Cint i), Exp.BinOp (Binop.Lt, e', e))
        when IntLit.isone i && exp_contains_only_normal_ids e' ->
          let construct e0 = Prop.mk_inequality tenv (Exp.BinOp (Binop.Lt, e', e0)) in
          build_other_atoms construct side e
      | Sil.Aeq _ | Aneq _ | Apred _ | Anpred _ ->
          None


  type data_opt = ExtFresh | ExtDefault of Exp.t

  (* Extend the renaming relation. At least one of e1 and e2
   * should be a primed or footprint variable *)
  let extend e1 e2 default_op =
    match
      List.find ~f:(fun (f1, f2, _) -> Exp.equal e1 f1 && Exp.equal e2 f2) !tbl
      |> Option.map ~f:trd3
    with
    | Some res ->
        res
    | None ->
        let some_primed () =
          Exp.free_vars e1 |> Sequence.exists ~f:Ident.is_primed
          || Exp.free_vars e2 |> Sequence.exists ~f:Ident.is_primed
        in
        let e =
          if
            (not (Exp.free_vars e1 |> Sequence.exists ~f:can_rename))
            && not (Exp.free_vars e2 |> Sequence.exists ~f:can_rename)
          then
            if Exp.equal e1 e2 then e1 else ( L.d_strln "failure reason 13" ; raise Sil.JoinFail )
          else
            match default_op with
            | ExtDefault e ->
                e
            | ExtFresh ->
                let kind =
                  if JoinState.get_footprint () && not (some_primed ()) then Ident.kfootprint
                  else Ident.kprimed
                in
                Exp.Var (Ident.create_fresh kind)
        in
        let entry = (e1, e2, e) in
        push entry ; Todo.push entry ; e
end

(** {2 Functions for constructing fresh sil data types} *)

let extend_side' kind side e =
  match Rename.get_others side e with
  | None ->
      let e_op = Exp.Var (Ident.create_fresh kind) in
      let e_new = Exp.Var (Ident.create_fresh kind) in
      let e1, e2 = match side with Lhs -> (e, e_op) | Rhs -> (e_op, e) in
      Rename.extend e1 e2 (Rename.ExtDefault e_new)
  | Some (e', _) ->
      e'


let rec exp_construct_fresh side e =
  match e with
  | Exp.Var id ->
      if Ident.is_normal id then (
        Todo.push (e, e, e) ;
        e )
      else if Ident.is_footprint id then extend_side' Ident.kfootprint side e
      else extend_side' Ident.kprimed side e
  | Exp.Const _ ->
      e
  | Exp.Cast (t, e1) ->
      let e1' = exp_construct_fresh side e1 in
      Exp.Cast (t, e1')
  | Exp.UnOp (unop, e1, topt) ->
      let e1' = exp_construct_fresh side e1 in
      Exp.UnOp (unop, e1', topt)
  | Exp.BinOp (binop, e1, e2) ->
      let e1' = exp_construct_fresh side e1 in
      let e2' = exp_construct_fresh side e2 in
      Exp.BinOp (binop, e1', e2')
  | Exp.Exn _ ->
      e
  | Exp.Closure _ ->
      e
  | Exp.Lvar _ ->
      e
  | Exp.Lfield (e1, fld, typ) ->
      let e1' = exp_construct_fresh side e1 in
      Exp.Lfield (e1', fld, typ)
  | Exp.Lindex (e1, e2) ->
      let e1' = exp_construct_fresh side e1 in
      let e2' = exp_construct_fresh side e2 in
      Exp.Lindex (e1', e2')
  | Exp.Sizeof {dynamic_length= None} ->
      e
  | Exp.Sizeof ({dynamic_length= Some len} as sizeof) ->
      Exp.Sizeof {sizeof with dynamic_length= Some (exp_construct_fresh side len)}


let strexp_construct_fresh side =
  let f (e, inst_opt) = (exp_construct_fresh side e, inst_opt) in
  Sil.strexp_expmap f


let hpred_construct_fresh side =
  let f (e, inst_opt) = (exp_construct_fresh side e, inst_opt) in
  Sil.hpred_expmap f


(** {2 Join and Meet for Ids} *)

let ident_same_kind_primed_footprint id1 id2 =
  (Ident.is_primed id1 && Ident.is_primed id2) || (Ident.is_footprint id1 && Ident.is_footprint id2)


let ident_partial_join (id1 : Ident.t) (id2 : Ident.t) =
  match (Ident.is_normal id1, Ident.is_normal id2) with
  | true, true ->
      if Ident.equal id1 id2 then Exp.Var id1
      else ( L.d_strln "failure reason 14" ; raise Sil.JoinFail )
  | true, _ | _, true ->
      Rename.extend (Exp.Var id1) (Exp.Var id2) Rename.ExtFresh
  | _ ->
      if not (ident_same_kind_primed_footprint id1 id2) then (
        L.d_strln "failure reason 15" ; raise Sil.JoinFail )
      else
        let e1 = Exp.Var id1 in
        let e2 = Exp.Var id2 in
        Rename.extend e1 e2 Rename.ExtFresh


let ident_partial_meet (id1 : Ident.t) (id2 : Ident.t) =
  match (Ident.is_normal id1, Ident.is_normal id2) with
  | true, true ->
      if Ident.equal id1 id2 then Exp.Var id1
      else ( L.d_strln "failure reason 16" ; raise Sil.JoinFail )
  | true, _ ->
      let e1, e2 = (Exp.Var id1, Exp.Var id2) in
      Rename.extend e1 e2 (Rename.ExtDefault e1)
  | _, true ->
      let e1, e2 = (Exp.Var id1, Exp.Var id2) in
      Rename.extend e1 e2 (Rename.ExtDefault e2)
  | _ ->
      if Ident.is_primed id1 && Ident.is_primed id2 then
        Rename.extend (Exp.Var id1) (Exp.Var id2) Rename.ExtFresh
      else if Ident.is_footprint id1 && Ident.equal id1 id2 then
        let e = Exp.Var id1 in
        Rename.extend e e (Rename.ExtDefault e)
      else ( L.d_strln "failure reason 17" ; raise Sil.JoinFail )


(** {2 Join and Meet for Exps} *)

let option_partial_join partial_join o1 o2 =
  match (o1, o2) with None, _ -> o2 | _, None -> o1 | Some x1, Some x2 -> partial_join x1 x2


let const_partial_join c1 c2 =
  let is_int = function Const.Cint _ -> true | _ -> false in
  if Const.equal c1 c2 then Exp.Const c1
  else if Const.kind_equal c1 c2 && not (is_int c1) then (
    L.d_strln "failure reason 18" ; raise Sil.JoinFail )
  else if !BiabductionConfig.abs_val >= 2 then
    FreshVarExp.get_fresh_exp (Exp.Const c1) (Exp.Const c2)
  else ( L.d_strln "failure reason 19" ; raise Sil.JoinFail )


let rec exp_partial_join (e1 : Exp.t) (e2 : Exp.t) : Exp.t =
  (* L.d_str "exp_partial_join "; Sil.d_exp e1; L.d_str " "; Sil.d_exp e2; L.d_ln (); *)
  match (e1, e2) with
  | Exp.Var id1, Exp.Var id2 ->
      ident_partial_join id1 id2
  | Exp.Var id, Exp.Const _ | Exp.Const _, Exp.Var id ->
      if Ident.is_normal id then ( L.d_strln "failure reason 20" ; raise Sil.JoinFail )
      else Rename.extend e1 e2 Rename.ExtFresh
  | Exp.Const c1, Exp.Const c2 ->
      const_partial_join c1 c2
  | Exp.Var id, Exp.Lvar _ | Exp.Lvar _, Exp.Var id ->
      if Ident.is_normal id then ( L.d_strln "failure reason 21" ; raise Sil.JoinFail )
      else Rename.extend e1 e2 Rename.ExtFresh
  | Exp.BinOp (Binop.PlusA _, Exp.Var id1, Exp.Const _), Exp.Var id2
  | Exp.Var id1, Exp.BinOp (Binop.PlusA _, Exp.Var id2, Exp.Const _)
    when ident_same_kind_primed_footprint id1 id2 ->
      Rename.extend e1 e2 Rename.ExtFresh
  | Exp.BinOp (Binop.PlusA _, Exp.Var id1, Exp.Const (Const.Cint c1)), Exp.Const (Const.Cint c2)
    when can_rename id1 ->
      let c2' = c2 -- c1 in
      let e_res = Rename.extend (Exp.Var id1) (Exp.int c2') Rename.ExtFresh in
      Exp.BinOp (Binop.PlusA None, e_res, Exp.int c1)
  | Exp.Const (Const.Cint c1), Exp.BinOp (Binop.PlusA _, Exp.Var id2, Exp.Const (Const.Cint c2))
    when can_rename id2 ->
      let c1' = c1 -- c2 in
      let e_res = Rename.extend (Exp.int c1') (Exp.Var id2) Rename.ExtFresh in
      Exp.BinOp (Binop.PlusA None, e_res, Exp.int c2)
  | Exp.Cast (t1, e1), Exp.Cast (t2, e2) ->
      if not (Typ.equal t1 t2) then ( L.d_strln "failure reason 22" ; raise Sil.JoinFail )
      else
        let e1'' = exp_partial_join e1 e2 in
        Exp.Cast (t1, e1'')
  | Exp.UnOp (unop1, e1, topt1), Exp.UnOp (unop2, e2, _) ->
      if not (Unop.equal unop1 unop2) then ( L.d_strln "failure reason 23" ; raise Sil.JoinFail )
      else Exp.UnOp (unop1, exp_partial_join e1 e2, topt1) (* should be topt1 = topt2 *)
  | Exp.BinOp (Binop.PlusPI, e1, e1'), Exp.BinOp (Binop.PlusPI, e2, e2') ->
      let e1'' = exp_partial_join e1 e2 in
      let e2'' =
        match (e1', e2') with
        | Exp.Const _, Exp.Const _ ->
            exp_partial_join e1' e2'
        | _ ->
            FreshVarExp.get_fresh_exp e1 e2
      in
      Exp.BinOp (Binop.PlusPI, e1'', e2'')
  | Exp.BinOp (binop1, e1, e1'), Exp.BinOp (binop2, e2, e2') ->
      if not (Binop.equal binop1 binop2) then ( L.d_strln "failure reason 24" ; raise Sil.JoinFail )
      else
        let e1'' = exp_partial_join e1 e2 in
        let e2'' = exp_partial_join e1' e2' in
        Exp.BinOp (binop1, e1'', e2'')
  | Exp.Lvar pvar1, Exp.Lvar pvar2 ->
      if not (Pvar.equal pvar1 pvar2) then ( L.d_strln "failure reason 25" ; raise Sil.JoinFail )
      else e1
  | Exp.Lfield (e1, f1, t1), Exp.Lfield (e2, f2, _) ->
      if not (Typ.Fieldname.equal f1 f2) then ( L.d_strln "failure reason 26" ; raise Sil.JoinFail )
      else Exp.Lfield (exp_partial_join e1 e2, f1, t1) (* should be t1 = t2 *)
  | Exp.Lindex (e1, e1'), Exp.Lindex (e2, e2') ->
      let e1'' = exp_partial_join e1 e2 in
      let e2'' = exp_partial_join e1' e2' in
      Exp.Lindex (e1'', e2'')
  | ( Exp.Sizeof {typ= t1; nbytes= nbytes1; dynamic_length= len1; subtype= st1}
    , Exp.Sizeof {typ= t2; nbytes= nbytes2; dynamic_length= len2; subtype= st2} ) ->
      (* forget the static sizes if they differ *)
      let nbytes_join i1 i2 = if Int.equal i1 i2 then Some i1 else None in
      Exp.Sizeof
        { typ= typ_partial_join t1 t2
        ; nbytes= option_partial_join nbytes_join nbytes1 nbytes2
        ; dynamic_length= dynamic_length_partial_join len1 len2
        ; subtype= Subtype.join st1 st2 }
  | _ ->
      L.d_str "exp_partial_join no match " ;
      Sil.d_exp e1 ;
      L.d_str " " ;
      Sil.d_exp e2 ;
      L.d_ln () ;
      raise Sil.JoinFail


and length_partial_join len1 len2 =
  match (len1, len2) with
  | Exp.BinOp (Binop.PlusA _, e1, Exp.Const c1), Exp.BinOp (Binop.PlusA _, e2, Exp.Const c2) ->
      let e' = exp_partial_join e1 e2 in
      let c' = exp_partial_join (Exp.Const c1) (Exp.Const c2) in
      Exp.BinOp (Binop.PlusA None, e', c')
  | Exp.BinOp (Binop.PlusA _, _, _), Exp.BinOp (Binop.PlusA _, _, _) ->
      Rename.extend len1 len2 Rename.ExtFresh
  | Exp.Var id1, Exp.Var id2 when Ident.equal id1 id2 ->
      len1
  | _ ->
      exp_partial_join len1 len2


and static_length_partial_join l1 l2 =
  option_partial_join (fun len1 len2 -> if IntLit.eq len1 len2 then Some len1 else None) l1 l2


and dynamic_length_partial_join l1 l2 =
  option_partial_join (fun len1 len2 -> Some (length_partial_join len1 len2)) l1 l2


and typ_partial_join (t1 : Typ.t) (t2 : Typ.t) =
  match (t1.desc, t2.desc) with
  | Typ.Tptr (t1, pk1), Typ.Tptr (t2, pk2)
    when Typ.equal_ptr_kind pk1 pk2 && Typ.equal_quals t1.quals t2.quals ->
      Typ.mk ~default:t1 (Tptr (typ_partial_join t1 t2, pk1))
      (* quals are the same for t1 and t2 *)
  | ( Typ.Tarray {elt= typ1; length= len1; stride= stride1}
    , Typ.Tarray {elt= typ2; length= len2; stride= stride2} )
    when Typ.equal_quals typ1.quals typ2.quals ->
      let elt = typ_partial_join typ1 typ2 in
      let length = static_length_partial_join len1 len2 in
      let stride = static_length_partial_join stride1 stride2 in
      Typ.mk_array ~default:t1 elt ?length ?stride
      (* quals are the same for t1 and t2 *)
  | _ when Typ.equal t1 t2 ->
      t1 (* common case *)
  | _ ->
      L.d_str "typ_partial_join no match " ;
      Typ.d_full t1 ;
      L.d_str " " ;
      Typ.d_full t2 ;
      L.d_ln () ;
      raise Sil.JoinFail


let rec exp_partial_meet (e1 : Exp.t) (e2 : Exp.t) : Exp.t =
  match (e1, e2) with
  | Exp.Var id1, Exp.Var id2 ->
      ident_partial_meet id1 id2
  | Exp.Var id, Exp.Const _ ->
      if not (Ident.is_normal id) then Rename.extend e1 e2 (Rename.ExtDefault e2)
      else ( L.d_strln "failure reason 27" ; raise Sil.JoinFail )
  | Exp.Const _, Exp.Var id ->
      if not (Ident.is_normal id) then Rename.extend e1 e2 (Rename.ExtDefault e1)
      else ( L.d_strln "failure reason 28" ; raise Sil.JoinFail )
  | Exp.Const c1, Exp.Const c2 ->
      if Const.equal c1 c2 then e1 else ( L.d_strln "failure reason 29" ; raise Sil.JoinFail )
  | Exp.Cast (t1, e1), Exp.Cast (t2, e2) ->
      if not (Typ.equal t1 t2) then ( L.d_strln "failure reason 30" ; raise Sil.JoinFail )
      else
        let e1'' = exp_partial_meet e1 e2 in
        Exp.Cast (t1, e1'')
  | Exp.UnOp (unop1, e1, topt1), Exp.UnOp (unop2, e2, _) ->
      if not (Unop.equal unop1 unop2) then ( L.d_strln "failure reason 31" ; raise Sil.JoinFail )
      else Exp.UnOp (unop1, exp_partial_meet e1 e2, topt1) (* should be topt1 = topt2 *)
  | Exp.BinOp (binop1, e1, e1'), Exp.BinOp (binop2, e2, e2') ->
      if not (Binop.equal binop1 binop2) then ( L.d_strln "failure reason 32" ; raise Sil.JoinFail )
      else
        let e1'' = exp_partial_meet e1 e2 in
        let e2'' = exp_partial_meet e1' e2' in
        Exp.BinOp (binop1, e1'', e2'')
  | Exp.Var id, Exp.Lvar _ ->
      if not (Ident.is_normal id) then Rename.extend e1 e2 (Rename.ExtDefault e2)
      else ( L.d_strln "failure reason 33" ; raise Sil.JoinFail )
  | Exp.Lvar _, Exp.Var id ->
      if not (Ident.is_normal id) then Rename.extend e1 e2 (Rename.ExtDefault e1)
      else ( L.d_strln "failure reason 34" ; raise Sil.JoinFail )
  | Exp.Lvar pvar1, Exp.Lvar pvar2 ->
      if not (Pvar.equal pvar1 pvar2) then ( L.d_strln "failure reason 35" ; raise Sil.JoinFail )
      else e1
  | Exp.Lfield (e1, f1, t1), Exp.Lfield (e2, f2, _) ->
      if not (Typ.Fieldname.equal f1 f2) then ( L.d_strln "failure reason 36" ; raise Sil.JoinFail )
      else Exp.Lfield (exp_partial_meet e1 e2, f1, t1) (* should be t1 = t2 *)
  | Exp.Lindex (e1, e1'), Exp.Lindex (e2, e2') ->
      let e1'' = exp_partial_meet e1 e2 in
      let e2'' = exp_partial_meet e1' e2' in
      Exp.Lindex (e1'', e2'')
  | _ ->
      L.d_strln "failure reason 37" ; raise Sil.JoinFail


let exp_list_partial_join = List.map2_exn ~f:exp_partial_join

let exp_list_partial_meet = List.map2_exn ~f:exp_partial_meet

(** {2 Join and Meet for Strexp} *)

let rec strexp_partial_join mode (strexp1 : Sil.strexp) (strexp2 : Sil.strexp) : Sil.strexp =
  let rec f_fld_se_list inst mode acc fld_se_list1 fld_se_list2 =
    match (fld_se_list1, fld_se_list2) with
    | [], [] ->
        Sil.Estruct (List.rev acc, inst)
    | [], _ | _, [] -> (
      match mode with
      | JoinState.Pre ->
          L.d_strln "failure reason 42" ; raise Sil.JoinFail
      | JoinState.Post ->
          Sil.Estruct (List.rev acc, inst) )
    | (fld1, se1) :: fld_se_list1', (fld2, se2) :: fld_se_list2' -> (
        let comparison = Typ.Fieldname.compare fld1 fld2 in
        if Int.equal comparison 0 then
          let strexp' = strexp_partial_join mode se1 se2 in
          let fld_se_list_new = (fld1, strexp') :: acc in
          f_fld_se_list inst mode fld_se_list_new fld_se_list1' fld_se_list2'
        else
          match mode with
          | JoinState.Pre ->
              L.d_strln "failure reason 43" ; raise Sil.JoinFail
          | JoinState.Post ->
              if comparison < 0 then f_fld_se_list inst mode acc fld_se_list1' fld_se_list2
              else if comparison > 0 then f_fld_se_list inst mode acc fld_se_list1 fld_se_list2'
              else assert false )
    (* This case should not happen. *)
  in
  let rec f_idx_se_list inst len idx_se_list_acc idx_se_list1 idx_se_list2 =
    match (idx_se_list1, idx_se_list2) with
    | [], [] ->
        Sil.Earray (len, List.rev idx_se_list_acc, inst)
    | [], _ | _, [] -> (
      match mode with
      | JoinState.Pre ->
          L.d_strln "failure reason 44" ; raise Sil.JoinFail
      | JoinState.Post ->
          Sil.Earray (len, List.rev idx_se_list_acc, inst) )
    | (idx1, se1) :: idx_se_list1', (idx2, se2) :: idx_se_list2' ->
        let idx = exp_partial_join idx1 idx2 in
        let strexp' = strexp_partial_join mode se1 se2 in
        let idx_se_list_new = (idx, strexp') :: idx_se_list_acc in
        f_idx_se_list inst len idx_se_list_new idx_se_list1' idx_se_list2'
  in
  match (strexp1, strexp2) with
  | Sil.Eexp (e1, inst1), Sil.Eexp (e2, inst2) ->
      Sil.Eexp (exp_partial_join e1 e2, Sil.inst_partial_join inst1 inst2)
  | Sil.Estruct (fld_se_list1, inst1), Sil.Estruct (fld_se_list2, inst2) ->
      let inst = Sil.inst_partial_join inst1 inst2 in
      f_fld_se_list inst mode [] fld_se_list1 fld_se_list2
  | Sil.Earray (len1, idx_se_list1, inst1), Sil.Earray (len2, idx_se_list2, inst2) ->
      let len = length_partial_join len1 len2 in
      let inst = Sil.inst_partial_join inst1 inst2 in
      f_idx_se_list inst len [] idx_se_list1 idx_se_list2
  | _ ->
      L.d_strln "no match in strexp_partial_join" ;
      raise Sil.JoinFail


let rec strexp_partial_meet (strexp1 : Sil.strexp) (strexp2 : Sil.strexp) : Sil.strexp =
  let construct side rev_list ref_list =
    let construct_offset_se (off, se) = (off, strexp_construct_fresh side se) in
    let acc = List.map ~f:construct_offset_se ref_list in
    List.rev_append rev_list acc
  in
  let rec f_fld_se_list inst acc fld_se_list1 fld_se_list2 =
    match (fld_se_list1, fld_se_list2) with
    | [], [] ->
        Sil.Estruct (List.rev acc, inst)
    | [], _ ->
        Sil.Estruct (construct Rhs acc fld_se_list2, inst)
    | _, [] ->
        Sil.Estruct (construct Lhs acc fld_se_list1, inst)
    | (fld1, se1) :: fld_se_list1', (fld2, se2) :: fld_se_list2' ->
        let comparison = Typ.Fieldname.compare fld1 fld2 in
        if comparison < 0 then
          let se' = strexp_construct_fresh Lhs se1 in
          let acc_new = (fld1, se') :: acc in
          f_fld_se_list inst acc_new fld_se_list1' fld_se_list2
        else if comparison > 0 then
          let se' = strexp_construct_fresh Rhs se2 in
          let acc_new = (fld2, se') :: acc in
          f_fld_se_list inst acc_new fld_se_list1 fld_se_list2'
        else
          let strexp' = strexp_partial_meet se1 se2 in
          let acc_new = (fld1, strexp') :: acc in
          f_fld_se_list inst acc_new fld_se_list1' fld_se_list2'
  in
  let rec f_idx_se_list inst len acc idx_se_list1 idx_se_list2 =
    match (idx_se_list1, idx_se_list2) with
    | [], [] ->
        Sil.Earray (len, List.rev acc, inst)
    | [], _ ->
        Sil.Earray (len, construct Rhs acc idx_se_list2, inst)
    | _, [] ->
        Sil.Earray (len, construct Lhs acc idx_se_list1, inst)
    | (idx1, se1) :: idx_se_list1', (idx2, se2) :: idx_se_list2' ->
        let idx = exp_partial_meet idx1 idx2 in
        let se' = strexp_partial_meet se1 se2 in
        let acc_new = (idx, se') :: acc in
        f_idx_se_list inst len acc_new idx_se_list1' idx_se_list2'
  in
  match (strexp1, strexp2) with
  | Sil.Eexp (e1, inst1), Sil.Eexp (e2, inst2) ->
      Sil.Eexp (exp_partial_meet e1 e2, Sil.inst_partial_meet inst1 inst2)
  | Sil.Estruct (fld_se_list1, inst1), Sil.Estruct (fld_se_list2, inst2) ->
      let inst = Sil.inst_partial_meet inst1 inst2 in
      f_fld_se_list inst [] fld_se_list1 fld_se_list2
  | Sil.Earray (len1, idx_se_list1, inst1), Sil.Earray (len2, idx_se_list2, inst2)
    when Exp.equal len1 len2 ->
      let inst = Sil.inst_partial_meet inst1 inst2 in
      f_idx_se_list inst len1 [] idx_se_list1 idx_se_list2
  | _ ->
      L.d_strln "failure reason 52" ; raise Sil.JoinFail


(** {2 Join and Meet for kind, hpara, hpara_dll} *)

let kind_join k1 k2 =
  match (k1, k2) with
  | Sil.Lseg_PE, _ ->
      Sil.Lseg_PE
  | _, Sil.Lseg_PE ->
      Sil.Lseg_PE
  | Sil.Lseg_NE, Sil.Lseg_NE ->
      Sil.Lseg_NE


let kind_meet k1 k2 =
  match (k1, k2) with
  | Sil.Lseg_NE, _ ->
      Sil.Lseg_NE
  | _, Sil.Lseg_NE ->
      Sil.Lseg_NE
  | Sil.Lseg_PE, Sil.Lseg_PE ->
      Sil.Lseg_PE


let hpara_partial_join tenv (hpara1 : Sil.hpara) (hpara2 : Sil.hpara) : Sil.hpara =
  if Match.hpara_match_with_impl tenv true hpara2 hpara1 then hpara1
  else if Match.hpara_match_with_impl tenv true hpara1 hpara2 then hpara2
  else ( L.d_strln "failure reason 53" ; raise Sil.JoinFail )


let hpara_partial_meet tenv (hpara1 : Sil.hpara) (hpara2 : Sil.hpara) : Sil.hpara =
  if Match.hpara_match_with_impl tenv true hpara2 hpara1 then hpara2
  else if Match.hpara_match_with_impl tenv true hpara1 hpara2 then hpara1
  else ( L.d_strln "failure reason 54" ; raise Sil.JoinFail )


let hpara_dll_partial_join tenv (hpara1 : Sil.hpara_dll) (hpara2 : Sil.hpara_dll) : Sil.hpara_dll =
  if Match.hpara_dll_match_with_impl tenv true hpara2 hpara1 then hpara1
  else if Match.hpara_dll_match_with_impl tenv true hpara1 hpara2 then hpara2
  else ( L.d_strln "failure reason 55" ; raise Sil.JoinFail )


let hpara_dll_partial_meet tenv (hpara1 : Sil.hpara_dll) (hpara2 : Sil.hpara_dll) : Sil.hpara_dll =
  if Match.hpara_dll_match_with_impl tenv true hpara2 hpara1 then hpara2
  else if Match.hpara_dll_match_with_impl tenv true hpara1 hpara2 then hpara1
  else ( L.d_strln "failure reason 56" ; raise Sil.JoinFail )


(** {2 Join and Meet for hpred} *)

let hpred_partial_join tenv mode (todo : Exp.t * Exp.t * Exp.t) (hpred1 : Sil.hpred)
    (hpred2 : Sil.hpred) : Sil.hpred =
  let e1, e2, e = todo in
  match (hpred1, hpred2) with
  | Sil.Hpointsto (_, se1, te1), Sil.Hpointsto (_, se2, te2) ->
      let te = exp_partial_join te1 te2 in
      Prop.mk_ptsto tenv e (strexp_partial_join mode se1 se2) te
  | Sil.Hlseg (k1, hpara1, _, next1, shared1), Sil.Hlseg (k2, hpara2, _, next2, shared2) ->
      let hpara' = hpara_partial_join tenv hpara1 hpara2 in
      let next' = exp_partial_join next1 next2 in
      let shared' = exp_list_partial_join shared1 shared2 in
      Prop.mk_lseg tenv (kind_join k1 k2) hpara' e next' shared'
  | ( Sil.Hdllseg (k1, para1, iF1, oB1, oF1, iB1, shared1)
    , Sil.Hdllseg (k2, para2, iF2, oB2, oF2, iB2, shared2) ) ->
      let fwd1 = Exp.equal e1 iF1 in
      let fwd2 = Exp.equal e2 iF2 in
      let hpara' = hpara_dll_partial_join tenv para1 para2 in
      let iF', iB' =
        if fwd1 && fwd2 then (e, exp_partial_join iB1 iB2)
        else if (not fwd1) && not fwd2 then (exp_partial_join iF1 iF2, e)
        else ( L.d_strln "failure reason 57" ; raise Sil.JoinFail )
      in
      let oF' = exp_partial_join oF1 oF2 in
      let oB' = exp_partial_join oB1 oB2 in
      let shared' = exp_list_partial_join shared1 shared2 in
      Prop.mk_dllseg tenv (kind_join k1 k2) hpara' iF' oB' oF' iB' shared'
  | _ ->
      assert false


let hpred_partial_meet tenv (todo : Exp.t * Exp.t * Exp.t) (hpred1 : Sil.hpred)
    (hpred2 : Sil.hpred) : Sil.hpred =
  let e1, e2, e = todo in
  match (hpred1, hpred2) with
  | Sil.Hpointsto (_, se1, te1), Sil.Hpointsto (_, se2, te2) when Exp.equal te1 te2 ->
      Prop.mk_ptsto tenv e (strexp_partial_meet se1 se2) te1
  | Sil.Hpointsto _, _ | _, Sil.Hpointsto _ ->
      L.d_strln "failure reason 58" ; raise Sil.JoinFail
  | Sil.Hlseg (k1, hpara1, _, next1, shared1), Sil.Hlseg (k2, hpara2, _, next2, shared2) ->
      let hpara' = hpara_partial_meet tenv hpara1 hpara2 in
      let next' = exp_partial_meet next1 next2 in
      let shared' = exp_list_partial_meet shared1 shared2 in
      Prop.mk_lseg tenv (kind_meet k1 k2) hpara' e next' shared'
  | ( Sil.Hdllseg (k1, para1, iF1, oB1, oF1, iB1, shared1)
    , Sil.Hdllseg (k2, para2, iF2, oB2, oF2, iB2, shared2) ) ->
      let fwd1 = Exp.equal e1 iF1 in
      let fwd2 = Exp.equal e2 iF2 in
      let hpara' = hpara_dll_partial_meet tenv para1 para2 in
      let iF', iB' =
        if fwd1 && fwd2 then (e, exp_partial_meet iB1 iB2)
        else if (not fwd1) && not fwd2 then (exp_partial_meet iF1 iF2, e)
        else ( L.d_strln "failure reason 59" ; raise Sil.JoinFail )
      in
      let oF' = exp_partial_meet oF1 oF2 in
      let oB' = exp_partial_meet oB1 oB2 in
      let shared' = exp_list_partial_meet shared1 shared2 in
      Prop.mk_dllseg tenv (kind_meet k1 k2) hpara' iF' oB' oF' iB' shared'
  | _ ->
      assert false


(** {2 Join and Meet for Sigma} *)

let find_hpred_by_address tenv (e : Exp.t) (sigma : Prop.sigma) : Sil.hpred option * Prop.sigma =
  let is_root_for_e e' =
    match Prover.is_root tenv Prop.prop_emp e' e with None -> false | Some _ -> true
  in
  let contains_e = function
    | Sil.Hpointsto (e', _, _) ->
        is_root_for_e e'
    | Sil.Hlseg (_, _, e', _, _) ->
        is_root_for_e e'
    | Sil.Hdllseg (_, _, iF, _, _, iB, _) ->
        is_root_for_e iF || is_root_for_e iB
  in
  let rec f sigma_acc = function
    | [] ->
        (None, sigma)
    | hpred :: sigma ->
        if contains_e hpred then (Some hpred, List.rev_append sigma_acc sigma)
        else f (hpred :: sigma_acc) sigma
  in
  f [] sigma


let same_pred (hpred1 : Sil.hpred) (hpred2 : Sil.hpred) : bool =
  match (hpred1, hpred2) with
  | Sil.Hpointsto _, Sil.Hpointsto _ ->
      true
  | Sil.Hlseg _, Sil.Hlseg _ ->
      true
  | Sil.Hdllseg _, Sil.Hdllseg _ ->
      true
  | _ ->
      false


(* check that applying renaming to the lhs / rhs of [sigma_new]
 * gives [sigma] and that the renaming is injective *)

let sigma_renaming_check (lhs : side) (sigma : Prop.sigma) (sigma_new : Prop.sigma) =
  (* apply the lhs / rhs of the renaming to sigma,
   * and check that the renaming of primed vars is injective *)
  let fav_sigma = Prop.sigma_free_vars sigma_new |> Ident.hashqueue_of_sequence in
  let sub = Rename.to_subst_proj lhs fav_sigma in
  let sigma' = Prop.sigma_sub sub sigma_new in
  equal_sigma sigma sigma'


let sigma_renaming_check_lhs = sigma_renaming_check Lhs

let sigma_renaming_check_rhs = sigma_renaming_check Rhs

let rec sigma_partial_join' tenv mode (sigma_acc : Prop.sigma) (sigma1_in : Prop.sigma)
    (sigma2_in : Prop.sigma) : Prop.sigma * Prop.sigma * Prop.sigma =
  let lookup_and_expand side e e' =
    match (Rename.get_others side e, side) with
    | None, _ ->
        L.d_strln "failure reason 60" ; raise Sil.JoinFail
    | Some (e_res, e_op), Lhs ->
        (e_res, exp_partial_join e' e_op)
    | Some (e_res, e_op), Rhs ->
        (e_res, exp_partial_join e_op e')
  in
  let join_list_and_non side root' hlseg e opposite =
    match hlseg with
    | Sil.Hlseg (_, hpara, root, next, shared) ->
        let next' = do_side side exp_partial_join next opposite in
        let shared' = Rename.lookup_list side shared in
        CheckJoin.add side root next ;
        Sil.Hlseg (Sil.Lseg_PE, hpara, root', next', shared')
    | Sil.Hdllseg (_, hpara, iF, oB, oF, iB, shared) when Exp.equal iF e ->
        let oF' = do_side side exp_partial_join oF opposite in
        let shared' = Rename.lookup_list side shared in
        let oB', iB' = lookup_and_expand side oB iB in
        (*
        let oB' = Rename.lookup side oB in
        let iB' = Rename.lookup side iB in
        *)
        CheckJoin.add side iF oF ;
        CheckJoin.add side oB iB ;
        Sil.Hdllseg (Sil.Lseg_PE, hpara, root', oB', oF', iB', shared')
    | Sil.Hdllseg (_, hpara, iF, oB, oF, iB, shared) when Exp.equal iB e ->
        let oB' = do_side side exp_partial_join oB opposite in
        let shared' = Rename.lookup_list side shared in
        let oF', iF' = lookup_and_expand side oF iF in
        (*
        let oF' = Rename.lookup side oF in
        let iF' = Rename.lookup side iF in
        *)
        CheckJoin.add side iF oF ;
        CheckJoin.add side oB iB ;
        Sil.Hdllseg (Sil.Lseg_PE, hpara, iF', oB', oF', root', shared')
    | _ ->
        assert false
  in
  let update_list side lseg root' =
    match lseg with
    | Sil.Hlseg (k, hpara, _, next, shared) ->
        let next' = Rename.lookup side next and shared' = Rename.lookup_list_todo side shared in
        Sil.Hlseg (k, hpara, root', next', shared')
    | _ ->
        assert false
  in
  let update_dllseg side dllseg iF iB =
    match dllseg with
    | Sil.Hdllseg (k, hpara, _, oB, oF, _, shared) ->
        let oB' = Rename.lookup side oB
        and oF' = Rename.lookup side oF
        and shared' = Rename.lookup_list_todo side shared in
        Sil.Hdllseg (k, hpara, iF, oB', oF', iB, shared')
    | _ ->
        assert false
  in
  (* Drop the part of 'other' sigma corresponding to 'target' sigma if possible.
     'side' describes that target is Lhs or Rhs.
     'todo' describes the start point. *)
  let cut_sigma side todo (target : Prop.sigma) (other : Prop.sigma) =
    let list_is_empty l = if l <> [] then ( L.d_strln "failure reason 61" ; raise Sil.JoinFail ) in
    let x = Todo.take () in
    Todo.push todo ;
    let res =
      match side with
      | Lhs ->
          let res, target', other' = sigma_partial_join' tenv mode [] target other in
          list_is_empty target' ;
          sigma_renaming_check_lhs target res ;
          other'
      | Rhs ->
          let res, other', target' = sigma_partial_join' tenv mode [] other target in
          list_is_empty target' ;
          sigma_renaming_check_rhs target res ;
          other'
    in
    Todo.set x ; res
  in
  let cut_lseg side todo lseg sigma =
    match lseg with
    | Sil.Hlseg (_, hpara, root, next, shared) ->
        let _, sigma_lseg = Sil.hpara_instantiate hpara root next shared in
        cut_sigma side todo sigma_lseg sigma
    | _ ->
        assert false
  in
  let cut_dllseg side todo root lseg sigma =
    match lseg with
    | Sil.Hdllseg (_, hpara, _, oB, oF, _, shared) ->
        let _, sigma_dllseg = Sil.hpara_dll_instantiate hpara root oB oF shared in
        cut_sigma side todo sigma_dllseg sigma
    | _ ->
        assert false
  in
  try
    let todo_curr = Todo.pop () in
    let e1, e2, e = todo_curr in
    if Config.trace_join then (
      L.d_strln ".... sigma_partial_join' ...." ;
      L.d_str "TODO: " ;
      Sil.d_exp e1 ;
      L.d_str "," ;
      Sil.d_exp e2 ;
      L.d_str "," ;
      Sil.d_exp e ;
      L.d_ln () ;
      L.d_strln "SIGMA1 =" ;
      Prop.d_sigma sigma1_in ;
      L.d_ln () ;
      L.d_strln "SIGMA2 =" ;
      Prop.d_sigma sigma2_in ;
      L.d_ln () ;
      L.d_ln () ) ;
    let hpred_opt1, sigma1 = find_hpred_by_address tenv e1 sigma1_in in
    let hpred_opt2, sigma2 = find_hpred_by_address tenv e2 sigma2_in in
    match (hpred_opt1, hpred_opt2) with
    | None, None ->
        sigma_partial_join' tenv mode sigma_acc sigma1 sigma2
    | Some (Sil.Hlseg (k, _, _, _, _) as lseg), None
    | Some (Sil.Hdllseg (k, _, _, _, _, _, _) as lseg), None ->
        if (not Config.nelseg) || Sil.equal_lseg_kind k Sil.Lseg_PE then
          let sigma_acc' = join_list_and_non Lhs e lseg e1 e2 :: sigma_acc in
          sigma_partial_join' tenv mode sigma_acc' sigma1 sigma2
        else ( L.d_strln "failure reason 62" ; raise Sil.JoinFail )
    | None, Some (Sil.Hlseg (k, _, _, _, _) as lseg)
    | None, Some (Sil.Hdllseg (k, _, _, _, _, _, _) as lseg) ->
        if (not Config.nelseg) || Sil.equal_lseg_kind k Sil.Lseg_PE then
          let sigma_acc' = join_list_and_non Rhs e lseg e2 e1 :: sigma_acc in
          sigma_partial_join' tenv mode sigma_acc' sigma1 sigma2
        else ( L.d_strln "failure reason 63" ; raise Sil.JoinFail )
    | None, _ | _, None ->
        L.d_strln "failure reason 64" ; raise Sil.JoinFail
    | Some hpred1, Some hpred2 when same_pred hpred1 hpred2 ->
        let hpred_res1 = hpred_partial_join tenv mode todo_curr hpred1 hpred2 in
        sigma_partial_join' tenv mode (hpred_res1 :: sigma_acc) sigma1 sigma2
    | Some (Sil.Hlseg _ as lseg), Some hpred2 ->
        let sigma2' = cut_lseg Lhs todo_curr lseg (hpred2 :: sigma2) in
        let sigma_acc' = update_list Lhs lseg e :: sigma_acc in
        sigma_partial_join' tenv mode sigma_acc' sigma1 sigma2'
    | Some hpred1, Some (Sil.Hlseg _ as lseg) ->
        let sigma1' = cut_lseg Rhs todo_curr lseg (hpred1 :: sigma1) in
        let sigma_acc' = update_list Rhs lseg e :: sigma_acc in
        sigma_partial_join' tenv mode sigma_acc' sigma1' sigma2
    | Some (Sil.Hdllseg (_, _, iF1, _, _, iB1, _) as dllseg), Some hpred2 when Exp.equal e1 iF1 ->
        let iB_res = exp_partial_join iB1 e2 in
        let sigma2' = cut_dllseg Lhs todo_curr iF1 dllseg (hpred2 :: sigma2) in
        let sigma_acc' = update_dllseg Lhs dllseg e iB_res :: sigma_acc in
        CheckJoin.add Lhs iF1 iB1 ;
        (* add equality iF1=iB1 *)
        sigma_partial_join' tenv mode sigma_acc' sigma1 sigma2'
    | Some (Sil.Hdllseg (_, _, iF1, _, _, iB1, _) as dllseg), Some hpred2
    (* when Exp.equal e1 iB1 *) ->
        let iF_res = exp_partial_join iF1 e2 in
        let sigma2' = cut_dllseg Lhs todo_curr iB1 dllseg (hpred2 :: sigma2) in
        let sigma_acc' = update_dllseg Lhs dllseg iF_res e :: sigma_acc in
        CheckJoin.add Lhs iF1 iB1 ;
        (* add equality iF1=iB1 *)
        sigma_partial_join' tenv mode sigma_acc' sigma1 sigma2'
    | Some hpred1, Some (Sil.Hdllseg (_, _, iF2, _, _, iB2, _) as dllseg) when Exp.equal e2 iF2 ->
        let iB_res = exp_partial_join e1 iB2 in
        let sigma1' = cut_dllseg Rhs todo_curr iF2 dllseg (hpred1 :: sigma1) in
        let sigma_acc' = update_dllseg Rhs dllseg e iB_res :: sigma_acc in
        CheckJoin.add Rhs iF2 iB2 ;
        (* add equality iF2=iB2 *)
        sigma_partial_join' tenv mode sigma_acc' sigma1' sigma2
    | Some hpred1, Some (Sil.Hdllseg (_, _, iF2, _, _, iB2, _) as dllseg) ->
        let iF_res = exp_partial_join e1 iF2 in
        let sigma1' = cut_dllseg Rhs todo_curr iB2 dllseg (hpred1 :: sigma1) in
        let sigma_acc' = update_dllseg Rhs dllseg iF_res e :: sigma_acc in
        CheckJoin.add Rhs iF2 iB2 ;
        (* add equality iF2=iB2 *)
        sigma_partial_join' tenv mode sigma_acc' sigma1' sigma2
    | Some (Sil.Hpointsto _), Some (Sil.Hpointsto _) ->
        assert false
    (* Should be handled by a guarded case *)
  with Todo.Empty -> (
    match (sigma1_in, sigma2_in) with
    | _ :: _, _ :: _ ->
        L.d_strln "todo is empty, but the sigmas are not" ;
        raise Sil.JoinFail
    | _ ->
        (sigma_acc, sigma1_in, sigma2_in) )


let sigma_partial_join tenv mode (sigma1 : Prop.sigma) (sigma2 : Prop.sigma) :
    Prop.sigma * Prop.sigma * Prop.sigma =
  CheckJoin.init mode sigma1 sigma2 ;
  let lost_little = CheckJoin.lost_little in
  let s1, s2, s3 = sigma_partial_join' tenv mode [] sigma1 sigma2 in
  SymOp.try_finally
    ~f:(fun () ->
      if Rename.check lost_little then (s1, s2, s3)
      else ( L.d_strln "failed Rename.check" ; raise Sil.JoinFail ) )
    ~finally:CheckJoin.final


let rec sigma_partial_meet' tenv (sigma_acc : Prop.sigma) (sigma1_in : Prop.sigma)
    (sigma2_in : Prop.sigma) : Prop.sigma =
  try
    let todo_curr = Todo.pop () in
    let e1, e2, e = todo_curr in
    L.d_strln ".... sigma_partial_meet' ...." ;
    L.d_str "TODO: " ;
    Sil.d_exp e1 ;
    L.d_str "," ;
    Sil.d_exp e2 ;
    L.d_str "," ;
    Sil.d_exp e ;
    L.d_ln () ;
    L.d_str "PROP1=" ;
    Prop.d_sigma sigma1_in ;
    L.d_ln () ;
    L.d_str "PROP2=" ;
    Prop.d_sigma sigma2_in ;
    L.d_ln () ;
    L.d_ln () ;
    let hpred_opt1, sigma1 = find_hpred_by_address tenv e1 sigma1_in in
    let hpred_opt2, sigma2 = find_hpred_by_address tenv e2 sigma2_in in
    match (hpred_opt1, hpred_opt2) with
    | None, None ->
        sigma_partial_meet' tenv sigma_acc sigma1 sigma2
    | Some hpred, None ->
        let hpred' = hpred_construct_fresh Lhs hpred in
        let sigma_acc' = hpred' :: sigma_acc in
        sigma_partial_meet' tenv sigma_acc' sigma1 sigma2
    | None, Some hpred ->
        let hpred' = hpred_construct_fresh Rhs hpred in
        let sigma_acc' = hpred' :: sigma_acc in
        sigma_partial_meet' tenv sigma_acc' sigma1 sigma2
    | Some hpred1, Some hpred2 when same_pred hpred1 hpred2 ->
        let hpred' = hpred_partial_meet tenv todo_curr hpred1 hpred2 in
        sigma_partial_meet' tenv (hpred' :: sigma_acc) sigma1 sigma2
    | Some _, Some _ ->
        L.d_strln "failure reason 65" ; raise Sil.JoinFail
  with Todo.Empty -> (
    match (sigma1_in, sigma2_in) with
    | [], [] ->
        sigma_acc
    | _, _ ->
        L.d_strln "todo is empty, but the sigmas are not" ;
        raise Sil.JoinFail )


let sigma_partial_meet tenv (sigma1 : Prop.sigma) (sigma2 : Prop.sigma) : Prop.sigma =
  sigma_partial_meet' tenv [] sigma1 sigma2


let widening_top =
  (* nearly max_int but not so close to overflow *)
  IntLit.of_int64 Int64.max_value -- IntLit.of_int 1000


let widening_bottom =
  (* nearly min_int but not so close to underflow *)
  IntLit.of_int64 Int64.min_value ++ IntLit.of_int 1000


(** {2 Join and Meet for Pi} *)
let pi_partial_join tenv mode (ep1 : Prop.exposed Prop.t) (ep2 : Prop.exposed Prop.t)
    (pi1 : Prop.pi) (pi2 : Prop.pi) : Prop.pi =
  let get_array_len prop =
    (* find some array length in the prop, to be used as heuritic for upper bound in widening *)
    let len_list = ref [] in
    let do_hpred = function
      | Sil.Hpointsto (_, Sil.Earray (Exp.Const (Const.Cint n), _, _), _) ->
          if IntLit.geq n IntLit.one then len_list := n :: !len_list
      | _ ->
          ()
    in
    List.iter ~f:do_hpred prop.Prop.sigma ;
    !len_list
  in
  let bounds =
    let bounds1 = get_array_len ep1 in
    let bounds2 = get_array_len ep2 in
    let bounds_sorted = List.sort ~compare:IntLit.compare_value (bounds1 @ bounds2) in
    List.rev (List.remove_consecutive_duplicates ~equal:IntLit.eq bounds_sorted)
  in
  let widening_atom a =
    (* widening heuristic for upper bound: take the length of some array, -2 and -1 *)
    match (Prop.atom_exp_le_const a, bounds) with
    | Some (e, n), len :: _ ->
        let first_try = IntLit.sub len IntLit.one in
        let second_try = IntLit.sub len IntLit.two in
        let bound =
          if IntLit.leq n first_try then if IntLit.leq n second_try then second_try else first_try
          else widening_top
        in
        let a' = Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, e, Exp.int bound)) in
        Some a'
    | Some (e, _), [] ->
        let bound = widening_top in
        let a' = Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, e, Exp.int bound)) in
        Some a'
    | _ -> (
      match Prop.atom_const_lt_exp a with
      | None ->
          None
      | Some (n, e) ->
          let bound =
            if IntLit.leq IntLit.minus_one n then IntLit.minus_one else widening_bottom
          in
          let a' = Prop.mk_inequality tenv (Exp.BinOp (Binop.Lt, Exp.int bound, e)) in
          Some a' )
  in
  let is_stronger_le e n a =
    match Prop.atom_exp_le_const a with
    | None ->
        false
    | Some (e', n') ->
        Exp.equal e e' && IntLit.lt n' n
  in
  let is_stronger_lt n e a =
    match Prop.atom_const_lt_exp a with
    | None ->
        false
    | Some (n', e') ->
        Exp.equal e e' && IntLit.lt n n'
  in
  let join_atom_check_pre p a =
    (* check for atoms in pre mode: fail if the negation is implied by the other side *)
    let not_a = Prover.atom_negate tenv a in
    if Prover.check_atom tenv p not_a then (
      L.d_str "join_atom_check failed on " ;
      Sil.d_atom a ;
      L.d_ln () ;
      raise Sil.JoinFail )
  in
  let join_atom_check_attribute p a =
    (* check for attribute: fail if the attribute is not in the other side *)
    if not (Prover.check_atom tenv p a) then (
      L.d_str "join_atom_check_attribute failed on " ;
      Sil.d_atom a ;
      L.d_ln () ;
      raise Sil.JoinFail )
  in
  let join_atom side p_op pi_op a =
    (* try to find the atom corresponding to a on the other side, and check if it is implied *)
    match Rename.get_other_atoms tenv side a with
    | None ->
        None
    | Some (a_res, a_op) -> (
        if JoinState.equal_mode mode JoinState.Pre then join_atom_check_pre p_op a_op ;
        if Attribute.is_pred a then join_atom_check_attribute p_op a_op ;
        if not (Prover.check_atom tenv p_op a_op) then None
        else
          match Prop.atom_exp_le_const a_op with
          | None -> (
            match Prop.atom_const_lt_exp a_op with
            | None ->
                Some a_res
            | Some (n, e) ->
                if List.exists ~f:(is_stronger_lt n e) pi_op then widening_atom a_res
                else Some a_res )
          | Some (e, n) ->
              if List.exists ~f:(is_stronger_le e n) pi_op then widening_atom a_res else Some a_res
        )
  in
  let handle_atom_with_widening len p_op pi_op atom_list a =
    (* find a join for the atom, if it fails apply widening heuristing and try again *)
    match join_atom len p_op pi_op a with
    | None -> (
      match widening_atom a with
      | None ->
          atom_list
      | Some a' -> (
        match join_atom len p_op pi_op a' with None -> atom_list | Some a' -> a' :: atom_list ) )
    | Some a' ->
        a' :: atom_list
  in
  if Config.trace_join then (
    L.d_str "pi1: " ; Prop.d_pi pi1 ; L.d_ln () ; L.d_str "pi2: " ; Prop.d_pi pi2 ; L.d_ln () ) ;
  let atom_list1 =
    let p2 = Prop.normalize tenv ep2 in
    List.fold ~f:(handle_atom_with_widening Lhs p2 pi2) ~init:[] pi1
  in
  if Config.trace_join then ( L.d_str "atom_list1: " ; Prop.d_pi atom_list1 ; L.d_ln () ) ;
  let atom_list2 =
    let p1 = Prop.normalize tenv ep1 in
    List.fold ~f:(handle_atom_with_widening Rhs p1 pi1) ~init:[] pi2
  in
  if Config.trace_join then ( L.d_str "atom_list2: " ; Prop.d_pi atom_list2 ; L.d_ln () ) ;
  let atom_list_combined = IList.inter ~cmp:Sil.compare_atom atom_list1 atom_list2 in
  if Config.trace_join then (
    L.d_str "atom_list_combined: " ; Prop.d_pi atom_list_combined ; L.d_ln () ) ;
  atom_list_combined


let pi_partial_meet tenv (p : Prop.normal Prop.t) (ep1 : 'a Prop.t) (ep2 : 'b Prop.t) :
    Prop.normal Prop.t =
  let sub1 = Rename.to_subst_emb Lhs in
  let sub2 = Rename.to_subst_emb Rhs in
  let dom1 = Ident.idlist_to_idset (Sil.sub_domain sub1) in
  let dom2 = Ident.idlist_to_idset (Sil.sub_domain sub2) in
  let handle_atom sub dom atom =
    if Sil.atom_free_vars atom |> Sequence.for_all ~f:(fun id -> Ident.Set.mem id dom) then
      Sil.atom_sub sub atom
    else ( L.d_str "handle_atom failed on " ; Sil.d_atom atom ; L.d_ln () ; raise Sil.JoinFail )
  in
  let f1 p' atom = Prop.prop_atom_and tenv p' (handle_atom sub1 dom1 atom) in
  let f2 p' atom = Prop.prop_atom_and tenv p' (handle_atom sub2 dom2 atom) in
  let pi1 = ep1.Prop.pi in
  let pi2 = ep2.Prop.pi in
  let p_pi1 = List.fold ~f:f1 ~init:p pi1 in
  let p_pi2 = List.fold ~f:f2 ~init:p_pi1 pi2 in
  if Prover.check_inconsistency_base tenv p_pi2 then (
    L.d_strln "check_inconsistency_base failed" ;
    raise Sil.JoinFail )
  else p_pi2


(** {2 Join and Meet for Prop} *)

let eprop_partial_meet tenv (ep1 : 'a Prop.t) (ep2 : 'b Prop.t) : 'c Prop.t =
  SymOp.pay () ;
  (* pay one symop *)
  let sigma1 = ep1.Prop.sigma in
  let sigma2 = ep2.Prop.sigma in
  let es1 = sigma_get_start_lexps_sort sigma1 in
  let es2 = sigma_get_start_lexps_sort sigma2 in
  let es = IList.merge_sorted_nodup ~cmp:Exp.compare ~res:[] es1 es2 in
  let sub_check _ =
    let sub1 = ep1.Prop.sub in
    let sub2 = ep2.Prop.sub in
    let range1 = Sil.sub_range sub1 in
    let f e = Exp.free_vars e |> Sequence.for_all ~f:Ident.is_normal in
    Sil.equal_subst sub1 sub2 && List.for_all ~f range1
  in
  if not (sub_check ()) then ( L.d_strln "sub_check() failed" ; raise Sil.JoinFail )
  else
    let todos = List.map ~f:(fun x -> (x, x, x)) es in
    List.iter ~f:Todo.push todos ;
    let sigma_new = sigma_partial_meet tenv sigma1 sigma2 in
    let ep = Prop.set ep1 ~sigma:sigma_new in
    let ep' = Prop.set ep ~pi:[] in
    let p' = Prop.normalize tenv ep' in
    let p'' = pi_partial_meet tenv p' ep1 ep2 in
    let res = Prop.prop_rename_primed_footprint_vars tenv p'' in
    res


let prop_partial_meet tenv p1 p2 =
  Rename.init () ;
  FreshVarExp.init () ;
  Todo.init () ;
  try
    SymOp.try_finally
      ~f:(fun () -> Some (eprop_partial_meet tenv p1 p2))
      ~finally:(fun () -> Rename.final () ; FreshVarExp.final () ; Todo.final ())
  with Sil.JoinFail -> None


let eprop_partial_join' tenv mode (ep1 : Prop.exposed Prop.t) (ep2 : Prop.exposed Prop.t) :
    Prop.normal Prop.t =
  SymOp.pay () ;
  (* pay one symop *)
  let sigma1 = ep1.Prop.sigma in
  let sigma2 = ep2.Prop.sigma in
  let es1 = sigma_get_start_lexps_sort sigma1 in
  let es2 = sigma_get_start_lexps_sort sigma2 in
  let simple_check = Int.equal (List.length es1) (List.length es2) in
  let rec expensive_check es1' es2' =
    match (es1', es2') with
    | [], [] ->
        true
    | [], _ :: _ | _ :: _, [] ->
        false
    | e1 :: es1'', e2 :: es2'' ->
        Exp.equal e1 e2 && expensive_check es1'' es2''
  in
  let sub_common, eqs_from_sub1, eqs_from_sub2 =
    let sub1 = ep1.Prop.sub in
    let sub2 = ep2.Prop.sub in
    let sub_common, sub1_only, sub2_only = Sil.sub_symmetric_difference sub1 sub2 in
    let sub_common_normal, sub_common_other =
      let f e = Exp.free_vars e |> Sequence.for_all ~f:Ident.is_normal in
      Sil.sub_range_partition f sub_common
    in
    let eqs1, eqs2 =
      let sub_to_eqs sub =
        List.map ~f:(fun (id, e) -> Sil.Aeq (Exp.Var id, e)) (Sil.sub_to_list sub)
      in
      let eqs1 = sub_to_eqs sub1_only @ sub_to_eqs sub_common_other in
      let eqs2 = sub_to_eqs sub2_only in
      (eqs1, eqs2)
    in
    (sub_common_normal, eqs1, eqs2)
  in
  if not (simple_check && expensive_check es1 es2) then (
    if not simple_check then L.d_strln "simple_check failed"
    else L.d_strln "expensive_check failed" ;
    raise Sil.JoinFail ) ;
  let todos = List.map ~f:(fun x -> (x, x, x)) es1 in
  List.iter ~f:Todo.push todos ;
  match sigma_partial_join tenv mode sigma1 sigma2 with
  | sigma_new, [], [] ->
      L.d_strln "sigma_partial_join succeeded" ;
      let ep_sub =
        let ep = Prop.set ep1 ~pi:[] in
        Prop.set ep ~sub:sub_common
      in
      let p_sub_sigma = Prop.normalize tenv (Prop.set ep_sub ~sigma:sigma_new) in
      let p_sub_sigma_pi =
        let pi1 = ep1.Prop.pi @ eqs_from_sub1 in
        let pi2 = ep2.Prop.pi @ eqs_from_sub2 in
        let pi' = pi_partial_join tenv mode ep1 ep2 pi1 pi2 in
        L.d_strln "pi_partial_join succeeded" ;
        let pi_from_fresh_vars = FreshVarExp.get_induced_pi tenv () in
        let pi_all = pi' @ pi_from_fresh_vars in
        List.fold ~f:(Prop.prop_atom_and tenv) ~init:p_sub_sigma pi_all
      in
      p_sub_sigma_pi
  | _ ->
      L.d_strln "leftovers not empty" ; raise Sil.JoinFail


let footprint_partial_join' tenv (p1 : Prop.normal Prop.t) (p2 : Prop.normal Prop.t) :
    Prop.normal Prop.t * Prop.normal Prop.t =
  if not !BiabductionConfig.footprint then (p1, p2)
  else
    let fp1 = Prop.extract_footprint p1 in
    let fp2 = Prop.extract_footprint p2 in
    let efp = eprop_partial_join' tenv JoinState.Pre fp1 fp2 in
    let pi_fp =
      let pi_fp0 = Prop.get_pure efp in
      let f a = Sil.atom_free_vars a |> Sequence.for_all ~f:Ident.is_footprint in
      List.filter ~f pi_fp0
    in
    let sigma_fp =
      let sigma_fp0 = efp.Prop.sigma in
      let f a =
        Sil.hpred_free_vars a |> Sequence.exists ~f:(fun a -> not (Ident.is_footprint a))
      in
      if List.exists ~f sigma_fp0 then ( L.d_strln "failure reason 66" ; raise Sil.JoinFail ) ;
      sigma_fp0
    in
    let ep1' = Prop.set p1 ~pi_fp ~sigma_fp in
    let ep2' = Prop.set p2 ~pi_fp ~sigma_fp in
    (Prop.normalize tenv ep1', Prop.normalize tenv ep2')


let prop_partial_join pname tenv mode p1 p2 =
  let res_by_implication_only =
    if !BiabductionConfig.footprint then None
    else if Prover.check_implication pname tenv p1 (Prop.expose p2) then Some p2
    else if Prover.check_implication pname tenv p2 (Prop.expose p1) then Some p1
    else None
  in
  match res_by_implication_only with
  | None -> (
      if !BiabductionConfig.footprint then JoinState.set_footprint true ;
      Rename.init () ;
      FreshVarExp.init () ;
      Todo.init () ;
      try
        SymOp.try_finally
          ~f:(fun () ->
            let p1', p2' = footprint_partial_join' tenv p1 p2 in
            let rename_footprint = Rename.reset () in
            Todo.reset rename_footprint ;
            let res = eprop_partial_join' tenv mode (Prop.expose p1') (Prop.expose p2') in
            if !BiabductionConfig.footprint then JoinState.set_footprint false ;
            Some res )
          ~finally:(fun () -> Rename.final () ; FreshVarExp.final () ; Todo.final ())
      with Sil.JoinFail -> None )
  | Some _ ->
      res_by_implication_only


let eprop_partial_join tenv mode (ep1 : Prop.exposed Prop.t) (ep2 : Prop.exposed Prop.t) :
    Prop.normal Prop.t =
  Rename.init () ;
  FreshVarExp.init () ;
  Todo.init () ;
  SymOp.try_finally
    ~f:(fun () -> eprop_partial_join' tenv mode ep1 ep2)
    ~finally:(fun () -> Rename.final () ; FreshVarExp.final () ; Todo.final ())


(** {2 Join and Meet for Propset} *)

let list_reduce name dd f list =
  let rec element_list_reduce acc (x, p1) = function
    | [] ->
        ((x, p1), List.rev acc)
    | (y, p2) :: ys -> (
        L.d_strln ("COMBINE[" ^ name ^ "] ....") ;
        L.d_strln "ENTRY1:" ;
        dd x ;
        L.d_ln () ;
        L.d_strln "ENTRY2:" ;
        dd y ;
        L.d_ln () ;
        L.d_ln () ;
        match f x y with
        | None ->
            L.d_strln ~color:Red (".... COMBINE[" ^ name ^ "] FAILED ...") ;
            element_list_reduce ((y, p2) :: acc) (x, p1) ys
        | Some x' ->
            L.d_strln ~color:Green (".... COMBINE[" ^ name ^ "] SUCCEEDED ....") ;
            L.d_strln "RESULT:" ;
            dd x' ;
            L.d_ln () ;
            element_list_reduce acc (x', p1) ys )
  in
  let rec reduce acc = function
    | [] ->
        List.rev acc
    | x :: xs ->
        let x', xs' = element_list_reduce [] x xs in
        reduce (x' :: acc) xs'
  in
  reduce [] list


let pathset_collapse_impl pname tenv pset =
  let f x y =
    if Prover.check_implication pname tenv x (Prop.expose y) then Some y
    else if Prover.check_implication pname tenv y (Prop.expose x) then Some x
    else None
  in
  let plist = Paths.PathSet.elements pset in
  let plist' = list_reduce "JOIN_IMPL" Prop.d_prop f plist in
  Paths.PathSet.from_renamed_list plist'


let jprop_partial_join tenv mode jp1 jp2 =
  let p1, p2 =
    ( Prop.expose (BiabductionSummary.Jprop.to_prop jp1)
    , Prop.expose (BiabductionSummary.Jprop.to_prop jp2) )
  in
  try
    let p = eprop_partial_join tenv mode p1 p2 in
    let p_renamed = Prop.prop_rename_primed_footprint_vars tenv p in
    Some (BiabductionSummary.Jprop.Joined (0, p_renamed, jp1, jp2))
  with Sil.JoinFail -> None


let jplist_collapse tenv mode jplist =
  let f = jprop_partial_join tenv mode in
  list_reduce "JOIN" BiabductionSummary.Jprop.d_shallow f jplist


(** Add identifiers to a list of jprops *)
let jprop_list_add_ids jplist =
  let seq_number = ref 0 in
  let rec do_jprop = function
    | BiabductionSummary.Jprop.Prop (_, p) ->
        incr seq_number ;
        BiabductionSummary.Jprop.Prop (!seq_number, p)
    | BiabductionSummary.Jprop.Joined (_, p, jp1, jp2) ->
        let jp1' = do_jprop jp1 in
        let jp2' = do_jprop jp2 in
        incr seq_number ;
        BiabductionSummary.Jprop.Joined (!seq_number, p, jp1', jp2')
  in
  List.map ~f:(fun (p, path) -> (do_jprop p, path)) jplist


let proplist_collapse tenv mode plist =
  let jplist = List.map ~f:(fun (p, path) -> (BiabductionSummary.Jprop.Prop (0, p), path)) plist in
  let jplist_joined = jplist_collapse tenv mode (jplist_collapse tenv mode jplist) in
  jprop_list_add_ids jplist_joined


let proplist_collapse_pre tenv plist =
  let plist' = List.map ~f:(fun p -> (p, ())) plist in
  List.map ~f:fst (proplist_collapse tenv JoinState.Pre plist')


let pathset_collapse tenv pset =
  let plist = Paths.PathSet.elements pset in
  let plist' = proplist_collapse tenv JoinState.Post plist in
  Paths.PathSet.from_renamed_list
    (List.map ~f:(fun (p, path) -> (BiabductionSummary.Jprop.to_prop p, path)) plist')


let pathset_join pname tenv (pset1 : Paths.PathSet.t) (pset2 : Paths.PathSet.t) :
    Paths.PathSet.t * Paths.PathSet.t =
  let mode = JoinState.Post in
  let pset_to_plist pset =
    let f_list p pa acc = (p, pa) :: acc in
    Paths.PathSet.fold f_list pset []
  in
  let ppalist1 = pset_to_plist pset1 in
  let ppalist2 = pset_to_plist pset2 in
  let rec join_proppath_plist ppalist2_acc ((p2, pa2) as ppa2) = function
    | [] ->
        (ppa2, List.rev ppalist2_acc)
    | ((p2', pa2') as ppa2') :: ppalist2_rest -> (
        L.d_strln ".... JOIN ...." ;
        L.d_strln "JOIN SYM HEAP1:" ;
        Prop.d_prop p2 ;
        L.d_ln () ;
        L.d_strln "JOIN SYM HEAP2:" ;
        Prop.d_prop p2' ;
        L.d_ln () ;
        L.d_ln () ;
        match prop_partial_join pname tenv mode p2 p2' with
        | None ->
            L.d_strln ~color:Red ".... JOIN FAILED ...." ;
            L.d_ln () ;
            join_proppath_plist (ppa2' :: ppalist2_acc) ppa2 ppalist2_rest
        | Some p2'' ->
            L.d_strln ~color:Green ".... JOIN SUCCEEDED ...." ;
            L.d_strln "RESULT SYM HEAP:" ;
            Prop.d_prop p2'' ;
            L.d_ln () ;
            L.d_ln () ;
            join_proppath_plist ppalist2_acc (p2'', Paths.Path.join pa2 pa2') ppalist2_rest )
  in
  let rec join ppalist1_cur ppalist2_acc = function
    | [] ->
        (ppalist1_cur, ppalist2_acc)
    | ppa2 :: ppalist2_rest ->
        let ppa2', ppalist2_acc' = join_proppath_plist [] ppa2 ppalist2_acc in
        let ppa2'', ppalist2_rest' = join_proppath_plist [] ppa2' ppalist2_rest in
        let ppa2_new, ppalist1_cur' = join_proppath_plist [] ppa2'' ppalist1_cur in
        join ppalist1_cur' (ppa2_new :: ppalist2_acc') ppalist2_rest'
  in
  let ppalist1_res_, ppalist2_res_ = join ppalist1 [] ppalist2 in
  let ren l = List.map ~f:(fun (p, x) -> (Prop.prop_rename_primed_footprint_vars tenv p, x)) l in
  let ppalist1_res, ppalist2_res = (ren ppalist1_res_, ren ppalist2_res_) in
  let res =
    (Paths.PathSet.from_renamed_list ppalist1_res, Paths.PathSet.from_renamed_list ppalist2_res)
  in
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
let proplist_meet_generate tenv plist =
  let props_done = ref Propset.empty in
  let combine p (porig, pcombined) =
    SymOp.pay () ;
    (* pay one symop *)
    L.d_strln ".... MEET ...." ;
    L.d_strln "MEET SYM HEAP1:" ;
    Prop.d_prop p ;
    L.d_ln () ;
    L.d_strln "MEET SYM HEAP2:" ;
    Prop.d_prop pcombined ;
    L.d_ln () ;
    match prop_partial_meet tenv p pcombined with
    | None ->
        L.d_strln ~color:Red ".... MEET FAILED ...." ;
        L.d_ln () ;
        (porig, pcombined)
    | Some pcombined' ->
        L.d_strln ~color:Green ".... MEET SUCCEEDED ...." ;
        L.d_strln "RESULT SYM HEAP:" ;
        Prop.d_prop pcombined' ;
        L.d_ln () ;
        L.d_ln () ;
        (porig, pcombined')
  in
  let rec proplist_meet = function
    | [] ->
        ()
    | (porig, pcombined) :: pplist ->
        (* use porig instead of pcombined because it might be combinable with more othe props *)
        (* e.g. porig might contain a global var to add to the ture branch of a conditional *)
        (* but pcombined might have been combined with the false branch already *)
        let pplist' = List.map ~f:(combine porig) pplist in
        props_done := Propset.add tenv pcombined !props_done ;
        proplist_meet pplist'
  in
  proplist_meet (List.map ~f:(fun p -> (p, p)) plist) ;
  !props_done


let propset_meet_generate_pre tenv pset =
  let plist = Propset.to_proplist pset in
  if Int.equal Config.meet_level 0 then plist
  else
    let pset1 = proplist_meet_generate tenv plist in
    let pset_new = Propset.diff pset1 pset in
    let plist_old = Propset.to_proplist pset in
    let plist_new = Propset.to_proplist pset_new in
    plist_new @ plist_old
