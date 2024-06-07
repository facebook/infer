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

let decrease_indent_when_exception thunk =
  try thunk ()
  with exn when Exception.exn_not_failure exn ->
    IExn.reraise_after exn ~f:(fun () -> L.d_decrease_indent ())


let compute_max_from_nonempty_int_list l =
  Option.value_exn (List.max_elt ~compare:IntLit.compare_value l)


let compute_min_from_nonempty_int_list l =
  Option.value_exn (List.min_elt ~compare:IntLit.compare_value l)


let rec list_rev_acc acc = function [] -> acc | x :: l -> list_rev_acc (x :: acc) l

let rec remove_redundancy have_same_key acc = function
  | [] ->
      List.rev acc
  | [x] ->
      List.rev (x :: acc)
  | x :: (y :: l' as l) ->
      if have_same_key x y then remove_redundancy have_same_key acc (x :: l')
      else remove_redundancy have_same_key (x :: acc) l


let rec is_java_class tenv (typ : Typ.t) =
  match typ.desc with
  | Tstruct name ->
      Typ.Name.Java.is_class name
  | Tarray {elt= inner_typ} | Tptr (inner_typ, _) ->
      is_java_class tenv inner_typ
  | _ ->
      false


let rec is_csharp_class tenv (typ : Typ.t) =
  match typ.desc with
  | Tstruct name ->
      Typ.Name.CSharp.is_class name
  | Tarray {elt= inner_typ} | Tptr (inner_typ, _) ->
      is_csharp_class tenv inner_typ
  | _ ->
      false


(** Negate an atom *)
let atom_negate tenv (atom : Predicates.atom) : Predicates.atom =
  match atom with
  | Aeq (BinOp (Le, e1, e2), Const (Cint i)) when IntLit.isone i ->
      Prop.mk_inequality tenv (Exp.lt e2 e1)
  | Aeq (BinOp (Lt, e1, e2), Const (Cint i)) when IntLit.isone i ->
      Prop.mk_inequality tenv (Exp.le e2 e1)
  | Aeq (e1, e2) ->
      Aneq (e1, e2)
  | Aneq (e1, e2) ->
      Aeq (e1, e2)
  | Apred (a, es) ->
      Anpred (a, es)
  | Anpred (a, es) ->
      Apred (a, es)


(** {2 Ordinary Theorem Proving} *)

let ( ++ ) = IntLit.add

let ( -- ) = IntLit.sub

(** Reasoning about constraints of the form x-y <= n *)

module DiffConstr : sig
  type t

  val to_leq : t -> Exp.t * Exp.t

  val to_lt : t -> Exp.t * Exp.t

  val to_triple : t -> Exp.t * Exp.t * IntLit.t

  val from_leq : t list -> Exp.t * Exp.t -> t list

  val from_lt : t list -> Exp.t * Exp.t -> t list

  val saturate : t list -> bool * t list
end = struct
  type t = Exp.t * Exp.t * IntLit.t [@@deriving compare, equal]

  let to_leq (e1, e2, n) = (Exp.BinOp (Binop.MinusA None, e1, e2), Exp.int n)

  let to_lt (e1, e2, n) =
    (Exp.int (IntLit.zero -- n -- IntLit.one), Exp.BinOp (Binop.MinusA None, e2, e1))


  let to_triple entry = entry

  let from_leq acc (e1, e2) =
    match (e1, e2) with
    | ( Exp.BinOp (Binop.MinusA _, (Exp.Var id11 as e11), (Exp.Var id12 as e12))
      , Exp.Const (Const.Cint n) )
      when not (Ident.equal id11 id12) -> (
      match IntLit.to_signed n with
      | None ->
          acc (* ignore: constraint algorithm only terminates on signed integers *)
      | Some n' ->
          (e11, e12, n') :: acc )
    | _ ->
        acc


  let from_lt acc (e1, e2) =
    match (e1, e2) with
    | ( Exp.Const (Const.Cint n)
      , Exp.BinOp (Binop.MinusA _, (Exp.Var id21 as e21), (Exp.Var id22 as e22)) )
      when not (Ident.equal id21 id22) -> (
      match IntLit.to_signed n with
      | None ->
          acc (* ignore: constraint algorithm only terminates on signed integers *)
      | Some n' ->
          let m = IntLit.zero -- n' -- IntLit.one in
          (e22, e21, m) :: acc )
    | _ ->
        acc


  let rec generate ((e1, e2, n) as constr) acc = function
    | [] ->
        (false, acc)
    | (f1, f2, m) :: rest ->
        let equal_e2_f1 = Exp.equal e2 f1 in
        let equal_e1_f2 = Exp.equal e1 f2 in
        if equal_e2_f1 && equal_e1_f2 && IntLit.lt (n ++ m) IntLit.zero then (true, [])
          (* constraints are inconsistent *)
        else if equal_e2_f1 && equal_e1_f2 then generate constr acc rest
        else if equal_e2_f1 then
          let constr_new = (e1, f2, n ++ m) in
          generate constr (constr_new :: acc) rest
        else if equal_e1_f2 then
          let constr_new = (f1, e2, m ++ n) in
          generate constr (constr_new :: acc) rest
        else generate constr acc rest


  let sort_then_remove_redundancy constraints =
    let constraints_sorted = List.sort ~compare constraints in
    let have_same_key (e1, e2, _) (f1, f2, _) = [%equal: Exp.t * Exp.t] (e1, e2) (f1, f2) in
    remove_redundancy have_same_key [] constraints_sorted


  let remove_redundancy constraints =
    let constraints' = sort_then_remove_redundancy constraints in
    List.filter ~f:(fun entry -> List.exists ~f:(equal entry) constraints') constraints


  let rec combine acc_todos acc_seen constraints_new constraints_old =
    match (constraints_new, constraints_old) with
    | [], [] ->
        (List.rev acc_todos, List.rev acc_seen)
    | [], _ ->
        (List.rev acc_todos, list_rev_acc constraints_old acc_seen)
    | _, [] ->
        (list_rev_acc constraints_new acc_todos, list_rev_acc constraints_new acc_seen)
    | constr :: rest, constr' :: rest' ->
        let e1, e2, n = constr in
        let f1, f2, m = constr' in
        let c1 = [%compare: Exp.t * Exp.t] (e1, e2) (f1, f2) in
        if Int.equal c1 0 && IntLit.lt n m then combine acc_todos acc_seen constraints_new rest'
        else if Int.equal c1 0 then combine acc_todos acc_seen rest constraints_old
        else if c1 < 0 then combine (constr :: acc_todos) (constr :: acc_seen) rest constraints_old
        else combine acc_todos (constr' :: acc_seen) constraints_new rest'


  let rec saturate_ seen todos =
    (* seen is a superset of todos. "seen" is sorted and doesn't have redundancy. *)
    match todos with
    | [] ->
        (false, seen)
    | constr :: rest ->
        let inconsistent, constraints_new = generate constr [] seen in
        if inconsistent then (true, [])
        else
          let constraints_new' = sort_then_remove_redundancy constraints_new in
          let todos_new, seen_new = combine [] [] constraints_new' seen in
          (* Important to use queue here. Otherwise, might diverge *)
          let rest_new = remove_redundancy (rest @ todos_new) in
          let seen_new' = sort_then_remove_redundancy seen_new in
          saturate_ seen_new' rest_new


  let saturate constraints =
    let constraints_cleaned = sort_then_remove_redundancy constraints in
    saturate_ constraints_cleaned constraints_cleaned
end

(** Return true if the two types have sizes which can be compared *)
let type_size_comparable t1 t2 =
  match (t1.Typ.desc, t2.Typ.desc) with Typ.Tint _, Typ.Tint _ -> true | _ -> false


(** Compare the size of comparable types *)
let type_size_compare t1 t2 =
  let ik_compare ik1 ik2 =
    let ik_size = function
      | Typ.IChar | Typ.ISChar | Typ.IUChar | Typ.IBool ->
          1
      | Typ.IShort | Typ.IUShort ->
          2
      | Typ.IInt | Typ.IUInt ->
          3
      | Typ.ILong | Typ.IULong ->
          4
      | Typ.ILongLong | Typ.IULongLong ->
          5
      | Typ.I128 | Typ.IU128 ->
          6
    in
    let n1 = ik_size ik1 in
    let n2 = ik_size ik2 in
    n1 - n2
  in
  match (t1.Typ.desc, t2.Typ.desc) with
  | Typ.Tint ik1, Typ.Tint ik2 ->
      Some (ik_compare ik1 ik2)
  | _ ->
      None


(** Check < on the size of comparable types *)
let check_type_size_lt t1 t2 = match type_size_compare t1 t2 with None -> false | Some n -> n < 0

(** Reasoning about inequalities *)
module Inequalities : sig
  (** type for inequalities (and implied disequalities) *)
  type t

  val from_prop : Tenv.t -> Prop.normal Prop.t -> t
  (** Extract inequalities and disequalities from [prop] *)

  val check_ne : t -> Exp.t -> Exp.t -> bool
  (** Check [t |- e1!=e2]. Result [false] means "don't know". *)

  val check_le : t -> Exp.t -> Exp.t -> bool
  (** Check [t |- e1<=e2]. Result [false] means "don't know". *)

  val check_lt : t -> Exp.t -> Exp.t -> bool
  (** Check [t |- e1<e2]. Result [false] means "don't know". *)

  val compute_upper_bound : t -> Exp.t -> IntLit.t option
  (** Find a IntLit.t n such that [t |- e<=n] if possible. *)

  val compute_lower_bound : t -> Exp.t -> IntLit.t option
  (** Find a IntLit.t n such that [t |- n<e] if possible. *)

  val inconsistent : t -> bool
  (** Return [true] if a simple inconsistency is detected *)
end = struct
  type t =
    { mutable leqs: (Exp.t * Exp.t) list  (** le fasts [e1 <= e2] *)
    ; mutable lts: (Exp.t * Exp.t) list  (** lt facts [e1 < e2] *)
    ; mutable neqs: (Exp.t * Exp.t) list  (** ne facts [e1 != e2] *) }

  let inconsistent_ineq = {leqs= [(Exp.one, Exp.zero)]; lts= []; neqs= []}

  let leq_compare (e1, e2) (f1, f2) =
    let c1 = Exp.compare e1 f1 in
    if c1 <> 0 then c1 else Exp.compare e2 f2


  let lt_compare (e1, e2) (f1, f2) =
    let c2 = Exp.compare e2 f2 in
    if c2 <> 0 then c2 else -Exp.compare e1 f1


  let leqs_sort_then_remove_redundancy leqs =
    let leqs_sorted = List.sort ~compare:leq_compare leqs in
    let have_same_key leq1 leq2 =
      match (leq1, leq2) with
      | (e1, Exp.Const (Const.Cint n1)), (e2, Exp.Const (Const.Cint n2)) ->
          Exp.equal e1 e2 && IntLit.leq n1 n2
      | _, _ ->
          false
    in
    remove_redundancy have_same_key [] leqs_sorted


  let lts_sort_then_remove_redundancy lts =
    let lts_sorted = List.sort ~compare:lt_compare lts in
    let have_same_key lt1 lt2 =
      match (lt1, lt2) with
      | (Exp.Const (Const.Cint n1), e1), (Exp.Const (Const.Cint n2), e2) ->
          Exp.equal e1 e2 && IntLit.geq n1 n2
      | _, _ ->
          false
    in
    remove_redundancy have_same_key [] lts_sorted


  let saturate {leqs; lts; neqs} =
    let diff_constraints1 =
      List.fold ~f:DiffConstr.from_lt ~init:(List.fold ~f:DiffConstr.from_leq ~init:[] leqs) lts
    in
    let inconsistent, diff_constraints2 = DiffConstr.saturate diff_constraints1 in
    if inconsistent then inconsistent_ineq
    else
      let umap_add umap e new_upper =
        try
          let old_upper = Exp.Map.find e umap in
          if IntLit.leq old_upper new_upper then umap else Exp.Map.add e new_upper umap
        with Caml.Not_found -> Exp.Map.add e new_upper umap
      in
      let lmap_add lmap e new_lower =
        try
          let old_lower = Exp.Map.find e lmap in
          if IntLit.geq old_lower new_lower then lmap else Exp.Map.add e new_lower lmap
        with Caml.Not_found -> Exp.Map.add e new_lower lmap
      in
      let rec umap_create_from_leqs umap = function
        | [] ->
            umap
        | (e1, Exp.Const (Const.Cint upper1)) :: leqs_rest ->
            let umap' = umap_add umap e1 upper1 in
            umap_create_from_leqs umap' leqs_rest
        | _ :: leqs_rest ->
            umap_create_from_leqs umap leqs_rest
      in
      let rec lmap_create_from_lts lmap = function
        | [] ->
            lmap
        | (Exp.Const (Const.Cint lower1), e1) :: lts_rest ->
            let lmap' = lmap_add lmap e1 lower1 in
            lmap_create_from_lts lmap' lts_rest
        | _ :: lts_rest ->
            lmap_create_from_lts lmap lts_rest
      in
      let rec umap_improve_by_difference_constraints umap = function
        | [] ->
            umap
        | constr :: constrs_rest -> (
          try
            let e1, e2, n = DiffConstr.to_triple constr (* e1 - e2 <= n *) in
            let upper2 = Exp.Map.find e2 umap in
            let new_upper1 = upper2 ++ n in
            let new_umap = umap_add umap e1 new_upper1 in
            umap_improve_by_difference_constraints new_umap constrs_rest
          with Caml.Not_found -> umap_improve_by_difference_constraints umap constrs_rest )
      in
      let rec lmap_improve_by_difference_constraints lmap = function
        | [] ->
            lmap
        | constr :: constrs_rest -> (
          try
            (* e2 - e1 > -n-1 *)
            let e1, e2, n = DiffConstr.to_triple constr (* e2 - e1 > -n-1 *) in
            let lower1 = Exp.Map.find e1 lmap in
            let new_lower2 = lower1 -- n -- IntLit.one in
            let new_lmap = lmap_add lmap e2 new_lower2 in
            lmap_improve_by_difference_constraints new_lmap constrs_rest
          with Caml.Not_found -> lmap_improve_by_difference_constraints lmap constrs_rest )
      in
      let leqs_res =
        let umap = umap_create_from_leqs Exp.Map.empty leqs in
        let umap' = umap_improve_by_difference_constraints umap diff_constraints2 in
        let leqs' =
          Exp.Map.fold (fun e upper acc_leqs -> (e, Exp.int upper) :: acc_leqs) umap' []
        in
        let leqs'' = List.map ~f:DiffConstr.to_leq diff_constraints2 @ leqs' in
        leqs_sort_then_remove_redundancy leqs''
      in
      let lts_res =
        let lmap = lmap_create_from_lts Exp.Map.empty lts in
        let lmap' = lmap_improve_by_difference_constraints lmap diff_constraints2 in
        let lts' = Exp.Map.fold (fun e lower acc_lts -> (Exp.int lower, e) :: acc_lts) lmap' [] in
        let lts'' = List.map ~f:DiffConstr.to_lt diff_constraints2 @ lts' in
        lts_sort_then_remove_redundancy lts''
      in
      {leqs= leqs_res; lts= lts_res; neqs}


  (** Extract inequalities and disequalities from [pi] *)
  let from_pi pi =
    let leqs = ref [] in
    (* <= facts *)
    let lts = ref [] in
    (* < facts *)
    let neqs = ref [] in
    (* != facts *)
    let process_atom (atom : Predicates.atom) =
      match atom with
      | Aneq (e1, e2) ->
          (* != *)
          neqs := (e1, e2) :: !neqs
      | Aeq (BinOp (Le, e1, e2), Const (Cint i)) when IntLit.isone i ->
          leqs := (e1, e2) :: !leqs (* <= *)
      | Aeq (BinOp (Lt, e1, e2), Const (Cint i)) when IntLit.isone i ->
          lts := (e1, e2) :: !lts (* < *)
      | Aeq _ | Apred _ | Anpred _ ->
          ()
    in
    List.iter ~f:process_atom pi ;
    saturate {leqs= !leqs; lts= !lts; neqs= !neqs}


  (** Extract inequalities and disequalities from [sigma] *)
  let from_sigma tenv sigma =
    let lookup = Tenv.lookup tenv in
    let leqs = ref [] in
    let lts = ref [] in
    let add_lt_minus1_e e = lts := (Exp.minus_one, e) :: !lts in
    let type_opt_is_unsigned = function
      | Some {Typ.desc= Tint ik} ->
          Typ.ikind_is_unsigned ik
      | _ ->
          false
    in
    let type_of_texp = function Exp.Sizeof {typ} -> Some typ | _ -> None in
    let texp_is_unsigned texp = type_opt_is_unsigned @@ type_of_texp texp in
    let strexp_lt_minus1 = function Predicates.Eexp (e, _) -> add_lt_minus1_e e | _ -> () in
    let rec strexp_extract = function
      | Predicates.Eexp (e, _), t ->
          if type_opt_is_unsigned t then add_lt_minus1_e e
      | Predicates.Estruct (fsel, _), t ->
          let get_field_type f =
            Option.bind t ~f:(fun t' ->
                Option.map ~f:fst @@ Struct.get_field_type_and_annotation ~lookup f t' )
          in
          List.iter ~f:(fun (f, se) -> strexp_extract (se, get_field_type f)) fsel
      | Predicates.Earray (len, isel, _), t ->
          let elt_t = match t with Some {Typ.desc= Tarray {elt}} -> Some elt | _ -> None in
          add_lt_minus1_e len ;
          List.iter
            ~f:(fun (idx, se) ->
              add_lt_minus1_e idx ;
              strexp_extract (se, elt_t) )
            isel
    in
    let hpred_extract = function
      | Predicates.Hpointsto (_, se, texp) ->
          if texp_is_unsigned texp then strexp_lt_minus1 se ;
          strexp_extract (se, type_of_texp texp)
      | Predicates.Hlseg _ | Predicates.Hdllseg _ ->
          ()
    in
    List.iter ~f:hpred_extract sigma ;
    saturate {leqs= !leqs; lts= !lts; neqs= []}


  (** Join two sets of inequalities *)
  let join ineq1 ineq2 =
    let leqs_new = ineq1.leqs @ ineq2.leqs in
    let lts_new = ineq1.lts @ ineq2.lts in
    let neqs_new = ineq1.neqs @ ineq2.neqs in
    saturate {leqs= leqs_new; lts= lts_new; neqs= neqs_new}


  let from_prop tenv prop =
    let sigma = prop.Prop.sigma in
    let pi = prop.Prop.pi in
    let ineq_sigma = from_sigma tenv sigma in
    let ineq_pi = from_pi pi in
    saturate (join ineq_sigma ineq_pi)


  (** Return true if the two pairs of expressions are equal *)
  let exp_pair_eq (e1, e2) (f1, f2) = Exp.equal e1 f1 && Exp.equal e2 f2

  (** Check [t |- e1<=e2]. Result [false] means "don't know". *)
  let check_le {leqs; lts; neqs= _} e1 e2 =
    (* L.d_str "check_le "; Predicates.d_exp e1; L.d_str " "; Predicates.d_exp e2; L.d_ln (); *)
    match (e1, e2) with
    | Exp.Const (Const.Cint n1), Exp.Const (Const.Cint n2) ->
        IntLit.leq n1 n2
    | ( Exp.BinOp (Binop.MinusA _, Exp.Sizeof {nbytes= Some nb1}, Exp.Sizeof {nbytes= Some nb2})
      , Exp.Const (Const.Cint n2) ) ->
        (* [ sizeof(t1) - sizeof(t2) <= n2 ] *)
        IntLit.(leq (sub (of_int nb1) (of_int nb2)) n2)
    | ( Exp.BinOp (Binop.MinusA _, Exp.Sizeof {typ= t1}, Exp.Sizeof {typ= t2})
      , Exp.Const (Const.Cint n2) )
      when IntLit.isminusone n2 && type_size_comparable t1 t2 ->
        (* [ sizeof(t1) - sizeof(t2) <= -1 ] *)
        check_type_size_lt t1 t2
    | e, Exp.Const (Const.Cint n) ->
        (* [e <= n' <= n |- e <= n] *)
        List.exists
          ~f:(function
            | e', Exp.Const (Const.Cint n') -> Exp.equal e e' && IntLit.leq n' n | _, _ -> false )
          leqs
    | Exp.Const (Const.Cint n), e ->
        (* [ n-1 <= n' < e |- n <= e] *)
        List.exists
          ~f:(function
            | Exp.Const (Const.Cint n'), e' ->
                Exp.equal e e' && IntLit.leq (n -- IntLit.one) n'
            | _, _ ->
                false )
          lts
    | _ ->
        Exp.equal e1 e2


  (** Check [prop |- e1<e2]. Result [false] means "don't know". *)
  let check_lt {leqs; lts; neqs= _} e1 e2 =
    (* L.d_str "check_lt "; Predicates.d_exp e1; L.d_str " "; Predicates.d_exp e2; L.d_ln (); *)
    match (e1, e2) with
    | Exp.Const (Const.Cint n1), Exp.Const (Const.Cint n2) ->
        IntLit.lt n1 n2
    | Exp.Const (Const.Cint n), e ->
        (* [n <= n' < e  |- n < e] *)
        List.exists
          ~f:(function
            | Exp.Const (Const.Cint n'), e' -> Exp.equal e e' && IntLit.leq n n' | _, _ -> false )
          lts
    | e, Exp.Const (Const.Cint n) ->
        (* [e <= n' <= n-1 |- e < n] *)
        List.exists
          ~f:(function
            | e', Exp.Const (Const.Cint n') ->
                Exp.equal e e' && IntLit.leq n' (n -- IntLit.one)
            | _, _ ->
                false )
          leqs
    | _ ->
        false


  (** Check [prop |- e1!=e2]. Result [false] means "don't know". *)
  let check_ne ineq e1_ e2_ =
    let e1, e2 = if Exp.compare e1_ e2_ <= 0 then (e1_, e2_) else (e2_, e1_) in
    List.exists ~f:(exp_pair_eq (e1, e2)) ineq.neqs || check_lt ineq e1 e2 || check_lt ineq e2 e1


  (** Find a IntLit.t n such that [t |- e<=n] if possible. *)
  let compute_upper_bound {leqs; lts= _; neqs= _} e1 =
    match e1 with
    | Exp.Const (Const.Cint n1) ->
        Some n1
    | _ ->
        let e_upper_list =
          List.filter
            ~f:(function e', Exp.Const (Const.Cint _) -> Exp.equal e1 e' | _, _ -> false)
            leqs
        in
        let upper_list =
          List.map ~f:(function _, Exp.Const (Const.Cint n) -> n | _ -> assert false) e_upper_list
        in
        if List.is_empty upper_list then None
        else Some (compute_min_from_nonempty_int_list upper_list)


  (** Find a IntLit.t n such that [t |- n < e] if possible. *)
  let compute_lower_bound {leqs= _; lts; neqs= _} e1 =
    match e1 with
    | Exp.Const (Const.Cint n1) ->
        Some (n1 -- IntLit.one)
    | Exp.Sizeof {nbytes= Some n1} ->
        Some (IntLit.of_int n1 -- IntLit.one)
    | Exp.Sizeof _ ->
        Some IntLit.zero
    | _ ->
        let e_lower_list =
          List.filter
            ~f:(function Exp.Const (Const.Cint _), e' -> Exp.equal e1 e' | _, _ -> false)
            lts
        in
        let lower_list =
          List.map ~f:(function Exp.Const (Const.Cint n), _ -> n | _ -> assert false) e_lower_list
        in
        if List.is_empty lower_list then None
        else Some (compute_max_from_nonempty_int_list lower_list)


  (** Return [true] if a simple inconsistency is detected *)
  let inconsistent ({leqs; lts; neqs} as ineq) =
    let inconsistent_neq (e1, e2) = check_le ineq e1 e2 && check_le ineq e2 e1 in
    let inconsistent_leq (e1, e2) = check_lt ineq e2 e1 in
    let inconsistent_lt (e1, e2) = check_le ineq e2 e1 in
    List.exists ~f:inconsistent_neq neqs
    || List.exists ~f:inconsistent_leq leqs
    || List.exists ~f:inconsistent_lt lts
end

(* End of module Inequalities *)

(** Check [prop |- e1=e2]. Result [false] means "don't know". *)
let check_equal tenv prop e1_0 e2_0 =
  let n_e1 = Prop.exp_normalize_prop ~destructive:true tenv prop e1_0 in
  let n_e2 = Prop.exp_normalize_prop ~destructive:true tenv prop e2_0 in
  let check_equal () = Exp.equal n_e1 n_e2 in
  let check_equal_const () =
    match (n_e1, n_e2) with
    | Exp.BinOp (Binop.PlusA _, e1, Exp.Const (Const.Cint d)), e2
    | e2, Exp.BinOp (Binop.PlusA _, e1, Exp.Const (Const.Cint d)) ->
        if Exp.equal e1 e2 then IntLit.iszero d else false
    | Exp.Const c1, Exp.Lindex (Exp.Const c2, Exp.Const (Const.Cint i)) when IntLit.iszero i ->
        Const.equal c1 c2
    | Exp.Lindex (Exp.Const c1, Exp.Const (Const.Cint i)), Exp.Const c2 when IntLit.iszero i ->
        Const.equal c1 c2
    | _, _ ->
        false
  in
  let check_equal_pi () =
    let eq = Predicates.Aeq (n_e1, n_e2) in
    let n_eq = Prop.atom_normalize_prop tenv prop eq in
    let pi = prop.Prop.pi in
    List.exists ~f:(Predicates.equal_atom n_eq) pi
  in
  check_equal () || check_equal_const () || check_equal_pi ()


(** Check [|- e=0]. Result [false] means "don't know". *)
let check_zero tenv e = check_equal tenv Prop.prop_emp e Exp.zero

(** [is_root prop base_exp exp] checks whether [base_exp = exp.offlist] for some list of offsets
    [offlist]. If so, it returns [Some(offlist)]. Otherwise, it returns [None]. Assumes that
    [base_exp] points to the beginning of a structure, not the middle. *)
let is_root tenv prop base_exp exp =
  let rec f offlist_past e =
    match e with
    | Exp.Var _
    | Exp.Const _
    | Exp.UnOp _
    | Exp.BinOp _
    | Exp.Exn _
    | Exp.Closure _
    | Exp.Lvar _
    | Exp.Sizeof _ ->
        if check_equal tenv prop base_exp e then Some offlist_past else None
    | Exp.Cast (_, sub_exp) ->
        f offlist_past sub_exp
    | Exp.Lfield (sub_exp, fldname, typ) ->
        f (Predicates.Off_fld (fldname, typ) :: offlist_past) sub_exp
    | Exp.Lindex (sub_exp, e) ->
        f (Predicates.Off_index e :: offlist_past) sub_exp
  in
  f [] exp


(** Get upper and lower bounds of an expression, if any *)
let get_bounds tenv prop e0 =
  let e_norm = Prop.exp_normalize_prop ~destructive:true tenv prop e0 in
  let e_root, off =
    match e_norm with
    | Exp.BinOp (Binop.PlusA _, e, Exp.Const (Const.Cint n1)) ->
        (e, IntLit.neg n1)
    | Exp.BinOp (Binop.MinusA _, e, Exp.Const (Const.Cint n1)) ->
        (e, n1)
    | _ ->
        (e_norm, IntLit.zero)
  in
  let ineq = Inequalities.from_prop tenv prop in
  let upper_opt = Inequalities.compute_upper_bound ineq e_root in
  let lower_opt = Inequalities.compute_lower_bound ineq e_root in
  let ( +++ ) n_opt k = match n_opt with None -> None | Some n -> Some (n ++ k) in
  (upper_opt +++ off, lower_opt +++ off)


(** Check whether [prop |- e1!=e2]. *)
let check_disequal tenv prop e1 e2 =
  let spatial_part = prop.Prop.sigma in
  let n_e1 = Prop.exp_normalize_prop ~destructive:true tenv prop e1 in
  let n_e2 = Prop.exp_normalize_prop ~destructive:true tenv prop e2 in
  let rec check_expr_disequal ce1 ce2 =
    match (ce1, ce2) with
    | Exp.Const c1, Exp.Const c2 ->
        Const.kind_equal c1 c2 && not (Const.equal c1 c2)
    | Exp.Const c1, Exp.Lindex (Exp.Const c2, Exp.Const (Const.Cint d)) ->
        if IntLit.iszero d then not (Const.equal c1 c2) (* offset=0 is no offset *)
        else Const.equal c1 c2 (* same base, different offsets *)
    | ( Exp.BinOp (Binop.PlusA _, e1, Exp.Const (Const.Cint d1))
      , Exp.BinOp (Binop.PlusA _, e2, Exp.Const (Const.Cint d2)) ) ->
        if Exp.equal e1 e2 then IntLit.neq d1 d2 else false
    | Exp.BinOp (Binop.PlusA _, e1, Exp.Const (Const.Cint d)), e2
    | e2, Exp.BinOp (Binop.PlusA _, e1, Exp.Const (Const.Cint d)) ->
        if Exp.equal e1 e2 then not (IntLit.iszero d) else false
    | Exp.Lindex (Exp.Const c1, Exp.Const (Const.Cint d)), Exp.Const c2 ->
        if IntLit.iszero d then not (Const.equal c1 c2) else Const.equal c1 c2
    | Exp.Lindex (Exp.Const c1, Exp.Const d1), Exp.Lindex (Exp.Const c2, Exp.Const d2) ->
        Const.equal c1 c2 && not (Const.equal d1 d2)
    | Exp.Const (Const.Cint n), Exp.BinOp (Binop.Mult _, Exp.Sizeof _, e21)
    | Exp.Const (Const.Cint n), Exp.BinOp (Binop.Mult _, e21, Sizeof _)
    | Exp.BinOp (Binop.Mult _, Exp.Sizeof _, e21), Exp.Const (Const.Cint n)
    | Exp.BinOp (Binop.Mult _, e21, Exp.Sizeof _), Exp.Const (Const.Cint n) ->
        IntLit.iszero n && not (Exp.is_zero e21)
    | Exp.Lvar pv0, Exp.Lvar pv1 ->
        (* Addresses of any two local vars must be different *)
        not (Pvar.equal pv0 pv1)
    | Exp.Lvar pv, Exp.Var id | Exp.Var id, Exp.Lvar pv ->
        (* Address of any non-global var must be different from the value of any footprint var *)
        (not (Pvar.is_global pv)) && Ident.is_footprint id
    | Exp.Lvar _, Exp.Const (Const.Cint _) | Exp.Const (Const.Cint _), Exp.Lvar _ ->
        (* Comparing pointer with nonzero integer is undefined behavior in ISO C++ *)
        (* Assume they are not equal *)
        true
    | Exp.UnOp (op1, e1, _), Exp.UnOp (op2, e2, _) ->
        if Unop.equal op1 op2 then check_expr_disequal e1 e2 else false
    | Exp.Lfield (e1, f1, _), Exp.Lfield (e2, f2, _) ->
        if Fieldname.equal f1 f2 then check_expr_disequal e1 e2 else false
    | Exp.Exn e1, Exp.Exn e2 ->
        check_expr_disequal e1 e2
    | _, _ ->
        false
  in
  let ineq = lazy (Inequalities.from_prop tenv prop) in
  let check_pi_implies_disequal e1 e2 = Inequalities.check_ne (Lazy.force ineq) e1 e2 in
  let neq_spatial_part () =
    let rec f sigma_irrelevant e = function
      | [] ->
          None
      | (Predicates.Hpointsto (base, _, _) as hpred) :: sigma_rest -> (
        match is_root tenv prop base e with
        | None ->
            let sigma_irrelevant' = hpred :: sigma_irrelevant in
            f sigma_irrelevant' e sigma_rest
        | Some _ ->
            let sigma_irrelevant' = List.rev_append sigma_irrelevant sigma_rest in
            Some (true, sigma_irrelevant') )
      | (Predicates.Hlseg (k, _, e1, e2, _) as hpred) :: sigma_rest -> (
        match is_root tenv prop e1 e with
        | None ->
            let sigma_irrelevant' = hpred :: sigma_irrelevant in
            f sigma_irrelevant' e sigma_rest
        | Some _ ->
            if Predicates.equal_lseg_kind k Lseg_NE || check_pi_implies_disequal e1 e2 then
              let sigma_irrelevant' = List.rev_append sigma_irrelevant sigma_rest in
              Some (true, sigma_irrelevant')
            else if Exp.equal e2 Exp.zero then
              let sigma_irrelevant' = List.rev_append sigma_irrelevant sigma_rest in
              Some (false, sigma_irrelevant')
            else
              let sigma_rest' = List.rev_append sigma_irrelevant sigma_rest in
              f [] e2 sigma_rest' )
      | Predicates.Hdllseg (Lseg_NE, _, iF, _, _, iB, _) :: sigma_rest ->
          if Option.is_some (is_root tenv prop iF e) || Option.is_some (is_root tenv prop iB e) then
            let sigma_irrelevant' = List.rev_append sigma_irrelevant sigma_rest in
            Some (true, sigma_irrelevant')
          else
            let sigma_irrelevant' = List.rev_append sigma_irrelevant sigma_rest in
            Some (false, sigma_irrelevant')
      | (Predicates.Hdllseg (Lseg_PE, _, iF, _, oF, _, _) as hpred) :: sigma_rest -> (
        match is_root tenv prop iF e with
        | None ->
            let sigma_irrelevant' = hpred :: sigma_irrelevant in
            f sigma_irrelevant' e sigma_rest
        | Some _ ->
            if check_pi_implies_disequal iF oF then
              let sigma_irrelevant' = List.rev_append sigma_irrelevant sigma_rest in
              Some (true, sigma_irrelevant')
            else if Exp.equal oF Exp.zero then
              let sigma_irrelevant' = List.rev_append sigma_irrelevant sigma_rest in
              Some (false, sigma_irrelevant')
            else
              let sigma_rest' = List.rev_append sigma_irrelevant sigma_rest in
              f [] oF sigma_rest' )
    in
    let f_null_check sigma_irrelevant e sigma_rest =
      if not (Exp.equal e Exp.zero) then f sigma_irrelevant e sigma_rest
      else
        let sigma_irrelevant' = List.rev_append sigma_irrelevant sigma_rest in
        Some (false, sigma_irrelevant')
    in
    match f_null_check [] n_e1 spatial_part with
    | None ->
        false
    | Some (e1_allocated, spatial_part_leftover) -> (
      match f_null_check [] n_e2 spatial_part_leftover with
      | None ->
          false
      | Some ((e2_allocated : bool), _) ->
          e1_allocated || e2_allocated )
  in
  let check_disequal_expr () = check_expr_disequal n_e1 n_e2 in
  let neq_pure_part () = check_pi_implies_disequal n_e1 n_e2 in
  check_disequal_expr () || neq_pure_part () || neq_spatial_part ()


(** Check [prop |- e1<=e2], to be called from normalized atom *)
let check_le_normalized tenv prop e1 e2 =
  let eL, eR, off =
    match (e1, e2) with
    | Exp.BinOp (Binop.MinusA _, f1, f2), Exp.Const (Const.Cint n) ->
        if Exp.equal f1 f2 then (Exp.zero, Exp.zero, n) else (f1, f2, n)
    | _ ->
        (e1, e2, IntLit.zero)
  in
  let ineq = Inequalities.from_prop tenv prop in
  let upper_lower_check () =
    let upperL_opt = Inequalities.compute_upper_bound ineq eL in
    let lowerR_opt = Inequalities.compute_lower_bound ineq eR in
    match (upperL_opt, lowerR_opt) with
    | None, _ | _, None ->
        false
    | Some upper1, Some lower2 ->
        IntLit.leq upper1 (lower2 ++ IntLit.one ++ off)
  in
  upper_lower_check () || Inequalities.check_le ineq e1 e2 || check_equal tenv prop e1 e2


(** Check [prop |- e1<e2], to be called from normalized atom *)
let check_lt_normalized tenv prop e1 e2 =
  let ineq = Inequalities.from_prop tenv prop in
  let upper_lower_check () =
    let upper1_opt = Inequalities.compute_upper_bound ineq e1 in
    let lower2_opt = Inequalities.compute_lower_bound ineq e2 in
    match (upper1_opt, lower2_opt) with
    | None, _ | _, None ->
        false
    | Some upper1, Some lower2 ->
        IntLit.leq upper1 lower2
  in
  upper_lower_check () || Inequalities.check_lt ineq e1 e2


(** Given an atom and a proposition returns a unique identifier. We use this to distinguish among
    different queries. *)
let get_smt_key a p =
  let tmp_filename = Filename.temp_file "smt_query" ".cns" in
  let outc_tmp = Out_channel.create tmp_filename in
  let fmt_tmp = F.formatter_of_out_channel outc_tmp in
  let () = F.fprintf fmt_tmp "%a%a" (Predicates.pp_atom Pp.text) a (Prop.pp_prop Pp.text) p in
  Out_channel.close outc_tmp ;
  Caml.Digest.to_hex (Caml.Digest.file tmp_filename)


(** Check whether [prop |- a]. False means dont know. *)
let check_atom tenv prop a0 =
  let a = Prop.atom_normalize_prop tenv prop a0 in
  let prop_no_fp = Prop.set prop ~pi_fp:[] ~sigma_fp:[] in
  if Config.smt_output then (
    let key = get_smt_key a prop_no_fp in
    let key_filename =
      let source = (AnalysisState.get_loc_exn ()).file in
      DB.Results_dir.path_to_filename (DB.Results_dir.Abs_source_dir source) [key ^ ".cns"]
    in
    let outc = Out_channel.create (DB.filename_to_string key_filename) in
    let fmt = F.formatter_of_out_channel outc in
    L.d_printfln "ID: %s" key ;
    L.d_str "CHECK_ATOM_BOUND: " ;
    Predicates.d_atom a ;
    L.d_ln () ;
    L.d_strln "WHERE:" ;
    Prop.d_prop prop_no_fp ;
    L.d_ln () ;
    L.d_ln () ;
    F.fprintf fmt "ID: %s @\nCHECK_ATOM_BOUND: %a@\nWHERE:@\n%a" key (Predicates.pp_atom Pp.text) a
      (Prop.pp_prop Pp.text) prop_no_fp ;
    Out_channel.close outc ) ;
  match (a : Predicates.atom) with
  | Aeq (BinOp (Le, e1, e2), Const (Cint i)) when IntLit.isone i ->
      check_le_normalized tenv prop e1 e2
  | Aeq (BinOp (Lt, e1, e2), Const (Cint i)) when IntLit.isone i ->
      check_lt_normalized tenv prop e1 e2
  | Aeq (e1, e2) ->
      check_equal tenv prop e1 e2
  | Aneq (e1, e2) ->
      check_disequal tenv prop e1 e2
  | Apred _ | Anpred _ ->
      List.exists ~f:(Predicates.equal_atom a) prop.Prop.pi


(** Check whether [prop |- allocated(e)]. *)
let check_allocatedness tenv prop e =
  let n_e = Prop.exp_normalize_prop ~destructive:true tenv prop e in
  let spatial_part = prop.Prop.sigma in
  let f = function
    | Predicates.Hpointsto (base, _, _) ->
        Option.is_some (is_root tenv prop base n_e)
    | Predicates.Hlseg (k, _, e1, e2, _) ->
        if Predicates.equal_lseg_kind k Lseg_NE || check_disequal tenv prop e1 e2 then
          Option.is_some (is_root tenv prop e1 n_e)
        else false
    | Predicates.Hdllseg (k, _, iF, oB, oF, iB, _) ->
        if
          Predicates.equal_lseg_kind k Lseg_NE
          || check_disequal tenv prop iF oF || check_disequal tenv prop iB oB
        then Option.is_some (is_root tenv prop iF n_e) || Option.is_some (is_root tenv prop iB n_e)
        else false
  in
  List.exists ~f spatial_part


(** Check if two hpreds have the same allocated lhs *)
let check_inconsistency_two_hpreds tenv prop =
  let sigma = prop.Prop.sigma in
  let rec f e sigma_seen = function
    | [] ->
        false
    | (Predicates.Hpointsto (e1, _, _) as hpred) :: sigma_rest
    | (Predicates.Hlseg (Lseg_NE, _, e1, _, _) as hpred) :: sigma_rest ->
        if Exp.equal e1 e then true else f e (hpred :: sigma_seen) sigma_rest
    | (Predicates.Hdllseg (Lseg_NE, _, iF, _, _, iB, _) as hpred) :: sigma_rest ->
        if Exp.equal iF e || Exp.equal iB e then true else f e (hpred :: sigma_seen) sigma_rest
    | (Predicates.Hlseg (Lseg_PE, _, e1, Exp.Const (Const.Cint i), _) as hpred) :: sigma_rest
      when IntLit.iszero i ->
        if Exp.equal e1 e then true else f e (hpred :: sigma_seen) sigma_rest
    | (Predicates.Hlseg (Lseg_PE, _, e1, e2, _) as hpred) :: sigma_rest ->
        if Exp.equal e1 e then
          let prop' = Prop.normalize tenv (Prop.from_sigma (sigma_seen @ sigma_rest)) in
          let prop_new = Prop.conjoin_eq tenv e1 e2 prop' in
          let sigma_new = prop_new.Prop.sigma in
          let e_new = Prop.exp_normalize_prop ~destructive:true tenv prop_new e in
          f e_new [] sigma_new
        else f e (hpred :: sigma_seen) sigma_rest
    | (Predicates.Hdllseg (Lseg_PE, _, e1, _, Exp.Const (Const.Cint i), _, _) as hpred)
      :: sigma_rest
      when IntLit.iszero i ->
        if Exp.equal e1 e then true else f e (hpred :: sigma_seen) sigma_rest
    | (Predicates.Hdllseg (Lseg_PE, _, e1, _, e3, _, _) as hpred) :: sigma_rest ->
        if Exp.equal e1 e then
          let prop' = Prop.normalize tenv (Prop.from_sigma (sigma_seen @ sigma_rest)) in
          let prop_new = Prop.conjoin_eq tenv e1 e3 prop' in
          let sigma_new = prop_new.Prop.sigma in
          let e_new = Prop.exp_normalize_prop ~destructive:true tenv prop_new e in
          f e_new [] sigma_new
        else f e (hpred :: sigma_seen) sigma_rest
  in
  let rec check sigma_seen = function
    | [] ->
        false
    | (Predicates.Hpointsto (e1, _, _) as hpred) :: sigma_rest
    | (Predicates.Hlseg (Lseg_NE, _, e1, _, _) as hpred) :: sigma_rest ->
        if f e1 [] (sigma_seen @ sigma_rest) then true else check (hpred :: sigma_seen) sigma_rest
    | (Predicates.Hdllseg (Lseg_NE, _, iF, _, _, iB, _) as hpred) :: sigma_rest ->
        if f iF [] (sigma_seen @ sigma_rest) || f iB [] (sigma_seen @ sigma_rest) then true
        else check (hpred :: sigma_seen) sigma_rest
    | (Predicates.Hlseg (Lseg_PE, _, _, _, _) as hpred) :: sigma_rest
    | (Predicates.Hdllseg (Lseg_PE, _, _, _, _, _, _) as hpred) :: sigma_rest ->
        check (hpred :: sigma_seen) sigma_rest
  in
  check [] sigma


(** Inconsistency checking ignoring footprint. *)
let check_inconsistency_base tenv prop =
  let pi = prop.Prop.pi in
  let sigma = prop.Prop.sigma in
  let inconsistent_ptsto _ = check_allocatedness tenv prop Exp.zero in
  let inconsistent_this_self_var () =
    match State.get_prop_tenv_pdesc () with
    | None ->
        false
    | Some (_, _, pdesc) ->
        let procedure_attr = Procdesc.get_attributes pdesc in
        let language = Procname.get_language (Procdesc.get_proc_name pdesc) in
        let is_java_this pvar = Language.equal language Java && Pvar.is_this pvar in
        let is_objc_instance_self pvar =
          Language.equal language Clang && Pvar.is_self pvar
          && ClangMethodKind.equal procedure_attr.ProcAttributes.clang_method_kind
               ClangMethodKind.OBJC_INSTANCE
        in
        let is_cpp_this pvar =
          Language.equal language Clang && Pvar.is_this pvar
          && ClangMethodKind.equal procedure_attr.ProcAttributes.clang_method_kind
               ClangMethodKind.CPP_INSTANCE
        in
        let do_hpred = function
          | Predicates.Hpointsto (Lvar pv, Eexp (e, _), _) ->
              Exp.equal e Exp.zero && Pvar.is_seed pv
              && (is_java_this pv || is_cpp_this pv || is_objc_instance_self pv)
          | _ ->
              false
        in
        List.exists ~f:do_hpred sigma
  in
  let inconsistent_atom (atom : Predicates.atom) =
    match atom with
    | Aeq (e1, e2) -> (
      match (e1, e2) with
      | Exp.Const c1, Exp.Const c2 ->
          not (Const.equal c1 c2)
      | _ ->
          check_disequal tenv prop e1 e2 )
    | Aneq (e1, e2) -> (
      match (e1, e2) with Exp.Const c1, Exp.Const c2 -> Const.equal c1 c2 | _ -> Exp.equal e1 e2 )
    | Apred _ | Anpred _ ->
        false
  in
  let inconsistent_inequalities () =
    let ineq = Inequalities.from_prop tenv prop in
    (*
    L.d_strln "Inequalities:";
    L.d_strln "Prop: "; Prop.d_prop prop; L.d_ln ();
    L.d_str "leqs: "; Inequalities.d_leqs ineq; L.d_ln ();
    L.d_str "lts: "; Inequalities.d_lts ineq; L.d_ln ();
    L.d_str "neqs: "; Inequalities.d_neqs ineq; L.d_ln ();
    *)
    Inequalities.inconsistent ineq
  in
  let tests =
    [ lazy (inconsistent_ptsto ())
    ; lazy (check_inconsistency_two_hpreds tenv prop)
    ; lazy (List.exists ~f:inconsistent_atom pi)
    ; lazy (inconsistent_inequalities ())
    ; lazy (inconsistent_this_self_var ()) ]
  in
  let f index = function
    | None ->
        fun test -> Option.some_if (Lazy.force test) index
    | s ->
        fun _ -> s
  in
  List.foldi ~init:None ~f tests


(** Shadows the above, adding some debug output. *)
let check_inconsistency_base tenv prop =
  let reason = check_inconsistency_base tenv prop in
  L.d_printfln "Prover.check_inconsistency_base: inconsistency reason %a" (Pp.option Int.pp) reason ;
  Option.is_some reason


(** Inconsistency checking. *)
let check_inconsistency tenv prop =
  check_inconsistency_base tenv prop
  || check_inconsistency_base tenv (Prop.normalize tenv (Prop.extract_footprint prop))


(** Inconsistency checking for the pi part ignoring footprint. *)
let check_inconsistency_pi tenv pi =
  check_inconsistency_base tenv (Prop.normalize tenv (Prop.from_pi pi))


(** {2 Abduction prover} *)

type subst2 = Predicates.subst * Predicates.subst

type exc_body =
  | EXC_FALSE
  | EXC_FALSE_HPRED of Predicates.hpred
  | EXC_FALSE_EXPS of Exp.t * Exp.t
  | EXC_FALSE_SEXPS of Predicates.strexp * Predicates.strexp
  | EXC_FALSE_ATOM of Predicates.atom
  | EXC_FALSE_SIGMA of Predicates.hpred list

exception IMPL_EXC of string * subst2 * exc_body

exception MISSING_EXC of string

type check = Bounds_check | Class_cast_check of Exp.t * Exp.t * Exp.t

let d_typings typings =
  let d_elem (exp, texp) =
    Exp.d_exp exp ;
    L.d_str ": " ;
    Exp.d_texp_full texp ;
    L.d_str " "
  in
  List.iter ~f:d_elem typings


(** Module to encapsulate operations on the internal state of the prover *)
module ProverState : sig
  val reset : Prop.normal Prop.t -> Prop.exposed Prop.t -> unit

  val checks : check list ref

  (** type for array bounds checks *)
  type bounds_check =
    | BClen_imply of Exp.t * Exp.t * Exp.t list  (** coming from array_len_imply *)
    | BCfrom_pre of Predicates.atom  (** coming implicitly from preconditions *)

  val add_bounds_check : bounds_check -> unit

  val add_frame_fld : Predicates.hpred -> unit

  val add_frame_typ : Exp.t * Exp.t -> unit

  val add_missing_fld : Predicates.hpred -> unit

  val add_missing_pi : Predicates.atom -> unit

  val add_missing_sigma : Predicates.hpred list -> unit

  val add_missing_typ : Exp.t * Exp.t -> unit

  val atom_is_array_bounds_check : Predicates.atom -> bool
  (** check if atom in pre is a bounds check *)

  val get_bounds_checks : unit -> bounds_check list

  val get_frame_fld : unit -> Predicates.hpred list

  val get_frame_typ : unit -> (Exp.t * Exp.t) list

  val get_missing_fld : unit -> Predicates.hpred list

  val get_missing_pi : unit -> Predicates.atom list

  val get_missing_sigma : unit -> Predicates.hpred list

  val get_missing_typ : unit -> (Exp.t * Exp.t) list

  val d_implication : Predicates.subst * Predicates.subst -> 'a Prop.t * 'b Prop.t -> unit

  val d_implication_error : string * (Predicates.subst * Predicates.subst) * exc_body -> unit
end = struct
  type bounds_check = BClen_imply of Exp.t * Exp.t * Exp.t list | BCfrom_pre of Predicates.atom

  let implication_lhs = ref Prop.prop_emp

  let implication_rhs = ref (Prop.expose Prop.prop_emp)

  let fav_in_array_len = ref Ident.Set.empty

  (* free variables in array len position *)
  let bounds_checks = ref []

  (* delayed bounds check for arrays *)
  let frame_fld = ref []

  let missing_fld = ref []

  let missing_pi = ref []

  let missing_sigma = ref []

  let frame_typ = ref []

  let missing_typ = ref []

  let checks = ref []

  (** free vars in array len position in current strexp part of prop *)
  let prop_fav_len prop =
    let do_hpred fav = function
      | Predicates.Hpointsto (_, Earray ((Var _ as len), _, _), _) ->
          Exp.free_vars len |> Ident.set_of_sequence ~init:fav
      | _ ->
          fav
    in
    List.fold_left ~init:Ident.Set.empty ~f:do_hpred prop.Prop.sigma


  let reset lhs rhs =
    checks := [] ;
    implication_lhs := lhs ;
    implication_rhs := rhs ;
    fav_in_array_len := prop_fav_len rhs ;
    bounds_checks := [] ;
    frame_fld := [] ;
    frame_typ := [] ;
    missing_fld := [] ;
    missing_pi := [] ;
    missing_sigma := [] ;
    missing_typ := []


  let add_bounds_check bounds_check = bounds_checks := bounds_check :: !bounds_checks

  let add_frame_fld hpred = frame_fld := hpred :: !frame_fld

  let add_missing_fld hpred = missing_fld := hpred :: !missing_fld

  let add_frame_typ typing = frame_typ := typing :: !frame_typ

  let add_missing_typ typing = missing_typ := typing :: !missing_typ

  let add_missing_pi a = missing_pi := a :: !missing_pi

  let add_missing_sigma sigma = missing_sigma := sigma @ !missing_sigma

  (** atom considered array bounds check if it contains vars present in array length position in the
      pre *)
  let atom_is_array_bounds_check atom =
    Prop.atom_is_inequality atom
    && Predicates.atom_free_vars atom
       |> Sequence.exists ~f:(fun id -> Ident.Set.mem id !fav_in_array_len)


  let get_bounds_checks () = !bounds_checks

  let get_frame_fld () = !frame_fld

  let get_frame_typ () = !frame_typ

  let get_missing_fld () = !missing_fld

  let get_missing_pi () = !missing_pi

  let get_missing_sigma () = !missing_sigma

  let get_missing_typ () = !missing_typ

  let d_missing_ sub =
    L.d_strln "SUB: " ;
    L.d_increase_indent () ;
    Prop.d_sub sub ;
    L.d_decrease_indent () ;
    if (not (List.is_empty !missing_pi)) && not (List.is_empty !missing_sigma) then (
      L.d_ln () ;
      Prop.d_pi !missing_pi ;
      L.d_strln "*" ;
      Prop.d_sigma !missing_sigma )
    else if not (List.is_empty !missing_pi) then (
      L.d_ln () ;
      Prop.d_pi !missing_pi )
    else if not (List.is_empty !missing_sigma) then (
      L.d_ln () ;
      Prop.d_sigma !missing_sigma ) ;
    if not (List.is_empty !missing_fld) then (
      L.d_ln () ;
      L.d_strln "MISSING FLD:" ;
      L.d_increase_indent () ;
      Prop.d_sigma !missing_fld ;
      L.d_decrease_indent () ) ;
    if not (List.is_empty !missing_typ) then (
      L.d_ln () ;
      L.d_strln "MISSING TYPING:" ;
      L.d_increase_indent () ;
      d_typings !missing_typ ;
      L.d_decrease_indent () )


  let d_missing sub =
    (* optional print of missing: if print something, prepend with newline *)
    if
      (not (List.is_empty !missing_pi))
      || (not (List.is_empty !missing_sigma))
      || (not (List.is_empty !missing_fld))
      || (not (List.is_empty !missing_typ))
      || not (Predicates.is_sub_empty sub)
    then (
      L.d_ln () ;
      L.d_str "[" ;
      d_missing_ sub ;
      L.d_str "]" )


  let d_frame_fld () =
    (* optional print of frame fld: if print something, prepend with newline *)
    if not (List.is_empty !frame_fld) then (
      L.d_ln () ;
      L.d_strln "[FRAME FLD:" ;
      L.d_increase_indent () ;
      Prop.d_sigma !frame_fld ;
      L.d_str "]" ;
      L.d_decrease_indent () )


  let d_frame_typ () =
    (* optional print of frame typ: if print something, prepend with newline *)
    if not (List.is_empty !frame_typ) then (
      L.d_ln () ;
      L.d_strln "[FRAME TYPING:" ;
      L.d_increase_indent () ;
      d_typings !frame_typ ;
      L.d_str "]" ;
      L.d_decrease_indent () )


  (** Dump an implication *)
  let d_implication (sub1, sub2) (p1, p2) =
    let p1, p2 = (Prop.prop_sub sub1 p1, Prop.prop_sub sub2 p2) in
    L.d_strln "SUB:" ;
    L.d_increase_indent () ;
    Prop.d_sub sub1 ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    Prop.d_prop p1 ;
    d_missing sub2 ;
    L.d_ln () ;
    L.d_strln "|-" ;
    Prop.d_prop p2 ;
    d_frame_fld () ;
    d_frame_typ ()


  let d_implication_error (s, subs, body) =
    let p1, p2 = (!implication_lhs, !implication_rhs) in
    let d_inner () =
      match body with
      | EXC_FALSE ->
          ()
      | EXC_FALSE_HPRED hpred ->
          L.d_str " on " ;
          Predicates.d_hpred hpred
      | EXC_FALSE_EXPS (e1, e2) ->
          L.d_str " on " ;
          Exp.d_exp e1 ;
          L.d_str "," ;
          Exp.d_exp e2
      | EXC_FALSE_SEXPS (se1, se2) ->
          L.d_str " on " ;
          Predicates.d_sexp se1 ;
          L.d_str "," ;
          Predicates.d_sexp se2
      | EXC_FALSE_ATOM a ->
          L.d_str " on " ;
          Predicates.d_atom a
      | EXC_FALSE_SIGMA sigma ->
          L.d_str " on " ;
          Prop.d_sigma sigma
    in
    L.d_ln () ;
    L.d_strln "$$$$$$$ Implication" ;
    d_implication subs (p1, p2) ;
    L.d_ln () ;
    L.d_printf "$$$$$$ error: %s" s ;
    d_inner () ;
    L.d_strln " returning FALSE" ;
    L.d_ln ()
end

let d_impl (s1, s2) = ProverState.d_implication (s1, s2)

let d_impl_err (arg1, (s1, s2), arg3) = ProverState.d_implication_error (arg1, (s1, s2), arg3)

(** extend a substitution *)
let extend_sub sub v e =
  let new_exp_sub = Predicates.subst_of_list [(v, e)] in
  Predicates.sub_join new_exp_sub (Predicates.sub_range_map (Predicates.exp_sub new_exp_sub) sub)


(** Extend [sub1] and [sub2] to witnesses that each instance of [e1[sub1]] is an instance of
    [e2[sub2]]. Raise IMPL_FALSE if not possible. *)
let exp_imply tenv calc_missing (subs : subst2) e1_in e2_in : subst2 =
  let e1 = Prop.exp_normalize_noabs tenv (fst subs) e1_in in
  let e2 = Prop.exp_normalize_noabs tenv (snd subs) e2_in in
  let var_imply (subs : subst2) v1 v2 : subst2 =
    match (Ident.is_primed v1, Ident.is_primed v2) with
    | false, false ->
        if Ident.equal v1 v2 then subs
        else if calc_missing && Ident.is_footprint v1 && Ident.is_footprint v2 then
          let () = ProverState.add_missing_pi (Aeq (e1_in, e2_in)) in
          subs
        else raise (IMPL_EXC ("exps", subs, EXC_FALSE_EXPS (e1, e2)))
    | true, false ->
        raise (IMPL_EXC ("exps", subs, EXC_FALSE_EXPS (e1, e2)))
    | false, true ->
        let sub2' = extend_sub (snd subs) v2 (Predicates.exp_sub (fst subs) (Exp.Var v1)) in
        (fst subs, sub2')
    | true, true ->
        let v1' = Ident.create_fresh Ident.knormal in
        let sub1' = extend_sub (fst subs) v1 (Exp.Var v1') in
        let sub2' = extend_sub (snd subs) v2 (Exp.Var v1') in
        (sub1', sub2')
  in
  let rec do_imply subs e1 e2 : subst2 =
    L.d_str "do_imply " ;
    Exp.d_exp e1 ;
    L.d_str " " ;
    Exp.d_exp e2 ;
    L.d_ln () ;
    match (e1, e2) with
    | Exp.Var v1, Exp.Var v2 ->
        var_imply subs v1 v2
    | Exp.BinOp ((PlusA _ | PlusPI | MinusA _ | MinusPI), Exp.Var v1, e2), Exp.Var v2
      when Ident.equal v1 v2 ->
        do_imply subs e2 Exp.zero
    | Exp.BinOp ((PlusA _ | PlusPI), e2, Exp.Var v1), Exp.Var v2 when Ident.equal v1 v2 ->
        do_imply subs e2 Exp.zero
    | e1, Exp.Var v2 ->
        let occurs_check v e =
          (* check whether [v] occurs in normalized [e] *)
          if
            Exp.ident_mem e v
            && Exp.ident_mem (Prop.exp_normalize_prop ~destructive:true tenv Prop.prop_emp e) v
          then raise (IMPL_EXC ("occurs check", subs, EXC_FALSE_EXPS (e1, e2)))
        in
        if Ident.is_primed v2 then
          let () = occurs_check v2 e1 in
          let sub2' = extend_sub (snd subs) v2 e1 in
          (fst subs, sub2')
        else raise (IMPL_EXC ("expressions not equal", subs, EXC_FALSE_EXPS (e1, e2)))
    | e1, Exp.BinOp (Binop.PlusA _, (Exp.Var v2 as e2), e2')
      when Ident.is_primed v2 || Ident.is_footprint v2 ->
        (* here e2' could also be a variable that we could try to substitute (as in the next match
           case), but we ignore that to avoid backtracking *)
        let e' = Exp.BinOp (Binop.MinusA None, e1, e2') in
        do_imply subs (Prop.exp_normalize_noabs tenv Predicates.sub_empty e') e2
    | e1, Exp.BinOp (Binop.PlusA _, e2, (Exp.Var v2 as e2'))
      when Ident.is_primed v2 || Ident.is_footprint v2 ->
        (* symmetric of above case *)
        let e' = Exp.BinOp (Binop.MinusA None, e1, e2') in
        do_imply subs (Prop.exp_normalize_noabs tenv Predicates.sub_empty e') e2
    | Exp.Var id, Exp.Lvar pv when Ident.is_footprint id && Pvar.is_local pv ->
        (* Footprint var could never be the same as local address *)
        raise (IMPL_EXC ("expression not equal", subs, EXC_FALSE_EXPS (e1, e2)))
    | Exp.Var _, e2 ->
        if calc_missing then
          let () = ProverState.add_missing_pi (Aeq (e1_in, e2_in)) in
          subs
        else raise (IMPL_EXC ("expressions not equal", subs, EXC_FALSE_EXPS (e1, e2)))
    | Exp.Lvar pv1, Exp.Const _ when Pvar.is_global pv1 ->
        if calc_missing then
          let () = ProverState.add_missing_pi (Aeq (e1_in, e2_in)) in
          subs
        else raise (IMPL_EXC ("expressions not equal", subs, EXC_FALSE_EXPS (e1, e2)))
    | Exp.Lvar v1, Exp.Lvar v2 ->
        if Pvar.equal v1 v2 then subs
        else raise (IMPL_EXC ("expressions not equal", subs, EXC_FALSE_EXPS (e1, e2)))
    | Exp.Const c1, Exp.Const c2 ->
        if Const.equal c1 c2 then subs
        else raise (IMPL_EXC ("constants not equal", subs, EXC_FALSE_EXPS (e1, e2)))
    | Exp.Const (Const.Cint _), Exp.BinOp (Binop.PlusPI, _, _) ->
        raise
          (IMPL_EXC ("pointer+index cannot evaluate to a constant", subs, EXC_FALSE_EXPS (e1, e2)))
    | Exp.Const (Const.Cint n1), Exp.BinOp (Binop.PlusA _, f1, Exp.Const (Const.Cint n2)) ->
        do_imply subs (Exp.int (n1 -- n2)) f1
    | Exp.BinOp (op1, e1, f1), Exp.BinOp (op2, e2, f2) when Binop.equal op1 op2 ->
        do_imply (do_imply subs e1 e2) f1 f2
    | Exp.BinOp (Binop.PlusA _, Exp.Var v1, e1), e2 ->
        do_imply subs (Exp.Var v1) (Exp.BinOp (Binop.MinusA None, e2, e1))
    | Exp.BinOp (Binop.PlusPI, Exp.Lvar pv1, e1), e2 ->
        do_imply subs (Exp.Lvar pv1) (Exp.BinOp (Binop.MinusA None, e2, e1))
    | ( Exp.Sizeof {typ= t1; dynamic_length= None; subtype= st1}
      , Exp.Sizeof {typ= t2; dynamic_length= None; subtype= st2} )
      when Typ.equal t1 t2 && Subtype.equal_modulo_flag st1 st2 ->
        subs
    | ( Exp.Sizeof {typ= t1; dynamic_length= Some d1; subtype= st1}
      , Exp.Sizeof {typ= t2; dynamic_length= Some d2; subtype= st2} )
      when Typ.equal t1 t2 && Exp.equal d1 d2 && Subtype.equal_modulo_flag st1 st2 ->
        subs
    | e', Exp.Const (Const.Cint n)
      when IntLit.iszero n && check_disequal tenv Prop.prop_emp e' Exp.zero ->
        raise (IMPL_EXC ("expressions not equal", subs, EXC_FALSE_EXPS (e1, e2)))
    | Exp.Const (Const.Cint n), e'
      when IntLit.iszero n && check_disequal tenv Prop.prop_emp e' Exp.zero ->
        raise (IMPL_EXC ("expressions not equal", subs, EXC_FALSE_EXPS (e1, e2)))
    | e1, Exp.Const _ ->
        raise (IMPL_EXC ("lhs not constant", subs, EXC_FALSE_EXPS (e1, e2)))
    | Exp.Lfield (e1, fd1, _), Exp.Lfield (e2, fd2, _) when Fieldname.equal fd1 fd2 ->
        do_imply subs e1 e2
    | Exp.Lindex (e1, f1), Exp.Lindex (e2, f2) ->
        do_imply (do_imply subs e1 e2) f1 f2
    | Exp.Exn e1, Exp.Exn e2 ->
        do_imply subs e1 e2
    | _ ->
        d_impl_err ("exp_imply not implemented", subs, EXC_FALSE_EXPS (e1, e2)) ;
        raise (Exceptions.Abduction_case_not_implemented __POS__)
  in
  do_imply subs e1 e2


(** Convert a path (from lhs of a |-> to a field name present only in the rhs) into an id. If the
    lhs was a footprint var, the id is a new footprint var. Othewise it is a var with the path in
    the name and stamp - 1 *)
let path_to_id path =
  let rec f = function
    | Exp.Var id ->
        if Ident.is_footprint id then None
        else Some (Ident.name_to_string (Ident.get_name id) ^ string_of_int (Ident.get_stamp id))
    | Exp.Lfield (e, fld, _) -> (
      match f e with None -> None | Some s -> Some (s ^ "_" ^ Fieldname.to_string fld) )
    | Exp.Lindex (e, ind) -> (
      match f e with None -> None | Some s -> Some (s ^ "_" ^ Exp.to_string ind) )
    | Exp.Lvar _ ->
        Some (Exp.to_string path)
    | Exp.Const (Const.Cstr s) ->
        Some ("_const_str_" ^ s)
    | Exp.Const (Const.Cclass c) ->
        Some ("_const_class_" ^ Ident.name_to_string c)
    | Exp.Const _ ->
        None
    | _ ->
        L.d_str "path_to_id undefined on " ;
        Exp.d_exp path ;
        L.d_ln () ;
        assert false
    (* None *)
  in
  if !BiabductionConfig.footprint then Ident.create_fresh Ident.kfootprint
  else
    match f path with None -> Ident.create_fresh Ident.kfootprint | Some s -> Ident.create_path s


(** Implication for the length of arrays *)
let array_len_imply tenv calc_missing subs len1 len2 indices2 =
  match (len1, len2) with
  | _, Exp.Var _
  | _, Exp.BinOp (Binop.PlusA _, Exp.Var _, _)
  | _, Exp.BinOp (Binop.PlusA _, _, Exp.Var _)
  | Exp.BinOp (Binop.Mult _, _, _), _ -> (
    try exp_imply tenv calc_missing subs len1 len2
    with IMPL_EXC (s, subs', x) -> raise (IMPL_EXC ("array len:" ^ s, subs', x)) )
  | _ ->
      ProverState.add_bounds_check (ProverState.BClen_imply (len1, len2, indices2)) ;
      subs


(** Extend [sub1] and [sub2] to witnesses that each instance of [se1[sub1]] is an instance of
    [se2[sub2]]. Raise IMPL_FALSE if not possible. *)
let rec sexp_imply tenv source calc_index_frame calc_missing subs se1 se2 typ2 :
    subst2 * Predicates.strexp option * Predicates.strexp option =
  (* L.d_str "sexp_imply "; Predicates.d_sexp se1; L.d_str " "; Predicates.d_sexp se2;
     L.d_str " : "; Typ.d_full typ2; L.d_ln(); *)
  match (se1, se2) with
  | Predicates.Eexp (e1, _), Predicates.Eexp (e2, _) ->
      (exp_imply tenv calc_missing subs e1 e2, None, None)
  | Predicates.Estruct (fsel1, inst1), Predicates.Estruct (fsel2, _) ->
      let subs', fld_frame, fld_missing =
        struct_imply tenv source calc_missing subs fsel1 fsel2 typ2
      in
      let fld_frame_opt =
        if not (List.is_empty fld_frame) then Some (Predicates.Estruct (fld_frame, inst1)) else None
      in
      let fld_missing_opt =
        if not (List.is_empty fld_missing) then Some (Predicates.Estruct (fld_missing, inst1))
        else None
      in
      (subs', fld_frame_opt, fld_missing_opt)
  | Predicates.Estruct _, Predicates.Eexp (e2, _) -> (
      let e2' = Predicates.exp_sub (snd subs) e2 in
      match e2' with
      | Exp.Var id2 when Ident.is_primed id2 ->
          let id2' = Ident.create_fresh Ident.knormal in
          let sub2' = extend_sub (snd subs) id2 (Exp.Var id2') in
          ((fst subs, sub2'), None, None)
      | _ ->
          d_impl_err ("sexp_imply not implemented", subs, EXC_FALSE_SEXPS (se1, se2)) ;
          raise (Exceptions.Abduction_case_not_implemented __POS__) )
  | Predicates.Earray (len1, esel1, inst1), Predicates.Earray (len2, esel2, _) ->
      let indices2 = List.map ~f:fst esel2 in
      let subs' = array_len_imply tenv calc_missing subs len1 len2 indices2 in
      let subs'', index_frame, index_missing =
        array_imply tenv source calc_index_frame calc_missing subs' esel1 esel2 typ2
      in
      let index_frame_opt =
        if not (List.is_empty index_frame) then Some (Predicates.Earray (len1, index_frame, inst1))
        else None
      in
      let index_missing_opt =
        if (not (List.is_empty index_missing)) && !BiabductionConfig.footprint then
          Some (Predicates.Earray (len1, index_missing, inst1))
        else None
      in
      (subs'', index_frame_opt, index_missing_opt)
  | Predicates.Eexp (_, inst), Predicates.Estruct (fsel, inst') ->
      d_impl_err
        ( "WARNING: function call with parameters of struct type, treating as unknown"
        , subs
        , EXC_FALSE_SEXPS (se1, se2) ) ;
      let fsel' =
        let g (f, _) = (f, Predicates.Eexp (Exp.Var (Ident.create_fresh Ident.knormal), inst)) in
        List.map ~f:g fsel
      in
      sexp_imply tenv source calc_index_frame calc_missing subs
        (Predicates.Estruct (fsel', inst'))
        se2 typ2
  | Predicates.Eexp _, Predicates.Earray (len, _, inst)
  | Predicates.Estruct _, Predicates.Earray (len, _, inst) ->
      let se1' = Predicates.Earray (len, [(Exp.zero, se1)], inst) in
      sexp_imply tenv source calc_index_frame calc_missing subs se1' se2 typ2
  | Predicates.Earray (len, _, _), Predicates.Eexp (_, inst) ->
      let se2' = Predicates.Earray (len, [(Exp.zero, se2)], inst) in
      let typ2' = Typ.mk_array typ2 in
      (* In the sexp_imply, struct_imply, array_imply, and sexp_imply_nolhs functions, the typ2
         argument is only used by eventually passing its value to Struct.fld, Exp.Lfield,
         Struct.fld, or Typ.array_elem.  None of these are sensitive to the length field
         of Tarray, so forgetting the length of typ2' here is not a problem. Not one of those
         functions use typ.quals either *)
      sexp_imply tenv source true calc_missing subs se1 se2' typ2'
      (* calculate index_frame because the rhs is a singleton array *)
  | _ ->
      d_impl_err ("sexp_imply not implemented", subs, EXC_FALSE_SEXPS (se1, se2)) ;
      raise (Exceptions.Abduction_case_not_implemented __POS__)


and struct_imply tenv source calc_missing subs fsel1 fsel2 typ2 :
    subst2 * (Fieldname.t * Predicates.strexp) list * (Fieldname.t * Predicates.strexp) list =
  let lookup = Tenv.lookup tenv in
  match (fsel1, fsel2) with
  | _, [] ->
      (subs, fsel1, [])
  | (f1, se1) :: fsel1', (f2, se2) :: fsel2' -> (
    match Fieldname.compare f1 f2 with
    | 0 ->
        let typ' = Struct.fld_typ ~lookup ~default:StdTyp.void f2 typ2 in
        let subs', se_frame, se_missing =
          sexp_imply tenv (Exp.Lfield (source, f2, typ2)) false calc_missing subs se1 se2 typ'
        in
        let subs'', fld_frame, fld_missing =
          struct_imply tenv source calc_missing subs' fsel1' fsel2' typ2
        in
        let fld_frame' =
          match se_frame with None -> fld_frame | Some se -> (f1, se) :: fld_frame
        in
        let fld_missing' =
          match se_missing with None -> fld_missing | Some se -> (f1, se) :: fld_missing
        in
        (subs'', fld_frame', fld_missing')
    | n when n < 0 ->
        let subs', fld_frame, fld_missing =
          struct_imply tenv source calc_missing subs fsel1' fsel2 typ2
        in
        (subs', (f1, se1) :: fld_frame, fld_missing)
    | _ ->
        let typ' = Struct.fld_typ ~lookup ~default:StdTyp.void f2 typ2 in
        let subs' =
          sexp_imply_nolhs tenv (Exp.Lfield (source, f2, typ2)) calc_missing subs se2 typ'
        in
        let subs', fld_frame, fld_missing =
          struct_imply tenv source calc_missing subs' fsel1 fsel2' typ2
        in
        let fld_missing' = (f2, se2) :: fld_missing in
        (subs', fld_frame, fld_missing') )
  | [], (f2, se2) :: fsel2' ->
      let typ' = Struct.fld_typ ~lookup ~default:StdTyp.void f2 typ2 in
      let subs' =
        sexp_imply_nolhs tenv (Exp.Lfield (source, f2, typ2)) calc_missing subs se2 typ'
      in
      let subs'', fld_frame, fld_missing =
        struct_imply tenv source calc_missing subs' [] fsel2' typ2
      in
      (subs'', fld_frame, (f2, se2) :: fld_missing)


and array_imply tenv source calc_index_frame calc_missing subs esel1 esel2 typ2 :
    subst2 * (Exp.t * Predicates.strexp) list * (Exp.t * Predicates.strexp) list =
  let typ_elem = Typ.array_elem (Some StdTyp.void) typ2 in
  match (esel1, esel2) with
  | _, [] ->
      (subs, esel1, [])
  | (e1, se1) :: esel1', (e2, se2) :: esel2' ->
      let e1n = Prop.exp_normalize_noabs tenv (fst subs) e1 in
      let e2n = Prop.exp_normalize_noabs tenv (snd subs) e2 in
      let n = Exp.compare e1n e2n in
      if n < 0 then array_imply tenv source calc_index_frame calc_missing subs esel1' esel2 typ2
      else if n > 0 then
        array_imply tenv source calc_index_frame calc_missing subs esel1 esel2' typ2
      else
        (* n=0 *)
        let subs', _, _ =
          sexp_imply tenv (Exp.Lindex (source, e1)) false calc_missing subs se1 se2 typ_elem
        in
        array_imply tenv source calc_index_frame calc_missing subs' esel1' esel2' typ2
  | [], (e2, se2) :: esel2' ->
      let subs' = sexp_imply_nolhs tenv (Exp.Lindex (source, e2)) calc_missing subs se2 typ_elem in
      let subs'', index_frame, index_missing =
        array_imply tenv source calc_index_frame calc_missing subs' [] esel2' typ2
      in
      let index_missing' = (e2, se2) :: index_missing in
      (subs'', index_frame, index_missing')


and sexp_imply_nolhs tenv source calc_missing (subs : subst2) se2 typ2 =
  match se2 with
  | Predicates.Eexp (e2_, _) -> (
      let e2 = Predicates.exp_sub (snd subs) e2_ in
      match e2 with
      | Exp.Var v2 when Ident.is_primed v2 ->
          let v2' = path_to_id source in
          (* L.d_str "called path_to_id on "; Exp.d_exp e2; *)
          (* L.d_str " returns "; Exp.d_exp (Exp.Var v2'); L.d_ln (); *)
          let sub2' = extend_sub (snd subs) v2 (Exp.Var v2') in
          (fst subs, sub2')
      | Exp.Var _ ->
          if calc_missing then subs
          else raise (IMPL_EXC ("exp only in rhs is not a primed var", subs, EXC_FALSE))
      | Exp.Const _ when calc_missing ->
          let id = path_to_id source in
          ProverState.add_missing_pi (Predicates.Aeq (Exp.Var id, e2_)) ;
          subs
      | _ ->
          raise (IMPL_EXC ("exp only in rhs is not a primed var", subs, EXC_FALSE)) )
  | Predicates.Estruct (fsel2, _) ->
      (fun (x, _, _) -> x) (struct_imply tenv source calc_missing subs [] fsel2 typ2)
  | Predicates.Earray (_, esel2, _) ->
      (fun (x, _, _) -> x) (array_imply tenv source false calc_missing subs [] esel2 typ2)


let rec exp_list_imply tenv calc_missing subs l1 l2 =
  match (l1, l2) with
  | [], [] ->
      subs
  | e1 :: l1, e2 :: l2 ->
      exp_list_imply tenv calc_missing (exp_imply tenv calc_missing subs e1 e2) l1 l2
  | _ ->
      assert false


let filter_ne_lhs sub e0 = function
  | Predicates.Hpointsto (e, _, _) ->
      if Exp.equal e0 (Predicates.exp_sub sub e) then Some () else None
  | Predicates.Hlseg (Lseg_NE, _, e, _, _) ->
      if Exp.equal e0 (Predicates.exp_sub sub e) then Some () else None
  | Predicates.Hdllseg (Lseg_NE, _, e, _, _, e', _) ->
      if Exp.equal e0 (Predicates.exp_sub sub e) || Exp.equal e0 (Predicates.exp_sub sub e') then
        Some ()
      else None
  | _ ->
      None


let filter_hpred sub hpred2 hpred1 =
  match (Predicates.hpred_sub sub hpred1, hpred2) with
  | Predicates.Hlseg (Lseg_NE, hpara1, e1, f1, el1), Predicates.Hlseg (Lseg_PE, _, _, _, _) ->
      if Predicates.equal_hpred (Hlseg (Lseg_PE, hpara1, e1, f1, el1)) hpred2 then Some false
      else None
  | Predicates.Hlseg (Lseg_PE, hpara1, e1, f1, el1), Predicates.Hlseg (Lseg_NE, _, _, _, _) ->
      if Predicates.equal_hpred (Hlseg (Lseg_NE, hpara1, e1, f1, el1)) hpred2 then Some true
      else None (* return missing disequality *)
  | Predicates.Hpointsto (e1, _, _), Predicates.Hlseg (_, _, e2, _, _) ->
      if Exp.equal e1 e2 then Some false else None
  | hpred1, hpred2 ->
      if Predicates.equal_hpred hpred1 hpred2 then Some false else None


let hpred_has_primed_lhs sub hpred =
  let rec find_primed e =
    match e with
    | Exp.Lfield (e, _, _) ->
        find_primed e
    | Exp.Lindex (e, _) ->
        find_primed e
    | Exp.BinOp (Binop.PlusPI, e1, _) ->
        find_primed e1
    | _ ->
        Exp.free_vars e |> Sequence.exists ~f:Ident.is_primed
  in
  let exp_has_primed e = find_primed (Predicates.exp_sub sub e) in
  match hpred with
  | Predicates.Hpointsto (e, _, _) ->
      exp_has_primed e
  | Predicates.Hlseg (_, _, e, _, _) ->
      exp_has_primed e
  | Predicates.Hdllseg (_, _, iF, _, _, iB, _) ->
      exp_has_primed iF && exp_has_primed iB


let move_primed_lhs_from_front subs sigma =
  match sigma with
  | [] ->
      sigma
  | hpred :: _ ->
      if hpred_has_primed_lhs (snd subs) hpred then
        let sigma_primed, sigma_unprimed =
          List.partition_tf ~f:(hpred_has_primed_lhs (snd subs)) sigma
        in
        match sigma_unprimed with
        | [] ->
            raise
              (IMPL_EXC ("every hpred has primed lhs, cannot proceed", subs, EXC_FALSE_SIGMA sigma))
        | _ :: _ ->
            sigma_unprimed @ sigma_primed
      else sigma


(** [expand_hpred_pointer calc_index_frame hpred] expands [hpred] if it is a |-> whose lhs is a
    Lfield or Lindex or ptr+off. Return [(changed, calc_index_frame', hpred')] where [changed]
    indicates whether the predicate has changed. *)
let expand_hpred_pointer =
  let count = ref 0 in
  fun tenv calc_index_frame hpred ->
    let rec expand changed calc_index_frame hpred =
      match hpred with
      | Predicates.Hpointsto (Lfield (adr_base, fld, adr_typ), cnt, cnt_texp) ->
          let cnt_texp' =
            match
              match adr_typ.desc with
              | Tstruct name -> (
                match Tenv.lookup tenv name with
                | Some _ ->
                    (* type of struct at adr_base is known *)
                    Some
                      (Exp.Sizeof
                         { typ= adr_typ
                         ; nbytes= None
                         ; dynamic_length= None
                         ; subtype= Subtype.exact
                         ; nullable= false } )
                | None ->
                    None )
              | _ ->
                  None
            with
            | Some se ->
                se
            | None -> (
              match cnt_texp with
              | Sizeof ({typ= cnt_typ} as sizeof_data) ->
                  (* type of struct at adr_base is unknown (typically Tvoid), but
                     type of contents is known, so construct struct type for single fld:cnt_typ *)
                  let name = Typ.Name.C.from_string ("counterfeit" ^ string_of_int !count) in
                  incr count ;
                  let fields = [Struct.mk_field fld cnt_typ] in
                  ignore (Tenv.mk_struct tenv ~fields name) ;
                  Exp.Sizeof {sizeof_data with typ= Typ.mk (Tstruct name)}
              | _ ->
                  (* type of struct at adr_base and of contents are both unknown: give up *)
                  L.(die InternalError) "expand_hpred_pointer: Unexpected non-sizeof type in Lfield"
              )
          in
          let hpred' =
            Predicates.Hpointsto (adr_base, Estruct ([(fld, cnt)], Predicates.inst_none), cnt_texp')
          in
          expand true true hpred'
      | Predicates.Hpointsto (Lindex (e, ind), se, t) ->
          let t' =
            match t with
            | Exp.Sizeof ({typ= t_} as sizeof_data) ->
                Exp.Sizeof {sizeof_data with typ= Typ.mk_array t_}
            | _ ->
                L.(die InternalError) "expand_hpred_pointer: Unexpected non-sizeof type in Lindex"
          in
          let len =
            match t' with
            | Exp.Sizeof {dynamic_length= Some len} ->
                len
            | _ ->
                Exp.get_undefined false
          in
          let hpred' =
            Predicates.Hpointsto (e, Predicates.Earray (len, [(ind, se)], Predicates.inst_none), t')
          in
          expand true true hpred'
      | Predicates.Hpointsto (BinOp (PlusPI, e1, e2), Earray (len, esel, inst), t) ->
          let shift_exp e = Exp.BinOp (Binop.PlusA None, e, e2) in
          let len' = shift_exp len in
          let esel' = List.map ~f:(fun (e, se) -> (shift_exp e, se)) esel in
          let hpred' = Predicates.Hpointsto (e1, Predicates.Earray (len', esel', inst), t) in
          expand true calc_index_frame hpred'
      | _ ->
          (changed, calc_index_frame, hpred)
    in
    expand false calc_index_frame hpred


let cast_exception tenv texp1 texp2 e1 subs =
  ( match (texp1, texp2) with
  | Exp.Sizeof {typ= t1}, Exp.Sizeof {typ= t2; subtype= st2} ->
      if
        Config.developer_mode
        || (Subtype.is_cast st2 && not (SubtypingCheck.check_subtype tenv t1 t2))
      then ProverState.checks := Class_cast_check (texp1, texp2, e1) :: !ProverState.checks
  | _ ->
      () ) ;
  raise (IMPL_EXC ("class cast exception", subs, EXC_FALSE))


(** Check the equality of two types ignoring flags in the subtyping components *)
let texp_equal_modulo_subtype_flag texp1 texp2 =
  match (texp1, texp2) with
  | ( Exp.Sizeof {typ= t1; dynamic_length= len1; subtype= st1}
    , Exp.Sizeof {typ= t2; dynamic_length= len2; subtype= st2} ) ->
      [%equal: Typ.t * Exp.t option] (t1, len1) (t2, len2) && Subtype.equal_modulo_flag st1 st2
  | _ ->
      Exp.equal texp1 texp2


(** check implication between type expressions *)
let texp_imply tenv subs texp1 texp2 e1 calc_missing =
  (* check whether the types could be subject to dynamic cast: *)
  (* classes and arrays in Java, and just classes in C++ and ObjC *)
  let types_subject_to_dynamic_cast =
    match (texp1, texp2) with
    | Exp.Sizeof {typ= typ1}, Exp.Sizeof {typ= typ2} -> (
      match (typ1.desc, typ2.desc) with
      | (Tstruct _ | Tarray _), (Tstruct _ | Tarray _) ->
          (is_java_class tenv typ1 || is_csharp_class tenv typ1)
          || (Typ.is_cpp_class typ1 && Typ.is_cpp_class typ2)
          || (Typ.is_objc_class typ1 && Typ.is_objc_class typ2)
      | _ ->
          false )
    | _ ->
        false
  in
  if types_subject_to_dynamic_cast then
    let pos_type_opt, neg_type_opt = SubtypingCheck.subtype_case_analysis tenv texp1 texp2 in
    let has_changed =
      match pos_type_opt with
      | Some texp1' ->
          not (texp_equal_modulo_subtype_flag texp1' texp1)
      | None ->
          false
    in
    if calc_missing then
      (* footprint *)
      match pos_type_opt with
      | None ->
          cast_exception tenv texp1 texp2 e1 subs
      | Some _ ->
          if has_changed then (None, pos_type_opt) (* missing *) else (pos_type_opt, None)
      (* frame *)
    else
      (* re-execution *)
      match neg_type_opt with
      | Some _ ->
          cast_exception tenv texp1 texp2 e1 subs
      | None ->
          if has_changed then cast_exception tenv texp1 texp2 e1 subs (* missing *)
          else (pos_type_opt, None) (* frame *)
  else (None, None)


(** pre-process implication between a non-array and an array: the non-array is turned into an array
    of length given by its type only active in type_size mode *)
let sexp_imply_preprocess se1 texp1 se2 =
  match (se1, texp1, se2) with
  | Predicates.Eexp (_, inst), Exp.Sizeof _, Predicates.Earray _ when Config.biabduction_type_size
    ->
      let se1' = Predicates.Earray (texp1, [(Exp.zero, se1)], inst) in
      L.d_strln ~color:Orange "sexp_imply_preprocess" ;
      L.d_str " se1: " ;
      Predicates.d_sexp se1 ;
      L.d_ln () ;
      L.d_str " se1': " ;
      Predicates.d_sexp se1' ;
      L.d_ln () ;
      se1'
  | _ ->
      se1


(** handle parameter subtype: when the type of a callee variable in the caller is a strict subtype
    of the one in the callee, add a type frame and type missing *)
let handle_parameter_subtype tenv prop1 sigma2 subs (e1, se1, texp1) (se2, texp2) =
  let is_callee = match e1 with Exp.Lvar pv -> Pvar.is_callee pv | _ -> false in
  let is_allocated_lhs e =
    let filter = function Predicates.Hpointsto (e', _, _) -> Exp.equal e' e | _ -> false in
    List.exists ~f:filter prop1.Prop.sigma
  in
  let type_rhs e =
    let sub_opt = ref None in
    let filter = function
      | Predicates.Hpointsto (e', _, Exp.Sizeof sizeof_data) when Exp.equal e' e ->
          sub_opt := Some sizeof_data ;
          true
      | _ ->
          false
    in
    if List.exists ~f:filter sigma2 then !sub_opt else None
  in
  let add_subtype () =
    match (texp1, texp2, se1, se2) with
    | ( Exp.Sizeof {typ= {desc= Tptr (t1, _)}; dynamic_length= None}
      , Exp.Sizeof {typ= {desc= Tptr (t2, _)}; dynamic_length= None}
      , Predicates.Eexp (e1', _)
      , Predicates.Eexp (e2', _) )
      when not (is_allocated_lhs e1') -> (
      match type_rhs e2' with
      | Some sizeof_data2 -> (
          if (not (Typ.equal t1 t2)) && SubtypingCheck.check_subtype tenv t1 t2 then
            let pos_type_opt, _ =
              SubtypingCheck.subtype_case_analysis tenv
                (Exp.Sizeof
                   { typ= t1
                   ; nbytes= None
                   ; dynamic_length= None
                   ; subtype= Subtype.subtypes
                   ; nullable= false } )
                (Exp.Sizeof sizeof_data2)
            in
            match pos_type_opt with
            | Some t1_noptr ->
                ProverState.add_frame_typ (e1', t1_noptr) ;
                ProverState.add_missing_typ (e2', t1_noptr)
            | None ->
                cast_exception tenv texp1 texp2 e1 subs )
      | None ->
          () )
    | _ ->
        ()
  in
  if is_callee && !BiabductionConfig.footprint then add_subtype ()


let rec hpred_imply tenv calc_index_frame calc_missing subs prop1 sigma2 hpred2 :
    subst2 * Prop.normal Prop.t =
  match hpred2 with
  | Predicates.Hpointsto (e2_, se2, texp2) -> (
      let e2 = Predicates.exp_sub (snd subs) e2_ in
      ( match e2 with
      | Exp.Lvar _ ->
          ()
      | Exp.Var v ->
          if Ident.is_primed v then (
            d_impl_err ("rhs |-> not implemented", subs, EXC_FALSE_HPRED hpred2) ;
            raise (Exceptions.Abduction_case_not_implemented __POS__) )
      | _ ->
          () ) ;
      match Prop.prop_iter_create prop1 with
      | None ->
          raise (IMPL_EXC ("lhs is empty", subs, EXC_FALSE))
      | Some iter1 -> (
        match Prop.prop_iter_find iter1 (filter_ne_lhs (fst subs) e2) with
        | None ->
            raise (IMPL_EXC ("lhs does not have e|->", subs, EXC_FALSE_HPRED hpred2))
        | Some iter1' -> (
          match Prop.prop_iter_current tenv iter1' with
          | Predicates.Hpointsto (e1, se1, texp1), _ -> (
            try
              let typ2 = Exp.texp_to_typ (Some StdTyp.void) texp2 in
              let typing_frame, typing_missing = texp_imply tenv subs texp1 texp2 e1 calc_missing in
              let se1' = sexp_imply_preprocess se1 texp1 se2 in
              let subs', fld_frame, fld_missing =
                sexp_imply tenv e1 calc_index_frame calc_missing subs se1' se2 typ2
              in
              if calc_missing then (
                handle_parameter_subtype tenv prop1 sigma2 subs (e1, se1, texp1) (se2, texp2) ;
                ( match fld_missing with
                | Some fld_missing ->
                    ProverState.add_missing_fld (Predicates.Hpointsto (e2_, fld_missing, texp1))
                | None ->
                    () ) ;
                ( match fld_frame with
                | Some fld_frame ->
                    ProverState.add_frame_fld (Predicates.Hpointsto (e1, fld_frame, texp1))
                | None ->
                    () ) ;
                ( match typing_missing with
                | Some t_missing ->
                    ProverState.add_missing_typ (e2_, t_missing)
                | None ->
                    () ) ;
                match typing_frame with
                | Some t_frame ->
                    ProverState.add_frame_typ (e1, t_frame)
                | None ->
                    () ) ;
              let prop1' = Prop.prop_iter_remove_curr_then_to_prop tenv iter1' in
              (subs', prop1')
            with IMPL_EXC (s, _, _) when calc_missing -> raise (MISSING_EXC s) )
          | Predicates.Hlseg (Lseg_NE, para1, e1, f1, elist1), _ ->
              (* Unroll lseg *)
              let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
              let _, para_inst1 = Predicates.hpara_instantiate para1 e1 n' elist1 in
              let hpred_list1 = para_inst1 @ [Prop.mk_lseg tenv Lseg_PE para1 n' f1 elist1] in
              let iter1'' = Prop.prop_iter_update_current_by_list iter1' hpred_list1 in
              L.d_increase_indent () ;
              let res =
                decrease_indent_when_exception (fun () ->
                    hpred_imply tenv calc_index_frame calc_missing subs
                      (Prop.prop_iter_to_prop tenv iter1'')
                      sigma2 hpred2 )
              in
              L.d_decrease_indent () ;
              res
          | Predicates.Hdllseg (Lseg_NE, para1, iF1, oB1, oF1, iB1, elist1), _
            when Exp.equal (Predicates.exp_sub (fst subs) iF1) e2 ->
              (* Unroll dllseg forward *)
              let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
              let _, para_inst1 = Predicates.hpara_dll_instantiate para1 iF1 oB1 n' elist1 in
              let hpred_list1 =
                para_inst1 @ [Prop.mk_dllseg tenv Lseg_PE para1 n' iF1 oF1 iB1 elist1]
              in
              let iter1'' = Prop.prop_iter_update_current_by_list iter1' hpred_list1 in
              L.d_increase_indent () ;
              let res =
                decrease_indent_when_exception (fun () ->
                    hpred_imply tenv calc_index_frame calc_missing subs
                      (Prop.prop_iter_to_prop tenv iter1'')
                      sigma2 hpred2 )
              in
              L.d_decrease_indent () ;
              res
          | Predicates.Hdllseg (Lseg_NE, para1, iF1, oB1, oF1, iB1, elist1), _
            when Exp.equal (Predicates.exp_sub (fst subs) iB1) e2 ->
              (* Unroll dllseg backward *)
              let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
              let _, para_inst1 = Predicates.hpara_dll_instantiate para1 iB1 n' oF1 elist1 in
              let hpred_list1 =
                para_inst1 @ [Prop.mk_dllseg tenv Lseg_PE para1 iF1 oB1 iB1 n' elist1]
              in
              let iter1'' = Prop.prop_iter_update_current_by_list iter1' hpred_list1 in
              L.d_increase_indent () ;
              let res =
                decrease_indent_when_exception (fun () ->
                    hpred_imply tenv calc_index_frame calc_missing subs
                      (Prop.prop_iter_to_prop tenv iter1'')
                      sigma2 hpred2 )
              in
              L.d_decrease_indent () ;
              res
          | _ ->
              assert false ) ) )
  | Predicates.Hlseg (k, para2, e2_, f2_, elist2_) -> (
      (* for now ignore implications between PE and NE *)
      let e2, f2 = (Predicates.exp_sub (snd subs) e2_, Predicates.exp_sub (snd subs) f2_) in
      ( match e2 with
      | Exp.Lvar _ ->
          ()
      | Exp.Var v ->
          if Ident.is_primed v then (
            d_impl_err ("rhs |-> not implemented", subs, EXC_FALSE_HPRED hpred2) ;
            raise (Exceptions.Abduction_case_not_implemented __POS__) )
      | _ ->
          () ) ;
      if Exp.equal e2 f2 && Predicates.equal_lseg_kind k Lseg_PE then (subs, prop1)
      else
        match Prop.prop_iter_create prop1 with
        | None ->
            raise (IMPL_EXC ("lhs is empty", subs, EXC_FALSE))
        | Some iter1 -> (
          match
            Prop.prop_iter_find iter1
              (filter_hpred (fst subs) (Predicates.hpred_sub (snd subs) hpred2))
          with
          | None ->
              let elist2 = List.map ~f:(fun e -> Predicates.exp_sub (snd subs) e) elist2_ in
              let _, para_inst2 = Predicates.hpara_instantiate para2 e2 f2 elist2 in
              L.d_increase_indent () ;
              let res =
                decrease_indent_when_exception (fun () ->
                    sigma_imply tenv calc_index_frame false subs prop1 para_inst2 )
              in
              (* calc_missing is false as we're checking an instantiation of the original list *)
              L.d_decrease_indent () ;
              res
          | Some iter1' -> (
              let elist2 = List.map ~f:(fun e -> Predicates.exp_sub (snd subs) e) elist2_ in
              (* force instantiation of existentials *)
              let subs' = exp_list_imply tenv calc_missing subs (f2 :: elist2) (f2 :: elist2) in
              let prop1' = Prop.prop_iter_remove_curr_then_to_prop tenv iter1' in
              let hpred1 =
                match Prop.prop_iter_current tenv iter1' with
                | hpred1, b ->
                    if b then ProverState.add_missing_pi (Predicates.Aneq (e2_, f2_)) ;
                    (* for PE |- NE *)
                    hpred1
              in
              match hpred1 with
              | Predicates.Hlseg _ ->
                  (subs', prop1')
              | Predicates.Hpointsto _ ->
                  (* unroll rhs list and try again *)
                  let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
                  let _, para_inst2 = Predicates.hpara_instantiate para2 e2_ n' elist2 in
                  let hpred_list2 = para_inst2 @ [Prop.mk_lseg tenv Lseg_PE para2 n' f2_ elist2_] in
                  L.d_increase_indent () ;
                  let res =
                    decrease_indent_when_exception (fun () ->
                        try sigma_imply tenv calc_index_frame calc_missing subs prop1 hpred_list2
                        with exn when Exception.exn_not_failure exn ->
                          L.d_strln ~color:Red "backtracking lseg: trying rhs of length exactly 1" ;
                          let _, para_inst3 = Predicates.hpara_instantiate para2 e2_ f2_ elist2 in
                          sigma_imply tenv calc_index_frame calc_missing subs prop1 para_inst3 )
                  in
                  L.d_decrease_indent () ;
                  res
              | Predicates.Hdllseg _ ->
                  assert false ) ) )
  | Predicates.Hdllseg (Lseg_PE, _, _, _, _, _, _) ->
      d_impl_err ("rhs dllsegPE not implemented", subs, EXC_FALSE_HPRED hpred2) ;
      raise (Exceptions.Abduction_case_not_implemented __POS__)
  | Predicates.Hdllseg (_, para2, iF2, oB2, oF2, iB2, elist2) -> (
      (* for now ignore implications between PE and NE *)
      let iF2, oF2 = (Predicates.exp_sub (snd subs) iF2, Predicates.exp_sub (snd subs) oF2) in
      let iB2, oB2 = (Predicates.exp_sub (snd subs) iB2, Predicates.exp_sub (snd subs) oB2) in
      ( match oF2 with
      | Exp.Lvar _ ->
          ()
      | Exp.Var v ->
          if Ident.is_primed v then (
            d_impl_err ("rhs dllseg not implemented", subs, EXC_FALSE_HPRED hpred2) ;
            raise (Exceptions.Abduction_case_not_implemented __POS__) )
      | _ ->
          () ) ;
      ( match oB2 with
      | Exp.Lvar _ ->
          ()
      | Exp.Var v ->
          if Ident.is_primed v then (
            d_impl_err ("rhs dllseg not implemented", subs, EXC_FALSE_HPRED hpred2) ;
            raise (Exceptions.Abduction_case_not_implemented __POS__) )
      | _ ->
          () ) ;
      match Prop.prop_iter_create prop1 with
      | None ->
          raise (IMPL_EXC ("lhs is empty", subs, EXC_FALSE))
      | Some iter1 -> (
        match
          Prop.prop_iter_find iter1
            (filter_hpred (fst subs) (Predicates.hpred_sub (snd subs) hpred2))
        with
        | None ->
            let elist2 = List.map ~f:(fun e -> Predicates.exp_sub (snd subs) e) elist2 in
            let _, para_inst2 =
              if Exp.equal iF2 iB2 then Predicates.hpara_dll_instantiate para2 iF2 oB2 oF2 elist2
              else assert false
              (* Only base case of rhs list considered for now *)
            in
            L.d_increase_indent () ;
            let res =
              decrease_indent_when_exception (fun () ->
                  sigma_imply tenv calc_index_frame false subs prop1 para_inst2 )
            in
            (* calc_missing is false as we're checking an instantiation of the original list *)
            L.d_decrease_indent () ;
            res
        | Some iter1' ->
            (* Only consider implications between identical listsegs for now *)
            let elist2 = List.map ~f:(fun e -> Predicates.exp_sub (snd subs) e) elist2 in
            (* force instantiation of existentials *)
            let subs' =
              exp_list_imply tenv calc_missing subs
                (iF2 :: oB2 :: oF2 :: iB2 :: elist2)
                (iF2 :: oB2 :: oF2 :: iB2 :: elist2)
            in
            let prop1' = Prop.prop_iter_remove_curr_then_to_prop tenv iter1' in
            (subs', prop1') ) )


(** Check that [sigma1] implies [sigma2] and return two substitution instantiations for the primed
    variables of [sigma1] and [sigma2] and a frame. Raise IMPL_FALSE if the implication cannot be
    proven. *)
and sigma_imply tenv calc_index_frame calc_missing subs prop1 sigma2 : subst2 * Prop.normal Prop.t =
  let is_constant_string_class subs = function
    (* if the hpred represents a constant string, return the string *)
    | Predicates.Hpointsto (e2_, _, _) -> (
        let e2 = Predicates.exp_sub (snd subs) e2_ in
        match e2 with
        | Exp.Const (Const.Cstr s) ->
            Some (s, true)
        | Exp.Const (Const.Cclass c) ->
            Some (Ident.name_to_string c, false)
        | _ ->
            None )
    | _ ->
        None
  in
  let mk_constant_string_hpred s =
    (* create an hpred from a constant string *)
    let len = IntLit.of_int (1 + String.length s) in
    let root = Exp.Const (Const.Cstr s) in
    let sexp =
      let index = Exp.int (IntLit.of_int (String.length s)) in
      match !Language.curr_language with
      | Clang ->
          Predicates.Earray
            ( Exp.int len
            , [(index, Predicates.Eexp (Exp.zero, Predicates.inst_none))]
            , Predicates.inst_none )
      | Java ->
          let mk_fld_sexp field_name =
            let fld = Fieldname.make StdTyp.Name.Java.java_lang_string field_name in
            let se =
              Predicates.Eexp (Exp.Var (Ident.create_fresh Ident.kprimed), Predicates.Inone)
            in
            (fld, se)
          in
          let fields = ["count"; "hash"; "offset"; "value"] in
          Predicates.Estruct (List.map ~f:mk_fld_sexp fields, Predicates.inst_none)
      | CIL ->
          let mk_fld_sexp field_name =
            let fld = Fieldname.make StdTyp.Name.CSharp.system_string field_name in
            let se =
              Predicates.Eexp (Exp.Var (Ident.create_fresh Ident.kprimed), Predicates.Inone)
            in
            (fld, se)
          in
          let fields =
            ["System.String.Empty" (* ; "System.String.Chars" *); "System.String.Length"]
          in
          Predicates.Estruct (List.map ~f:mk_fld_sexp fields, Predicates.inst_none)
      | Erlang ->
          L.die InternalError "Erlang not supported"
      | Hack ->
          L.die InternalError "Hack not supported"
      | Python ->
          L.die InternalError "Python not supported"
    in
    let const_string_texp =
      match !Language.curr_language with
      | Clang ->
          Exp.Sizeof
            { typ= Typ.mk_array (Typ.mk (Tint Typ.IChar)) ~length:len ~stride:(IntLit.of_int 1)
            ; nbytes= None
            ; dynamic_length= None
            ; subtype= Subtype.exact
            ; nullable= false }
      | Java ->
          let object_type = StdTyp.Name.Java.java_lang_string in
          Exp.Sizeof
            { typ= Typ.mk (Tstruct object_type)
            ; nbytes= None
            ; dynamic_length= None
            ; subtype= Subtype.exact
            ; nullable= false }
      | CIL ->
          (* cil todo *)
          (* Logging.die Logging.InternalError "No string constant support for CIL yet." *)
          let object_type = Typ.Name.CSharp.from_string "System.String" in
          Exp.Sizeof
            { typ= Typ.mk (Tstruct object_type)
            ; nbytes= None
            ; dynamic_length= None
            ; subtype= Subtype.exact
            ; nullable= false }
      | Erlang ->
          L.die InternalError "Erlang not supported"
      | Hack ->
          L.die InternalError "Hack not supported"
      | Python ->
          L.die InternalError "Python not supported"
    in
    Predicates.Hpointsto (root, sexp, const_string_texp)
  in
  let mk_constant_class_hpred s =
    (* create an hpred from a constant class *)
    let root = Exp.Const (Const.Cclass (Ident.string_to_name s)) in
    let sexp =
      (* TODO: add appropriate fields *)
      Predicates.Estruct
        ( [ ( Fieldname.make StdTyp.Name.Java.java_lang_class "name"
            , Predicates.Eexp (Exp.Const (Const.Cstr s), Predicates.Inone) ) ]
        , Predicates.inst_none )
    in
    let class_texp =
      let class_type = StdTyp.Name.Java.java_lang_class in
      Exp.Sizeof
        { typ= Typ.mk (Tstruct class_type)
        ; nbytes= None
        ; dynamic_length= None
        ; subtype= Subtype.exact
        ; nullable= false }
    in
    Predicates.Hpointsto (root, sexp, class_texp)
  in
  try
    match move_primed_lhs_from_front subs sigma2 with
    | [] ->
        L.d_strln "Final Implication" ;
        d_impl subs (prop1, Prop.prop_emp) ;
        (subs, prop1)
    | hpred2 :: sigma2' -> (
        L.d_strln "Current Implication" ;
        d_impl subs (prop1, Prop.normalize tenv (Prop.from_sigma (hpred2 :: sigma2'))) ;
        L.d_ln () ;
        L.d_ln () ;
        let normal_case hpred2' =
          let subs', prop1' =
            try
              L.d_increase_indent () ;
              let res =
                decrease_indent_when_exception (fun () ->
                    hpred_imply tenv calc_index_frame calc_missing subs prop1 sigma2 hpred2' )
              in
              L.d_decrease_indent () ;
              res
            with IMPL_EXC _ when calc_missing -> (
              match is_constant_string_class subs hpred2' with
              | Some (s, is_string) ->
                  (* allocate constant string hpred1', do implication, then add hpred1' as missing *)
                  let hpred1' =
                    if is_string then mk_constant_string_hpred s else mk_constant_class_hpred s
                  in
                  let prop1' =
                    Prop.normalize tenv (Prop.set prop1 ~sigma:(hpred1' :: prop1.Prop.sigma))
                  in
                  let subs', frame_prop =
                    hpred_imply tenv calc_index_frame calc_missing subs prop1' sigma2 hpred2'
                  in
                  (* ProverState.add_missing_sigma [hpred1']; *)
                  (subs', frame_prop)
              | None ->
                  let subs' =
                    match hpred2' with
                    | Predicates.Hpointsto (e2, se2, te2) ->
                        let typ2 = Exp.texp_to_typ (Some StdTyp.void) te2 in
                        sexp_imply_nolhs tenv e2 calc_missing subs se2 typ2
                    | _ ->
                        subs
                  in
                  ProverState.add_missing_sigma [hpred2'] ;
                  (subs', prop1) )
          in
          L.d_increase_indent () ;
          let res =
            decrease_indent_when_exception (fun () ->
                sigma_imply tenv calc_index_frame calc_missing subs' prop1' sigma2' )
          in
          L.d_decrease_indent () ;
          res
        in
        match hpred2 with
        | Predicates.Hpointsto (e2_, se2, t) ->
            let changed, calc_index_frame', hpred2' =
              expand_hpred_pointer tenv calc_index_frame
                (Predicates.Hpointsto (Prop.exp_normalize_noabs tenv (snd subs) e2_, se2, t))
            in
            if changed then
              sigma_imply tenv calc_index_frame' calc_missing subs prop1 (hpred2' :: sigma2')
              (* calc_index_frame=true *)
            else normal_case hpred2'
        | _ ->
            normal_case hpred2 )
  with IMPL_EXC (s, _, _) when calc_missing ->
    L.d_printfln "Adding rhs as missing: %s" s ;
    ProverState.add_missing_sigma sigma2 ;
    (subs, prop1)


let prepare_prop_for_implication tenv (_, sub2) pi1 sigma1 =
  let pi1' = Prop.pi_sub sub2 (ProverState.get_missing_pi ()) @ pi1 in
  let sigma1' = Prop.sigma_sub sub2 (ProverState.get_missing_sigma ()) @ sigma1 in
  let ep = Prop.set Prop.prop_emp ~sub:sub2 ~sigma:sigma1' ~pi:pi1' in
  Prop.normalize tenv ep


let imply_pi tenv calc_missing (sub1, sub2) prop pi2 =
  let do_atom a =
    let a' = Predicates.atom_sub sub2 a in
    try
      if not (check_atom tenv prop a') then
        raise (IMPL_EXC ("rhs atom missing in lhs", (sub1, sub2), EXC_FALSE_ATOM a'))
    with IMPL_EXC _ when calc_missing ->
      L.d_str "imply_pi: adding missing atom " ;
      Predicates.d_atom a ;
      L.d_ln () ;
      ProverState.add_missing_pi a
  in
  List.iter ~f:do_atom pi2


let imply_atom tenv calc_missing (sub1, sub2) prop a =
  imply_pi tenv calc_missing (sub1, sub2) prop [a]


(** Check pure implications before looking at the spatial part. Add necessary instantiations for
    equalities and check that instantiations are possible for disequalities. *)
let rec pre_check_pure_implication tenv calc_missing (subs : subst2) pi1 pi2 =
  match (pi2 : Predicates.atom list) with
  | [] ->
      subs
  | (Aeq (e2_in, f2_in) as a) :: pi2' when not (Prop.atom_is_inequality a) -> (
      let e2, f2 = (Predicates.exp_sub (snd subs) e2_in, Predicates.exp_sub (snd subs) f2_in) in
      if Exp.equal e2 f2 then pre_check_pure_implication tenv calc_missing subs pi1 pi2'
      else
        match (e2, f2) with
        | Exp.Var v2, f2 when Ident.is_primed v2 (* && not (Predicates.mem_sub v2 (snd subs)) *) ->
            (* The commented-out condition should always hold. *)
            let sub2' = extend_sub (snd subs) v2 f2 in
            pre_check_pure_implication tenv calc_missing (fst subs, sub2') pi1 pi2'
        | e2, Exp.Var v2 when Ident.is_primed v2 (* && not (Predicates.mem_sub v2 (snd subs)) *) ->
            (* The commented-out condition should always hold. *)
            let sub2' = extend_sub (snd subs) v2 e2 in
            pre_check_pure_implication tenv calc_missing (fst subs, sub2') pi1 pi2'
        | _ ->
            let pi1' = Prop.pi_sub (fst subs) pi1 in
            let prop_for_impl = prepare_prop_for_implication tenv subs pi1' [] in
            imply_atom tenv calc_missing subs prop_for_impl (Predicates.Aeq (e2_in, f2_in)) ;
            pre_check_pure_implication tenv calc_missing subs pi1 pi2' )
  | (Aneq (e, _) | Apred (_, e :: _) | Anpred (_, e :: _)) :: _
    when (not calc_missing) && match e with Var v -> not (Ident.is_primed v) | _ -> true ->
      raise
        (IMPL_EXC
           ( "ineq e2=f2 in rhs with e2 not primed var"
           , (Predicates.sub_empty, Predicates.sub_empty)
           , EXC_FALSE ) )
  | (Aeq _ | Aneq _ | Apred _ | Anpred _) :: pi2' ->
      pre_check_pure_implication tenv calc_missing subs pi1 pi2'


(** Perform the array bound checks delayed (to instantiate variables) by the prover. If there is a
    provable violation of the array bounds, set the prover status to Bounds_check and make the proof
    fail. *)
let check_array_bounds tenv (sub1, sub2) prop =
  let check_failed atom =
    ProverState.checks := Bounds_check :: !ProverState.checks ;
    L.d_str ~color:Red "bounds_check failed: provable atom: " ;
    Predicates.d_atom atom ;
    L.d_ln () ;
    if not Config.bound_error_allowed_in_procedure_call then
      raise (IMPL_EXC ("bounds check", (sub1, sub2), EXC_FALSE))
  in
  let fail_if_le e' e'' =
    let lt_ineq = Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, e', e'')) in
    if check_atom tenv prop lt_ineq then check_failed lt_ineq
  in
  let check_bound = function
    | ProverState.BClen_imply (len1_, len2_, _indices2) ->
        let len1 = Predicates.exp_sub sub1 len1_ in
        let len2 = Predicates.exp_sub sub2 len2_ in
        (* L.d_strln ~color:Orange "check_bound ";
           Exp.d_exp len1; L.d_str " "; Exp.d_exp len2; L.d_ln(); *)
        let indices_to_check =
          [Exp.BinOp (Binop.PlusA None, len2, Exp.minus_one)]
          (* only check len *)
        in
        List.iter ~f:(fail_if_le len1) indices_to_check
    | ProverState.BCfrom_pre atom_ ->
        let atom_neg = atom_negate tenv (Predicates.atom_sub sub2 atom_) in
        (* L.d_strln ~color:Orange "BCFrom_pre"; Predicates.d_atom atom_neg; L.d_ln (); *)
        if check_atom tenv prop atom_neg then check_failed atom_neg
  in
  List.iter ~f:check_bound (ProverState.get_bounds_checks ())


(** [check_implication_base] returns true if [prop1|-prop2], ignoring the footprint part of the
    props *)
let check_implication_base {InterproceduralAnalysis.proc_desc; err_log; tenv} check_frame_empty
    calc_missing prop1 prop2 =
  try
    ProverState.reset prop1 prop2 ;
    let filter (id, e) =
      Ident.is_normal id && Exp.free_vars e |> Sequence.for_all ~f:Ident.is_normal
    in
    let sub1_base = Predicates.sub_filter_pair ~f:filter prop1.Prop.sub in
    let pi1, pi2 = (Prop.get_pure prop1, Prop.get_pure prop2) in
    let sigma1, sigma2 = (prop1.Prop.sigma, prop2.Prop.sigma) in
    let subs = pre_check_pure_implication tenv calc_missing (prop1.Prop.sub, sub1_base) pi1 pi2 in
    let pi2_bcheck, pi2_nobcheck =
      (* find bounds checks implicit in pi2 *)
      List.partition_tf ~f:ProverState.atom_is_array_bounds_check pi2
    in
    List.iter ~f:(fun a -> ProverState.add_bounds_check (ProverState.BCfrom_pre a)) pi2_bcheck ;
    L.d_strln "pre_check_pure_implication" ;
    L.d_strln "pi1:" ;
    L.d_increase_indent () ;
    Prop.d_pi pi1 ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_strln "pi2:" ;
    L.d_increase_indent () ;
    Prop.d_pi pi2 ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    if not (List.is_empty pi2_bcheck) then (
      L.d_str "pi2 bounds checks: " ;
      Prop.d_pi pi2_bcheck ;
      L.d_ln () ) ;
    L.d_strln "returns" ;
    L.d_strln "sub1:" ;
    L.d_increase_indent () ;
    Prop.d_sub (fst subs) ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_strln "sub2:" ;
    L.d_increase_indent () ;
    Prop.d_sub (snd subs) ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    let (sub1, sub2), frame_prop = sigma_imply tenv false calc_missing subs prop1 sigma2 in
    let pi1' = Prop.pi_sub sub1 pi1 in
    let sigma1' = Prop.sigma_sub sub1 sigma1 in
    L.d_ln () ;
    let prop_for_impl = prepare_prop_for_implication tenv (sub1, sub2) pi1' sigma1' in
    (* only deal with pi2 without bound checks *)
    imply_pi tenv calc_missing (sub1, sub2) prop_for_impl pi2_nobcheck ;
    (* handle implicit bound checks, plus those from array_len_imply *)
    check_array_bounds tenv (sub1, sub2) prop_for_impl ;
    L.d_strln "Result of Abduction" ;
    L.d_increase_indent () ;
    d_impl (sub1, sub2) (prop1, prop2) ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_strln "returning TRUE" ;
    let frame = frame_prop.Prop.sigma in
    if check_frame_empty && not (List.is_empty frame) then
      raise (IMPL_EXC ("frame not empty", subs, EXC_FALSE)) ;
    Some ((sub1, sub2), frame)
  with
  | IMPL_EXC (s, subs, body) ->
      d_impl_err (s, subs, body) ;
      None
  | MISSING_EXC s ->
      L.d_printfln "WARNING: footprint failed to find MISSING because: %s" s ;
      None
  | Exceptions.Abduction_case_not_implemented _ as exn ->
      BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log exn ;
      None


type implication_result =
  | ImplOK of
      ( check list
      * Predicates.subst
      * Predicates.subst
      * Predicates.hpred list
      * Predicates.atom list
      * Predicates.hpred list
      * Predicates.hpred list
      * Predicates.hpred list
      * (Exp.t * Exp.t) list
      * (Exp.t * Exp.t) list )
  | ImplFail of check list

(** [check_implication_for_footprint p1 p2] returns [Some(sub, frame, missing)] if
    [sub(p1 * missing) |- sub(p2 * frame)] where [sub] is a substitution which instantiates the
    primed vars of [p1] and [p2], which are assumed to be disjoint. *)
let check_implication_for_footprint analysis_data p1 (p2 : Prop.exposed Prop.t) =
  let check_frame_empty = false in
  let calc_missing = true in
  match check_implication_base analysis_data check_frame_empty calc_missing p1 p2 with
  | Some ((sub1, sub2), frame) ->
      ImplOK
        ( !ProverState.checks
        , sub1
        , sub2
        , frame
        , ProverState.get_missing_pi ()
        , ProverState.get_missing_sigma ()
        , ProverState.get_frame_fld ()
        , ProverState.get_missing_fld ()
        , ProverState.get_frame_typ ()
        , ProverState.get_missing_typ () )
  | None ->
      ImplFail !ProverState.checks


(** [check_implication p1 p2] returns true if [p1|-p2] *)
let check_implication ({InterproceduralAnalysis.tenv; _} as analysis_data) p1 p2 =
  let check p1 p2 =
    let check_frame_empty = true in
    let calc_missing = false in
    match check_implication_base analysis_data check_frame_empty calc_missing p1 p2 with
    | Some _ ->
        true
    | None ->
        false
  in
  check p1 p2
  &&
  if !BiabductionConfig.footprint then
    check (Prop.normalize tenv (Prop.extract_footprint p1)) (Prop.extract_footprint p2)
  else true


(** {2 Cover: miminum set of pi's whose disjunction is equivalent to true} *)

(** check if the pi's in [cases] cover true *)
let is_cover tenv cases =
  let cnt = ref 0 in
  (* counter for timeout checks, as this function can take exponential time *)
  let check () =
    incr cnt ;
    if Int.equal (!cnt mod 100) 0 then SymOp.check_wallclock_alarm ()
  in
  let rec is_cover_ acc_pi cases =
    check () ;
    match cases with
    | [] ->
        check_inconsistency_pi tenv acc_pi
    | (pi, _) :: cases' ->
        List.for_all ~f:(fun a -> is_cover_ (atom_negate tenv a :: acc_pi) cases') pi
  in
  is_cover_ [] cases


exception NO_COVER

(** Find miminum set of pi's in [cases] whose disjunction covers true *)
let find_minimum_pure_cover tenv cases =
  let cases =
    let compare (pi1, _) (pi2, _) = Int.compare (List.length pi1) (List.length pi2) in
    List.sort ~compare cases
  in
  let rec grow seen todo =
    match todo with
    | [] ->
        raise NO_COVER
    | (pi, x) :: todo' ->
        if is_cover tenv ((pi, x) :: seen) then (pi, x) :: seen else grow ((pi, x) :: seen) todo'
  in
  let rec shrink_ seen todo =
    match todo with
    | [] ->
        seen
    | (pi, x) :: todo' ->
        if is_cover tenv (seen @ todo') then shrink_ seen todo' else shrink_ ((pi, x) :: seen) todo'
  in
  let shrink cases = if List.length cases > 2 then shrink_ [] cases else cases in
  try Some (shrink (grow [] cases)) with NO_COVER -> None
