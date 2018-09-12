(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Subtypes *)

open! IStd
module F = Format

let list_to_string list =
  if List.is_empty list then "( sub )"
  else "- {" ^ String.concat ~sep:", " (List.map ~f:Typ.Name.name list) ^ "}"


type t' = Exact  (** denotes the current type only *) | Subtypes of Typ.Name.t list
[@@deriving compare]

let equal_modulo_flag (st1, _) (st2, _) = [%compare.equal: t'] st1 st2

(** denotes the current type and a list of types that are not their subtypes  *)
type kind = CAST | INSTOF | NORMAL [@@deriving compare]

let equal_kind = [%compare.equal: kind]

type t = t' * kind [@@deriving compare]

type result = No | Unknown | Yes [@@deriving compare]

let equal_result = [%compare.equal: result]

let max_result res1 res2 = if compare_result res1 res2 <= 0 then res2 else res1

let is_interface tenv (class_name : Typ.Name.t) =
  match (class_name, Tenv.lookup tenv class_name) with
  | JavaClass _, Some {fields= []; methods= []} ->
      true
  | _ ->
      false


let is_root_class class_name =
  match class_name with
  | Typ.JavaClass _ ->
      Typ.Name.equal class_name Typ.Name.Java.java_lang_object
  | _ ->
      false


(** check if c1 is a subclass of c2 *)
let check_subclass_tenv tenv c1 c2 : result =
  let rec loop best_result classnames : result =
    (* Check if the name c2 is found in the list of super types and
       keep the best results according to Yes > Unknown > No *)
    if equal_result best_result Yes then Yes
    else
      match classnames with
      | [] ->
          best_result
      | cn :: cns ->
          loop (max_result best_result (check cn)) cns
  and check cn : result =
    if Typ.Name.equal cn c2 then Yes
    else
      match Tenv.lookup tenv cn with
      | None when is_root_class cn ->
          No
      | None ->
          Unknown
      | Some {supers} ->
          loop No supers
  in
  if is_root_class c2 then Yes else check c1


module SubtypesMap = Caml.Map.Make (struct
  (* pair of subtypes *)
  type t = Typ.Name.t * Typ.Name.t [@@deriving compare]
end)

let check_subtype =
  let subtMap = ref SubtypesMap.empty in
  fun tenv c1 c2 ->
    ( try SubtypesMap.find (c1, c2) !subtMap with Caml.Not_found ->
        let is_subt = check_subclass_tenv tenv c1 c2 in
        subtMap := SubtypesMap.add (c1, c2) is_subt !subtMap ;
        is_subt
      : result )


let is_known_subtype tenv c1 c2 : bool = equal_result (check_subtype tenv c1 c2) Yes

let flag_to_string flag = match flag with CAST -> "(cast)" | INSTOF -> "(instof)" | NORMAL -> ""

let pp f (t, flag) =
  if Config.print_types then
    match t with
    | Exact ->
        F.pp_print_string f (flag_to_string flag)
    | Subtypes list ->
        F.fprintf f "%s%s" (list_to_string list) (flag_to_string flag)


let exact = (Exact, NORMAL)

let all_subtypes = Subtypes []

let subtypes = (all_subtypes, NORMAL)

let subtypes_cast = (all_subtypes, CAST)

let subtypes_instof = (all_subtypes, INSTOF)

let is_cast t = equal_kind (snd t) CAST

let is_instof t = equal_kind (snd t) INSTOF

let list_intersect equal l1 l2 =
  let in_l2 a = List.mem ~equal l2 a in
  List.filter ~f:in_l2 l1


let join_flag flag1 flag2 =
  match (flag1, flag2) with CAST, _ -> CAST | _, CAST -> CAST | _, _ -> NORMAL


let join (s1, flag1) (s2, flag2) =
  let s =
    match (s1, s2) with
    | Exact, _ ->
        s2
    | _, Exact ->
        s1
    | Subtypes l1, Subtypes l2 ->
        Subtypes (list_intersect Typ.Name.equal l1 l2)
  in
  let flag = join_flag flag1 flag2 in
  (s, flag)


let update_flag c1 c2 flag flag' =
  match flag with INSTOF -> if Typ.Name.equal c1 c2 then flag else flag' | _ -> flag'


let change_flag st_opt c1 c2 flag' =
  match st_opt with
  | Some st -> (
    match st with
    | Exact, flag ->
        let new_flag = update_flag c1 c2 flag flag' in
        Some (Exact, new_flag)
    | Subtypes t, flag ->
        let new_flag = update_flag c1 c2 flag flag' in
        Some (Subtypes t, new_flag) )
  | None ->
      None


let normalize_subtypes t_opt c1 c2 flag1 flag2 =
  let new_flag = update_flag c1 c2 flag1 flag2 in
  match t_opt with
  | Some t -> (
    match t with
    | Exact ->
        Some (t, new_flag)
    | Subtypes l ->
        Some (Subtypes (List.sort ~compare:Typ.Name.compare l), new_flag) )
  | None ->
      None


(* c is a subtype when it does not appear in the list l of no-subtypes *)
let no_subtype_in_list tenv c l = not (List.exists ~f:(is_known_subtype tenv c) l)

let is_strict_subtype tenv c1 c2 = is_known_subtype tenv c1 c2 && not (Typ.Name.equal c1 c2)

(* checks for redundancies when adding c to l
   Xi in A - { X1,..., Xn } is redundant in two cases:
   1) not (Xi <: A) because removing the subtypes of Xi has no effect unless Xi is a subtype of A
   2) Xi <: Xj because the subtypes of Xi are a subset of the subtypes of Xj *)
let check_redundancies tenv c l =
  let aux (l, add) ci =
    let l, should_add =
      if is_known_subtype tenv ci c then (l, true)
      else if is_known_subtype tenv c ci then (ci :: l, false)
      else (ci :: l, true)
    in
    (l, add && should_add)
  in
  List.fold ~f:aux ~init:([], true) l


let rec updates_head f c l =
  match l with
  | [] ->
      []
  | ci :: rest ->
      if is_strict_subtype f ci c then ci :: updates_head f c rest else updates_head f c rest


(* adds the classes of l2 to l1 and checks that no redundancies or inconsistencies will occur
   A - { X1,..., Xn } is inconsistent if A <: Xi for some i *)
let rec add_not_subtype tenv c1 l1 l2 =
  match l2 with
  | [] ->
      l1
  | c :: rest ->
      if is_known_subtype tenv c1 c then add_not_subtype tenv c1 l1 rest
      else
        (* checks for inconsistencies *)
        let l1', should_add = check_redundancies tenv c l1 in
        (* checks for redundancies *)
        let rest' = add_not_subtype tenv c1 l1' rest in
        if should_add then c :: rest' else rest'


let get_subtypes tenv (c1, ((st1, flag1) : t)) (c2, ((st2, flag2) : t)) =
  let is_sub = is_known_subtype tenv c1 c2 in
  let pos_st, neg_st =
    match (st1, st2) with
    | Exact, Exact ->
        if is_sub then (Some st1, None) else (None, Some st1)
    | Exact, Subtypes l2 ->
        if is_sub && no_subtype_in_list tenv c1 l2 then (Some st1, None) else (None, Some st1)
    | Subtypes l1, Exact ->
        if is_sub then (Some st1, None)
        else
          let l1' = updates_head tenv c2 l1 in
          if no_subtype_in_list tenv c2 l1 then
            (Some (Subtypes l1'), Some (Subtypes (add_not_subtype tenv c1 l1 [c2])))
          else (None, Some st1)
    | Subtypes l1, Subtypes l2 ->
        if is_interface tenv c2 || is_sub then
          if no_subtype_in_list tenv c1 l2 then
            let l2' = updates_head tenv c1 l2 in
            (Some (Subtypes (add_not_subtype tenv c1 l1 l2')), None)
          else (None, Some st1)
        else if
          (is_interface tenv c1 || is_known_subtype tenv c2 c1) && no_subtype_in_list tenv c2 l1
        then
          let l1' = updates_head tenv c2 l1 in
          ( Some (Subtypes (add_not_subtype tenv c2 l1' l2))
          , Some (Subtypes (add_not_subtype tenv c1 l1 [c2])) )
        else (None, Some st1)
  in
  (normalize_subtypes pos_st c1 c2 flag1 flag2, normalize_subtypes neg_st c1 c2 flag1 flag2)


let case_analysis_basic tenv (c1, st) (c2, (_, flag2)) =
  let pos_st, neg_st =
    if is_known_subtype tenv c1 c2 then (Some st, None)
    else if is_known_subtype tenv c2 c1 then
      match st with
      | Exact, _ ->
          if Typ.Name.equal c1 c2 then (Some st, None) else (None, Some st)
      | Subtypes _, _ ->
          if Typ.Name.equal c1 c2 then (Some st, None) else (Some st, Some st)
    else (None, Some st)
  in
  (change_flag pos_st c1 c2 flag2, change_flag neg_st c1 c2 flag2)


(** [case_analysis (c1, st1) (c2, st2) f] performs case analysis on [c1 <: c2]
    according to [st1] and [st2]
    where f c1 c2 is true if c1 is a subtype of c2.
    get_subtypes returning a pair:
    - whether [st1] and [st2] admit [c1 <: c2], and in case return the updated subtype [st1]
    - whether [st1] and [st2] admit [not(c1 <: c2)],
    and in case return the updated subtype [st1] *)
let case_analysis tenv (c1, st1) (c2, st2) =
  if Config.subtype_multirange then get_subtypes tenv (c1, st1) (c2, st2)
  else case_analysis_basic tenv (c1, st1) (c2, st2)
