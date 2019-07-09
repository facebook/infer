(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Equality over uninterpreted functions and linear rational arithmetic *)

type 'a exp_map = 'a Map.M(Exp).t [@@deriving compare, equal, sexp]

let empty_map = Map.empty (module Exp)

type subst = Exp.t exp_map [@@deriving compare, equal, sexp]

(** see also [invariant] *)
type t =
  { sat: bool  (** [false] only if constraints are inconsistent *)
  ; rep: subst
        (** functional set of oriented equations: map [a] to [a'],
            indicating that [a = a'] holds, and that [a'] is the
            'rep(resentative)' of [a] *) }
[@@deriving compare, equal, sexp]

let classes r =
  Map.fold r.rep ~init:empty_map ~f:(fun ~key ~data cls ->
      if Exp.equal key data then cls
      else Map.add_multi cls ~key:data ~data:key )

(** Pretty-printing *)

let pp fs {sat; rep} =
  let pp_alist pp_k pp_v fs alist =
    let pp_assoc fs (k, v) =
      Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_k k pp_v (k, v)
    in
    Format.fprintf fs "[@[<hv>%a@]]" (List.pp ";@ " pp_assoc) alist
  in
  let pp_exp_v fs (k, v) = if not (Exp.equal k v) then Exp.pp fs v in
  Format.fprintf fs "@[{@[<hv>sat= %b;@ rep= %a@]}@]" sat
    (pp_alist Exp.pp pp_exp_v)
    (Map.to_alist rep)

let pp_classes ?is_x fs r =
  List.pp "@ @<2>∧ "
    (fun fs (key, data) ->
      Format.fprintf fs "@[%a@ = %a@]" (Exp.pp_full ?is_x) key
        (List.pp "@ = " (Exp.pp_full ?is_x))
        (List.sort ~compare:Exp.compare data) )
    fs
    (Map.to_alist (classes r))

let pp_diff fs (r, s) =
  let pp_sdiff_map pp_elt_diff equal nam fs x y =
    let sd = Sequence.to_list (Map.symmetric_diff ~data_equal:equal x y) in
    if not (List.is_empty sd) then
      Format.fprintf fs "%s= [@[<hv>%a@]];@ " nam
        (List.pp ";@ " pp_elt_diff)
        sd
  in
  let pp_sdiff_elt pp_key pp_val pp_sdiff_val fs = function
    | k, `Left v ->
        Format.fprintf fs "-- [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
    | k, `Right v ->
        Format.fprintf fs "++ [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
    | k, `Unequal vv ->
        Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_key k pp_sdiff_val vv
  in
  let pp_sdiff_exp_map =
    let pp_sdiff_exp fs (u, v) =
      Format.fprintf fs "-- %a ++ %a" Exp.pp u Exp.pp v
    in
    pp_sdiff_map (pp_sdiff_elt Exp.pp Exp.pp pp_sdiff_exp) Exp.equal
  in
  let pp_sat fs =
    if not (Bool.equal r.sat s.sat) then
      Format.fprintf fs "sat= @[-- %b@ ++ %b@];@ " r.sat s.sat
  in
  let pp_rep fs = pp_sdiff_exp_map "rep" fs r.rep s.rep in
  Format.fprintf fs "@[{@[<hv>%t%t@]}@]" pp_sat pp_rep

(** Invariant *)

(** test membership in carrier *)
let in_car r e = Map.mem r.rep e

let rec iter_max_solvables e ~f =
  match Exp.classify e with
  | `Interpreted -> Exp.iter ~f:(iter_max_solvables ~f) e
  | _ -> f e

let invariant r =
  Invariant.invariant [%here] r [%sexp_of: t]
  @@ fun () ->
  Map.iteri r.rep ~f:(fun ~key:a ~data:_ ->
      (* no interpreted exps in carrier *)
      assert (Poly.(Exp.classify a <> `Interpreted)) ;
      (* carrier is closed under sub-expressions *)
      iter_max_solvables a ~f:(fun b ->
          assert (
            in_car r b
            || Trace.fail "@[subexp %a of %a not in carrier of@ %a@]" Exp.pp
                 b Exp.pp a pp r ) ) )

(** Core operations *)

let true_ = {sat= true; rep= empty_map} |> check invariant

(** apply a subst to an exp *)
let apply s a = try Map.find_exn s a with Caml.Not_found -> a

(** apply a subst to maximal non-interpreted subexps *)
let rec norm s a =
  match Exp.classify a with
  | `Interpreted -> Exp.map ~f:(norm s) a
  | `Simplified -> apply s (Exp.map ~f:(norm s) a)
  | `Atomic | `Uninterpreted -> apply s a

(** exps are congruent if equal after normalizing subexps *)
let congruent r a b =
  Exp.equal (Exp.map ~f:(norm r.rep) a) (Exp.map ~f:(norm r.rep) b)

(** [lookup r a] is [b'] if [a ~ b = b'] for some equation [b = b'] in rep *)
let lookup r a =
  With_return.with_return
  @@ fun {return} ->
  (* congruent specialized to assume [a] canonized and [b] non-interpreted *)
  let semi_congruent r a b = Exp.equal a (Exp.map ~f:(apply r.rep) b) in
  Map.iteri r.rep ~f:(fun ~key ~data ->
      if semi_congruent r a key then return data ) ;
  a

(** rewrite an exp into canonical form using rep and, for uninterpreted
    exps, congruence composed with rep *)
let rec canon r a =
  match Exp.classify a with
  | `Interpreted -> Exp.map ~f:(canon r) a
  | `Simplified | `Uninterpreted -> lookup r (Exp.map ~f:(canon r) a)
  | `Atomic -> apply r.rep a

(** add an exp to the carrier *)
let rec extend a r =
  match Exp.classify a with
  | `Interpreted | `Simplified -> Exp.fold ~f:extend a ~init:r
  | `Uninterpreted ->
      Map.find_or_add r.rep a
        ~if_found:(fun _ -> r)
        ~default:a
        ~if_added:(fun rep -> Exp.fold ~f:extend a ~init:{r with rep})
  | `Atomic -> r

let extend a r = extend a r |> check invariant

let compose r s =
  let rep = Map.map ~f:(norm s) r.rep in
  let rep =
    Map.merge_skewed rep s ~combine:(fun ~key v1 v2 ->
        if Exp.equal v1 v2 then v1
        else fail "domains intersect: %a" Exp.pp key () )
  in
  {r with rep}

let merge a b r =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Exp.pp a Exp.pp b pp r]
  ;
  ( match Exp.solve a b with
  | Some s -> compose r s
  | None -> {r with sat= false} )
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

(** find an unproved equation between congruent exps *)
let find_missing r =
  With_return.with_return
  @@ fun {return} ->
  Map.iteri r.rep ~f:(fun ~key:a ~data:a' ->
      Map.iteri r.rep ~f:(fun ~key:b ~data:b' ->
          if
            Exp.compare a b < 0
            && (not (Exp.equal a' b'))
            && congruent r a b
          then return (Some (a', b')) ) ) ;
  None

let rec close r =
  if not r.sat then r
  else
    match find_missing r with
    | Some (a', b') -> close (merge a' b' r)
    | None -> r

let close r =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  close r
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let and_eq a b r =
  if not r.sat then r
  else
    let a' = canon r a in
    let b' = canon r b in
    let r = extend a' r in
    let r = extend b' r in
    if Exp.equal a' b' then r else close (merge a' b' r)

(** Exposed interface *)

let is_true {sat; rep} =
  sat && Map.for_alli rep ~f:(fun ~key:a ~data:a' -> Exp.equal a a')

let is_false {sat} = not sat
let entails_eq r d e = Exp.equal (canon r d) (canon r e)

let entails r s =
  Map.for_alli s.rep ~f:(fun ~key:e ~data:e' -> entails_eq r e e')

let normalize = canon

let class_of r e =
  let e' = normalize r e in
  e' :: Map.find_multi (classes r) e'

let difference r a b =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Exp.pp a Exp.pp b pp r]
  ;
  let a = canon r a in
  let b = canon r b in
  ( if Exp.equal a b then Some Z.zero
  else
    match (Exp.typ a, Exp.typ b) with
    | Some typ, _ | _, Some typ -> (
      match normalize r (Exp.sub typ a b) with
      | Integer {data} -> Some data
      | _ -> None )
    | _ -> None )
  |>
  [%Trace.retn fun {pf} ->
    function Some d -> pf "%a" Z.pp_print d | None -> pf ""]

let and_ r s =
  if not r.sat then r
  else if not s.sat then s
  else
    let s, r =
      if Map.length s.rep <= Map.length r.rep then (s, r) else (r, s)
    in
    Map.fold s.rep ~init:r ~f:(fun ~key:e ~data:e' r -> and_eq e e' r)

let or_ r s =
  if not s.sat then r
  else if not r.sat then s
  else
    let merge_mems rs r s =
      Map.fold s.rep ~init:rs ~f:(fun ~key:e ~data:e' rs ->
          if entails_eq r e e' then and_eq e e' rs else rs )
    in
    let rs = true_ in
    let rs = merge_mems rs r s in
    let rs = merge_mems rs s r in
    rs

(* assumes that f is injective and for any set of exps E, f[E] is disjoint
   from E *)
let map_exps ({sat= _; rep} as r) ~f =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  let map m =
    Map.fold m ~init:m ~f:(fun ~key ~data m ->
        let key' = f key in
        let data' = f data in
        if Exp.equal key' key then
          if Exp.equal data' data then m else Map.set m ~key ~data:data'
        else Map.remove m key |> Map.add_exn ~key:key' ~data:data' )
  in
  let rep' = map rep in
  (if rep' == rep then r else {r with rep= rep'})
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let rename r sub = map_exps r ~f:(fun e -> Exp.rename e sub)

let fold_exps r ~init ~f =
  Map.fold r.rep ~f:(fun ~key ~data z -> f (f z data) key) ~init

let fold_vars r ~init ~f =
  fold_exps r ~init ~f:(fun init -> Exp.fold_vars ~f ~init)

let fv e = fold_vars e ~f:Set.add ~init:Var.Set.empty
