(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Congruence closure with integer offsets *)

(** For background, see:

    Robert Nieuwenhuis, Albert Oliveras: Fast congruence closure and
    extensions. Inf. Comput. 205(4): 557-580 (2007)

    and, for a more detailed correctness proof of the case without integer
    offsets, see section 5 of:

    Aleksandar Nanevski, Viktor Vafeiadis, Josh Berdine: Structuring the
    verification of heap-manipulating programs. POPL 2010: 261-274 *)

(** Lazy flattening:

    The congruence closure data structure is used to lazily flatten
    expressions. Flattening expressions gives each compound expression (e.g.
    an application) a "name", which is treated as an atomic symbol. In the
    background papers, fresh symbols are introduced to name compound
    expressions in a pre-processing pass, but here we do not a priori know
    the carrier (set of all expressions equations might relate). Instead, we
    use the expression itself as its "name" and use the representative map
    to record this naming. That is, if [f(a+i)] is in the domain of the
    representative map, then "f(a+i)" is the name of the compound expression
    [f(a+i)]. If [f(a+i)] is not in the domain of the representative map,
    then it is not yet in the "carrier" of the relation. Adding it to the
    carrier, which logically amounts to adding the equation [f(a+i) =
    "f(a+i)"], extends the representative map, as well as the lookup map and
    use lists, after which [f(a+i)] can be used as if it was a simple symbol
    name for the compound expression.

    Note that merging a compound equation of the form [f(a+i)+j = b+k]
    results in naming [f(a+i)] and then merging the simple equation
    ["f(a+i)"+j = b+k] normalized to ["f(a+i)" = b+(k-j)]. In particular,
    every equation is either of the form ["f(a+i)" = f(a+i)] or of the form
    [a = b+i], but not of the form [f(a+i) = b+j].

    By the same reasoning, the range of the lookup table does not need
    offsets, as every exp in the range of the lookup table will be the name
    of a compound exp.

    A consequence of lazy flattening is that the equations stored in the
    lookup table, use lists, and pending equation list in the background
    papers are here all of the form [f(a+i) = "f(a+i)"], and hence are
    represented by the application expression itself.

    Sparse carrier:

    For symbols, that is expressions that are not compound [App]lications,
    there are no cooperative invariants between components of the data
    structure that need to be established. So adding a symbol to the carrier
    would amount to adding an identity association to the representatives
    map. Since we need to use a map instead of an array anyhow, this can be
    represented sparsely by omitting identity associations in the
    representative map. Note that identity associations for compound
    expressions are still needed to record which compound expressions have
    been added to the carrier.

    Notation:

    - often use identifiers such as [a'] for the representative of [a] *)

(** set of exps representing congruence classes *)
module Cls = struct
  type t = Exp.t list [@@deriving compare, sexp]

  let equal = [%compare.equal: t]

  let pp fs cls =
    Format.fprintf fs "@[<hov 1>{@[%a@]}@]" (List.pp ",@ " Exp.pp)
      (List.sort ~compare:Exp.compare cls)

  let singleton e = [e]
  let add cls exp = exp :: cls
  let remove_exn = List.remove_exn
  let union = List.rev_append
  let fold_map = List.fold_map
  let iter = List.iter
  let is_empty = List.is_empty
  let length = List.length
  let map = List.map_preserving_phys_equal
  let mem = List.mem ~equal:Exp.equal
end

(** set of exps representing "use lists" encoding super-expression relation *)
module Use = struct
  type t = Exp.t list [@@deriving compare, sexp]

  let equal = [%compare.equal: t]

  let pp fs uses =
    Format.fprintf fs "@[<hov 1>{@[%a@]}@]" (List.pp ",@ " Exp.pp) uses

  let empty = []
  let singleton use = [use]
  let add uses use = use :: uses
  let union = List.rev_append
  let fold = List.fold
  let iter = List.iter
  let exists = List.exists
  let is_empty = List.is_empty
  let map = List.map_preserving_phys_equal
end

type 'a exp_map = 'a Map.M(Exp).t [@@deriving compare, sexp]

(** see also [invariant] *)
type t =
  { sat: bool  (** [false] only if constraints are inconsistent *)
  ; rep: Exp.t exp_map
        (** map [a] to [a'+k], indicating that [a = a'+k] holds, and that
            [a'] (without the offset [k]) is the 'rep(resentative)' of [a] *)
  ; lkp: Exp.t exp_map
        (** map [f'(a'+i)] to [f(a+j)], indicating that [f'(a'+i) = f(a+j)]
            holds, where [f(a+j)] is in the carrier *)
  ; cls: Cls.t exp_map
        (** inverse rep: map each rep [a'] to all the [a+k] in its class,
            i.e., [cls a' = {a+(-k) | rep a = a'+k}] *)
  ; use: Use.t exp_map
        (** super-expression relation for representatives: map each
            representative [a'] of [a] to the compound expressions in the
            carrier where [a] (possibly + an offset) appears as an immediate
            sub-expression *)
  ; pnd: (Exp.t * Exp.t) list
        (** equations of the form [a+i = b+j], where [a] and [b] are in the
            carrier, to be added to the relation by merging the classes of
            [a] and [b] *) }
[@@deriving compare, sexp]

(** Pretty-printing *)

let pp_eq fs (e, f) = Format.fprintf fs "@[%a = %a@]" Exp.pp e Exp.pp f

let pp fs {sat; rep; lkp; cls; use; pnd} =
  let pp_alist pp_k pp_v fs alist =
    let pp_assoc fs (k, v) =
      Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_k k pp_v (k, v)
    in
    Format.fprintf fs "[@[<hv>%a@]]" (List.pp ";@ " pp_assoc) alist
  in
  let pp_exp_v fs (k, v) = if not (Exp.equal k v) then Exp.pp fs v in
  let pp_cls_v fs (_, v) = Cls.pp fs v in
  let pp_use_v fs (_, v) = Use.pp fs v in
  Format.fprintf fs
    "@[{@[<hv>sat= %b;@ rep= %a;@ lkp= %a;@ cls= %a;@ use= %a%a@]}@]" sat
    (pp_alist Exp.pp pp_exp_v)
    (Map.to_alist rep)
    (pp_alist Exp.pp pp_exp_v)
    (Map.to_alist lkp)
    (pp_alist Exp.pp pp_cls_v)
    (Map.to_alist cls)
    (pp_alist Exp.pp pp_use_v)
    (Map.to_alist use)
    (List.pp ~pre:";@ pnd= [@[<hv>" ";@ " pp_eq ~suf:"@]];")
    pnd

let pp_classes fs {cls} =
  List.pp "@ @<2>∧ "
    (fun fs (key, data) ->
      let es = Cls.remove_exn ~equal:Exp.equal data key in
      if not (Cls.is_empty es) then
        Format.fprintf fs "@[%a@ = %a@]" Exp.pp key (List.pp "@ = " Exp.pp)
          (List.sort ~compare:Exp.compare es) )
    fs (Map.to_alist cls)

let pp_diff fs (r, s) =
  let pp_sdiff_map pp_elt_diff equal nam fs x y =
    let sd = Sequence.to_list (Map.symmetric_diff ~data_equal:equal x y) in
    if not (List.is_empty sd) then
      Format.fprintf fs "%s= [@[<hv>%a@]];@ " nam
        (List.pp ";@ " pp_elt_diff)
        sd
  in
  let pp_sdiff_list pre pp_elt compare fs (c, d) =
    let sd = List.symmetric_diff ~compare c d in
    let pp_either fs = function
      | Either.First v -> Format.fprintf fs "-- %a" pp_elt v
      | Either.Second v -> Format.fprintf fs "++ %a" pp_elt v
    in
    if not (List.is_empty sd) then
      Format.fprintf fs "%s[@[<hv>%a@]]" pre (List.pp ";@ " pp_either) sd
  in
  let pp_sdiff_exps fs (c, d) =
    pp_sdiff_list "" Exp.pp Exp.compare fs (c, d)
  in
  let pp_sdiff_uses fs (c, d) =
    pp_sdiff_list "" Exp.pp Exp.compare fs (c, d)
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
  let pp_sdiff_app_map =
    let pp_sdiff_app fs (u, v) =
      Format.fprintf fs "-- %a ++ %a" Exp.pp u Exp.pp v
    in
    pp_sdiff_map (pp_sdiff_elt Exp.pp Exp.pp pp_sdiff_app) Exp.equal
  in
  let pp_sat fs =
    if not (Bool.equal r.sat s.sat) then
      Format.fprintf fs "sat= @[-- %b@ ++ %b@];@ " r.sat s.sat
  in
  let pp_rep fs = pp_sdiff_exp_map "rep" fs r.rep s.rep in
  let pp_lkp fs = pp_sdiff_app_map "lkp" fs r.lkp s.lkp in
  let pp_cls fs =
    let pp_sdiff_cls = pp_sdiff_elt Exp.pp Cls.pp pp_sdiff_exps in
    pp_sdiff_map pp_sdiff_cls Cls.equal "cls" fs r.cls s.cls
  in
  let pp_use fs =
    let pp_sdiff_use = pp_sdiff_elt Exp.pp Use.pp pp_sdiff_uses in
    pp_sdiff_map pp_sdiff_use Use.equal "use" fs r.use s.use
  in
  let pp_pnd fs =
    pp_sdiff_list "pnd= " pp_eq [%compare: Exp.t * Exp.t] fs (r.pnd, s.pnd)
  in
  Format.fprintf fs "@[{@[<hv>%t%t%t%t%t%t@]}@]" pp_sat pp_rep pp_lkp pp_cls
    pp_use pp_pnd

(** Auxiliary functions for manipulating "base plus offset" expressions *)

(** solve a+i = b for a, yielding a = b-i *)
let solve_for_base ai b =
  match Exp.base_offset ai with
  | Some (a, i, typ) -> (a, Exp.sub typ b (Exp.integer i typ))
  | None -> (ai, b)

(** subtract offset from both sides of equation a+i = b, yielding b-i *)
let subtract_offset ai b =
  match Exp.offset ai with
  | Some (i, typ) -> Exp.sub typ b (Exp.integer i typ)
  | None -> b

(** [map_base ~f a+i] is [f(a) + i] and [map_base ~f a] is [f(a)] *)
let map_base ai ~f =
  match Exp.base_offset ai with
  | Some (a, i, typ) ->
      let a' = f a in
      if a' == a then ai else Exp.add typ a' (Exp.integer i typ)
  | None -> f ai

(** [norm_base r a] is [a'+k] where [r] implies [a = a'+k] and [a'] is a
    rep, requires [a] to not have any offset and be in the carrier *)
let norm_base r e =
  assert (Option.is_none (Exp.offset e)) ;
  try Map.find_exn r.rep e with Caml.Not_found ->
    assert (Exp.is_simple e) ;
    e

(** [norm r a+i] is [a'+k] where [r] implies [a+i = a'+k] and [a'] is a rep,
    requires [a] to be in the carrier *)
let norm r e = map_base ~f:(norm_base r) e

(** test membership in carrier, strictly in the sense that an exp with an
    offset is not in the carrier even when its base is *)
let in_car r e = Exp.is_simple e || Map.mem r.rep e

(** test if an exp is a representative, requires exp to have no offset *)
let is_rep r e = Exp.equal e (norm_base r e)

let pre_invariant r =
  Invariant.invariant [%here] r [%sexp_of: t]
  @@ fun () ->
  Map.iteri r.rep ~f:(fun ~key:a ~data:a'k ->
      (* carrier is stored without offsets *)
      assert (Option.is_none (Exp.offset a)) ;
      (* carrier is closed under sub-expressions *)
      Exp.iter a ~f:(fun bj ->
          assert (
            in_car r (Exp.base bj)
            || Trace.report "@[subexp %a of %a not in carrier of@ %a@]"
                 Exp.pp bj Exp.pp a pp r ) ) ;
      let a', a_k = solve_for_base a'k a in
      (* carrier is closed under rep *)
      assert (in_car r a') ;
      if Exp.is_simple a' then
        (* rep is sparse for symbols *)
        assert (
          (not (Map.mem r.rep a'))
          || Trace.report
               "no symbol rep should be in rep domain: %a @<2>↦ %a@\n%a"
               Exp.pp a Exp.pp a' pp r )
      else
        (* rep is idempotent for applications *)
        assert (
          is_rep r a'
          || Trace.report
               "every app rep should be its own rep: %a @<2>↦ %a" Exp.pp a
               Exp.pp a' ) ;
      match Map.find r.cls a' with
      | None ->
          (* every rep in dom of cls *)
          assert (
            Trace.report "rep not in dom of cls: %a@\n%a" Exp.pp a' pp r )
      | Some a_cls ->
          (* every exp is in class of its rep *)
          assert (
            (* rep a = a'+k so expect a-k in cls a' *)
            Cls.mem a_cls a_k
            || Trace.report "%a = %a by rep but %a not in cls@\n%a" Exp.pp a
                 Exp.pp a'k Exp.pp a_k pp r ) ) ;
  Map.iteri r.cls ~f:(fun ~key:a' ~data:a_cls ->
      (* domain of cls are reps *)
      assert (is_rep r a') ;
      (* cls contained in inverse of rep *)
      Cls.iter a_cls ~f:(fun ak ->
          let a, a'_k = solve_for_base ak a' in
          assert (
            in_car r a
            || Trace.report "%a in cls of %a but not in carrier" Exp.pp a
                 Exp.pp a' ) ;
          let a'' = norm_base r a in
          assert (
            (* a' = a+k in cls so expect rep a = a'-k *)
            Exp.equal a'' a'_k
            || Trace.report "%a = %a by cls but @<2>≠ %a by rep" Exp.pp a'
                 Exp.pp ak Exp.pp a'' ) ) ) ;
  Map.iteri r.use ~f:(fun ~key:a' ~data:a_use ->
      assert (
        (not (Use.is_empty a_use))
        || Trace.report "empty use list should not have been added" ) ;
      Use.iter a_use ~f:(fun u ->
          (* uses are applications *)
          assert (not (Exp.is_simple u)) ;
          (* uses have no offsets *)
          assert (Option.is_none (Exp.offset u)) ;
          (* subexps of uses in carrier *)
          Exp.iter u ~f:(fun bj -> assert (in_car r (Exp.base bj))) ;
          (* every rep is a subexp-modulo-rep of each of its uses *)
          assert (
            Exp.exists u ~f:(fun bj -> Exp.equal a' (Exp.base (norm r bj)))
            || Trace.report
                 "rep %a has use %a, but is not the rep of any immediate \
                  subexp of the use"
                 Exp.pp a' Exp.pp u ) ;
          (* every use has a corresponding entry in lkp... *)
          let v =
            try Map.find_exn r.lkp (Exp.map ~f:(norm r) u)
            with Caml.Not_found ->
              fail "no lkp entry for use %a of %a" Exp.pp u Exp.pp a'
          in
          (* ...which is (eventually) provably equal *)
          if List.is_empty r.pnd then
            assert (Exp.equal (norm_base r u) (norm_base r v)) ) ) ;
  Map.iteri r.lkp ~f:(fun ~key:a ~data:c ->
      (* subexps of domain of lkp in carrier *)
      Exp.iter a ~f:(fun bj -> assert (in_car r (Exp.base bj))) ;
      (* range of lkp are applications in carrier *)
      assert (in_car r c) ;
      (* there may be stale entries in lkp whose subexps are no longer reps,
         which will therefore never be used, and hence are unconstrained *)
      if Exp.equal a (Exp.map ~f:(norm r) a) then (
        let c_' = Exp.map ~f:(norm r) c in
        (* lkp contains equalities provable modulo normalizing sub-exps *)
        assert (
          Exp.equal a c_'
          || Trace.report "%a sub-normalizes to %a @<2>≠ %a" Exp.pp c
               Exp.pp c_' Exp.pp a ) ;
        let c' = norm_base r c in
        Exp.iter a ~f:(fun bj ->
            (* every subexp of an app in domain of lkp has an associated use *)
            let b' = Exp.base (norm r bj) in
            let b_use =
              try Map.find_exn r.use b' with Caml.Not_found ->
                fail "no use list for subexp %a of lkp key %a" Exp.pp bj
                  Exp.pp a
            in
            assert (
              Use.exists b_use ~f:(fun u ->
                  Exp.equal a (Exp.map ~f:(norm r) u)
                  && Exp.equal c' (norm_base r u) )
              || Trace.report
                   "no corresponding use for subexp %a of lkp key %a" Exp.pp
                   bj Exp.pp a ) ) ) ) ;
  List.iter r.pnd ~f:(fun (ai, bj) ->
      assert (in_car r (Exp.base ai)) ;
      assert (in_car r (Exp.base bj)) )

let invariant r =
  Invariant.invariant [%here] r [%sexp_of: t]
  @@ fun () ->
  pre_invariant r ;
  assert (List.is_empty r.pnd)

(** Core closure operations *)

type prefer = Exp.t -> over:Exp.t -> int

let empty_map = Map.empty (module Exp)

let true_ =
  { sat= true
  ; rep= empty_map
  ; lkp= empty_map
  ; cls= empty_map
  ; use= empty_map
  ; pnd= [] }
  |> check invariant

let false_ = {true_ with sat= false} |> check invariant

(** Add an equation [b+j] = [a+i] using [a] as the new rep. This removes [b]
    from the [cls] and [use] maps, as it is no longer a rep. The [rep] map
    is updated to map each element of [b]'s [cls] (including [b]) to [a].
    This also adjusts the offsets in [b]'s [cls] and merges it into [a]'s.
    Likewise [b]'s [use] exps are merged into [a]'s. Finally, [b]'s [use]
    exps are used to traverse one step up the expression dag, looking for
    new equations involving a super-expression of [b], whose rep changed. *)
let add_directed_equation r0 ~exp:bj ~rep:ai =
  [%Trace.call fun {pf} -> pf "@[%a@ %a@]@ %a" Exp.pp bj Exp.pp ai pp r0]
  ;
  let r = r0 in
  (* b+j = a+i so b = a+i-j *)
  let b, aij = solve_for_base bj ai in
  assert ((not (in_car r b)) || is_rep r b) ;
  (* compute a from aij in case ai is an int and j is a non-0 offset *)
  let a = Exp.base aij in
  assert ((not (in_car r a)) || is_rep r a) ;
  let b_cls, cls =
    try Map.find_and_remove_exn r.cls b with Caml.Not_found ->
      (Cls.singleton b, r.cls)
  in
  let b_use, use =
    try Map.find_and_remove_exn r.use b with Caml.Not_found ->
      (Use.empty, r.use)
  in
  let r, a_cls_delta =
    Cls.fold_map b_cls ~init:r ~f:(fun r ck ->
        (* c+k = b = a+i-j so c = a+i-j-k *)
        let c, aijk = solve_for_base ck aij in
        let r =
          if Exp.equal a c || Exp.is_false (Exp.eq c aijk) then false_
          else if Exp.is_simple c && Exp.equal c aijk then r
          else {r with rep= Map.set r.rep ~key:c ~data:aijk}
        in
        (* a+i-j-k = c so a = c-i+j+k *)
        let cijk = subtract_offset aijk c in
        (r, cijk) )
  in
  let r, a_use_delta =
    Use.fold b_use ~init:(r, Use.empty) ~f:(fun (r, a_use_delta) u ->
        let u_' = Exp.map ~f:(norm r) u in
        Map.find_or_add r.lkp u_'
          ~if_found:(fun v ->
            let r = {r with pnd= (u, v) :: r.pnd} in
            (* no need to add u to use a since u is already in r (since u_'
               found in r.lkp) that is equal to v, so will be in use of u_'s
               subexps, and u = v is added to pnd so propagate will combine
               them later *)
            (r, a_use_delta) )
          ~default:u
          ~if_added:(fun lkp ->
            let r = {r with lkp} in
            let a_use_delta = Use.add a_use_delta u in
            (r, a_use_delta) ) )
  in
  let cls =
    Map.update cls a ~f:(function
      | Some a_cls -> Cls.union a_cls_delta a_cls
      | None -> Cls.add a_cls_delta a )
  in
  let use =
    if Use.is_empty a_use_delta then use
    else
      Map.update use a ~f:(function
        | Some a_use -> Use.union a_use_delta a_use
        | None -> a_use_delta )
  in
  let r = if not r.sat then false_ else {r with cls; use} in
  r
  |>
  [%Trace.retn fun {pf} r ->
    pf "%a" pp_diff (r0, r) ;
    pre_invariant r]

let prefer_rep ?prefer r e ~over:d =
  [%Trace.call fun {pf} -> pf "@[%a@ %a@]@ %a" Exp.pp d Exp.pp e pp r]
  ;
  let prefer_e_over_d =
    match (Exp.is_constant d, Exp.is_constant e) with
    | true, false -> -1
    | false, true -> 1
    | _ -> (
      match prefer with Some prefer -> prefer e ~over:d | None -> 0 )
  in
  ( match prefer_e_over_d with
  | n when n < 0 -> false
  | p when p > 0 -> true
  | _ ->
      let len_e =
        try Cls.length (Map.find_exn r.cls e) with Caml.Not_found -> 1
      in
      let len_d =
        try Cls.length (Map.find_exn r.cls d) with Caml.Not_found -> 1
      in
      len_e >= len_d )
  |>
  [%Trace.retn fun {pf} -> pf "%b"]

let choose_preferred ?prefer r d e =
  if prefer_rep ?prefer r e ~over:d then (d, e) else (e, d)

let add_equation ?prefer r d e =
  let d = norm r d in
  let e = norm r e in
  if (not r.sat) || Exp.equal d e then r
  else (
    [%Trace.call fun {pf} -> pf "@[%a@ %a@]@ %a" Exp.pp d Exp.pp e pp r]
    ;
    let exp, rep = choose_preferred ?prefer r d e in
    add_directed_equation r ~exp ~rep
    |>
    [%Trace.retn fun {pf} r' -> pf "%a" pp_diff (r, r')] )

(** normalize, and add base to carrier if needed *)
let rec norm_extend r ek =
  [%Trace.call fun {pf} -> pf "%a@ %a" Exp.pp ek pp r]
  ;
  let e = Exp.base ek in
  ( if Exp.is_simple e then (r, norm r ek)
  else
    Map.find_or_add r.rep e
      ~if_found:(fun e' ->
        match Exp.offset ek with
        | Some (k, typ) -> (r, Exp.add typ e' (Exp.integer k typ))
        | None -> (r, e') )
      ~default:e
      ~if_added:(fun rep ->
        let cls = Map.set r.cls ~key:e ~data:(Cls.singleton e) in
        let r = {r with rep; cls} in
        let r, e_' = Exp.fold_map ~f:norm_extend ~init:r e in
        Map.find_or_add r.lkp e_'
          ~if_found:(fun d ->
            let pnd = (e, d) :: r.pnd in
            let d' = norm_base r d in
            ({r with rep; pnd}, d') )
          ~default:e
          ~if_added:(fun lkp ->
            let use =
              Exp.fold e_' ~init:r.use ~f:(fun use b'j ->
                  let b' = Exp.base b'j in
                  Map.update use b' ~f:(function
                    | Some b_use -> Use.add b_use e
                    | None -> Use.singleton e ) )
            in
            ({r with lkp; use}, e) ) ) )
  |>
  [%Trace.retn fun {pf} (r', e') -> pf "%a@ %a" Exp.pp e' pp_diff (r, r')]

let norm_extend r ek = norm_extend r ek |> check (fst >> pre_invariant)
let extend r ek = fst (norm_extend r ek)

(** Close the relation with the pending equations. *)
let rec propagate ?prefer r =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  ( match r.pnd with
  | (d, e) :: pnd ->
      let r = {r with pnd} in
      let r = add_equation ?prefer r d e in
      propagate ?prefer r
  | [] -> r )
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let merge ?prefer r d e =
  if not r.sat then r
  else
    let r = extend r d in
    let r = extend r e in
    let r = add_equation ?prefer r d e in
    propagate ?prefer r

let rec normalize r ek =
  [%Trace.call fun {pf} -> pf "%a@ %a" Exp.pp ek pp r]
  ;
  map_base ek ~f:(fun e ->
      try Map.find_exn r.rep e with Caml.Not_found ->
        Exp.map ~f:(normalize r) e )
  |>
  [%Trace.retn fun {pf} -> pf "%a" Exp.pp]

let mem_eq r d e = Exp.equal (normalize r d) (normalize r e)

(** Exposed interface *)

let extend r e = propagate (extend r e)
let and_eq = merge

let and_ ?prefer r s =
  if not r.sat then r
  else if not s.sat then s
  else
    let s, r =
      if Map.length s.rep <= Map.length r.rep then (s, r) else (r, s)
    in
    Map.fold s.rep ~init:r ~f:(fun ~key:e ~data:e' r -> merge ?prefer r e e')

let or_ ?prefer r s =
  if not s.sat then r
  else if not r.sat then s
  else
    let merge_mems rs r s =
      Map.fold s.rep ~init:rs ~f:(fun ~key:e ~data:e' rs ->
          if mem_eq r e e' then merge ?prefer rs e e' else rs )
    in
    let rs = true_ in
    let rs = merge_mems rs r s in
    let rs = merge_mems rs s r in
    rs

(* assumes that f is injective and for any set of exps E, f[E] is disjoint
   from E *)
let map_exps ({sat= _; rep; lkp; cls; use; pnd} as r) ~f =
  [%Trace.call fun {pf} ->
    pf "%a" pp r ;
    assert (List.is_empty pnd)]
  ;
  let map ~equal_key ~equal_data ~f_key ~f_data m =
    Map.fold m ~init:m ~f:(fun ~key ~data m ->
        let key' = f_key key in
        let data' = f_data data in
        if equal_key key' key then
          if equal_data data' data then m else Map.set m ~key ~data:data'
        else Map.remove m key |> Map.add_exn ~key:key' ~data:data' )
  in
  let rep' =
    map rep ~equal_key:Exp.equal ~equal_data:Exp.equal ~f_key:f ~f_data:f
  in
  let lkp' =
    map lkp ~equal_key:Exp.equal ~equal_data:Exp.equal ~f_key:f ~f_data:f
  in
  let cls' =
    map cls ~equal_key:Exp.equal ~equal_data:[%compare.equal: Exp.t list]
      ~f_key:f ~f_data:(Cls.map ~f)
  in
  let use' =
    map use ~equal_key:Exp.equal ~equal_data:[%compare.equal: Exp.t list]
      ~f_key:f ~f_data:(Use.map ~f)
  in
  ( if rep' == rep && lkp' == lkp && cls' == cls && use' == use then r
  else {r with rep= rep'; lkp= lkp'; cls= cls'; use= use'} )
  |>
  [%Trace.retn fun {pf} r -> pf "%a" pp r ; invariant r]

let rename r sub = map_exps r ~f:(fun e -> Exp.rename e sub)

let fold_exps r ~init ~f =
  Map.fold r.lkp ~f:(fun ~key:_ ~data z -> f z data) ~init
  |> fun init ->
  Map.fold r.rep ~f:(fun ~key ~data z -> f (f z data) key) ~init

let fold_vars r ~init ~f =
  fold_exps r ~init ~f:(fun init -> Exp.fold_vars ~f ~init)

let fv e = fold_vars e ~f:Set.add ~init:Var.Set.empty

let is_true {sat; rep; pnd; _} =
  sat && Map.is_empty rep && List.is_empty pnd

let is_false {sat} = not sat
let classes {cls} = cls

let entails r s =
  Map.for_alli s.rep ~f:(fun ~key:e ~data:e' -> mem_eq r e e')

let difference r a b =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Exp.pp a Exp.pp b pp r]
  ;
  let r, a = norm_extend r a in
  let r, b = norm_extend r b in
  ( match (a, b) with
  | _ when Exp.equal a b -> Some Z.zero
  | (Add {typ} | Mul {typ} | Integer {typ}), _
   |_, (Add {typ} | Mul {typ} | Integer {typ}) -> (
      let a_b = Exp.sub typ a b in
      let r, a_b = norm_extend r a_b in
      let r = propagate r in
      match normalize r a_b with Integer {data} -> Some data | _ -> None )
  | _ -> None )
  |>
  [%Trace.retn fun {pf} ->
    function Some d -> pf "%a" Z.pp_print d | None -> ()]

(** Tests *)

let%test_module _ =
  ( module struct
    let printf pp = Format.printf "@.%a@." pp
    let ( ! ) i = Exp.integer (Z.of_int i) Typ.siz

    let%expect_test _ =
      printf pp {true_ with pnd= [(!0, !1)]} ;
      [%expect
        {| {sat= true; rep= []; lkp= []; cls= []; use= []; pnd= [0 = 1];} |}]
  end )
