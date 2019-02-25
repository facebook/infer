(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Congruence closure with integer offsets *)

(* For background, see:

   Robert Nieuwenhuis, Albert Oliveras: Fast congruence closure and
   extensions. Inf. Comput. 205(4): 557-580 (2007)

   and, for a more detailed correctness proof of the case without integer
   offsets, see section 5 of:

   Aleksandar Nanevski, Viktor Vafeiadis, Josh Berdine: Structuring the
   verification of heap-manipulating programs. POPL 2010: 261-274 *)

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
  let is_empty = List.is_empty
  let length = List.length
  let mem = List.mem ~equal:Exp.equal
end

(** set of exps representing "use lists" encoding super-expression relation *)
module Use = struct
  type t = Exp.t list [@@deriving compare, sexp]

  let equal = [%compare.equal: t]

  let pp fs use =
    Format.fprintf fs "@[<hov 1>{@[%a@]}@]" (List.pp ",@ " Exp.pp) use

  let empty = []
  let singleton exp = [exp]
  let add use exp = exp :: use
  let union = List.rev_append
  let fold = List.fold
  let is_empty = List.is_empty
end

type 'a exp_map = 'a Map.M(Exp).t [@@deriving compare, sexp]

let empty_map = Map.empty (module Exp)

type t =
  { sat: bool  (** [false] if constraints are inconsistent *)
  ; rep: Exp.t exp_map
        (** map [a] to [a'+k], indicating that [a=a'+k] holds, and that [a']
            (without the offset [k]) is the 'rep(resentative)' of [a] *)
  ; lkp: Exp.t exp_map
        (** inverse of mapping rep over sub-expressions: map [f'(a'+i)] to
            [f(a+j)+k], an (offsetted) app(lication expression) in the
            relation which normalizes to one in the 'equivalence modulo
            offset' class of [f'(a'+i)], indicating that [f'(a'+i) =
            f(a+j)+k] holds, for some [k] where [rep f = f'] and [rep a =
            a'+(i-j)] *)
  ; cls: Cls.t exp_map
        (** inverse rep: map each rep [a'] to all the [a+k] in its class,
            i.e., [cls a' = {a+k | rep a = a'+(-k)}] *)
  ; use: Use.t exp_map
        (** super-expression relation for representatives: map each
            representative [a'] of [a] to the application expressions in the
            relation where [a] (possibly + an offset) appears as an
            immediate sub-expression *)
  ; pnd: (Exp.t * Exp.t) list
        (** equations to be added to the relation, to enable delaying adding
            equations discovered while invariants are temporarily broken *)
  }
[@@deriving compare, sexp]

(** The expressions in the range of [lkp] and [use], as well as those in
    [pnd], are 'in the relation' in the sense that there is some constraint
    involving them, and in practice are expressions which have been passed
    to [merge] as opposed to having been constructed internally. *)

let pp_eq fs (e, f) = Format.fprintf fs "@[%a = %a@]" Exp.pp e Exp.pp f

let pp fs {sat; rep; lkp; cls; use; pnd} =
  let pp_alist pp_k pp_v fs alist =
    let pp_assoc fs (k, v) =
      Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_k k pp_v v
    in
    Format.fprintf fs "[@[<hv>%a@]]" (List.pp ";@ " pp_assoc) alist
  in
  let pp_pnd fs pnd =
    if not (List.is_empty pnd) then
      Format.fprintf fs ";@ pnd= [@[<hv>%a@]];" (List.pp ";@ " pp_eq) pnd
  in
  Format.fprintf fs
    "@[{@[<hv>sat= %b;@ rep= %a;@ lkp= %a;@ cls= %a;@ use= %a%a@]}@]" sat
    (pp_alist Exp.pp Exp.pp) (Map.to_alist rep) (pp_alist Exp.pp Exp.pp)
    (Map.to_alist lkp) (pp_alist Exp.pp Cls.pp) (Map.to_alist cls)
    (pp_alist Exp.pp Use.pp) (Map.to_alist use) pp_pnd pnd

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
  let pp_sdiff_elt pp_val pp_sdiff_val fs = function
    | k, `Left v ->
        Format.fprintf fs "-- [@[%a@ @<2>↦ %a@]]" Exp.pp k pp_val v
    | k, `Right v ->
        Format.fprintf fs "++ [@[%a@ @<2>↦ %a@]]" Exp.pp k pp_val v
    | k, `Unequal vv ->
        Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" Exp.pp k pp_sdiff_val vv
  in
  let pp_sdiff_exp_map =
    let pp_sdiff_exp fs (u, v) =
      Format.fprintf fs "-- %a ++ %a" Exp.pp u Exp.pp v
    in
    pp_sdiff_map (pp_sdiff_elt Exp.pp pp_sdiff_exp) Exp.equal
  in
  let pp_sat fs =
    if not (Bool.equal r.sat s.sat) then
      Format.fprintf fs "sat= @[-- %b@ ++ %b@];@ " r.sat s.sat
  in
  let pp_rep fs = pp_sdiff_exp_map "rep" fs r.rep s.rep in
  let pp_lkp fs = pp_sdiff_exp_map "lkp" fs r.lkp s.lkp in
  let pp_cls fs =
    let pp_sdiff_cls = pp_sdiff_elt Cls.pp pp_sdiff_exps in
    pp_sdiff_map pp_sdiff_cls Cls.equal "cls" fs r.cls s.cls
  in
  let pp_use fs =
    let pp_sdiff_use = pp_sdiff_elt Use.pp pp_sdiff_exps in
    pp_sdiff_map pp_sdiff_use Use.equal "use" fs r.use s.use
  in
  let pp_pnd fs =
    pp_sdiff_list "pnd= " pp_eq [%compare: Exp.t * Exp.t] fs (r.pnd, s.pnd)
  in
  Format.fprintf fs "@[{@[<hv>%t%t%t%t%t%t@]}@]" pp_sat pp_rep pp_lkp pp_cls
    pp_use pp_pnd

let invariant r =
  Invariant.invariant [%here] r [%sexp_of: t]
  @@ fun () ->
  Map.iteri r.rep ~f:(fun ~key:e ~data:e' -> assert (not (Exp.equal e e'))) ;
  Map.iteri r.cls ~f:(fun ~key:e' ~data:cls -> assert (Cls.mem cls e')) ;
  Map.iteri r.use ~f:(fun ~key:_ ~data:use -> assert (not (Use.is_empty use))
  )

(* Auxiliary functions for manipulating "base plus offset" expressions *)

let map_sum e ~f =
  match e with
  | Exp.App {op= App {op= Add {typ}; arg= a}; arg= i} ->
      let a' = f a in
      if a' == a then e else Exp.add typ a' i
  | a -> f a

let fold_sum e ~init ~f =
  match e with
  | Exp.App {op= App {op= Add _; arg= a}; arg= Integer _} -> f init a
  | a -> f init a

let base_of = function
  | Exp.App {op= App {op= Add _; arg= a}; arg= Integer _} -> a
  | a -> a

(** solve a+i = b for a, yielding a = b-i *)
let solve_for_base ai b =
  match ai with
  | Exp.App {op= App {op= Add {typ}; arg= a}; arg= i} -> (a, Exp.sub typ b i)
  | _ -> (ai, b)

(** [norm r a+i] = [a'+k] where [r] implies [a+i=a'+k] and [a'] is a rep *)
let norm r e =
  map_sum e ~f:(fun a -> try Map.find_exn r.rep a with Caml.Not_found -> a)

(* Core closure operations *)

type prefer = Exp.t -> over:Exp.t -> int

let true_ =
  { sat= true
  ; rep= empty_map
  ; lkp= empty_map
  ; cls= empty_map
  ; use= empty_map
  ; pnd= [] }

let false_ = {true_ with sat= false}

(** Add app exps (and sub-exps) to the relation. This populates the [lkp]
    and [use] maps, treating an exp [e] of form [f(a)] as an equation
    between the app [f(a)] and the 'symbol' [e]. This has the effect of
    using [e] as a 'name' of the app [f(a)], rather than using an explicit
    'flattening' transformation introducing new symbols for each
    application. *)
let rec extend r e =
  fold_sum e ~init:r ~f:(fun r -> function
    | App _ as fa ->
        let r, fa' =
          Exp.fold_map fa ~init:r ~f:(fun r b ->
              let r, c = extend r b in
              (r, norm r c) )
        in
        Map.find_or_add r.lkp fa'
          ~if_found:(fun d ->
            let r = {r with pnd= (e, d) :: r.pnd} in
            (r, d) )
          ~default:e
          ~if_added:(fun lkp ->
            let use =
              Exp.fold fa' ~init:r.use ~f:(fun use b' ->
                  if Exp.is_constant b' then use
                  else
                    Map.update use b' ~f:(function
                      | Some b_use -> Use.add b_use fa
                      | None -> Use.singleton fa ) )
            in
            let r = {r with lkp; use} in
            (r, e) )
    | _ -> (r, e) )

exception Unsat

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
  let a = base_of ai in
  (* b+j = a+i so b = a+i-j *)
  let b, ai_j = solve_for_base bj ai in
  let b_cls, cls =
    try Map.find_and_remove_exn r.cls b with Caml.Not_found ->
      (Cls.singleton b, r.cls)
  in
  let b_use, use =
    try Map.find_and_remove_exn r.use b with Caml.Not_found ->
      (Use.empty, r.use)
  in
  let rep, a_cls_delta =
    Cls.fold_map b_cls ~init:r.rep ~f:(fun rep ck ->
        (* c+k = b = a+i-j so c = a+i-j-k *)
        let c, ai_j_k = solve_for_base ck ai_j in
        if Exp.is_false (Exp.eq c ai_j_k) then raise Unsat ;
        let rep = Map.set rep ~key:c ~data:ai_j_k in
        (* a+i-j = c+k so a = c+k+j-i *)
        let _, ckj_i = solve_for_base ai_j ck in
        (rep, ckj_i) )
  in
  let cls =
    Map.update cls a ~f:(function
      | Some a_cls -> Cls.union a_cls_delta a_cls
      | None -> Cls.add a_cls_delta a )
  in
  let r = {r with rep; cls; use} in
  let r, a_use_delta =
    Use.fold b_use ~init:(r, Use.empty) ~f:(fun (r, a_use_delta) u ->
        let u' = Exp.map ~f:(norm r) u in
        Map.find_or_add r.lkp u'
          ~if_found:(fun v ->
            let r = {r with pnd= (u, v) :: r.pnd} in
            (* no need to add u to use a since u is an app already in r
               (since u' found in r.lkp) that is equal to v, so will be in
               use of u's subexps, and u = v is added to pnd so propagate
               will combine them later *)
            (r, a_use_delta) )
          ~default:u
          ~if_added:(fun lkp ->
            let r = {r with lkp} in
            let a_use_delta = Use.add a_use_delta u in
            (r, a_use_delta) ) )
  in
  let use =
    if Use.is_empty a_use_delta then use
    else
      Map.update use a ~f:(function
        | Some a_use -> Use.union a_use_delta a_use
        | None -> a_use_delta )
  in
  let r = {r with use} in
  r |> check invariant
  |>
  [%Trace.retn fun {pf} r -> pf "%a" pp_diff (r0, r)]

(** Close the relation with the pending equations. *)
let rec propagate_ ?prefer r =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  ( match r.pnd with
  | [] -> r
  | (d, e) :: pnd ->
      let d' = norm r d in
      let e' = norm r e in
      let r = {r with pnd} in
      let r =
        if Exp.equal (base_of d') (base_of e') then
          if Exp.equal d' e' then r else {r with sat= false; pnd= []}
        else
          let prefer_e_over_d =
            match (Exp.is_constant d, Exp.is_constant e) with
            | true, false -> -1
            | false, true -> 1
            | _ -> (
              match prefer with
              | Some prefer -> prefer e' ~over:d'
              | None -> 0 )
          in
          match prefer_e_over_d with
          | n when n < 0 -> add_directed_equation r ~exp:e' ~rep:d'
          | p when p > 0 -> add_directed_equation r ~exp:d' ~rep:e'
          | _ ->
              let len_d =
                try Cls.length (Map.find_exn r.cls d')
                with Caml.Not_found -> 1
              in
              let len_e =
                try Cls.length (Map.find_exn r.cls e')
                with Caml.Not_found -> 1
              in
              if len_d > len_e then add_directed_equation r ~exp:e' ~rep:d'
              else add_directed_equation r ~exp:d' ~rep:e'
      in
      propagate_ ?prefer r )
  |>
  [%Trace.retn fun {pf} r' -> pf "%a" pp_diff (r, r')]

let propagate ?prefer r =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  (try propagate_ ?prefer r with Unsat -> false_)
  |>
  [%Trace.retn fun {pf} r' -> pf "%a" pp_diff (r, r')]

let merge ?prefer r d e =
  if not r.sat then r
  else
    let r, a = extend r d in
    let r =
      if Exp.equal d e then r
      else
        let r, b = extend r e in
        let r = {r with pnd= (a, b) :: r.pnd} in
        r
    in
    propagate ?prefer r

let rec normalize_ r e =
  [%Trace.call fun {pf} -> pf "%a" Exp.pp e]
  ;
  map_sum e ~f:(function
    | App _ as fa -> (
        let fa' = Exp.map ~f:(normalize_ r) fa in
        match Map.find_exn r.lkp fa' with
        | exception _ -> fa'
        | c -> norm r (Exp.map ~f:(norm r) c) )
    | c ->
        let c' = norm r c in
        if c' == c then c else normalize_ r c' )
  |>
  [%Trace.retn fun {pf} -> pf "%a" Exp.pp]

let normalize r e =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  normalize_ r e
  |>
  [%Trace.retn fun {pf} -> pf "%a" Exp.pp]

let mem_eq r e f = Exp.equal (normalize r e) (normalize r f)

(** Exposed interface *)

let and_eq = merge

let and_ ?prefer r s =
  if not r.sat then r
  else if not s.sat then s
  else
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
  [%Trace.call fun {pf} -> pf "%a@." pp r]
  ;
  assert (List.is_empty pnd) ;
  let map ~equal_data ~f_data m =
    Map.fold m ~init:m ~f:(fun ~key ~data m ->
        let key' = f key in
        let data' = f_data data in
        if Exp.equal key' key then
          if equal_data data' data then m else Map.set m ~key ~data:data'
        else
          Map.remove m key
          |> Map.add_exn ~key:key'
               ~data:(if data' == data then data else data') )
  in
  let exp_map = map ~equal_data:Exp.equal ~f_data:f in
  let exp_list_map =
    map ~equal_data:[%compare.equal: Exp.t list]
      ~f_data:(List.map_preserving_phys_equal ~f)
  in
  let rep' = exp_map rep in
  let lkp' = exp_map lkp in
  let cls' = exp_list_map cls in
  let use' = exp_list_map use in
  ( if rep' == rep && lkp' == lkp && cls' == cls && use' == use then r
  else {r with rep= rep'; lkp= lkp'; cls= cls'; use= use'} )
  |>
  [%Trace.retn fun {pf} -> pf "%a@." pp]

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

(* a - b = k if a = b+k *)
let difference r a b =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Exp.pp a Exp.pp b pp r]
  ;
  let r, a = extend r a in
  let r, b = extend r b in
  let r = propagate r in
  let ci = normalize r a in
  let dj = normalize r b in
  (* a - b = (c+i) - (d+j) *)
  ( match solve_for_base dj ci with
  (* d = (c+i)-j = c+(i-j) & c = d *)
  | d, App {op= App {op= Add _; arg= c}; arg= Integer {data= i_j}}
    when Exp.equal d c ->
      (* a - b = (c+i) - (d+j) = i-j *)
      Some i_j
  | Integer {data= j}, Integer {data= i} -> Some (Z.sub i j)
  | d, ci_j when Exp.equal d ci_j -> Some Z.zero
  | _ -> None )
  |>
  [%Trace.retn fun {pf} ->
    function
    | Some d -> pf "%a" Z.pp_print d
    | None -> pf "c+i: %a@ d+j: %a" Exp.pp ci Exp.pp dj]

let%test_module _ =
  ( module struct
    let printf pp = Format.printf "@.%a@." pp
    let ( ! ) i = Exp.integer (Z.of_int i) Typ.siz

    let%expect_test _ =
      printf pp {true_ with pnd= [(!0, !1)]} ;
      [%expect
        {| {sat= true; rep= []; lkp= []; cls= []; use= []; pnd= [0 = 1];} |}]
  end )
