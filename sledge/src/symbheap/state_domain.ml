(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

type t = Sh.t [@@deriving equal, sexp_of]

let pp_simp fs q =
  let q' = ref q in
  [%Trace.printf "%a" (fun _ q -> q' := Sh.simplify q) q] ;
  Sh.pp fs !q'

let pp = pp_simp

let init globals =
  Vector.fold globals ~init:Sh.emp ~f:(fun q -> function
    | {Global.var; init= Some (arr, siz)} ->
        let loc = Exp.var var in
        let len = Exp.integer (Z.of_int siz) Typ.siz in
        Sh.star q (Sh.seg {loc; bas= loc; len; siz= len; arr})
    | _ -> q )

let join = Sh.or_
let is_false = Sh.is_false
let exec_assume = Exec.assume
let exec_return = Exec.return
let exec_inst = Exec.inst
let exec_intrinsic = Exec.intrinsic
let dnf = Sh.dnf

let exp_eq_class_has_only_vars_in fvs cong exp =
  [%Trace.call fun {pf} ->
    pf "@[<v> fvs: @[%a@] @,cong: @[%a@] @,exp: @[%a@]@]" Var.Set.pp fvs
      Equality.pp cong Exp.pp exp]
  ;
  let exp_has_only_vars_in fvs exp = Set.is_subset (Exp.fv exp) ~of_:fvs in
  let exp_eq_class = Equality.class_of cong exp in
  List.exists ~f:(exp_has_only_vars_in fvs) exp_eq_class
  |>
  [%Trace.retn fun {pf} -> pf "%b"]

let garbage_collect (q : t) ~wrt =
  [%Trace.call fun {pf} -> pf "%a" pp q]
  ;
  (* only support DNF for now *)
  assert (List.is_empty q.djns) ;
  let rec all_reachable_vars previous current (q : t) =
    if Var.Set.equal previous current then current
    else
      let new_set =
        List.fold ~init:current q.heap ~f:(fun current seg ->
            if exp_eq_class_has_only_vars_in current q.cong seg.loc then
              List.fold (Equality.class_of q.cong seg.arr) ~init:current
                ~f:(fun c e -> Set.union c (Exp.fv e))
            else current )
      in
      all_reachable_vars current new_set q
  in
  let r_vars = all_reachable_vars Var.Set.empty wrt q in
  Sh.filter_heap q ~f:(fun seg ->
      exp_eq_class_has_only_vars_in r_vars q.cong seg.loc )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

type from_call =
  {subst: Var.Subst.t; frame: Sh.t; actuals_to_formals: (Exp.t * Var.t) list}
[@@deriving compare, equal, sexp]

(** Express formula in terms of formals instead of actuals, and enter scope
    of locals: rename formals to fresh vars in formula and actuals, add
    equations between each formal and actual, and quantify the temps and
    fresh vars. *)
let jump actuals formals ?temps q =
  [%Trace.call fun {pf} ->
    pf "@[<hv>actuals: (@[%a@])@ formals: (@[%a@])@ q: %a@]"
      (List.pp ",@ " Exp.pp) (List.rev actuals) (List.pp ",@ " Var.pp)
      (List.rev formals) Sh.pp q]
  ;
  let q', freshen_locals = Sh.freshen q ~wrt:(Var.Set.of_list formals) in
  let and_eq q formal actual =
    let actual' = Exp.rename actual freshen_locals in
    Sh.and_ (Exp.eq (Exp.var formal) actual') q
  in
  let and_eqs formals actuals q =
    List.fold2_exn ~f:and_eq formals actuals ~init:q
  in
  ( Option.fold ~f:(Fn.flip Sh.exists) temps
      ~init:(and_eqs formals actuals q')
  , {subst= freshen_locals; frame= Sh.emp; actuals_to_formals= []} )
  |>
  [%Trace.retn fun {pf} (q', {subst}) ->
    pf "@[<hv>subst: %a@ q': %a@]" Var.Subst.pp subst Sh.pp q']

(** Express formula in terms of formals instead of actuals, and enter scope
    of locals: rename formals to fresh vars in formula and actuals, add
    equations between each formal and actual, and quantify fresh vars. *)
let call ~summaries actuals formals locals globals q =
  [%Trace.call fun {pf} ->
    pf
      "@[<hv>actuals: (@[%a@])@ formals: (@[%a@])@ locals: {@[%a@]}@ q: %a@]"
      (List.pp ",@ " Exp.pp) (List.rev actuals) (List.pp ",@ " Var.pp)
      (List.rev formals) Var.Set.pp locals pp q]
  ;
  let wrt = Set.add_list formals locals in
  let q', freshen_locals = Sh.freshen q ~wrt in
  let and_eq q formal actual =
    let actual' = Exp.rename actual freshen_locals in
    Sh.and_ (Exp.eq (Exp.var formal) actual') q
  in
  let and_eqs formals actuals q =
    List.fold2_exn ~f:and_eq formals actuals ~init:q
  in
  let q'' = and_eqs formals actuals q' in
  ( if not summaries then
    let q'', subst = (Sh.extend_us locals q'', freshen_locals) in
    (q'', {subst; frame= Sh.emp; actuals_to_formals= []})
  else
    let formals_set = Var.Set.of_list formals in
    (* Add the formals here to do garbage collection and then get rid of
       them *)
    let function_summary_pre =
      garbage_collect q'' ~wrt:(Set.union formals_set globals)
    in
    [%Trace.info "function summary pre %a" pp function_summary_pre] ;
    let foot = Sh.exists formals_set function_summary_pre in
    let pre = q' in
    let xs, foot = Sh.bind_exists ~wrt:pre.us foot in
    let frame =
      Option.value ~default:Sh.emp (Solver.infer_frame pre xs foot)
    in
    let q'', subst =
      (Sh.extend_us locals (and_eqs formals actuals foot), freshen_locals)
    in
    (q'', {subst; frame; actuals_to_formals= List.zip_exn actuals formals})
  )
  |>
  [%Trace.retn fun {pf} (q', {subst; frame}) ->
    pf "@[<v>subst: %a@ frame: %a@ q': %a@]" Var.Subst.pp subst pp frame pp
      q']

(** Leave scope of locals: existentially quantify locals. *)
let post locals q =
  [%Trace.call fun {pf} ->
    pf "@[<hv>locals: {@[%a@]}@ q: %a@]" Var.Set.pp locals Sh.pp q]
  ;
  Sh.exists locals q
  |>
  [%Trace.retn fun {pf} -> pf "%a" Sh.pp]

(** Express in terms of actuals instead of formals: existentially quantify
    formals, and apply inverse of fresh variables for formals renaming to
    restore the shadowed variables. *)
let retn formals {subst; frame} q =
  [%Trace.call fun {pf} ->
    pf "@[<v>formals: {@[%a@]}@ subst: %a@ q: %a@ frame: %a@]"
      (List.pp ", " Var.pp) formals Var.Subst.pp (Var.Subst.invert subst) pp
      q pp frame]
  ;
  let q = Sh.exists (Var.Set.of_list formals) q in
  let q = Sh.rename (Var.Subst.invert subst) q in
  Sh.star frame q
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

let resolve_callee lookup ptr _ =
  match Var.of_exp ptr with
  | Some callee_name -> lookup callee_name
  | None -> []

type function_summary = {xs: Var.Set.t; foot: t; post: t}

let pp_function_summary fs {xs; foot; post} =
  Format.fprintf fs "@[<v>xs: @[%a@]@ foot: %a@ post: %a @]" Var.Set.pp xs
    pp foot pp post

let create_summary ~locals ~formals entry current =
  [%Trace.call fun {pf} ->
    pf "formals %a@ entry: %a@ current: %a" Var.Set.pp formals pp entry pp
      current]
  ;
  let foot = Sh.exists locals entry in
  let post = Sh.exists locals current in
  let foot, subst = Sh.freshen ~wrt:(Set.union foot.us post.us) foot in
  let restore_formals q =
    Set.fold formals ~init:q ~f:(fun q var ->
        let var = Exp.var var in
        let renamed_var = Exp.rename var subst in
        Sh.and_ (Exp.eq renamed_var var) q )
  in
  (* Add back the original formals name *)
  let post = Sh.rename subst post in
  let foot = restore_formals foot in
  let post = restore_formals post in
  [%Trace.info "subst: %a" Var.Subst.pp subst] ;
  let xs = Set.inter (Sh.fv foot) (Sh.fv post) in
  let xs = Set.diff xs formals in
  let xs_and_formals = Set.union xs formals in
  let foot = Sh.exists (Set.diff foot.us xs_and_formals) foot in
  let post = Sh.exists (Set.diff post.us xs_and_formals) post in
  {xs; foot; post}
  |>
  [%Trace.retn fun {pf} -> pf "@,%a" pp_function_summary]

let apply_summary ({xs; foot; post} as fs) q =
  [%Trace.call fun {pf} -> pf "fs: %a@ q: %a" pp_function_summary fs pp q]
  ;
  let frame = Solver.infer_frame q xs foot in
  [%Trace.info "frame %a" (Option.pp "%a" pp) frame] ;
  Option.map ~f:(Sh.star post) frame
  |>
  [%Trace.retn fun {pf} r ->
    match r with None -> pf "None" | Some q -> pf "@,%a" pp q]

let%test_module _ =
  ( module struct
    let pp = Format.printf "@.%a@." Sh.pp
    let wrt = Var.Set.empty
    let main_, wrt = Var.fresh "main" ~wrt
    let a_, wrt = Var.fresh "a" ~wrt
    let n_, wrt = Var.fresh "n" ~wrt
    let b_, wrt = Var.fresh "b" ~wrt
    let end_, _ = Var.fresh "end" ~wrt
    let a = Exp.var a_
    let main = Exp.var main_
    let b = Exp.var b_
    let n = Exp.var n_
    let endV = Exp.var end_
    let seg_main = Sh.seg {loc= main; bas= b; len= n; siz= n; arr= a}
    let seg_a = Sh.seg {loc= a; bas= b; len= n; siz= n; arr= endV}
    let seg_cycle = Sh.seg {loc= a; bas= b; len= n; siz= n; arr= main}

    let%expect_test _ =
      pp (garbage_collect seg_main ~wrt:(Var.Set.of_list [])) ;
      [%expect {| emp |}]

    let%expect_test _ =
      pp
        (garbage_collect (Sh.star seg_a seg_main)
           ~wrt:(Var.Set.of_list [a_])) ;
      [%expect {| %a_2 -[ %b_4, %n_3 )-> ⟨%n_3,%end_5⟩ |}]

    let%expect_test _ =
      pp
        (garbage_collect (Sh.star seg_a seg_main)
           ~wrt:(Var.Set.of_list [main_])) ;
      [%expect
        {|
          %main_1 -[ %b_4, %n_3 )-> ⟨%n_3,%a_2⟩
        * %a_2 -[ %b_4, %n_3 )-> ⟨%n_3,%end_5⟩ |}]

    let%expect_test _ =
      pp
        (garbage_collect
           (Sh.star seg_cycle seg_main)
           ~wrt:(Var.Set.of_list [a_])) ;
      [%expect
        {|
          %main_1 -[ %b_4, %n_3 )-> ⟨%n_3,%a_2⟩
        * %a_2 -[ %b_4, %n_3 )-> ⟨%n_3,%main_1⟩ |}]
  end )
