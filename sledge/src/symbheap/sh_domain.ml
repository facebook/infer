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
let report_fmt_thunk = Fn.flip pp

let init globals =
  Vector.fold globals ~init:Sh.emp ~f:(fun q -> function
    | {Global.reg; init= Some arr} ->
        let loc = Term.var (Reg.var reg) in
        let len = Term.size_of (Exp.typ arr) in
        let arr = arr.term in
        Sh.star q (Sh.seg {loc; bas= loc; len; siz= len; arr})
    | _ -> q )

let join l r = Some (Sh.or_ l r)
let is_false = Sh.is_false
let dnf = Sh.dnf
let exec_assume q b = Exec.assume q (Exp.term b)
let exec_kill q r = Exec.kill q (Reg.var r)

let exec_move q res =
  Exec.move q (Vector.map res ~f:(fun (r, e) -> (Reg.var r, Exp.term e)))

let exec_inst pre inst =
  [%Trace.info
    "@[<2>exec inst %a from@ @[{ %a@ }@]@]" Llair.Inst.pp inst Sh.pp pre] ;
  match (inst : Llair.inst) with
  | Move {reg_exps; _} ->
      Some
        (Exec.move pre
           (Vector.map reg_exps ~f:(fun (r, e) -> (Reg.var r, Exp.term e))))
  | Load {reg; ptr; len; _} ->
      Exec.load pre ~reg:(Reg.var reg) ~ptr:(Exp.term ptr)
        ~len:(Exp.term len)
  | Store {ptr; exp; len; _} ->
      Exec.store pre ~ptr:(Exp.term ptr) ~exp:(Exp.term exp)
        ~len:(Exp.term len)
  | Memset {dst; byt; len; _} ->
      Exec.memset pre ~dst:(Exp.term dst) ~byt:(Exp.term byt)
        ~len:(Exp.term len)
  | Memcpy {dst; src; len; _} ->
      Exec.memcpy pre ~dst:(Exp.term dst) ~src:(Exp.term src)
        ~len:(Exp.term len)
  | Memmov {dst; src; len; _} ->
      Exec.memmov pre ~dst:(Exp.term dst) ~src:(Exp.term src)
        ~len:(Exp.term len)
  | Alloc {reg; num; len; _} ->
      Exec.alloc pre ~reg:(Reg.var reg) ~num:(Exp.term num)
        ~len:(Exp.term len)
  | Free {ptr; _} -> Exec.free pre ~ptr:(Exp.term ptr)
  | Nondet {reg; _} -> Some (Exec.nondet pre (Option.map ~f:Reg.var reg))
  | Abort _ -> Exec.abort pre

let exec_intrinsic ~skip_throw q r i es =
  Exec.intrinsic ~skip_throw q (Option.map ~f:Reg.var r) (Reg.var i)
    (List.map ~f:Exp.term es)

let term_eq_class_has_only_vars_in fvs cong term =
  [%Trace.call fun {pf} ->
    pf "@[<v> fvs: @[%a@] @,cong: @[%a@] @,term: @[%a@]@]" Var.Set.pp fvs
      Equality.pp cong Term.pp term]
  ;
  let term_has_only_vars_in fvs term =
    Set.is_subset (Term.fv term) ~of_:fvs
  in
  let term_eq_class = Equality.class_of cong term in
  List.exists ~f:(term_has_only_vars_in fvs) term_eq_class
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
            if term_eq_class_has_only_vars_in current q.cong seg.loc then
              List.fold (Equality.class_of q.cong seg.arr) ~init:current
                ~f:(fun c e -> Set.union c (Term.fv e))
            else current )
      in
      all_reachable_vars current new_set q
  in
  let r_vars = all_reachable_vars Var.Set.empty wrt q in
  Sh.filter_heap q ~f:(fun seg ->
      term_eq_class_has_only_vars_in r_vars q.cong seg.loc )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

let and_eqs sub formals actuals q =
  let and_eq q formal actual =
    let actual' = Term.rename sub actual in
    Sh.and_ (Term.eq (Term.var formal) actual') q
  in
  List.fold2_exn ~f:and_eq formals actuals ~init:q

let localize_entry globals actuals formals freturn locals subst pre entry =
  (* Add the formals here to do garbage collection and then get rid of them *)
  let formals_set = Var.Set.of_list formals in
  let freturn_locals = Reg.Set.vars (Set.add_option freturn locals) in
  let function_summary_pre =
    garbage_collect entry
      ~wrt:(Set.union formals_set (Reg.Set.vars globals))
  in
  [%Trace.info "function summary pre %a" pp function_summary_pre] ;
  let foot = Sh.exists formals_set function_summary_pre in
  let xs, foot = Sh.bind_exists ~wrt:pre.Sh.us foot in
  let frame =
    Option.value_exn
      (Solver.infer_frame pre xs foot)
      ~message:"Solver couldn't infer frame of a garbage-collected pre"
  in
  let q'' =
    Sh.extend_us freturn_locals (and_eqs subst formals actuals foot)
  in
  (q'', frame)

type from_call = {areturn: Var.t option; subst: Var.Subst.t; frame: Sh.t}
[@@deriving compare, equal, sexp]

(** Express formula in terms of formals instead of actuals, and enter scope
    of locals: rename formals to fresh vars in formula and actuals, add
    equations between each formal and actual, and quantify fresh vars. *)
let call ~summaries ~globals ~actuals ~areturn ~formals ~freturn ~locals q =
  [%Trace.call fun {pf} ->
    pf
      "@[<hv>actuals: (@[%a@])@ formals: (@[%a@])@ locals: {@[%a@]}@ \
       globals: {@[%a@]}@ q: %a@]"
      (List.pp ",@ " Exp.pp) (List.rev actuals) (List.pp ",@ " Reg.pp)
      (List.rev formals) Reg.Set.pp locals Reg.Set.pp globals pp q]
  ;
  let actuals = List.map ~f:Exp.term actuals in
  let areturn = Option.map ~f:Reg.var areturn in
  let formals = List.map ~f:Reg.var formals in
  let freturn_locals = Reg.Set.vars (Set.add_option freturn locals) in
  let modifs = Var.Set.of_option areturn in
  (* quantify modifs, their current value will be overwritten and so does
     not need to be saved in the freshening renaming *)
  let q = Sh.exists modifs q in
  (* save current values of shadowed formals and locals with a renaming *)
  let q', subst = Sh.freshen q ~wrt:(Set.add_list formals freturn_locals) in
  assert (Set.disjoint modifs (Var.Subst.domain subst)) ;
  (* pass arguments by conjoining equations between formals and actuals *)
  let entry = and_eqs subst formals actuals q' in
  (* note: locals and formals are in scope *)
  assert (Set.is_subset (Set.add_list formals freturn_locals) ~of_:entry.us) ;
  ( if not summaries then (entry, {areturn; subst; frame= Sh.emp})
  else
    let q'', frame =
      localize_entry globals actuals formals freturn locals subst q' entry
    in
    (q'', {areturn; subst; frame}) )
  |>
  [%Trace.retn fun {pf} (entry, {subst; frame}) ->
    pf "@[<v>subst: %a@ frame: %a@ entry: %a@]" Var.Subst.pp subst pp frame
      pp entry]

(** Leave scope of locals: existentially quantify locals. *)
let post locals _ q =
  [%Trace.call fun {pf} ->
    pf "@[<hv>locals: {@[%a@]}@ q: %a@]" Reg.Set.pp locals Sh.pp q]
  ;
  Sh.exists (Reg.Set.vars locals) q
  |>
  [%Trace.retn fun {pf} -> pf "%a" Sh.pp]

(** Express in terms of actuals instead of formals: existentially quantify
    formals, and apply inverse of fresh variables for formals renaming to
    restore the shadowed variables. *)
let retn formals freturn {areturn; subst; frame} q =
  [%Trace.call fun {pf} ->
    pf "@[<v>formals: {@[%a@]}%a%a@ subst: %a@ q: %a@ frame: %a@]"
      (List.pp ", " Reg.pp) formals
      (Option.pp "@ freturn: %a" Reg.pp)
      freturn
      (Option.pp "@ areturn: %a" Var.pp)
      areturn Var.Subst.pp (Var.Subst.invert subst) pp q pp frame]
  ;
  let formals = List.map ~f:Reg.var formals in
  let freturn = Option.map ~f:Reg.var freturn in
  let inv_subst = Var.Subst.invert subst in
  let q, inv_subst =
    match areturn with
    | Some areturn -> (
        (* reenter scope of areturn just before exiting scope of formals *)
        let q = Sh.extend_us (Var.Set.of_ areturn) q in
        (* pass return value *)
        match freturn with
        | Some freturn ->
            (Exec.move q (Vector.of_ (areturn, Term.var freturn)), inv_subst)
        | None -> (Exec.kill q areturn, inv_subst) )
    | None -> (q, inv_subst)
  in
  (* exit scope of formals *)
  let q = Sh.exists (Set.add_list formals (Var.Set.of_option freturn)) q in
  (* reinstate shadowed values of locals *)
  let q = Sh.rename inv_subst q in
  (* reconjoin frame *)
  Sh.star frame q
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

let resolve_callee lookup ptr q =
  match Reg.of_exp ptr with
  | Some callee -> (lookup (Reg.name callee), q)
  | None -> ([], q)

let recursion_beyond_bound = `prune

type summary = {xs: Var.Set.t; foot: t; post: t}

let pp_summary fs {xs; foot; post} =
  Format.fprintf fs "@[<v>xs: @[%a@]@ foot: %a@ post: %a @]" Var.Set.pp xs
    pp foot pp post

let create_summary ~locals ~formals ~entry ~current:(post : Sh.t) =
  [%Trace.call fun {pf} ->
    pf "formals %a@ entry: %a@ current: %a" Reg.Set.pp formals pp entry pp
      post]
  ;
  let locals = Reg.Set.vars locals in
  let formals = Reg.Set.vars formals in
  let foot = Sh.exists locals entry in
  let foot, subst = Sh.freshen ~wrt:(Set.union foot.us post.us) foot in
  let restore_formals q =
    Set.fold formals ~init:q ~f:(fun q var ->
        let var = Term.var var in
        let renamed_var = Term.rename subst var in
        Sh.and_ (Term.eq renamed_var var) q )
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
  let current = Sh.extend_us xs post in
  ({xs; foot; post}, current)
  |>
  [%Trace.retn fun {pf} (fs, _) -> pf "@,%a" pp_summary fs]

let apply_summary q ({xs; foot; post} as fs) =
  [%Trace.call fun {pf} -> pf "fs: %a@ q: %a" pp_summary fs pp q]
  ;
  let xs_in_q = Set.inter xs q.Sh.us in
  let xs_in_fv_q = Set.inter xs (Sh.fv q) in
  (* Between creation of a summary and its use, the vocabulary of q (q.us)
     might have been extended. That means infer_frame would fail, because q
     and foot have different vocabulary. This might indicate that the
     summary cannot be applied to q, however in the case where
     free-variables of q and foot match it is benign. In the case where free
     variables match, we temporarily reduce the vocabulary of q to match the
     vocabulary of foot. *)
  [%Trace.info "xs inter q.us: %a" Var.Set.pp xs_in_q] ;
  [%Trace.info "xs inter fv.q %a" Var.Set.pp xs_in_fv_q] ;
  let q, add_back =
    if Set.is_empty xs_in_fv_q then (Sh.exists xs_in_q q, xs_in_q)
    else (q, Var.Set.empty)
  in
  let frame =
    if Set.is_empty xs_in_fv_q then Solver.infer_frame q xs foot else None
  in
  [%Trace.info "frame %a" (Option.pp "%a" pp) frame] ;
  Option.map ~f:(Sh.star post) frame
  |> Option.map ~f:(Sh.extend_us add_back)
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
    let a = Term.var a_
    let main = Term.var main_
    let b = Term.var b_
    let n = Term.var n_
    let endV = Term.var end_
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
