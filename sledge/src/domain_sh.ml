(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

module X = Llair_to_Fol
open Fol

type t = Sh.t [@@deriving equal, sexp]

let pp fs q = Format.fprintf fs "@[{ %a@ }@]" Sh.pp q
let report_fmt_thunk = Fun.flip pp

(* set by cli *)
let simplify_states = ref true
let simplify q = if !simplify_states then Sh.simplify q else q

let init globals =
  IArray.fold globals Sh.emp ~f:(fun global q ->
      match global with
      | {Llair.Global.reg; init= Some (seq, siz)} ->
          let loc = Term.var (X.reg reg) in
          let len = Term.integer (Z.of_int siz) in
          let seq = X.term seq in
          Sh.star q (Sh.seg {loc; bas= loc; len; siz= len; seq})
      | _ -> q )

let join p q =
  [%Trace.call fun {pf} -> pf "%a@ %a" pp p pp q]
  ;
  Some (Sh.or_ p q) |> Option.map ~f:simplify
  |>
  [%Trace.retn fun {pf} -> pf "%a" (Option.pp "%a" pp)]

let is_false = Sh.is_false
let dnf = Sh.dnf
let exec_assume q b = Exec.assume q (X.formula b) |> Option.map ~f:simplify
let exec_kill r q = Exec.kill q (X.reg r) |> simplify

let exec_move res q =
  Exec.move q (IArray.map res ~f:(fun (r, e) -> (X.reg r, X.term e)))
  |> simplify

let exec_inst inst pre =
  ( match (inst : Llair.inst) with
  | Move {reg_exps; _} ->
      Some
        (Exec.move pre
           (IArray.map reg_exps ~f:(fun (r, e) -> (X.reg r, X.term e))))
  | Load {reg; ptr; len; _} ->
      Exec.load pre ~reg:(X.reg reg) ~ptr:(X.term ptr) ~len:(X.term len)
  | Store {ptr; exp; len; _} ->
      Exec.store pre ~ptr:(X.term ptr) ~exp:(X.term exp) ~len:(X.term len)
  | Memset {dst; byt; len; _} ->
      Exec.memset pre ~dst:(X.term dst) ~byt:(X.term byt) ~len:(X.term len)
  | Memcpy {dst; src; len; _} ->
      Exec.memcpy pre ~dst:(X.term dst) ~src:(X.term src) ~len:(X.term len)
  | Memmov {dst; src; len; _} ->
      Exec.memmov pre ~dst:(X.term dst) ~src:(X.term src) ~len:(X.term len)
  | Alloc {reg; num; len; _} ->
      Exec.alloc pre ~reg:(X.reg reg) ~num:(X.term num) ~len
  | Free {ptr; _} -> Exec.free pre ~ptr:(X.term ptr)
  | Nondet {reg; _} -> Some (Exec.nondet pre (Option.map ~f:X.reg reg))
  | Abort _ -> Exec.abort pre )
  |> Option.map ~f:simplify

let exec_intrinsic ~skip_throw r i es q =
  Exec.intrinsic ~skip_throw q (Option.map ~f:X.reg r) (X.reg i)
    (List.map ~f:X.term es)
  |> Option.map ~f:(Option.map ~f:simplify)

let term_eq_class_has_only_vars_in fvs ctx term =
  [%Trace.call fun {pf} ->
    pf "@[<v> fvs: @[%a@] @,ctx: @[%a@] @,term: @[%a@]@]" Var.Set.pp fvs
      Context.pp ctx Term.pp term]
  ;
  let term_has_only_vars_in fvs term =
    Var.Set.subset (Term.fv term) ~of_:fvs
  in
  let term_eq_class = Context.class_of ctx term in
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
        List.fold q.heap current ~f:(fun seg current ->
            if term_eq_class_has_only_vars_in current q.ctx seg.loc then
              List.fold (Context.class_of q.ctx seg.seq) current
                ~f:(fun e c -> Var.Set.union c (Term.fv e))
            else current )
      in
      all_reachable_vars current new_set q
  in
  let r_vars = all_reachable_vars Var.Set.empty wrt q in
  Sh.filter_heap q ~f:(fun seg ->
      term_eq_class_has_only_vars_in r_vars q.ctx seg.loc )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

let and_eqs sub formals actuals q =
  let and_eq formal actual q =
    let actual' = Term.rename sub actual in
    Sh.and_ (Formula.eq (Term.var formal) actual') q
  in
  List.fold2_exn ~f:and_eq formals actuals q

let localize_entry globals actuals formals freturn locals shadow pre entry =
  (* Add the formals here to do garbage collection and then get rid of them *)
  let formals_set = Var.Set.of_list formals in
  let freturn_locals = X.regs (Llair.Reg.Set.add_option freturn locals) in
  let function_summary_pre =
    garbage_collect entry ~wrt:(Var.Set.union formals_set (X.regs globals))
  in
  [%Trace.info "function summary pre %a" pp function_summary_pre] ;
  let foot = Sh.exists formals_set function_summary_pre in
  let xs, foot = Sh.bind_exists ~wrt:pre.Sh.us foot in
  let frame =
    try Option.get_exn (Solver.infer_frame pre xs foot)
    with _ ->
      fail "Solver couldn't infer frame of a garbage-collected pre" ()
  in
  let q'' =
    Sh.extend_us freturn_locals (and_eqs shadow formals actuals foot)
  in
  (q'', frame)

type from_call = {areturn: Var.t option; unshadow: Var.Subst.t; frame: Sh.t}
[@@deriving compare, equal, sexp]

(** Express formula in terms of formals instead of actuals, and enter scope
    of locals: rename formals to fresh vars in formula and actuals, add
    equations between each formal and actual, and quantify fresh vars. *)
let call ~summaries ~globals ~actuals ~areturn ~formals ~freturn ~locals q =
  [%Trace.call fun {pf} ->
    pf
      "@[<hv>actuals: (@[%a@])@ formals: (@[%a@])@ locals: {@[%a@]}@ \
       globals: {@[%a@]}@ q: %a@]"
      (List.pp ",@ " Llair.Exp.pp)
      (List.rev actuals)
      (List.pp ",@ " Llair.Reg.pp)
      (List.rev formals) Llair.Reg.Set.pp locals Llair.Reg.Set.pp globals pp
      q]
  ;
  let actuals = List.map ~f:X.term actuals in
  let areturn = Option.map ~f:X.reg areturn in
  let formals = List.map ~f:X.reg formals in
  let freturn_locals = X.regs (Llair.Reg.Set.add_option freturn locals) in
  let modifs = Var.Set.of_option areturn in
  (* quantify modifs, their current value will be overwritten and so does
     not need to be saved in the freshening renaming *)
  let q = Sh.exists modifs q in
  (* save current values of shadowed formals and locals with a renaming *)
  let q', shadow =
    Sh.freshen q ~wrt:(Var.Set.add_list formals freturn_locals)
  in
  let unshadow = Var.Subst.invert shadow in
  assert (Var.Set.disjoint modifs (Var.Subst.domain shadow)) ;
  (* pass arguments by conjoining equations between formals and actuals *)
  let entry = and_eqs shadow formals actuals q' in
  (* note: locals and formals are in scope *)
  assert (
    Var.Set.subset (Var.Set.add_list formals freturn_locals) ~of_:entry.us
  ) ;
  (* simplify *)
  let entry = simplify entry in
  ( if not summaries then (entry, {areturn; unshadow; frame= Sh.emp})
  else
    let q'', frame =
      localize_entry globals actuals formals freturn locals shadow q' entry
    in
    (q'', {areturn; unshadow; frame}) )
  |>
  [%Trace.retn fun {pf} (entry, {unshadow; frame}) ->
    pf "@[<v>unshadow: %a@ frame: %a@ entry: %a@]" Var.Subst.pp unshadow pp
      frame pp entry]

(** Leave scope of locals: existentially quantify locals. *)
let post locals _ q =
  [%Trace.call fun {pf} ->
    pf "@[<hv>locals: {@[%a@]}@ q: %a@]" Llair.Reg.Set.pp locals Sh.pp q]
  ;
  Sh.exists (X.regs locals) q |> simplify
  |>
  [%Trace.retn fun {pf} -> pf "%a" Sh.pp]

(** Express in terms of actuals instead of formals: existentially quantify
    formals, and apply inverse of fresh variables for formals renaming to
    restore the shadowed variables. *)
let retn formals freturn {areturn; unshadow; frame} q =
  [%Trace.call fun {pf} ->
    pf "@[<v>formals: {@[%a@]}%a%a@ shadows: %a@ q: %a@ frame: %a@]"
      (List.pp ", " Llair.Reg.pp)
      formals
      (Option.pp "@ freturn: %a" Llair.Reg.pp)
      freturn
      (Option.pp "@ areturn: %a" Var.pp)
      areturn Var.Subst.pp unshadow pp q pp frame]
  ;
  let formals = List.map ~f:X.reg formals in
  let freturn = Option.map ~f:X.reg freturn in
  let q =
    match areturn with
    | Some areturn -> (
        (* reenter scope of areturn just before exiting scope of formals *)
        let q = Sh.extend_us (Var.Set.of_ areturn) q in
        (* pass return value *)
        match freturn with
        | Some freturn ->
            Exec.move q (IArray.of_ (areturn, Term.var freturn))
        | None -> Exec.kill q areturn )
    | None -> q
  in
  (* exit scope of formals *)
  let q =
    Sh.exists (Var.Set.add_list formals (Var.Set.of_option freturn)) q
  in
  (* reinstate shadowed values of locals *)
  let q = Sh.rename unshadow q in
  (* reconjoin frame *)
  Sh.star frame q
  (* simplify *)
  |> simplify
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

let resolve_callee lookup ptr q =
  match Llair.Reg.of_exp ptr with
  | Some callee -> (lookup (Llair.Reg.name callee), q)
  | None -> ([], q)

let recursion_beyond_bound = `prune

type summary = {xs: Var.Set.t; foot: t; post: t}

let pp_summary fs {xs; foot; post} =
  Format.fprintf fs "@[<v>xs: @[%a@]@ foot: %a@ post: %a @]" Var.Set.pp xs
    pp foot pp post

let create_summary ~locals ~formals ~entry ~current:(post : Sh.t) =
  [%Trace.call fun {pf} ->
    pf "formals %a@ entry: %a@ current: %a" Llair.Reg.Set.pp formals pp
      entry pp post]
  ;
  let locals = X.regs locals in
  let formals = X.regs formals in
  let foot = Sh.exists locals entry in
  let foot, subst = Sh.freshen ~wrt:(Var.Set.union foot.us post.us) foot in
  let restore_formals q =
    Var.Set.fold formals q ~f:(fun var q ->
        let var = Term.var var in
        let renamed_var = Term.rename subst var in
        Sh.and_ (Formula.eq renamed_var var) q )
  in
  (* Add back the original formals name *)
  let post = Sh.rename subst post in
  let foot = restore_formals foot in
  let post = restore_formals post in
  [%Trace.info "subst: %a" Var.Subst.pp subst] ;
  let xs = Var.Set.inter (Sh.fv foot) (Sh.fv post) in
  let xs = Var.Set.diff xs formals in
  let xs_and_formals = Var.Set.union xs formals in
  let foot = Sh.exists (Var.Set.diff foot.us xs_and_formals) foot in
  let post = Sh.exists (Var.Set.diff post.us xs_and_formals) post in
  let current = Sh.extend_us xs post in
  ({xs; foot; post}, current)
  |>
  [%Trace.retn fun {pf} (fs, _) -> pf "@,%a" pp_summary fs]

let apply_summary q ({xs; foot; post} as fs) =
  [%Trace.call fun {pf} -> pf "fs: %a@ q: %a" pp_summary fs pp q]
  ;
  let xs_in_q = Var.Set.inter xs q.Sh.us in
  let xs_in_fv_q = Var.Set.inter xs (Sh.fv q) in
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
    if Var.Set.is_empty xs_in_fv_q then (Sh.exists xs_in_q q, xs_in_q)
    else (q, Var.Set.empty)
  in
  let frame =
    if Var.Set.is_empty xs_in_fv_q then Solver.infer_frame q xs foot
    else None
  in
  [%Trace.info "frame %a" (Option.pp "%a" pp) frame] ;
  Option.map ~f:(Sh.extend_us add_back) (Option.map ~f:(Sh.star post) frame)
  |>
  [%Trace.retn fun {pf} r ->
    match r with None -> pf "None" | Some q -> pf "@,%a" pp q]

let%test_module _ =
  ( module struct
    let () = Trace.init ~margin:68 ()
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
    let seg_main = Sh.seg {loc= main; bas= b; len= n; siz= n; seq= a}
    let seg_a = Sh.seg {loc= a; bas= b; len= n; siz= n; seq= endV}
    let seg_cycle = Sh.seg {loc= a; bas= b; len= n; siz= n; seq= main}

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
