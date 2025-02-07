(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Llair

(** An upper bound on the number of constant-return disjuncts distinguished
    in each [proc_summaries] record. *)
let max_disjuncts = ref 3

(** An automaton state in the sequence of calls and returns that the
    symbolic executor will attempt to follow. Represented as an integer and
    interpreted as an amount of progress through the shared ref [trace],
    analogously to the [cursor] of [Goal.Sparse_trace] but with a
    lighter-weight representation and different operations/structures
    defined over it. *)
module Automaton_state = struct
  type checkpoint = [`Call of FuncName.t | `Retn of FuncName.t]

  let trace : checkpoint iarray ref = ref IArray.empty
  let set_sparse_trace st = trace := st

  type t = int [@@deriving compare, equal, sexp_of]

  module Map = Int.Map

  (** Because there are always exactly [IArray.length !trace + 1] automaton
      states, we can represent a "table" keyed on automaton states using an
      array of that length. *)
  module Tbl : sig
    type 'a t

    val create : unit -> _ t
    val find_or_add : 'a t -> int -> default:(unit -> 'a) -> 'a
  end = struct
    type 'a t = 'a option array

    let create () = Array.init (IArray.length !trace + 1) ~f:(fun _ -> None)

    let find_or_add tbl key ~default =
      match Array.get tbl key with
      | Some data -> data
      | None ->
          let data = default () in
          Array.set tbl key (Some data) ;
          data
  end

  let join = Int.max
  let is_accepting state = Int.equal state (IArray.length !trace)

  let call_is_next f state =
    state < IArray.length !trace
    &&
    match IArray.get !trace state with
    | `Call f' -> FuncName.equal f f'
    | _ -> false

  let retn_is_next f state =
    state < IArray.length !trace
    &&
    match IArray.get !trace state with
    | `Retn f' -> FuncName.equal f f'
    | _ -> false

  let pp ppf state =
    if is_accepting state then Format.fprintf ppf "accept state"
    else
      match IArray.get !trace state with
      | `Call f -> Format.fprintf ppf "call(%a)" FuncName.pp f
      | `Retn f -> Format.fprintf ppf "retn(%a)" FuncName.pp f
end

(** The shortest distance to the current location from each block that can
    reach it. *)
module Distance_map = struct
  module Map = Block.Map

  (** A distance map [{max; offsets}] conceptually maps each key [k] of
      [offsets] to distance [max - offsets(k)], but is more efficient to
      manipulate than a direct representation. Note that this representation
      is non-canonical: two different values can represent the same distance
      map, if the difference in their [max] field is equal to the pointwise
      difference between each of their offsets. This requires [equal] below
      to do some extra work, but no additional allocation is required. *)
  type t = {max: int; offsets: int Map.t} [@@deriving compare, sexp_of]

  let equal dm dm' =
    dm == dm'
    ||
    if dm.max = dm'.max then Map.equal Int.equal dm.offsets dm'.offsets
    else
      let diff_max = dm.max - dm'.max in
      Map.equal (fun x y -> x - y = diff_max) dm.offsets dm'.offsets

  let pp ppf {max; offsets} =
    Format.fprintf ppf "@[<v 2>" ;
    Map.iteri offsets ~f:(fun ~key ~data:offset ->
        Format.fprintf ppf "@ %a -> %i" Block.pp_ident key (max - offset) ) ;
    Format.fprintf ppf "@]"

  let init = {max= 0; offsets= Map.empty}

  (** Join two distance maps, choosing the shorter distance to each
      location. *)
  let join {max; offsets} {max= max'; offsets= offsets'} =
    (* When [max = max'], the offsets can be joined directly with pointwise
       `Int.min`. When they are unequal, we take the larger one and shift
       the offsets of the other distance map accordingly. *)
    if max = max' then
      { max
      ; offsets=
          Map.union offsets offsets' ~f:(fun _ d d' -> Some (Int.max d d'))
      }
    else if max > max' then
      let diff = max - max' in
      { max
      ; offsets=
          Map.merge offsets offsets' ~f:(fun _ -> function
            | `Left d -> Some d
            | `Right d -> Some (d + diff)
            | `Both (d, d') -> Some (Int.max d (d' + diff)) ) }
    else
      let diff = max' - max in
      { max= max'
      ; offsets=
          Map.merge offsets offsets' ~f:(fun _ -> function
            | `Left d -> Some (d + diff)
            | `Right d -> Some d
            | `Both (d, d') -> Some (Int.max (d + diff) d') ) }

  (** Take one step to [block], incrementing the [max] distance and adding
      [block] with distance [0] (i.e. with offset equal to the new [max]). *)
  let visit block {max; offsets} =
    let max = max + 1 in
    {max; offsets= Map.add ~key:block ~data:max offsets}

  let add_pointwise ~n dm = {dm with max= dm.max + n}

  let iteri ~f {max; offsets} =
    Map.iteri offsets ~f:(fun ~key ~data -> f ~key ~data:(max - data))
end

(** The shortest distance to each visited trace checkpoint on any path to
    some location. *)
module Checkpoint_dists = struct
  open Automaton_state.Map

  type t = int Automaton_state.Map.t [@@deriving compare, equal, sexp_of]

  let pp = pp Int.pp Int.pp
  let init = empty
  let join = union ~f:(fun _ d d' -> Some (min d d'))

  let hit_checkpoint auto_state shortest_path =
    add ~key:auto_state ~data:shortest_path

  let add_pointwise ~n = map ~f:(Int.add n)
end

(** Constant propagation machinery, used to distinguish summaries that take
    an early exit with some error code from "successful" executions that
    reach a non-error-state/non-constant-return exit. *)
module Constant_prop = struct
  (** A standard constant propagation domain to track registers that hold
      compile-time constant integers. *)
  module Env = struct
    module T = struct
      open Reg.Map

      type nonrec t = Z.t t [@@deriving compare, equal, sexp_of]

      let init = empty

      let join =
        merge ~f:(fun _ -> function
          | `Both (c, c') -> if Z.equal c c' then Some c else None
          | _ -> None )

      let set reg const_val env =
        update reg env ~f:(fun _ -> Some const_val)

      let find = find
      let pp = pp Reg.pp Z.pp

      let interpret cmnd env =
        IArray.fold cmnd env ~f:(fun instr consts ->
            match instr with
            | Move {reg_exps; _} ->
                IArray.fold reg_exps consts ~f:(fun (reg, exp) consts ->
                    match exp with
                    | Exp.Integer {data; _} -> set reg data consts
                    | _ -> consts )
            | Load {reg; _}
             |AtomicRMW {reg; _}
             |AtomicCmpXchg {reg; _}
             |Alloc {reg; _}
             |Nondet {reg= Some reg; _}
             |Builtin {reg= Some reg; _} ->
                remove reg consts
            | _ -> consts )

      (** Evaluate a case of a LLAIR [Switch] block terminator, i.e. a
          conditional jump, in the context of some constant propagation
          [env].

          The result is one or more of the following possibilities:

          [`Match env'] indicates that the switch expression definitely
          matches the case expression, and [env'] is [env] updated to
          reflect any additional constant information inferred from the
          match.

          [`Nonmatch env'] is analogous, except that the switch expression
          definitely does not match the case expression

          [`Unknown] indicates that the analysis can't statically resolve
          the jump nor infer any constant information. *)
      let eval_switch env switch_exp case_exp :
          [`Match of t | `Nonmatch of t | `Unknown] iter =
        let const_of_exp e =
          match e with
          | Exp.Reg _ as r -> (
            match
              let* r = Exp.Reg.of_exp r in
              find r env
            with
            | Some const -> Ok const
            | None -> Error e )
          | Exp.Integer {data} -> Ok data
          | _ -> Error e
        in
        let match_binop l r ~consts ~const_and_reg =
          match (const_of_exp l, const_of_exp r) with
          | Ok l, Ok r -> consts l r
          | Ok const, Error nonconst | Error nonconst, Ok const -> (
            match Exp.Reg.of_exp nonconst with
            | Some reg -> const_and_reg const reg
            | None -> Iter.singleton `Unknown )
          | _ -> Iter.singleton `Unknown
        in
        let eq =
          match_binop
            ~consts:(fun l r ->
              Iter.singleton
                (if Z.equal l r then `Match env else `Nonmatch env) )
            ~const_and_reg:(fun const reg ->
              let match_consts = set reg const env in
              Iter.doubleton (`Match match_consts) (`Nonmatch env) )
        in
        let diseq =
          match_binop
            ~consts:(fun l r ->
              Iter.singleton
                (if Z.equal l r then `Nonmatch env else `Match env) )
            ~const_and_reg:(fun const reg ->
              let nonmatch_consts = set reg const env in
              Iter.doubleton (`Match env) (`Nonmatch nonmatch_consts) )
        in
        match const_of_exp switch_exp with
        | Ok switch_const ->
            Iter.singleton
              ( match const_of_exp case_exp with
              | Ok case_const ->
                  if Z.equal switch_const case_const then `Match env
                  else `Nonmatch env
              | _ -> `Unknown )
        | Error Exp.(Ap2 (Eq, _, l, r)) ->
            if Exp.is_true case_exp then eq l r
            else if Exp.is_false case_exp then diseq l r
            else Iter.singleton `Unknown
        | Error Exp.(Ap2 (Dq, _, l, r)) ->
            if Exp.is_true case_exp then diseq l r
            else if Exp.is_false case_exp then eq l r
            else Iter.singleton `Unknown
        | _ -> Iter.singleton `Unknown
    end

    include T
    module Map = Map.Make (T)
  end

  (** Separately from the constant-propagation abstract domain, we
      optionally tag procedure summaries with their integer-constant return
      value. *)
  module Retn_value = struct
    type t = Z.t option [@@deriving equal]

    let top = None
    let join l r = if Option.equal Z.equal l r then l else None

    let pp ppf = function
      | Some x -> Format.fprintf ppf "retn: %a" Z.pp x
      | _ -> Format.fprintf ppf "retn: any"
  end
end

(** A [summary] is composed of

    (1) the length of the shortest path from procedure entry to the current
    location,

    (2) the current automaton state,

    (3) a distance map upper-bounding the shortest path from blocks to the
    current location, and

    (4) the lengths of the shortest paths to each trace checkpoint that has
    already been reached from the program entry.

    (5) the integer-constant return value associated with that summary,
    optionally *)
module Summary = struct
  module Dist_info = struct
    type t =
      { shortest_path: int
      ; auto_state: Automaton_state.t
      ; dists: Distance_map.t
      ; checkpoint_dists: Checkpoint_dists.t }
    [@@deriving compare, equal, sexp_of]

    let join
        ( {shortest_path= n; auto_state= a; dists= d; checkpoint_dists= c}
        as di )
        ( { shortest_path= n'
          ; auto_state= a'
          ; dists= d'
          ; checkpoint_dists= c' } as di' ) =
      if di == di' then di
      else if a > a' then di
      else if a < a' then di'
      else
        { shortest_path= Int.min n n'
        ; auto_state= Automaton_state.join a a'
        ; dists= Distance_map.join d d'
        ; checkpoint_dists= Checkpoint_dists.join c c' }

    let pp ppf {shortest_path; auto_state; dists; checkpoint_dists} =
      Format.fprintf ppf
        "@[<v 4>Distance Computation Summary@ shortest path: %i@ next \
         trace checkpoint: %a@ distances: %a@ checkpoint distances: %a@]"
        shortest_path Automaton_state.pp auto_state Distance_map.pp dists
        Checkpoint_dists.pp checkpoint_dists
  end

  (* Summaries and their basic operations: join, transfer, apply, etc. *)
  type summary = {di: Dist_info.t; retn: Constant_prop.Retn_value.t}
  [@@deriving equal]

  type absstate = {di: Dist_info.t; consts: Constant_prop.Env.t}
  [@@deriving compare, equal, sexp_of]

  type t = Summary of summary | Bottom [@@deriving equal]

  let summ_of_absstate return_exp_opt absstate =
    let retn =
      let* return_exp = return_exp_opt in
      match return_exp with
      | Exp.Integer {data; _} -> Some data
      | Exp.Reg _ as r ->
          let* reg = Reg.of_exp r in
          Constant_prop.Env.find reg absstate.consts
      | _ -> None
    in
    Summary {di= absstate.di; retn}

  let init auto_state =
    let di =
      { Dist_info.shortest_path= 0
      ; auto_state
      ; dists= Distance_map.init
      ; checkpoint_dists= Checkpoint_dists.init }
    in
    {di; consts= Constant_prop.Env.init}

  let join_summary {di; retn} {di= di'; retn= retn'} =
    { di= Dist_info.join di di'
    ; retn= Constant_prop.Retn_value.join retn retn' }

  let join_absstate {di; consts} {di= di'; consts= consts'} =
    { di= Dist_info.join di di'
    ; consts= Constant_prop.Env.join consts consts' }

  let join x x' =
    match (x, x') with
    | Summary s, Summary s' -> Summary (join_summary s s')
    | (Summary _ as s), _ | _, (Summary _ as s) -> s
    | _ -> Bottom

  let pp_summary ppf {di; retn} =
    Format.fprintf ppf "Distance Summary (%a): %a"
      Constant_prop.Retn_value.pp retn Dist_info.pp di

  let pp ppf = function
    | Bottom -> Format.fprintf ppf "Distance Summary: BOTTOM"
    | Summary s -> pp_summary ppf s

  (** A disjunction of abstract states, such that no two disjuncts share the
      same [consts] information. When a disjunction is [update]d with a new
      abstract state, it is added as a disjunct if it has a new [consts] or
      joined onto the existing state for that [consts] otherwise. *)
  module Disjunction : sig
    type t

    val pp : t pp
    val iter : t -> absstate iter
    val singleton : absstate -> t
    val update : absstate -> t -> t
    val to_absstate : t -> absstate
    val get_state_by_consts : Constant_prop.Env.t -> t -> absstate option
  end = struct
    include Constant_prop.Env.Map

    type nonrec t = absstate t

    let singleton absstate = singleton absstate.consts absstate

    let join_opt absstate = function
      | None -> Some absstate
      | Some absstate' -> Some (join_absstate absstate' absstate)

    let update absstate = update absstate.consts ~f:(join_opt absstate)

    (* [get_exn] is safe here because it is impossible to construct an empty
       disjunction *)
    let to_absstate disj =
      fold disj None ~f:(fun ~key:_ ~data -> join_opt data)
      |> Option.get_exn

    let pp ppf disj =
      Format.fprintf ppf "@[<v 2>disjunction: {" ;
      iteri disj ~f:(fun ~key ~(data : absstate) ->
          Format.fprintf ppf "@ CONSTS(%a) -> %a" Constant_prop.Env.pp key
            Dist_info.pp data.di ) ;
      Format.fprintf ppf "}@]"

    let get_state_by_consts = find
    let iter disj f = iter disj ~f
  end

  let intraproc_transfer ~block (absstate : absstate) =
    if Automaton_state.is_accepting absstate.di.auto_state then absstate
    else
      let dists = Distance_map.visit block absstate.di.dists in
      let shortest_path = absstate.di.shortest_path + 1 in
      let consts =
        if !max_disjuncts = 0 then absstate.consts
        else Constant_prop.Env.interpret block.cmnd absstate.consts
      in
      {di= {absstate.di with dists; shortest_path}; consts}

  let apply (summary : summary) areturn (absstate : absstate) =
    if Automaton_state.is_accepting absstate.di.auto_state then absstate
    else
      let dists =
        let open Distance_map in
        join summary.di.dists
          (add_pointwise ~n:summary.di.shortest_path absstate.di.dists)
      in
      let checkpoint_dists =
        let open Checkpoint_dists in
        join absstate.di.checkpoint_dists
          (add_pointwise ~n:absstate.di.shortest_path
             summary.di.checkpoint_dists )
      in
      let consts =
        let* retn_val = summary.retn in
        let+ retn_reg = areturn in
        Constant_prop.Env.set retn_reg retn_val absstate.consts
      in
      { di=
          { shortest_path=
              absstate.di.shortest_path + summary.di.shortest_path
          ; auto_state= summary.di.auto_state
          ; dists
          ; checkpoint_dists }
      ; consts= Option.value consts ~default:absstate.consts }

  let hit_checkpoint (absstate : absstate) =
    let checkpoint_dists =
      Checkpoint_dists.hit_checkpoint absstate.di.auto_state
        absstate.di.shortest_path absstate.di.checkpoint_dists
    in
    { absstate with
      di=
        { absstate.di with
          checkpoint_dists
        ; auto_state= absstate.di.auto_state + 1 } }

  (* Machinery to deal with pausing/restarting _intraprocedural_ analyses
     during tabulation without performing redundant work. We associate a
     (mutable) invariant map and worklist with each intraprocedural analysis
     problem (indexed by function name and initial automaton state). *)
  type intraproc_work =
    {state: absstate; loc: Block.t; check_convergence: bool}

  let intraproc_work ?(check_convergence = true) state loc =
    {state; loc; check_convergence}

  type intraproc_state =
    { worklist: intraproc_work Stack.t
    ; invariant_map: Disjunction.t Block.Tbl.t }

  let set_invariant_map {invariant_map; _} block state =
    Block.Tbl.update invariant_map block ~f:(function
      | None -> Some (Disjunction.singleton state)
      | Some disj -> Some (Disjunction.update state disj) )

  let intraproc_analyses :
      intraproc_state Automaton_state.Tbl.t FuncName.Tbl.t =
    FuncName.Tbl.create ()

  let intraproc_analysis f a =
    let analyses_over_f =
      FuncName.Tbl.find_or_add intraproc_analyses f
        ~default:Automaton_state.Tbl.create
    in
    Automaton_state.Tbl.find_or_add analyses_over_f a ~default:(fun () ->
        {worklist= Stack.create (); invariant_map= Block.Tbl.create ()} )

  (* Interprocedural analysis: summary queries and tabulation *)

  (** For a given procedure/automaton-state, we can have multiple partial
      summaries distinguished by their integer-constant return values
      ([const_retns]), along with at most one summary with unknown return
      value ([unknown_retn]); each [proc_summaries] record is a mutable
      representation of such a collection of partial summaries *)
  type proc_summaries =
    {const_retns: summary Z.Tbl.t; mutable unknown_retn: t option}

  (** The tabulation algorithm's summary table, supporting memoization and
      lookup. *)
  let summary_table : proc_summaries Automaton_state.Tbl.t FuncName.Tbl.t =
    FuncName.Tbl.create ()

  let memoize f a (new_summ : t) =
    let summaries_of_f =
      FuncName.Tbl.find_or_add summary_table f
        ~default:Automaton_state.Tbl.create
    in
    let summs =
      Automaton_state.Tbl.find_or_add summaries_of_f a ~default:(fun _ ->
          { const_retns= Z.Tbl.create ~size:!max_disjuncts ()
          ; unknown_retn= None } )
    in
    match new_summ with
    | Summary ({retn= Some const_retn; _} as summ)
      when Z.Tbl.length summs.const_retns < !max_disjuncts ->
        [%Dbg.info
          "memoizing %a as const_retn: %a" FuncName.pp f pp_summary summ] ;
        Z.Tbl.update summs.const_retns const_retn ~f:(function
          | Some existing_summ -> Some (join_summary existing_summ summ)
          | None -> Some summ )
    | _ ->
        [%Dbg.info
          "memoizing %a as unknown retn: %a" FuncName.pp f pp new_summ] ;
        summs.unknown_retn <-
          ( match summs.unknown_retn with
          | Some existing_summ -> Some (join existing_summ new_summ)
          | None -> Some new_summ )

  let lookup_summaries f a =
    let summaries_of_f =
      FuncName.Tbl.find_or_add summary_table f
        ~default:Automaton_state.Tbl.create
    in
    Automaton_state.Tbl.find_or_add summaries_of_f a ~default:(fun _ ->
        { const_retns= Z.Tbl.create ~size:!max_disjuncts ()
        ; unknown_retn= None } )

  exception Summary_query of (FuncName.t * Automaton_state.t)

  let lookup_summaries_exn f a =
    match lookup_summaries f a with
    | {unknown_retn= Some _; _} as s -> s
    | _ -> raise_notrace (Summary_query (f, a))

  (** Interprocedural worklist, consisting of two types of work: "Summarize"
      work elements require computing a summary of some function in some
      initial automaton state, while "Fix" elements require computing the
      fixed point of some summary of [caller_name] with a recursive call to
      [callee_name]. *)
  module Worklist = struct
    type work_elt =
      | Summarize of FuncName.t * Automaton_state.t
      | Fix of
          { caller_name: FuncName.t
          ; caller_state: Automaton_state.t
          ; callee_name: FuncName.t
          ; callee_state: Automaton_state.t
          ; prev_summary: t }

    let init f a = Stack.create () $> Stack.push (Summarize (f, a))

    let contains callee init_state worklist =
      Iter.(exists (of_stack worklist)) ~f:(function
        | Fix _ -> false
        | Summarize (f, a) ->
            Automaton_state.equal init_state a && FuncName.equal callee f )

    let push f a callee_f callee_a wl =
      Stack.push (Summarize (f, a)) wl ;
      Stack.push (Summarize (callee_f, callee_a)) wl

    let pop = Stack.pop
  end

  let summarize_call callee areturn (absstate : absstate) =
    let prestate =
      if Automaton_state.call_is_next callee absstate.di.auto_state then
        hit_checkpoint absstate
      else absstate
    in
    let {const_retns; unknown_retn} =
      lookup_summaries callee prestate.di.auto_state
    in
    match unknown_retn with
    | None -> Error (callee, prestate.di.auto_state)
    | Some unknown_retn_summary ->
        let summaries =
          match unknown_retn_summary with
          | Summary s -> Iter.cons s (Z.Tbl.values const_retns)
          | Bottom -> Z.Tbl.values const_retns
        in
        Ok
          ( unknown_retn_summary
          , Iter.map summaries ~f:(fun summary ->
                let poststate = apply summary areturn prestate in
                if
                  Automaton_state.retn_is_next callee
                    poststate.di.auto_state
                then hit_checkpoint poststate
                else poststate ) )

  (** Compute and return a summary of the given [func] in the given
      [init_state]. Raises a [Summary_query] if a callee summary is required
      that has not yet been computed. *)
  let summarize func (init_state : absstate) worklist =
    if
      Automaton_state.is_accepting init_state.di.auto_state
      || Func.is_undefined func
    then Summary {di= init_state.di; retn= Constant_prop.Retn_value.top}
    else
      let analysis =
        intraproc_analysis func.name init_state.di.auto_state
      in
      let push absstate block =
        Stack.push (intraproc_work absstate block) analysis.worklist
      in
      push init_state func.entry ;
      while not (Stack.is_empty analysis.worklist) do
        let {state= worklist_absstate; loc= block; check_convergence} =
          Stack.pop analysis.worklist
        in
        ignore
        @@ let+ state, converged =
             let+ new_state =
               let transfer_res =
                 intraproc_transfer ~block worklist_absstate
               in
               match block.term with
               | Return {exp; _} -> (
                 match summ_of_absstate exp transfer_res with
                 | Summary {retn= Some _; _} as s ->
                     memoize func.name init_state.di.auto_state s ;
                     None
                 | _ -> Some transfer_res )
               | _ -> Some transfer_res
             in
             match
               Block.Tbl.find analysis.invariant_map block
               >>= Disjunction.get_state_by_consts new_state.consts
             with
             | None -> (new_state, false)
             | Some old_state ->
                 let joined_absstate = join_absstate old_state new_state in
                 (joined_absstate, equal_absstate joined_absstate old_state)
           in
           if (not check_convergence) || not converged then (
             ( match block.term with
             | Switch {key; tbl; els; _} ->
                 if !max_disjuncts = 0 then (
                   IArray.iter tbl ~f:(fun (_, jmp) -> push state jmp.dst) ;
                   push state els.dst )
                 else
                   let has_unknown_branch = ref (IArray.is_empty tbl) in
                   IArray.iter tbl ~f:(fun (case, jmp) ->
                       (Constant_prop.Env.eval_switch state.consts key case)
                         (function
                         | `Nonmatch consts ->
                             push {state with consts} els.dst
                         | `Match consts -> push {state with consts} jmp.dst
                         | `Unknown ->
                             has_unknown_branch := true ;
                             push state jmp.dst ) ) ;
                   if !has_unknown_branch then push state els.dst
             | Iswitch {tbl; _} ->
                 IArray.iter tbl ~f:(fun jmp -> push state jmp.dst)
             | Return _ | Throw _ | Abort _ | Unreachable -> ()
             | Call {callee; return; areturn; _} -> (
                 (* Summarize a procedure call using existing summaries if
                    possible, raising a [Summary_query] if one or more is
                    missing. Push a [Fix] element onto the interprocedural
                    worklist if [callee] is recursive. *)
                 let call {func= callee; _} =
                   let is_recursive () =
                     let directly =
                       FuncName.equal func.name callee.name
                       && Automaton_state.equal init_state.di.auto_state
                            state.di.auto_state
                     in
                     let mutually () =
                       Worklist.contains callee.name state.di.auto_state
                         worklist
                     in
                     directly || mutually ()
                   in
                   let interproc_fixed_point prev_summary =
                     let fix_on_call =
                       Worklist.Fix
                         { caller_name= func.name
                         ; caller_state= init_state.di.auto_state
                         ; callee_name= callee.name
                         ; callee_state= state.di.auto_state
                         ; prev_summary }
                     in
                     Stack.push fix_on_call worklist
                   in
                   match summarize_call callee.name areturn state with
                   | Ok (prev_summary, retn_states) ->
                       if is_recursive () then
                         interproc_fixed_point prev_summary ;
                       retn_states (fun retn_state ->
                           push retn_state return.dst )
                   | Error sq ->
                       if is_recursive () then interproc_fixed_point Bottom
                       else (
                         (* re-add the current worklist element so as to
                            continue from the same point once the requisite
                            summary is computed *)
                         push worklist_absstate block ;
                         raise_notrace (Summary_query sq) )
                 in
                 match callee with
                 | Direct f -> call f
                 | Indirect {candidates; _} ->
                     IArray.iter candidates ~f:call
                 | Intrinsic _ -> push state return.dst ) ) ;
             set_invariant_map analysis block state )
      done ;
      Func.fold_cfg func Bottom ~f:(fun block acc_summary ->
          match block.term with
          | Return {exp; _} ->
              let return_state =
                Block.Tbl.find analysis.invariant_map block
                |> Option.map_or ~default:Bottom ~f:(fun disj ->
                       [%Dbg.info "Return state: %a" Disjunction.pp disj] ;
                       summ_of_absstate exp (Disjunction.to_absstate disj) )
              in
              join acc_summary return_state
          | _ -> acc_summary )

  let top_down pgm ~entry =
    let worklist = Worklist.init entry 0 in
    while not (Stack.is_empty worklist) do
      match Worklist.pop worklist with
      | Fix
          { caller_name= caller_f
          ; caller_state= caller_a
          ; callee_name= callee_f
          ; callee_state= callee_a
          ; prev_summary } ->
          let {unknown_retn; _} = lookup_summaries callee_f callee_a in
          (* If there has been no change in the summary for which a fixed
             point is requested, then analysis has converged and there's
             nothing to do.

             `Fix` work elements are always pushed immediately beneath a
             corresponding `Summarize`, so we compare the result summary of
             that `Summarize` to the original (possibly unsound) summary
             that was applied to compute it.

             Otherwise, propagate any new dataflow to each recursive return
             site in the intraprocedural analysis for this summary and
             continue. *)
          if Option.exists unknown_retn ~f:(equal prev_summary) then ()
          else
            let caller = FuncName.Map.find_exn caller_f pgm.functions in
            let intraproc_state = intraproc_analysis caller_f caller_a in
            Func.fold_cfg caller () ~f:(fun blk () ->
                let resummarize_call () =
                  Block.Tbl.get intraproc_state.invariant_map blk
                  |> Option.iter ~f:(fun disj ->
                         Disjunction.iter disj (fun absstate ->
                             let work =
                               intraproc_work ~check_convergence:false
                                 absstate blk
                             in
                             Stack.push work intraproc_state.worklist ) )
                in
                let is_recursive_callee {func; _} =
                  FuncName.equal callee_f func.name
                in
                match blk.term with
                | Call {callee= Direct tgt; _} ->
                    if is_recursive_callee tgt then resummarize_call ()
                | Call {callee= Indirect {candidates; _}; _} ->
                    if IArray.exists candidates ~f:is_recursive_callee then
                      resummarize_call ()
                | _ -> () )
      | Summarize (f, a) -> (
        match (lookup_summaries f a).unknown_retn with
        | Some _ ->
            ()
            (* this summary has already been computed on some other path *)
        | None -> (
          try
            summarize
              (FuncName.Map.find_exn f pgm.functions)
              (init a) worklist
            |> memoize f a
          with Summary_query (callee_f, callee_a) ->
            Worklist.push f a callee_f callee_a worklist ) )
    done ;
    let {const_retns; unknown_retn} = lookup_summaries_exn entry 0 in
    let combined_summary =
      Option.get_exn unknown_retn
      |> Z.Tbl.fold const_retns ~f:(fun ~key:_ ~data -> function
           | Bottom -> Summary data
           | Summary s -> Summary (join_summary s data) )
    in
    match combined_summary with
    | Summary s ->
        [%Dbg.info
          "Combined summary of %a:@\n%a" FuncName.pp entry pp_summary s] ;
        Ok s
    | _ -> Error (Format.dprintf "No path found through entrypoint func")
end

let top_down pgm ~entry sparse_trace =
  Automaton_state.set_sparse_trace sparse_trace ;
  let ( let* ) = Result.bind in
  let* summ = Summary.top_down pgm ~entry in
  if Automaton_state.is_accepting summ.di.auto_state then
    Ok
      (Distance_map.iteri summ.di.dists ~f:(fun ~key:block ~data:dist ->
           Block.set_goal_distance dist block ) )
  else
    Error
      (Format.dprintf "Unable to find a path to trace checkpoint %a"
         Automaton_state.pp summ.di.auto_state )
