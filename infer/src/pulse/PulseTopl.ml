(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
module BaseDomain = PulseBaseDomain
module Memory = PulseBaseMemory
open IOption.Let_syntax

type value = AbstractValue.t [@@deriving compare, equal]

type static_type = Typ.t [@@deriving compare, equal]

type value_and_type = value * static_type [@@deriving compare, equal]

type event =
  | ArrayWrite of {aw_array: value; aw_index: value}
  | Call of {return: value_and_type option; arguments: value_and_type list; procname: Procname.t}
[@@deriving compare, equal]

let pp_comma_seq f xs = Pp.comma_seq ~print_env:Pp.text_break f xs

let pp_value = AbstractValue.pp

(* When printing types below, use only this function, to make sure it's done always in the same way. *)
let pp_type f type_ = F.fprintf f "%s" (Typ.to_string type_)

(* When printing Procname.t below for matching purposes, use only this function. *)
let pp_procname f procname = Procname.pp_fullname_only f procname

let pp_value_and_type f (value, type_) = F.fprintf f "@[%a:@ %a@]" pp_value value pp_type type_

let pp_event f = function
  | ArrayWrite {aw_array; aw_index} ->
      F.fprintf f "@[ArrayWrite %a[%a]@]" AbstractValue.pp aw_array AbstractValue.pp aw_index
  | Call {return; arguments; procname} ->
      F.fprintf f "@[call@ %a=%a(%a)@]" (Pp.option pp_value_and_type) return pp_procname procname
        (pp_comma_seq pp_value_and_type) arguments


type vertex = ToplAutomaton.vindex [@@deriving compare, equal]

type register = ToplAst.register_name [@@deriving compare, equal]

type configuration = {vertex: vertex; memory: (register * value) list} [@@deriving compare, equal]

type substitution = (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t

type 'a substitutor = substitution * 'a -> substitution * 'a

let sub_value (sub, value) =
  match AbstractValue.Map.find_opt value sub with
  | Some (v, _history) ->
      (sub, v)
  | None ->
      let v = AbstractValue.mk_fresh () in
      let sub = AbstractValue.Map.add value (v, ValueHistory.epoch) sub in
      (sub, v)


let sub_list : 'a substitutor -> 'a list substitutor =
 fun sub_elem (sub, xs) ->
  let f (sub, xs) x =
    let sub, x = sub_elem (sub, x) in
    (sub, x :: xs)
  in
  let sub, xs = List.fold ~init:(sub, []) ~f xs in
  (sub, List.rev xs)


type pulse_state =
  { pulse_post: BaseDomain.t
  ; pulse_pre: BaseDomain.t
  ; path_condition: Formula.t
  ; get_reachable: unit -> AbstractValue.Set.t }

module Constraint : sig
  type predicate

  type t [@@deriving compare, equal]

  type operator = LeadsTo | NotLeadsTo | Builtin of Binop.t

  val make : operator -> Formula.operand -> Formula.operand -> predicate

  val true_ : t

  val false_ : t

  val and_predicate : predicate -> t -> t

  val and_constr : t -> t -> t

  val and_n : t list -> t

  val normalize : t -> t

  val negate : t list -> t list
  (** computes ¬(c1∨...∨cm) as d1∨...∨dn, where n=|c1|x...x|cm| *)

  val eliminate_exists : keep:(AbstractValue.t -> bool) -> t -> t
  (** quantifier elimination *)

  val size : t -> int

  val substitute : t substitutor

  val simplify : pulse_state -> t -> t
  (** Drop constraints implied by Pulse state. Detect infeasible constraints. *)

  val pp : F.formatter -> t -> unit
end = struct
  type operator = LeadsTo | NotLeadsTo | Builtin of Binop.t [@@deriving compare, equal]

  type predicate = True | False | Binary of operator * Formula.operand * Formula.operand
  [@@deriving compare, equal]

  type t = predicate list [@@deriving compare, equal]

  let make binop lhs rhs = Binary (binop, lhs, rhs)

  let true_ = []

  let false_ = [False]

  let is_trivially_true (predicate : predicate) =
    match predicate with
    | True ->
        true
    | Binary (Builtin Eq, AbstractValueOperand l, AbstractValueOperand r)
      when AbstractValue.equal l r ->
        true
    | _ ->
        false


  let is_trivially_false (predicate : predicate) (constr : predicate list) =
    match predicate with False -> true | _ -> ( match constr with [False] -> true | _ -> false )


  let and_predicate predicate constr =
    if is_trivially_false predicate constr then [False]
    else if is_trivially_true predicate then constr
    else predicate :: constr


  let and_constr constr_a constr_b = List.rev_append constr_a constr_b

  let and_n constraints = List.concat_no_order constraints

  let normalize constr = List.dedup_and_sort ~compare:compare_predicate constr

  let negate_predicate (predicate : predicate) : predicate =
    match predicate with
    | True ->
        False
    | False ->
        True
    | Binary (LeadsTo, l, r) ->
        Binary (NotLeadsTo, l, r)
    | Binary (NotLeadsTo, l, r) ->
        Binary (LeadsTo, l, r)
    | Binary (Builtin Eq, l, r) ->
        Binary (Builtin Ne, l, r)
    | Binary (Builtin Ne, l, r) ->
        Binary (Builtin Eq, l, r)
    | Binary (Builtin Ge, l, r) ->
        Binary (Builtin Lt, r, l)
    | Binary (Builtin Gt, l, r) ->
        Binary (Builtin Le, r, l)
    | Binary (Builtin Le, l, r) ->
        Binary (Builtin Gt, r, l)
    | Binary (Builtin Lt, l, r) ->
        Binary (Builtin Ge, r, l)
    | Binary (Builtin _, _, _) ->
        L.die InternalError
          "PulseTopl.negate_predicate should handle all outputs of PulseTopl.binop_to"


  let negate disjunction = IList.product (List.map ~f:(List.map ~f:negate_predicate) disjunction)

  let size constr = List.length constr

  let substitute_predicate (sub, predicate) =
    let avo x : Formula.operand = AbstractValueOperand x in
    match (predicate : predicate) with
    | Binary (op, AbstractValueOperand l, AbstractValueOperand r) ->
        let sub, l = sub_value (sub, l) in
        let sub, r = sub_value (sub, r) in
        (sub, Binary (op, avo l, avo r))
    | Binary (op, AbstractValueOperand l, r) ->
        let sub, l = sub_value (sub, l) in
        (sub, Binary (op, avo l, r))
    | Binary (op, l, AbstractValueOperand r) ->
        let sub, r = sub_value (sub, r) in
        (sub, Binary (op, l, avo r))
    | _ ->
        (sub, predicate)


  let substitute = sub_list substitute_predicate

  let pp_operator f operator =
    match operator with
    | Builtin op ->
        F.fprintf f "@[%a@]" Binop.pp op
    | LeadsTo ->
        F.fprintf f "~~>"
    | NotLeadsTo ->
        F.fprintf f "!~>"


  let pp_predicate f pred =
    match pred with
    | Binary (op, l, r) ->
        F.fprintf f "@[%a%a%a@]" PulseFormula.pp_operand l pp_operator op PulseFormula.pp_operand r
    | True ->
        F.fprintf f "true"
    | False ->
        F.fprintf f "false"


  let pp = Pp.seq ~sep:"∧" pp_predicate

  let simplify pulse_state constr : t =
    (* Algorithm. We do a best effort attempt at achieving two goals: (a) drop redundant
       predicates, and (b) detect unsatisfiability. The meaning of the formula should not change.
       A predicate is not redundant on its own. For an example, consider the three predicates
       a=b, b=c, c=a: we can drop any of them while preserving semantics, but not two of them.
       For speed, we won't attempt to drop as *many* predicates as possible. Instead, we'll look
       at predicates one by one and ask if they are implied by pulse_state together with the (Topl)
       predicates we had previously decided to keep. In the a=b,b=c,c=a example we'd drop the last
       one, where "last" is with respect to some unspecified iteration order. Same order-related
       considerations apply to other predicates, such as LeadsTo.

       The iteration order is unspecified in the sense that it's dangerous for callers of this
       function to rely on it. But, we will look at predicates in some fixed order, convenient for
       implementation.

       (Terminology for below:
          "predicate" = value of type Constraint.predicate
          "constraint" = set/list of predicates
          "path predicate" = value of shape Constraint.Builtin (_, _, _)
          "heap predicate" = value of shape Constraint.LeadsTo (_,_) or Constraint.NotLeadsTo (_,_)
          "path condition" = (possibly modified) pulse_state.path_condition (of type Formula.t)
          "heap" = (possibly modified) pulse_state.pulse_post.heap (of type Memory.t)
          "new_eqs" = value of type Formula.new_eqs
       end of terminology aside.)

       Whenever we process a predicate, we first re-write it according to `Formula.get_var_repr`
       applied to the current path condition.

       First, we iterate through path predicates. If the predicate is implied by the path condition
       or by the heap, we drop it. Otherwise we conjoin it to the path condition and we normalize
       the path condition. Both conjoining and normalization may render the path condition
       unsatisfiable; if they do not then they may generate new deduced equalities. If the predicate
       was an equality, we also add it to new equalities, before updating the heap according.
       When we apply new equalities as substitutions to the heap, we have one more opportunity to
       detect unsatisfiability. After these updates to the path condition and the heap, we are ready
       to process the next predicate. (The invariant we try to maintain here is that the path
       condition and the heap reflect all the information in the predicates that have been
       processed. This may be expensive but: (a) we do not apply this algorithm often; and (b) Topl
       tends to track few predicates.)

       Next, we iterate through LeadsTo predicates. If they are implied by the heap, we drop them;
       otherwise, we keep them and we add them to the heap. (For implementation, we do not actually
       add them to the heap, but to a fake-heap.)

       Next, we iterate through NotLeadsTo predicates. If the predicate contradicts the union of the
       heap with the fake-heap, we detected unsatisfiability. Otherwise, we keep the predicate.
    *)
    let module ConstraintByType = struct
      type binary = (Formula.operand * Formula.operand) list

      type t =
        { path_constr: (Binop.t * Formula.operand * Formula.operand) list
        ; leadsto_constr: binary
        ; notleadsto_constr: binary }

      let empty = {path_constr= []; leadsto_constr= []; notleadsto_constr= []}
    end in
    let module C = ConstraintByType in
    let open SatUnsat.Import in
    let go () =
      let* in_constr =
        let f in_constr predicate =
          match predicate with
          | Binary (Builtin op, l, r) ->
              Sat C.{in_constr with path_constr= (op, l, r) :: in_constr.path_constr}
          | Binary (LeadsTo, l, r) ->
              Sat C.{in_constr with leadsto_constr= (l, r) :: in_constr.leadsto_constr}
          | Binary (NotLeadsTo, l, r) ->
              Sat C.{in_constr with notleadsto_constr= (l, r) :: in_constr.notleadsto_constr}
          | True ->
              Sat in_constr
          | False ->
              Unsat
        in
        SatUnsat.list_fold constr ~f ~init:C.empty
      in
      let out_constr = C.empty in
      let rep path_condition (operand : Formula.operand) : Formula.operand =
        match operand with
        | AbstractValueOperand v ->
            AbstractValueOperand (Formula.get_var_repr path_condition v)
        | other ->
            other
      in
      (* Handle path predicates. *)
      let* _path_condition, heap, out_constr =
        let f (path_condition, heap, out_constr) ((op : Binop.t), l, r) =
          let l, r = (rep path_condition l, rep path_condition r) in
          let is_implied_by_pathcondition () =
            Formula.prune_binop ~negated:true op l r path_condition
            |> SatUnsat.sat |> Option.is_none
          in
          let is_implied_by_heap () =
            match (op, l, r) with
            | Ne, AbstractValueOperand l, AbstractValueOperand r ->
                Memory.is_allocated heap l && Memory.is_allocated heap r
            | Ne, AbstractValueOperand v, ConstOperand (Cint z)
            | Ne, ConstOperand (Cint z), AbstractValueOperand v ->
                IntLit.iszero z && Memory.is_allocated heap v
            | _ ->
                false
          in
          let is_trivial_unsat () =
            match op with Ne | Gt | Lt -> Formula.equal_operand l r | _ -> false
          in
          let is_trivial_valid () =
            match op with Eq | Ge | Le -> Formula.equal_operand l r | _ -> false
          in
          if is_trivial_unsat () then Unsat
          else if is_trivial_valid () || is_implied_by_heap () || is_implied_by_pathcondition ()
          then (* drop (op.l.r) *) Sat (path_condition, heap, out_constr)
          else
            let* path_condition, new_eqs_a =
              Formula.prune_binop ~negated:false op l r path_condition
            in
            let new_eqs =
              let new_eqs = RevList.empty in
              let new_eqs =
                match (op, l, r) with
                | Eq, AbstractValueOperand lv, AbstractValueOperand rv ->
                    RevList.cons (Formula.Equal (lv, rv)) new_eqs
                | Eq, ConstOperand (Cint z), AbstractValueOperand v
                | Eq, AbstractValueOperand v, ConstOperand (Cint z)
                  when IntLit.iszero z ->
                    RevList.cons (Formula.EqZero v) new_eqs
                | _ ->
                    new_eqs
              in
              let ( ++ ) = RevList.append in
              new_eqs ++ new_eqs_a
            in
            let* heap =
              let incorporate_eq heap (eq : Formula.new_eq) =
                match eq with
                | Equal (v1, v2) ->
                    Memory.subst_var ~for_summary:true (v1, v2) heap
                | EqZero v ->
                    if Memory.is_allocated heap v then Unsat else Sat heap
              in
              SatUnsat.list_fold (RevList.to_list new_eqs) ~init:heap ~f:incorporate_eq
            in
            Sat
              ( path_condition
              , heap
              , C.{out_constr with path_constr= (op, l, r) :: out_constr.path_constr} )
        in
        SatUnsat.list_fold in_constr.C.path_constr ~f
          ~init:(pulse_state.path_condition, pulse_state.pulse_post.heap, out_constr)
      in
      (* Digresion: heap (and fake-heap) traversal. *)
      let leadsto fake_heap heap source target =
        let pointsto v =
          let fake_successors =
            match AbstractValue.Map.find_opt v fake_heap with
            | Some fake_successors ->
                fake_successors
            | None ->
                []
          in
          let true_successors =
            match Memory.find_opt v heap with
            | Some edges ->
                Memory.Edges.bindings edges |> List.map ~f:(function _access, (w, _history) -> w)
            | None ->
                []
          in
          fake_successors @ true_successors
        in
        let seen = ref AbstractValue.Set.empty in
        let rec dfs value =
          seen := AbstractValue.Set.add value !seen ;
          let successors = pointsto value in
          let f succ = (not (AbstractValue.Set.mem succ !seen)) && dfs succ in
          AbstractValue.equal value target || List.exists successors ~f
        in
        dfs source
      in
      (* Handle LeadsTo predicates.. *)
      let fake_heap, out_constr =
        (* We use adjacency lists rather than sets because we expect them to be small, and we
           iterate but not test membership. *)
        let f (fake_heap, out_constr) (l, r) =
          match (l, r) with
          | Formula.AbstractValueOperand lv, Formula.AbstractValueOperand rv ->
              if leadsto fake_heap heap lv rv then (fake_heap, out_constr)
              else
                let fake_heap =
                  let extend rs = match rs with None -> Some [rv] | Some rs -> Some (rv :: rs) in
                  AbstractValue.Map.update lv extend fake_heap
                in
                let out_constr =
                  C.{out_constr with leadsto_constr= (l, r) :: out_constr.C.leadsto_constr}
                in
                (fake_heap, out_constr)
          | _, _ ->
              L.die InternalError "TODO: parse only simple leadsto"
        in
        List.fold in_constr.C.leadsto_constr ~init:(AbstractValue.Map.empty, out_constr) ~f
      in
      (* Handle NotLeadsTo predicates. *)
      let* out_constr =
        let ok (l, r) =
          match (l, r) with
          | Formula.AbstractValueOperand l, Formula.AbstractValueOperand r ->
              not (leadsto fake_heap heap l r)
          | _, _ ->
              L.die InternalError "TODO: Forbid non-variables as arguments to ~~>, at parsing stage"
        in
        if List.for_all in_constr.C.notleadsto_constr ~f:ok then
          Sat C.{out_constr with notleadsto_constr= in_constr.C.notleadsto_constr}
        else Unsat
      in
      Sat out_constr
    in
    match go () with
    | Sat out_constr ->
        List.map out_constr.path_constr ~f:(function op, l, r -> Binary (Builtin op, l, r))
        @ List.map out_constr.leadsto_constr ~f:(function l, r -> Binary (LeadsTo, l, r))
        @ List.map out_constr.notleadsto_constr ~f:(function l, r -> Binary (NotLeadsTo, l, r))
    | Unsat ->
        [False]


  let eliminate_exists ~keep constr =
    (* TODO(rgrigore): replace the current weak approximation *)
    let is_live_operand : Formula.operand -> bool = function
      | AbstractValueOperand v ->
          keep v
      | ConstOperand _ ->
          true
      | FunctionApplicationOperand _ ->
          true
    in
    let is_live_predicate pred =
      match pred with Binary (_op, l, r) -> is_live_operand l && is_live_operand r | _ -> true
    in
    List.filter ~f:is_live_predicate constr
end

type step =
  { step_location: Location.t
  ; step_predecessor: simple_state  (** state before this step *)
  ; step_data: step_data }

and step_data =
  | SmallStep of event
  | LargeStep of
      {procname: Procname.t; post: simple_state; pulse_is_manifest: bool; topl_is_manifest: bool}

and simple_state =
  { pre: configuration  (** at the start of the procedure *)
  ; post: configuration  (** at the current program point *)
  ; pruned: Constraint.t  (** path-condition for the automaton *)
  ; last_step: step option [@ignore]  (** for trace error reporting *) }
[@@deriving compare, equal]

(* TODO: include a hash of the automaton in a summary to avoid caching problems. *)
type state = simple_state list [@@deriving compare, equal]

let pp_mapping f (x, value) = F.fprintf f "@[%s↦%a@]@," x AbstractValue.pp value

let pp_memory f memory = F.fprintf f "@[<2>[%a]@]" (pp_comma_seq pp_mapping) memory

let pp_configuration f {vertex; memory} =
  F.fprintf f "@[{ topl-config@;vertex=%d@;memory=%a }@]" vertex pp_memory memory


let pp_simple_state f {pre; post; pruned} =
  F.fprintf f "@[<2>{ topl-simple-state@;pre=%a@;post=%a@;pruned=(%a) }@]" pp_configuration pre
    pp_configuration post Constraint.pp pruned


let pp_state f state =
  if List.is_empty state then F.pp_print_string f "{empty}"
  else
    F.fprintf f "@[<v2>{len=%d;content=@;@[<2>[ %a ]@]}@]" (List.length state)
      (pp_comma_seq pp_simple_state) state


let start () =
  let mk_simple_states () =
    let a = Topl.automaton () in
    let memory =
      List.map ~f:(fun r -> (r, AbstractValue.mk_fresh ())) (ToplAutomaton.registers a)
    in
    let configurations =
      let n = ToplAutomaton.vcount a in
      let f acc vertex = {vertex; memory} :: acc in
      IContainer.forto n ~init:[] ~f
    in
    List.map
      ~f:(fun c -> {pre= c; post= c; pruned= Constraint.true_; last_step= None})
      configurations
  in
  if Topl.is_active () then mk_simple_states () else (* Avoids work later *) []


let get env x =
  match List.Assoc.find env ~equal:String.equal x with
  | Some v ->
      v
  | None ->
      L.die InternalError "Topl: Cannot find %s. Should be caught by static checks" x


let set = List.Assoc.add ~equal:String.equal

let make_field class_name field_name : Fieldname.t =
  match !Language.curr_language with
  | Erlang -> (
    match ErlangTypeName.from_string class_name with
    | None ->
        L.die UserError "Unknown/unsupported Erlang type %s" class_name
    | Some typ ->
        Fieldname.make (ErlangType typ) field_name )
  | _ ->
      L.die InternalError "Field access is not supported for current language (%s)"
        (Language.to_string !Language.curr_language)


let deref_field_access pulse_state value class_name field_name : Formula.operand option =
  (* Dereferencing is done in 2 steps: (v) --f-> (v1) --*-> (v2) *)
  let heap = pulse_state.pulse_post.heap in
  let* edges = Memory.find_opt value heap in
  let field = make_field class_name field_name in
  let* v1, _hist = Memory.Edges.find_opt (FieldAccess field) edges in
  let* edges = Memory.find_opt v1 heap in
  let* v2, _hist = Memory.Edges.find_opt Dereference edges in
  match Formula.as_constant_string pulse_state.path_condition v2 with
  | Some r ->
      Some (Formula.ConstOperand (Const.Cstr r))
  | _ ->
      Some (Formula.AbstractValueOperand v2)


let binop_to : ToplAst.binop -> Constraint.operator = function
  | LeadsTo ->
      LeadsTo
  | OpEq ->
      Builtin Eq
  | OpNe ->
      Builtin Ne
  | OpGe ->
      Builtin Ge
  | OpGt ->
      Builtin Gt
  | OpLe ->
      Builtin Le
  | OpLt ->
      Builtin Lt


let eval_guard pulse_state memory tcontext guard : Constraint.t =
  let rec operand_of_value (value : ToplAst.value) : Formula.operand option =
    match value with
    | Constant (LiteralInt x) ->
        Some (ConstOperand (Cint (IntLit.of_int x)))
    | Constant (LiteralStr s) ->
        Some (ConstOperand (Cstr s))
    | Register reg ->
        Some (AbstractValueOperand (get memory reg))
    | Binding v ->
        Some (AbstractValueOperand (get tcontext v))
    | FieldAccess {value; class_name; field_name} -> (
        let* value_op = operand_of_value value in
        match value_op with
        | Formula.AbstractValueOperand v ->
            deref_field_access pulse_state v class_name field_name
        | _ ->
            (* TODO: enforce in parser and make this an internal error. *)
            L.die UserError "Unsupported value for accessing field %s" field_name )
  in
  let conjoin_predicate (pruned : Constraint.t option) (predicate : ToplAst.predicate) =
    match predicate with
    | Binop (binop, l, r) ->
        let* l = operand_of_value l in
        let* r = operand_of_value r in
        let* pruned in
        let binop = binop_to binop in
        Some (Constraint.and_predicate (Constraint.make binop l r) pruned)
    | Value v ->
        let* v = operand_of_value v in
        let* pruned in
        let one = Formula.ConstOperand (Cint IntLit.one) in
        Some (Constraint.and_predicate (Constraint.make (Builtin Ne) v one) pruned)
  in
  match List.fold ~init:(Some Constraint.true_) ~f:conjoin_predicate guard with
  | Some result ->
      result
  | None ->
      Constraint.false_


let apply_action tcontext assignments memory =
  let apply_one memory (register, variable) = set memory register (get tcontext variable) in
  List.fold ~init:memory ~f:apply_one assignments


type tcontext = (ToplAst.variable_name * AbstractValue.t) list

let pp_tcontext f tcontext =
  F.fprintf f "@[[%a]@]"
    (pp_comma_seq (Pp.pair ~fst:F.pp_print_string ~snd:AbstractValue.pp))
    tcontext


let static_match_array_write arr index label : tcontext option =
  match label.ToplAst.pattern with
  | ArrayWritePattern ->
      let v1, v2 =
        match label.ToplAst.arguments with
        | Some [v1; v2] ->
            (v1, v2)
        | _ ->
            L.die InternalError "Topl: #ArrayWrite should have exactly two arguments"
      in
      Some [(v1, arr); (v2, index)]
  | _ ->
      None


let typ_void = Typ.{desc= Tvoid; quals= mk_type_quals ()}

let static_match_call tenv return arguments procname label : tcontext option =
  let is_match re text = Option.for_all re ~f:(fun re -> Str.string_match re.ToplAst.re text 0) in
  let is_match_type_base re typ = is_match re (Fmt.to_to_string pp_type typ) in
  let is_match_type_name mk_typ re name _struct = is_match_type_base re (mk_typ name) in
  (* NOTE: If B has supertype A, and we get type B**, then regex is matched against A** and B**.
     In other words, [Tptr] and [Tarray] are treated as covariant. *)
  let rec is_match_type (map : Typ.t -> Typ.t) re (typ : Typ.t) =
    match typ with
    | {desc= Tptr (typ, kind); quals} ->
        is_match_type (fun t -> map {Typ.desc= Tptr (t, kind); quals}) re typ
    | {desc= Tarray arr; quals} ->
        is_match_type (fun t -> map {Typ.desc= Tarray {arr with elt= t}; quals}) re arr.elt
    | {desc= Tstruct name; quals} ->
        Tenv.mem_supers tenv name
          ~f:(is_match_type_name (fun n -> map {Typ.desc= Tstruct n; quals}) re)
    | _ ->
        is_match_type_base re (map typ)
  in
  let match_name () : bool =
    match label.ToplAst.pattern with
    | CallPattern {procedure_name_regex; type_regexes} -> (
        is_match (Some procedure_name_regex) (Fmt.to_to_string pp_procname procname)
        &&
        match type_regexes with
        | None ->
            true
        | Some regexes -> (
            let types =
              let argument_types = List.map ~f:snd arguments in
              let return_type = Option.value_map ~default:typ_void ~f:snd return in
              argument_types @ [return_type]
            in
            match List.for_all2 regexes types ~f:(is_match_type Fn.id) with
            | Ok ok ->
                ok
            | Unequal_lengths ->
                false ) )
    | _ ->
        false
  in
  let match_args () : tcontext option =
    let rev_arguments = List.rev_map ~f:fst arguments in
    let match_formals formals : tcontext option =
      let bind ~init rev_formals =
        let f tcontext variable value = (variable, value) :: tcontext in
        match List.fold2 ~init ~f rev_formals rev_arguments with
        | Ok c ->
            Some c
        | Unequal_lengths ->
            None
      in
      match (List.rev formals, Option.map ~f:fst return) with
      | [], Some _ ->
          None
      | rev_formals, None ->
          bind ~init:[] rev_formals
      | r :: rev_formals, Some v ->
          bind ~init:[(r, v)] rev_formals
    in
    Option.value_map ~default:(Some []) ~f:match_formals label.ToplAst.arguments
  in
  if match_name () then match_args () else None


module Debug = struct
  let dropped_disjuncts_count = ref 0

  let rec matched_transitions =
    lazy
      ( Epilogues.register ~f:log_unseen
          ~description:"log which transitions never match because of their static pattern" ;
        let automaton = Topl.automaton () in
        let tcount = ToplAutomaton.tcount automaton in
        Array.create ~len:tcount false )


  and set_seen tindex = (Lazy.force matched_transitions).(tindex) <- true

  and get_unseen () =
    let f index seen = if seen then None else Some index in
    Array.filter_mapi ~f (Lazy.force matched_transitions)


  (** The transitions reported here *probably* have patterns that were miswrote by the user. *)
  and log_unseen () =
    let unseen = Array.to_list (get_unseen ()) in
    let pp f i = ToplAutomaton.pp_tindex (Topl.automaton ()) f i in
    if Config.trace_topl && not (List.is_empty unseen) then
      L.user_warning "@[<v>@[<v2>The following Topl transitions never match:@;%a@]@;@]"
        (F.pp_print_list pp) unseen


  let () = AnalysisGlobalState.register_ref dropped_disjuncts_count ~init:(fun () -> 0)

  let get_dropped_disjuncts_count () = !dropped_disjuncts_count
end

(** Returns a list of transitions whose pattern matches (e.g., event type matches). Each match
    produces a tcontext (transition context), which matches transition-local variables to abstract
    values. *)
let static_match tenv event : (ToplAutomaton.transition * tcontext) list =
  let match_one index transition =
    let f label =
      match event with
      | ArrayWrite {aw_array; aw_index} ->
          static_match_array_write aw_array aw_index label
      | Call {return; arguments; procname} ->
          static_match_call tenv return arguments procname label
    in
    let tcontext_opt = Option.value_map ~default:(Some []) ~f transition.ToplAutomaton.label in
    L.d_printfln_escaped "@[<2>PulseTopl.static_match:@;transition %a@;event %a@;result %a@]"
      (ToplAutomaton.pp_transition (Topl.automaton ()))
      transition pp_event event (Pp.option pp_tcontext) tcontext_opt ;
    Option.map
      ~f:(fun tcontext ->
        Debug.set_seen index ;
        (transition, tcontext) )
      tcontext_opt
  in
  ToplAutomaton.tfilter_mapi (Topl.automaton ()) ~f:match_one


let drop_infeasible _pulse_state state =
  let f {pruned} = not Constraint.(equal false_ pruned) in
  List.filter ~f state


let normalize_memory memory = List.sort ~compare:[%compare: register * value] memory

let normalize_configuration {vertex; memory} = {vertex; memory= normalize_memory memory}

let normalize_simple_state {pre; post; pruned; last_step} =
  { pre= normalize_configuration pre
  ; post= normalize_configuration post
  ; pruned= Constraint.normalize pruned
  ; last_step }


let normalize_state state = List.map ~f:normalize_simple_state state

(** Filters out simple states that cannot reach error because their registers refer to garbage.

    - "Garbage" is a value unreachable from the program state and different from all values held by
      registers in the pre-simple-state.
    - The current implementation is an approximation. If a register refers to garbage, it might
      still be the case that "error" could be reached, depending on the structure of the automaton.
      This could be determined by a pre-analysis of the automaton. However, because such cases are
      empirically rare, we just under-approximate by dropping always when a register has garbage.
    - We never drop simple-states corresponding to "error" vertices. *)
let drop_garbage ~keep state =
  let should_keep {pre; post} =
    ToplAutomaton.is_error (Topl.automaton ()) post.vertex
    ||
    let add_register values (_register, v) = AbstractValue.Set.add v values in
    let ok = List.fold ~f:add_register ~init:keep pre.memory in
    let register_is_ok (_register, v) = AbstractValue.Set.mem v ok in
    List.for_all ~f:register_is_ok post.memory
  in
  List.filter ~f:should_keep state


let simplify pulse_state state =
  (* NOTE: For dropping garbage, we do not consider registers live. If the Topl monitor has a hold
     of something that is garbage for the program, then that something is still garbage. *)
  let reachable =
    Stats.incr_topl_reachable_calls () ;
    pulse_state.get_reachable ()
  in
  let eliminate_exists {pre; post; pruned; last_step} =
    L.d_printfln "@[<v>@[DBG eliminate_exists pulse_state.path_condition %a@]@;@]" Formula.pp
      pulse_state.path_condition ;
    let collect memory keep =
      List.fold ~init:keep ~f:(fun keep (_reg, value) -> AbstractValue.Set.add value keep) memory
    in
    let keep = reachable |> collect pre.memory |> collect post.memory in
    L.d_printfln "@[<v>@[DBG eliminate_exists keep = %a@]@;@]" AbstractValue.Set.pp keep ;
    let keep v = AbstractValue.Set.mem v keep in
    let keep v =
      let result = keep v in
      L.d_printfln "@[<v>@[DBG keep %a %b@]@;@]" AbstractValue.pp v result ;
      result
    in
    let pruned = Constraint.eliminate_exists ~keep pruned in
    {pre; post; pruned; last_step}
  in
  let simplify_pruned {pre; post; pruned; last_step} =
    let pruned = Constraint.simplify pulse_state pruned in
    {pre; post; pruned; last_step}
  in
  (* The following three steps are ordered from fastest to slowest, except that `drop_infeasible`
     has to come after `simplify_pruned`. *)
  let state =
    (* T147875161 *)
    if false then List.map ~f:eliminate_exists state else state
  in
  let state = drop_garbage ~keep:reachable state in
  let state = List.map ~f:simplify_pruned state in
  let state = drop_infeasible pulse_state state in
  List.dedup_and_sort ~compare:compare_simple_state state


let simplify =
  if Config.trace_topl then ( fun pulse_state state ->
    let result = simplify pulse_state state in
    L.d_printfln "@[<v2>@[PulseTopl.simplify@]@;@[before=%a@]@;@[after=%a@]@;@]" pp_state state
      pp_state result ;
    result )
  else simplify


let apply_limits pulse_state state =
  let expensive_simplification state = simplify pulse_state state in
  let drop_disjuncts state =
    let old_len = List.length state in
    let new_len = (Config.topl_max_disjuncts / 2) + 1 in
    if Config.trace_topl then
      Debug.dropped_disjuncts_count := !Debug.dropped_disjuncts_count + old_len - new_len ;
    let add_score simple_state =
      let score = Constraint.size simple_state.pruned in
      if score > Config.topl_max_conjuncts then None else Some (score, simple_state)
    in
    let strip_score = snd in
    let compare_score (score1, _simple_state1) (score2, _simple_state2) =
      Int.compare score1 score2
    in
    state |> List.filter_map ~f:add_score |> List.sort ~compare:compare_score
    |> Fn.flip List.take new_len |> List.map ~f:strip_score
  in
  let needs_shrinking state = List.length state > Config.topl_max_disjuncts in
  let maybe condition transform x = if condition x then transform x else x in
  state |> maybe needs_shrinking expensive_simplification |> maybe needs_shrinking drop_disjuncts


let small_step tenv loc pulse_state event simple_states =
  let tmatches = static_match tenv event in
  let evolve_transition (old : simple_state) (transition, tcontext) : state =
    let mk ?(memory = old.post.memory) ?(pruned = Constraint.true_) significant =
      let last_step =
        if significant then
          Some {step_location= loc; step_predecessor= old; step_data= SmallStep event}
        else old.last_step
      in
      (* NOTE: old pruned is discarded, because evolve_simple_state needs to see only new prunes
         to determine skip transitions. It will then add back old prunes. *)
      let post = {vertex= transition.ToplAutomaton.target; memory} in
      {old with post; pruned; last_step}
    in
    match transition.ToplAutomaton.label with
    | None ->
        (* "any" transition *)
        let is_loop = Int.equal transition.ToplAutomaton.source transition.ToplAutomaton.target in
        [mk (not is_loop)]
    | Some label ->
        let memory = old.post.memory in
        let pruned = eval_guard pulse_state memory tcontext label.ToplAst.condition in
        let memory = apply_action tcontext label.ToplAst.action memory in
        [mk ~memory ~pruned true]
  in
  let evolve_simple_state old =
    let tmatches =
      List.filter ~f:(fun (t, _) -> Int.equal old.post.vertex t.ToplAutomaton.source) tmatches
    in
    let nonskip = List.concat_map ~f:(evolve_transition old) tmatches in
    let skip =
      let nonskip_disjunction = List.map ~f:(fun {pruned} -> pruned) nonskip in
      let skip_disjunction = Constraint.negate nonskip_disjunction in
      let f pruned = {old with pruned} (* keeps last_step from old *) in
      List.map ~f skip_disjunction
    in
    let add_old_pruned s = {s with pruned= Constraint.and_constr s.pruned old.pruned} in
    List.map ~f:add_old_pruned (List.rev_append nonskip skip)
  in
  let result = List.concat_map ~f:evolve_simple_state simple_states in
  L.d_printfln "@[<2>PulseTopl.small_step:@;%a@ -> %a@]" pp_state simple_states pp_state result ;
  result |> apply_limits pulse_state


let of_unequal (or_unequal : 'a List.Or_unequal_lengths.t) =
  match or_unequal with
  | Ok x ->
      x
  | Unequal_lengths ->
      L.die InternalError "PulseTopl expected lists to be of equal lengths"


let sub_configuration (sub, {vertex; memory}) =
  let keys, values = List.unzip memory in
  let sub, values = sub_list sub_value (sub, values) in
  let memory = of_unequal (List.zip keys values) in
  (sub, {vertex; memory})


(* Does not substitute in [last_step]: not usually needed, and takes much time. *)
let sub_simple_state (sub, {pre; post; pruned; last_step}) =
  let sub, pre = sub_configuration (sub, pre) in
  let sub, post = sub_configuration (sub, post) in
  let sub, pruned = Constraint.substitute (sub, pruned) in
  (sub, {pre; post; pruned; last_step})


let large_step ~call_location ~callee_proc_name ~substitution pulse_state ~callee_summary
    ~callee_is_manifest state =
  let seq ((p : simple_state), (q : simple_state)) =
    if not (Int.equal p.post.vertex q.pre.vertex) then None
    else
      let substitution, new_eqs =
        (* Update the substitution, matching formals with actuals. We work a bit to avoid introducing
           equalities, because a growing [pruned] leads to quadratic behaviour. *)
        let mk_eq val1 val2 =
          let op x = Formula.AbstractValueOperand x in
          Constraint.make (Builtin Eq) (op val1) (op val2)
        in
        let f (sub, eqs) (reg1, val1) (reg2, val2) =
          if not (String.equal reg1 reg2) then
            L.die InternalError
              "PulseTopl: normalized memories are expected to have the same domain"
          else
            match AbstractValue.Map.find_opt val2 sub with
            | Some (old_val1, _history) ->
                if AbstractValue.equal old_val1 val1 then (sub, eqs)
                else (sub, Constraint.and_predicate (mk_eq old_val1 val1) eqs)
            | None ->
                (AbstractValue.Map.add val2 (val1, ValueHistory.epoch) sub, eqs)
        in
        of_unequal (List.fold2 p.post.memory q.pre.memory ~init:(substitution, Constraint.true_) ~f)
      in
      let _substitution, q = sub_simple_state (substitution, q) in
      let pruned = Constraint.and_n [new_eqs; q.pruned; p.pruned] in
      let last_step =
        Some
          { step_location= call_location
          ; step_predecessor= p
          ; step_data=
              LargeStep
                { procname= callee_proc_name
                ; post= q
                ; pulse_is_manifest= callee_is_manifest
                ; topl_is_manifest= Constraint.(equal true_ q.pruned) } }
      in
      Some {pre= p.pre; post= q.post; pruned; last_step}
  in
  (* TODO(rgrigore): may be worth optimizing the cartesian_product *)
  let state = normalize_state state in
  let callee_summary = normalize_state callee_summary in
  let result = List.filter_map ~f:seq (List.cartesian_product state callee_summary) in
  L.d_printfln "@[<2>PulseTopl.large_step:@;callee_prepost=%a@;%a@ -> %a@]" pp_state callee_summary
    pp_state state pp_state result ;
  result |> apply_limits pulse_state


let filter_for_summary pulse_state state = drop_infeasible pulse_state state

let description_of_step_data step_data =
  ( match step_data with
  | SmallStep (Call {procname}) | LargeStep {procname} ->
      F.fprintf F.str_formatter "@[call to %a@]" Procname.pp_verbose procname
  | SmallStep (ArrayWrite _) ->
      F.fprintf F.str_formatter "@[write to array@]" ) ;
  F.flush_str_formatter ()


(* The rules for reporting an error from a Topl simple_state are:
   (a) it should go from vertex start to vertex error;
   (b) its trace does not have a nested similar error.
   The rules for deciding if the error is manifest are:
   (c) the corresponding Pulse state is manifest;
   (d) the Topl constraint ("pruned") is valid.
   To decide (c), we rely on Pulse to tell Topl (when calling report_errors and large_step) whether
   the Pulse state is manifest. To decide (b), we record information about (c)&(d) in the trace (in
   large_step), and use it here (in report_errors).
*)
let report_errors proc_desc err_log ~pulse_is_manifest state =
  let a = Topl.automaton () in
  let rec make_trace nesting trace q =
    match q.last_step with
    | None ->
        trace
    | Some {step_location; step_predecessor; step_data} ->
        let description = description_of_step_data step_data in
        let trace =
          let trace_element = Errlog.make_trace_element nesting step_location description [] in
          match step_data with
          | SmallStep _ ->
              trace_element :: trace
          | LargeStep {post} ->
              let new_trace = make_trace (nesting + 1) trace post in
              (* Skip trivial large steps (those with no substeps) *)
              if phys_equal new_trace trace then trace else trace_element :: new_trace
        in
        make_trace nesting trace step_predecessor
  in
  let rec first_error_ss q =
    match q.last_step with
    | Some {step_predecessor} ->
        if not (ToplAutomaton.is_error a step_predecessor.post.vertex) then q
        else first_error_ss step_predecessor
    | None ->
        L.die InternalError "PulseTopl.report_errors inv broken"
  in
  let is_issue q =
    ToplAutomaton.is_start a q.pre.vertex && ToplAutomaton.is_error a q.post.vertex
  in
  let is_nested_large_step ~caller_pulse_is_manifest ~caller_topl_is_manifest q =
    (* This is a simple heuristic that tries to avoid reporting too many issues that are "the same".
       Since pulse_is_manifest and topl_is_manifest are coarse abstraction of what the issue is,
       relying on them can filter out issues that are not "the same". But, in practice, it seems a
       useful heuristic to reduce noise. *)
    match q.last_step with
    | Some {step_data= LargeStep {post; pulse_is_manifest; topl_is_manifest}} ->
        is_issue post
        && Bool.(equal caller_pulse_is_manifest pulse_is_manifest)
        && Bool.(equal caller_topl_is_manifest topl_is_manifest)
    | _ ->
        false
  in
  let report_simple_state q =
    (* We assume simplifications happened before calling report_errors. *)
    let topl_is_manifest = Constraint.(equal true_ q.pruned) in
    if is_issue q then (
      let q = first_error_ss q in
      L.d_printfln ~color:Red "found TOPL error %a" pp_simple_state q ;
      (* Only report at the innermost level where error appears. *)
      if
        not
          (is_nested_large_step ~caller_pulse_is_manifest:pulse_is_manifest
             ~caller_topl_is_manifest:topl_is_manifest q )
      then
        let loc = Procdesc.get_loc proc_desc in
        let ltr = make_trace 0 [] q in
        let latent = (not topl_is_manifest) || not pulse_is_manifest in
        let message =
          (* 1. topl_is_manifest && not pulse_is_manifest:
           *    if program execution conditions would be satisfied, monitor would signal error
           * 2. not topl_is_manifest && pulse_is_manifest:
           *    execution is unconditional, but the monitor still waits for some conditions to be
           *    satisfied before signaling error *)
          String.concat ~sep:" "
            ( [ToplAutomaton.message a q.post.vertex]
            @ (if latent && topl_is_manifest then ["TOPL_MANIFEST"] else [])
            @ if latent && pulse_is_manifest then ["PULSE_MANIFEST"] else [] )
        in
        if (not latent) || Config.topl_report_latent_issues then
          Reporting.log_issue proc_desc err_log ~loc ~ltr Topl (IssueType.topl_error ~latent)
            message )
  in
  List.iter ~f:report_simple_state state


(* {1 Fast path} *)

let small_step tenv location pulse_state event state =
  match state with [] -> state | _ -> small_step tenv location pulse_state event state


let large_step ~call_location ~callee_proc_name ~substitution pulse_state ~callee_summary
    ~callee_is_manifest state =
  match state with
  | [] ->
      state
  | _ ->
      large_step ~call_location ~callee_proc_name ~substitution pulse_state ~callee_summary
        ~callee_is_manifest state


let filter_for_summary pulse_state state =
  match state with [] -> state | _ -> filter_for_summary pulse_state state


let simplify pulse_state state = match state with [] -> state | _ -> simplify pulse_state state
