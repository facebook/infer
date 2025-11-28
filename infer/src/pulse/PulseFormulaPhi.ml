(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CItv = PulseCItv
module SatUnsat = PulseSatUnsat
module Debug = PulseFormulaDebug
module Var = PulseFormulaVar
module Q = QSafeCapped
open SatUnsat.Import
module LinArith = PulseFormulaLinArit
module Tableau = PulseFormulaTableau
module Term = PulseFormulaTerm
module Atom = PulseFormulaAtom
module VarUF = UnionFind.Make (Var) (Var.Set) (Var.Map)
module InstanceOf = PulseFormulaInstanceOf

let pp_var_map ?filter ~arrow pp_val pp_var fmt var_map =
  Pp.collection ~sep:"@;∧ "
    ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
    ?filter
    (fun fmt (v, value) -> F.fprintf fmt "%a%s%a" pp_var v arrow pp_val value)
    fmt var_map


let pp_var_set pp_var fmt var_set =
  Pp.collection ~sep:","
    ~fold:(IContainer.fold_of_pervasives_set_fold Var.Set.fold)
    pp_var fmt var_set


type new_eq = EqZero of Var.t | Equal of Var.t * Var.t

let pp_new_eq fmt = function
  | EqZero v ->
      F.fprintf fmt "%a=0" Var.pp v
  | Equal (v1, v2) ->
      F.fprintf fmt "%a=%a" Var.pp v1 Var.pp v2


type new_eqs = new_eq RevList.t

let pp_new_eqs fmt new_eqs =
  F.fprintf fmt "[@[%a@]]" (Pp.seq ~sep:"," pp_new_eq) (RevList.to_list new_eqs)


module MakeOccurrences (In : sig
  type t

  module Set : Stdlib.Set.S with type elt = t

  val pp_set : (F.formatter -> Var.t -> unit) -> F.formatter -> Set.t -> unit
end) =
struct
  type t = In.Set.t Var.Map.t [@@deriving compare, equal]

  let pp pp_var fmt occurrences = (pp_var_map ~arrow:"->" (In.pp_set pp_var) pp_var) fmt occurrences

  let yojson_of_t = [%yojson_of: _]

  (** add [in_] to [occurrences(v)] *)
  let add v ~occurs_in:in_ occurrences =
    Var.Map.update v
      (fun in_vs_opt ->
        let in_vs = Option.value ~default:In.Set.empty in_vs_opt in
        Some (In.Set.add in_ in_vs) )
      occurrences


  (** remove [in_] from [occurrences(v)] *)
  let remove v ~occurred_in:in_ occurrences =
    Var.Map.update v
      (function
        | None ->
            None
        | Some in_vs ->
            let in_vs' = In.Set.remove in_ in_vs in
            if In.Set.is_empty in_vs' then None else Some in_vs' )
      occurrences
end

module VarMapOccurrences = MakeOccurrences (struct
  include Var

  let pp_set = pp_var_set
end)

module TermDomainOrRange = struct
  (** for a binding [t=v], describes whether a variable [v'] appears in [t] (domain), is [v]
      (range), or both; we could have a different representation of term occurrences using a map
      [t -> Domain | Range | DomainAndRange] to match what we are trying to do closer but we use a
      set like other occurrence maps, oh well. *)
  type domain_or_range = Domain | Range | DomainAndRange [@@deriving compare]

  type t = Term.t * domain_or_range [@@deriving compare]

  module Set = Stdlib.Set.Make (struct
    type nonrec t = t [@@deriving compare]
  end)

  let pp_set pp_var fmt set =
    if Set.is_empty set then F.pp_print_string fmt "(empty)"
    else
      Pp.collection ~sep:","
        ~fold:(IContainer.fold_of_pervasives_set_fold Set.fold)
        (fun fmt (term, domain_or_range) ->
          ( match domain_or_range with
          | Domain ->
              F.pp_print_char fmt 'd'
          | Range ->
              F.pp_print_char fmt 'r'
          | DomainAndRange ->
              F.pp_print_string fmt "dr" ) ;
          F.fprintf fmt "(%a)" (Term.pp_no_paren pp_var) term )
        fmt set
end

module TermMapOccurrences = MakeOccurrences (TermDomainOrRange)

module AtomMapOccurrences = MakeOccurrences (struct
  include Atom

  let pp_set fmt atoms = Atom.Set.pp_with_pp_var fmt atoms
end)

(* redefined for yojson output *)
type var_eqs = VarUF.t [@@deriving compare, equal]

let yojson_of_var_eqs var_eqs =
  `List
    (VarUF.fold_congruences var_eqs ~init:[] ~f:(fun jsons (repr, eqs) ->
         `List
           (Var.yojson_of_t (repr :> Var.t) :: List.map ~f:Var.yojson_of_t (Var.Set.elements eqs))
         :: jsons ) )


type linear_eqs = LinArith.t Var.Map.t [@@deriving compare, equal]

let yojson_of_linear_eqs linear_eqs = Var.Map.yojson_of_t LinArith.yojson_of_t linear_eqs

type intervals = CItv.t Var.Map.t [@@deriving compare, equal, yojson_of]

module Unsafe : sig
  (** opaque because we need to normalize variables in the co-domain of term equalities on the fly
  *)
  type term_eqs

  type t = private
    { var_eqs: var_eqs
          (** Equality relation between variables. We want to only use canonical representatives
              from this equality relation in the rest of the formula and more generally in all of
              the abstract state. See also {!AbductiveDomain}. *)
    ; const_eqs: Term.t Var.Map.t
    ; type_constraints: InstanceOf.t
    ; linear_eqs: linear_eqs
          (** Equalities of the form [x = l] where [l] is from linear arithmetic. These are
              interpreted over the *rationals*, not integers (or floats), so this will be
              incomplete. There are mitigations to recover a little of that lost completeness:

              - [atoms] have [is_int()] terms that will detect when we get (constant) rationals
                where we expected integers eg [is_int(1.5)] becomes [false].

              - [intervals] are over integers

              INVARIANT:

              1. the domain and the range of [phi.linear_eqs] mention distinct variables:
              [domain(linear_eqs) ∩ range(linear_eqs) = ∅], when seeing [linear_eqs] as a map [x->l]

              2. for all [x=l ∊ linear_eqs], [x < min({x'|x'∊l})] according to [is_simpler_than] (in
              other words: [x] is the simplest variable in [x=l]). *)
    ; term_eqs: term_eqs
          (** Equalities of the form [t = x], used to detect when two abstract values are equal to
              the same term (hence equal). Together with [var_eqs] and [linear_eqs] this gives a
              congruence closure capability to the domain over uninterpreted parts of the domain
              (meaning the atoms and term equalities that are not just linear arithmetic and handled
              in a complete-ish fashion by [linear_eqs], [var_eqs], [tableau]). Even on interpreted
              domains like linear arithmetic [term_eqs] is needed to provide on-the-fly
              normalisation by detecting when two variables are equal to the same term, which
              [linear_eqs] will do nothing about (eg [x=0∧y=0] doesn't trigger [x=y] in [linear_eqs]
              but [term_eqs] is in charge of detecting it).

              Under the hood this is a map [t -> x] that may contain un-normalized [x]s in its
              co-domain; these are normalized on the fly when reading from the map

              INVARIANT: each term in [term_eqs] is *shallow*, meaning it is either a constant or a
              term of the form [f(x1, ..., xN)] with [x1], ..., [xN] either variables or constants
              themselves. *)
    ; tableau: Tableau.t
          (** linear equalities similar to [linear_eqs] but involving only "restricted" (aka
              "slack") variables; this is used for reasoning about inequalities, see \[2\]

              INVARIANT: see {!Tableau} *)
    ; intervals: intervals
          (** A simple, non-relational domain of concrete integer intervals of the form [x∈[i,j]] or
              [x∉[i,j]].

              This is used to recover a little bit of completeness on integer reasoning at no great
              cost. *)
    ; atoms: Atom.Set.t
          (** "everything else": atoms that cannot be expressed in a form suitable for one of the
              other domains, in particular disequalities.

              INVARIANT: Contrarily to [term_eqs] atoms are *maximally expanded* meaning if a
              variable [x] is equal to a term [t] then [x] shouldn't appear in [atoms] and should be
              substituted by [t] instead. This is looser than other invariants since a given
              variable can be equal to several syntactically-distinct terms so "maximally expanded"
              doesn't really make sense in general and there is incompleteness there. *)
    ; linear_eqs_occurrences: VarMapOccurrences.t
          (** occurrences of variables in [linear_eqs]: a binding [x -> y] means that [x] appears in
              [linear_eqs(y)] and will be used to propagate new (linear) equalities about [x] to
              maintain the [linear_eqs] invariant without having to do a linear scan of [linear_eqs]
          *)
    ; tableau_occurrences: VarMapOccurrences.t  (** likewise for [tableau] *)
    ; term_eqs_occurrences: TermMapOccurrences.t
          (** like [linear_eqs_occurrences] but for [term_eqs] so bindings are from variables to
              sets of terms *)
    ; atoms_occurrences: AtomMapOccurrences.t  (** likewise for [atoms] *)
    ; term_conditions: Atom.Set.t  (** Termination conditions for pulse-inf *)
    ; term_conditions2: Term.Set.t  (** Termination conditions for pulse-inf *) }
  [@@deriving compare, equal, yojson_of]

  val pp_with_pp_var : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

  val get_repr : t -> Var.t -> VarUF.repr
  (** the canonical representative of a given variable *)

  val ttrue : t

  (* {2 [term_eqs] interface due to the totally opaque type} *)

  val get_term_eq : t -> Term.t -> Var.t option

  val term_eqs_fold : (Term.t -> Var.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  val term_eqs_iter : (Term.t -> Var.t -> unit) -> t -> unit

  val term_eqs_exists : (Term.t -> Var.t -> bool) -> t -> bool

  val term_eqs_filter : (Term.t -> Var.t -> bool) -> t -> Term.VarMap.t_

  val fold_term_eqs_vars : t -> init:'acc -> f:('acc -> Var.t -> 'acc) -> 'acc

  val subst_term_eqs : Var.t Var.Map.t -> t -> Term.VarMap.t_

  val pp_term_eqs_with_pp_var :
    ?filter:(Term.t -> Var.t -> bool) -> (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

  (* {2 mutations} *)

  val add_const_eq : Var.t -> Term.t -> t -> t SatUnsat.t
  (** [add_const_eq v t phi] adds [v=t] to [const_eqs]; [Unsat] if [v] was already bound to a
      different constant *)

  val remove_const_eq : Var.t -> t -> t

  val add_dynamic_type : Var.t -> Typ.t -> ?source_file:SourceFile.t -> t -> t * bool
  (** We're now tracking nullability more precisely. If the language includes nulls and we have
      apparently contradictory type information then, instead of immediately returning [Unsat], we
      want to add an assertion that the value must be null (which inhabits all (nullable) types).
      But at this low level in the solver stack (the [Unsafe] module), we can't yet decide if the
      resulting state is [Unsat]. So we return an extra boolean that indicates whether or not a
      higher-level wrapper should add, and compute the consequences of, that assertion. See
      [and_dynamic_type] further down (in the [Normalizer] module) for such a wrapper around
      [add_dynamic_type] *)

  val add_below : Var.t -> Typ.t -> t -> t * bool

  val add_notbelow : Var.t -> Typ.t -> t -> t * bool

  val copy_type_constraints : Var.t -> Var.t -> t -> t

  val add_linear_eq : Var.t -> LinArith.t -> t -> t * Var.t option
  (** [add_linear_eq v l phi] adds [v=l] to [linear_eqs] and updates the occurrences maps and
      [term_eqs] appropriately; don't forget to call [propagate_linear_eq] after this *)

  val remove_linear_eq : Var.t -> LinArith.t -> t -> t
  (** [remove_linear_eq v l phi] removes [v] from [linear_eqs] and updates the occurrences maps and
      [term_eqs] appropriately; [l] is the existing binding for [v] *)

  val remove_tableau_eq : Var.t -> LinArith.t -> t -> t

  val add_term_eq : Term.t -> Var.t -> t -> t * Var.t option
  (** [add_term_eq t v phi] adds [t=v] to [term_eqs] and updates the occurrences maps appropriately;
      don't forget to call [propagate_term_eq] after this *)

  val remove_term_eq : Term.t -> Var.t -> t -> t
  (** [remove_term_eq t v phi] removes [t] from the [term_eqs] map and updates the occurrences maps
      appropriately; [v] is the existing (representative for the) binding for [t] *)

  val add_atom : Atom.t -> t -> t

  val and_termcond_atoms : t -> Atom.t list -> t

  val and_termcond_binop : t -> Term.t -> t

  val and_path_flush : t -> t

  val remove_atom : Atom.t -> t -> t

  val add_tableau_eq : Var.t -> LinArith.t -> t -> t

  val add_interval : Var.t -> CItv.t -> t -> t SatUnsat.t

  val add_interval_ : Var.t -> CItv.t -> intervals -> intervals SatUnsat.t
  (** same as [add_interval] but operates on the inner [intervals] datatype *)

  val add_occurrence_to_range_of_term_eq : Term.t -> VarUF.repr -> t -> t
  (** assumes that the variable doesn't appear in the term itself *)

  val remove_from_linear_eqs_occurrences : Var.t -> t -> t

  val remove_from_tableau_occurrences : Var.t -> t -> t

  val remove_from_term_eqs_occurrences : Var.t -> t -> t

  val remove_from_atoms_occurrences : Var.t -> t -> t

  val set_var_eqs : var_eqs -> t -> t

  val set_tableau : Tableau.t -> t -> t

  val set_intervals : intervals -> t -> t

  val get_terminal_conds : t -> Atom.Set.t

  val get_terminal_terms : t -> Term.Set.t

  val join : t -> t -> t

  val remove_conditions_for_join : Atom.t list -> t -> t -> t

  val unsafe_mk :
       var_eqs:var_eqs
    -> const_eqs:Term.t Var.Map.t
    -> type_constraints:InstanceOf.t
    -> linear_eqs:linear_eqs
    -> term_eqs:Term.VarMap.t_
    -> tableau:Tableau.t
    -> intervals:intervals
    -> atoms:Atom.Set.t
    -> linear_eqs_occurrences:VarMapOccurrences.t
    -> tableau_occurrences:VarMapOccurrences.t
    -> term_eqs_occurrences:TermMapOccurrences.t
    -> atoms_occurrences:AtomMapOccurrences.t
    -> t
  (** escape hatch *)
end = struct
  type term_eqs = Term.VarMap.t_ [@@deriving compare, equal, yojson_of]

  let term_eqs_is_empty = Term.VarMap.is_empty

  type t =
    { var_eqs: var_eqs
    ; const_eqs: Term.t Var.Map.t
    ; type_constraints: InstanceOf.t
    ; linear_eqs: linear_eqs
    ; term_eqs: term_eqs
    ; tableau: Tableau.t
    ; intervals: intervals
    ; atoms: Atom.Set.t
    ; linear_eqs_occurrences: VarMapOccurrences.t
    ; tableau_occurrences: VarMapOccurrences.t
    ; term_eqs_occurrences: TermMapOccurrences.t
    ; atoms_occurrences: AtomMapOccurrences.t
    ; term_conditions: Atom.Set.t  (** Termination conditions for pulse-inf *)
    ; term_conditions2: Term.Set.t }
  [@@deriving compare, equal, yojson_of]

  let ttrue =
    { var_eqs= VarUF.empty
    ; const_eqs= Var.Map.empty
    ; type_constraints= Var.Map.empty
    ; linear_eqs= Var.Map.empty
    ; term_eqs= Term.VarMap.empty
    ; tableau= Tableau.empty
    ; intervals= Var.Map.empty
    ; atoms= Atom.Set.empty
    ; linear_eqs_occurrences= Var.Map.empty
    ; tableau_occurrences= Var.Map.empty
    ; term_eqs_occurrences= Var.Map.empty
    ; atoms_occurrences= Var.Map.empty
    ; term_conditions= Atom.Set.empty
    ; term_conditions2= Term.Set.empty }


  let get_repr phi x = VarUF.find phi.var_eqs x

  let get_repr_as_var phi x = (get_repr phi x :> Var.t)

  let is_empty
      ({ var_eqs
       ; const_eqs
       ; type_constraints
       ; linear_eqs
       ; term_eqs
       ; tableau
       ; intervals
       ; atoms
       ; linear_eqs_occurrences= _
       ; tableau_occurrences= _
       ; term_eqs_occurrences= _
       ; atoms_occurrences= _
       ; term_conditions
       ; term_conditions2 }
       [@warning "+missing-record-field-pattern"] ) =
    VarUF.is_empty var_eqs && Var.Map.is_empty const_eqs && Var.Map.is_empty type_constraints
    && Var.Map.is_empty linear_eqs && term_eqs_is_empty term_eqs && Var.Map.is_empty tableau
    && Var.Map.is_empty intervals && Atom.Set.is_empty atoms && Atom.Set.is_empty term_conditions
    && Term.Set.is_empty term_conditions2


  (* {2 [term_eqs] interface due to the totally opaque type} *)

  let get_term_eq phi t = Term.VarMap.find_opt t phi.term_eqs |> Option.map ~f:(get_repr_as_var phi)

  let term_eqs_fold f phi init =
    Term.VarMap.fold (fun t x acc -> f t (get_repr_as_var phi x) acc) phi.term_eqs init


  let term_eqs_iter f phi = Term.VarMap.iter (fun t x -> f t (get_repr_as_var phi x)) phi.term_eqs

  let term_eqs_exists f phi =
    Term.VarMap.exists (fun t x -> f t (get_repr_as_var phi x)) phi.term_eqs


  let term_eqs_filter f phi =
    Term.VarMap.filter (fun t x -> f t (get_repr_as_var phi x)) phi.term_eqs


  let fold_term_eqs_vars phi ~init ~f =
    let f_eq term var acc = Term.fold_variables term ~f ~init:(f acc (get_repr phi var :> Var.t)) in
    Term.VarMap.fold f_eq phi.term_eqs init


  let subst_term_eqs subst phi =
    term_eqs_fold
      (fun t v acc ->
        let t' = Term.subst_variables t ~f:(Term.targetted_subst_var subst) in
        let v' = Term.subst_f subst v in
        Term.VarMap.add t' v' acc )
      phi Term.VarMap.empty


  let pp_term_eqs_with_pp_var ?(filter = fun _ _ -> true) pp_var fmt phi =
    (* Change this to get the raw data in the map, otherwise by default print what clients would
         get out of the map. The only difference is in the range of the map: whether variables get
         through [get_repr] or not before being printed. *)
    let print_raw_map = false in
    if print_raw_map then Term.VarMap.pp_with_pp_var pp_var fmt phi.term_eqs
    else
      (* inspired by {!Pp.collection} *)
      let is_first = ref true in
      F.pp_open_hvbox fmt 0 ;
      term_eqs_iter
        (fun term var ->
          if filter term var then (
            F.pp_open_hbox fmt () ;
            if not !is_first then F.pp_print_string fmt "∧" ;
            is_first := false ;
            F.fprintf fmt "%a=%a@]" (Term.pp pp_var) term pp_var var ) )
        phi ;
      F.pp_close_box fmt ()


  let pp_with_pp_var pp_var fmt
      ( ({ var_eqs
         ; const_eqs
         ; type_constraints
         ; linear_eqs
         ; term_eqs
         ; tableau
         ; intervals
         ; atoms
         ; linear_eqs_occurrences
         ; tableau_occurrences
         ; term_eqs_occurrences
         ; atoms_occurrences
         ; term_conditions
         ; term_conditions2 }
         [@warning "+missing-record-field-pattern"] ) as phi ) =
    let is_first = ref true in
    let pp_if condition header pp fmt x =
      let pp_and fmt = if not !is_first then F.fprintf fmt "@;&& " else is_first := false in
      if condition then F.fprintf fmt "%t%s: %a" pp_and header pp x
    in
    F.pp_open_hvbox fmt 0 ;
    if is_empty phi then F.pp_print_string fmt "(empty)" ;
    (pp_if (not (VarUF.is_empty var_eqs)) "var_eqs" (VarUF.pp pp_var)) fmt var_eqs ;
    (pp_if
       (not (Var.Map.is_empty const_eqs))
       "const_eqs"
       (pp_var_map ~arrow:"=" (Term.pp pp_var) pp_var) )
      fmt const_eqs ;
    (pp_if
       (not (Var.Map.is_empty type_constraints))
       "type_constraints"
       (InstanceOf.pp_with_pp_var pp_var) )
      fmt type_constraints ;
    (pp_if
       (not (Var.Map.is_empty linear_eqs))
       "linear_eqs"
       (pp_var_map ~arrow:" = " (LinArith.pp pp_var) pp_var) )
      fmt linear_eqs ;
    (pp_if (not (term_eqs_is_empty term_eqs)) "term_eqs" (pp_term_eqs_with_pp_var pp_var)) fmt phi ;
    (pp_if (not (Var.Map.is_empty tableau)) "tableau" (Tableau.pp pp_var)) fmt tableau ;
    (pp_if (not (Var.Map.is_empty intervals)) "intervals" (pp_var_map ~arrow:"" CItv.pp pp_var))
      fmt intervals ;
    (pp_if (not (Atom.Set.is_empty atoms)) "atoms" (Atom.Set.pp_with_pp_var pp_var)) fmt atoms ;
    (pp_if (not (Atom.Set.is_empty term_conditions)) "term_conds" (Atom.Set.pp_with_pp_var pp_var))
      fmt term_conditions ;
    (pp_if
       (not (Term.Set.is_empty term_conditions2))
       "term_conds2" (Term.Set.pp_with_pp_var pp_var) )
      fmt term_conditions2 ;
    if Config.debug_level_analysis >= 3 then (
      (pp_if
         (not (Var.Map.is_empty linear_eqs_occurrences))
         "linear_eqs_occurrences" (VarMapOccurrences.pp pp_var) )
        fmt linear_eqs_occurrences ;
      (pp_if
         (not (Var.Map.is_empty tableau_occurrences))
         "tableau_occurrences" (VarMapOccurrences.pp pp_var) )
        fmt tableau_occurrences ;
      (pp_if
         (not (Var.Map.is_empty term_eqs_occurrences))
         "term_eqs_occurrences" (TermMapOccurrences.pp pp_var) )
        fmt term_eqs_occurrences ;
      (pp_if
         (not (Var.Map.is_empty atoms_occurrences))
         "atoms_occurrences" (AtomMapOccurrences.pp pp_var) )
        fmt atoms_occurrences ) ;
    F.pp_close_box fmt ()


  (* {2 mutations} *)

  let add_const_eq v t phi =
    Debug.p "add_const_eq %a->%a@\n" Var.pp v (Term.pp Var.pp) t ;
    match Var.Map.find_opt v phi.const_eqs with
    | None ->
        Sat {phi with const_eqs= Var.Map.add v t phi.const_eqs}
    | Some t' ->
        if Term.equal_syntax t t' then Sat phi
        else
          let reason () =
            F.asprintf "equating distinct const terms %a and %a" (Term.pp Var.pp) t (Term.pp Var.pp)
              t'
          in
          Unsat {reason; source= __POS__}


  let remove_const_eq v phi =
    Debug.p "remove_const_eq for %a@\n" Var.pp v ;
    {phi with const_eqs= Var.Map.remove v phi.const_eqs}


  let add_dynamic_type v t ?source_file phi =
    match Var.Map.find_opt v phi.type_constraints with
    | None ->
        ( { phi with
            type_constraints=
              Var.Map.add v (InstanceOf.Known {typ= t; source_file}) phi.type_constraints }
        , false )
    | Some (InstanceOf.Known {typ= t'}) ->
        if Typ.equal t t' then (phi, false) else (phi, true)
    | Some (InstanceOf.Unknown {below; notbelow}) ->
        if List.exists below ~f:(fun t' -> InstanceOf.is_subtype t t' |> not) then (phi, true)
        else if List.exists notbelow ~f:(fun t' -> InstanceOf.is_subtype t t') then (phi, true)
        else
          ( { phi with
              type_constraints=
                Var.Map.add v (InstanceOf.Known {typ= t; source_file= None}) phi.type_constraints }
          , false )


  let add_below v t phi =
    Debug.p "add_below %a %a@\n" Var.pp v (Typ.pp_full Pp.text) t ;
    if InstanceOf.is_final t then
      if InstanceOf.is_abstract t then (
        (* Not sure this ever actually happens *)
        Debug.p "type is abstract and final so v=0" ;
        (phi, true) )
      else (
        Debug.p "type is final so adding as known dynamic type" ;
        add_dynamic_type v t phi )
    else
      match Var.Map.find_opt v phi.type_constraints with
      | None ->
          Debug.p "not found so adding below constraint@\n" ;
          let phi =
            { phi with
              type_constraints=
                Var.Map.add v (InstanceOf.Unknown {below= [t]; notbelow= []}) phi.type_constraints
            }
          in
          Debug.p "new phi is %a@\n" (pp_with_pp_var Var.pp) phi ;
          (phi, false)
      | Some (InstanceOf.Known {typ= t'}) ->
          (phi, not (InstanceOf.is_subtype t' t))
      | Some (InstanceOf.Unknown {below; notbelow}) ->
          if List.exists below ~f:(fun t' -> InstanceOf.is_subtype t' t) then
            (* new below constraint is redundant with an existing one *) (phi, false)
          else if List.exists notbelow ~f:(fun t' -> InstanceOf.is_subtype t t') then
            (* New below constraint is incompatible with a notbelow one.
                 Note that once we know the value is zero, we shouldn't care
                 any more what type_constraints says about it, so we don't
                 bother to update, or do any further checks
              *)
            (phi, true)
          else if
            InstanceOf.is_concrete_or_abstract t
            && List.exists below ~f:(fun t' ->
                   InstanceOf.is_concrete_or_abstract t'
                   && (not (InstanceOf.is_subtype t t'))
                   && not (InstanceOf.is_subtype t' t) )
          then (
            Debug.p "inconsistent concrete upper bounds %a and <something>, zeroing"
              (Typ.pp Pp.text) t ;
            (phi, true) )
          else
            ( { phi with
                type_constraints=
                  Var.Map.add v
                    (InstanceOf.Unknown {below= t :: below; notbelow})
                    phi.type_constraints }
            , false )


  let add_notbelow v t phi =
    match Var.Map.find_opt v phi.type_constraints with
    | None ->
        Debug.p "couldn't find %a in type constraints, writing singleton notbelow" Var.pp v ;
        ( { phi with
            type_constraints=
              Var.Map.add v (InstanceOf.Unknown {below= []; notbelow= [t]}) phi.type_constraints }
        , false )
    | Some (InstanceOf.Known {typ= t'}) ->
        if InstanceOf.is_subtype t' t then (phi, true) else (phi, false)
    | Some (InstanceOf.Unknown {below; notbelow}) ->
        (* new notbelow constraint is redundant with an existing one *)
        if List.exists notbelow ~f:(fun t' -> InstanceOf.is_subtype t t') then (
          Debug.p "adding %a to notbelow is redundant" (Typ.pp_full Pp.text) t ;
          (phi, false) )
        else if List.exists below ~f:(fun t' -> InstanceOf.is_subtype t' t) then (
          Debug.p "adding %a to notbelow is inconsistent" (Typ.pp_full Pp.text) t ;
          (phi, true) )
        else (
          Debug.p "actually going ahead and adding %a to notbelow" (Typ.pp_full Pp.text) t ;
          ( { phi with
              type_constraints=
                Var.Map.add v
                  (InstanceOf.Unknown {below; notbelow= t :: notbelow})
                  phi.type_constraints }
          , false ) )


  let copy_type_constraints v_src v_target phi =
    match Var.Map.find_opt v_src phi.type_constraints with
    | None ->
        phi
    | Some src_constraints -> (
      match Var.Map.find_opt v_target phi.type_constraints with
      | None ->
          {phi with type_constraints= Var.Map.add v_target src_constraints phi.type_constraints}
      | Some _ ->
          L.die InternalError "Failed attempt to copy type constraints" )


  let remove_term_eq_ t v term_eqs term_eqs_occurrences =
    let term_eqs_occurrences =
      match Term.get_as_linear t with
      | Some _ ->
          term_eqs_occurrences
      | None ->
          Term.fold_variables t ~init:term_eqs_occurrences ~f:(fun occurrences v' ->
              TermMapOccurrences.remove v' ~occurred_in:(t, Domain) occurrences )
          |> TermMapOccurrences.remove v ~occurred_in:(t, Range)
          |> TermMapOccurrences.remove v ~occurred_in:(t, DomainAndRange)
    in
    let term_eqs = Term.VarMap.remove t term_eqs in
    (term_eqs, term_eqs_occurrences)


  let remove_term_eq t v phi =
    Debug.p "remove_term_eq %a->%a in %a@\n" (Term.pp Var.pp) t Var.pp v (pp_with_pp_var Var.pp) phi ;
    let term_eqs, term_eqs_occurrences =
      remove_term_eq_ t v phi.term_eqs phi.term_eqs_occurrences
    in
    {phi with term_eqs; term_eqs_occurrences}


  let add_term_eq t v phi =
    Debug.p "add_term_eq %a->%a@\n" (Term.pp Var.pp) t Var.pp v ;
    match get_term_eq phi t with
    | Some v' when Var.equal v v' ->
        (phi, None)
    | (Some _ | None) as new_eq ->
        let term_eqs_occurrences =
          match Term.get_as_linear t with
          | Some _ ->
              TermMapOccurrences.add v ~occurs_in:(t, Range) phi.term_eqs_occurrences
          | None ->
              let term_eqs_occurrences, added_to_range =
                Term.fold_variables t ~init:(phi.term_eqs_occurrences, false)
                  ~f:(fun (occurrences, added_to_range) v' ->
                    let target, added_to_range =
                      if Var.equal v v' then (TermDomainOrRange.DomainAndRange, true)
                      else (TermDomainOrRange.Domain, added_to_range)
                    in
                    (TermMapOccurrences.add v' ~occurs_in:(t, target) occurrences, added_to_range) )
              in
              if added_to_range then term_eqs_occurrences
              else TermMapOccurrences.add v ~occurs_in:(t, Range) term_eqs_occurrences
        in
        ({phi with term_eqs= Term.VarMap.add t v phi.term_eqs; term_eqs_occurrences}, new_eq)


  let remove_linear_eq v l phi =
    Debug.p "remove_linear_eq %a=%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
    let linear_eqs_occurrences =
      LinArith.get_variables l
      |> Seq.fold_left
           (fun occurrences v' -> VarMapOccurrences.remove v' ~occurred_in:v occurrences)
           phi.linear_eqs_occurrences
    in
    let phi = {phi with linear_eqs= Var.Map.remove v phi.linear_eqs; linear_eqs_occurrences} in
    let t = Linear l |> Term.simplify_linear in
    match get_term_eq phi t with Some v' when Var.equal v v' -> remove_term_eq t v phi | _ -> phi


  let add_linear_eq v l phi =
    Debug.p "add_linear_eq %a=%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
    let phi =
      match Var.Map.find_opt v phi.linear_eqs with
      | Some l_old ->
          (* get rid of soon-to-be-junk occurrences and [term_eqs] associated with the previous
               binding *)
          remove_linear_eq v l_old phi
      | None ->
          phi
    in
    let linear_eqs_occurrences =
      LinArith.get_variables l
      |> Seq.fold_left
           (fun occurrences v' -> VarMapOccurrences.add v' ~occurs_in:v occurrences)
           phi.linear_eqs_occurrences
    in
    let phi = {phi with linear_eqs= Var.Map.add v l phi.linear_eqs; linear_eqs_occurrences} in
    (* update [term_eqs] too with the reverse equality *)
    add_term_eq (Linear l |> Term.simplify_linear) v phi


  let remove_tableau_eq v l phi =
    Debug.p "remove_tableau_eq %a=%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
    let tableau_occurrences =
      LinArith.get_variables l
      |> Seq.fold_left
           (fun occurrences v' -> VarMapOccurrences.remove v' ~occurred_in:v occurrences)
           phi.tableau_occurrences
    in
    {phi with tableau= Var.Map.remove v phi.tableau; tableau_occurrences}


  let add_tableau_eq v l phi =
    Debug.p "add_tableau_eq %a=%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
    let phi =
      match Var.Map.find_opt v phi.tableau with
      | Some l_old ->
          (* get rid of soon-to-be-junk occurrences associated with the previous
               binding *)
          remove_tableau_eq v l_old phi
      | None ->
          phi
    in
    let tableau_occurrences =
      LinArith.get_variables l
      |> Seq.fold_left
           (fun occurrences v' -> VarMapOccurrences.add v' ~occurs_in:v occurrences)
           phi.tableau_occurrences
    in
    {phi with tableau= Var.Map.add v l phi.tableau; tableau_occurrences}


  let add_interval_ v intv intervals =
    let+ possibly_better_intv =
      match Var.Map.find_opt v intervals with
      | None ->
          Sat intv
      | Some intv' ->
          let reason () = F.asprintf "intersection %a*%a" CItv.pp intv CItv.pp intv' in
          Debug.p "intersection %a*%a" CItv.pp intv CItv.pp intv' ;
          CItv.intersection intv intv' |> SatUnsat.of_option {reason; source= __POS__}
    in
    Debug.p "adding %a%a@\n" Var.pp v CItv.pp possibly_better_intv ;
    Var.Map.add v possibly_better_intv intervals


  let add_interval v intv phi =
    let+ intervals = add_interval_ v intv phi.intervals in
    {phi with intervals}


  let add_atom atom phi =
    let atoms_occurrences =
      Atom.fold_variables atom ~init:phi.atoms_occurrences ~f:(fun occurrences v' ->
          AtomMapOccurrences.add v' ~occurs_in:atom occurrences )
    in
    {phi with atoms= Atom.Set.add atom phi.atoms; atoms_occurrences}


  let add_termination_atom atom phi =
    let atoms_occurrences =
      Atom.fold_variables atom ~init:phi.atoms_occurrences ~f:(fun occurrences v' ->
          AtomMapOccurrences.add v' ~occurs_in:atom occurrences )
    in
    {phi with term_conditions= Atom.Set.add atom phi.term_conditions; atoms_occurrences}


  let rec and_termcond_atoms (phi : t) (atoms : Atom.t list) : t =
    match atoms with
    | hd :: tl ->
        let newphi = add_termination_atom hd phi in
        and_termcond_atoms newphi tl
    | _ ->
        phi


  let and_termcond_binop (phi : t) (term : Term.t) : t =
    {phi with term_conditions2= Term.Set.add term phi.term_conditions2}


  let and_path_flush (phi : t) =
    {phi with term_conditions2= Term.Set.empty; term_conditions= Atom.Set.empty}


  let remove_atom_ atom atoms atoms_occurrences =
    let atoms_occurrences =
      Atom.fold_variables atom ~init:atoms_occurrences ~f:(fun occurrences v' ->
          AtomMapOccurrences.remove v' ~occurred_in:atom occurrences )
    in
    let atoms = Atom.Set.remove atom atoms in
    (atoms, atoms_occurrences)


  let remove_atom atom phi =
    let atoms, atoms_occurrences = remove_atom_ atom phi.atoms phi.atoms_occurrences in
    {phi with atoms; atoms_occurrences}


  let remove_from_linear_eqs_occurrences v phi =
    {phi with linear_eqs_occurrences= Var.Map.remove v phi.linear_eqs_occurrences}


  let remove_from_tableau_occurrences v phi =
    {phi with tableau_occurrences= Var.Map.remove v phi.tableau_occurrences}


  let add_occurrence_to_range_of_term_eq t (v : VarUF.repr) phi =
    { phi with
      term_eqs_occurrences=
        TermMapOccurrences.add (v :> Var.t) ~occurs_in:(t, Range) phi.term_eqs_occurrences }


  let remove_from_term_eqs_occurrences v phi =
    {phi with term_eqs_occurrences= Var.Map.remove v phi.term_eqs_occurrences}


  let remove_from_atoms_occurrences v phi =
    {phi with atoms_occurrences= Var.Map.remove v phi.atoms_occurrences}


  let set_var_eqs var_eqs phi = if phys_equal phi.var_eqs var_eqs then phi else {phi with var_eqs}

  let set_tableau tableau phi = {phi with tableau}

  let set_intervals intervals phi = {phi with intervals}

  let get_terminal_conds t = t.term_conditions

  let get_terminal_terms t = t.term_conditions2

  let unsafe_mk ~var_eqs ~const_eqs ~type_constraints ~linear_eqs ~term_eqs ~tableau ~intervals
      ~atoms ~linear_eqs_occurrences ~tableau_occurrences ~term_eqs_occurrences ~atoms_occurrences =
    { var_eqs
    ; const_eqs
    ; type_constraints
    ; linear_eqs
    ; term_eqs
    ; tableau
    ; intervals
    ; atoms
    ; linear_eqs_occurrences
    ; tableau_occurrences
    ; term_eqs_occurrences
    ; atoms_occurrences
    ; term_conditions= Atom.Set.empty
    ; term_conditions2= Term.Set.empty }


  let join phi1 _phi2 =
    (* TODO: do phi1 /\ phi2 *)
    phi1


  let scramble_var phi_rhs phi v =
    let remove_if_different mem_lhs test_equal find remove lhs rhs =
      if (not mem_lhs) || test_equal (find v lhs) (find v rhs) then (lhs, false)
      else (remove v lhs, true)
    in
    let var_eqs, _removed =
      remove_if_different true VarUF.equal_repr
        (fun v var_eqs -> VarUF.find var_eqs v)
        VarUF.remove phi.var_eqs phi_rhs.var_eqs
    in
    let const_eqs, _removed =
      remove_if_different (Var.Map.mem v phi.const_eqs) (Option.equal Term.equal) Var.Map.find_opt
        Var.Map.remove phi.const_eqs phi_rhs.const_eqs
    in
    let type_constraints, _removed =
      remove_if_different
        (Var.Map.mem v phi.type_constraints)
        (Option.equal InstanceOf.equal_instance_fact)
        Var.Map.find_opt Var.Map.remove phi.type_constraints phi_rhs.type_constraints
    in
    let linear_eqs, removed_linear_eqs =
      remove_if_different (Var.Map.mem v phi.linear_eqs) (Option.equal LinArith.equal)
        Var.Map.find_opt Var.Map.remove phi.linear_eqs phi_rhs.linear_eqs
    in
    let linear_eqs_occurrences =
      if removed_linear_eqs then Var.Map.remove v phi.linear_eqs_occurrences
      else phi.linear_eqs_occurrences
    in
    let tableau, removed_tableau =
      remove_if_different (Var.Map.mem v phi.tableau) (Option.equal LinArith.equal) Var.Map.find_opt
        Var.Map.remove phi.tableau phi_rhs.tableau
    in
    let tableau_occurrences =
      if removed_tableau then Var.Map.remove v phi.tableau_occurrences else phi.tableau_occurrences
    in
    let intervals, _removed =
      remove_if_different (Var.Map.mem v phi.intervals) (Option.equal CItv.equal) Var.Map.find_opt
        Var.Map.remove phi.intervals phi_rhs.intervals
    in
    let atoms, atoms_occurrences =
      match Var.Map.find_opt v phi.atoms_occurrences with
      | None ->
          (phi.atoms, phi.atoms_occurrences)
      | Some atoms_v ->
          Atom.Set.fold
            (fun atom_v (atoms, atoms_occurrences) ->
              if Atom.Set.mem atom_v phi_rhs.atoms then (atoms, atoms_occurrences)
              else remove_atom_ atom_v atoms atoms_occurrences )
            atoms_v
            (phi.atoms, phi.atoms_occurrences)
    in
    let term_eqs, term_eqs_occurrences =
      match Var.Map.find_opt v phi.term_eqs_occurrences with
      | None ->
          (phi.term_eqs, phi.term_eqs_occurrences)
      | Some term_eqs_v ->
          TermDomainOrRange.Set.fold
            (fun (term_v, _) (term_eqs, term_eqs_occurrences) ->
              if Term.VarMap.find_opt term_v phi_rhs.term_eqs |> Option.exists ~f:(Var.equal v) then
                (term_eqs, term_eqs_occurrences)
              else remove_term_eq_ term_v v term_eqs term_eqs_occurrences )
            term_eqs_v
            (phi.term_eqs, phi.term_eqs_occurrences)
    in
    let term_conditions = phi.term_conditions in
    let term_conditions2 = phi.term_conditions2 in
    { var_eqs
    ; const_eqs
    ; type_constraints
    ; linear_eqs
    ; term_eqs
    ; tableau
    ; intervals
    ; atoms
    ; linear_eqs_occurrences
    ; tableau_occurrences
    ; term_eqs_occurrences
    ; atoms_occurrences
    ; term_conditions
    ; term_conditions2 }


  let remove_condition_for_join atom phi_lhs phi_rhs =
    Atom.fold_variables atom ~init:phi_lhs ~f:(scramble_var phi_rhs)


  let remove_conditions_for_join atoms phi_lhs phi_rhs =
    List.fold atoms ~init:phi_lhs ~f:(fun phi atom -> remove_condition_for_join atom phi phi_rhs)
end

include Unsafe

let fold_constant_var_map map ~init ~f =
  let f var _constant acc = f acc var in
  Var.Map.fold f map init


let fold_type_constraints_map map ~init ~f =
  let f var _constraint acc = f acc var in
  Var.Map.fold f map init


let fold_linear_eqs_vars linear_eqs ~init ~f =
  let f_eq var linarith acc = Seq.fold_left f (f acc var) (LinArith.get_variables linarith) in
  Var.Map.fold f_eq linear_eqs init


let fold_variables
    ( ({ var_eqs
       ; const_eqs
       ; type_constraints
       ; linear_eqs
       ; term_eqs= _
       ; tableau
       ; intervals
       ; atoms
       ; linear_eqs_occurrences= _
       ; tableau_occurrences= _
       ; term_eqs_occurrences= _
       ; atoms_occurrences= _
       ; term_conditions= _
       ; term_conditions2= _ }
       [@warning "+missing-record-field-pattern"] ) as phi ) ~init ~f =
  let init = VarUF.fold_elements var_eqs ~init ~f in
  let init = fold_constant_var_map const_eqs ~init ~f in
  let init = fold_type_constraints_map type_constraints ~init ~f in
  let init = fold_linear_eqs_vars linear_eqs ~init ~f in
  let init = fold_term_eqs_vars phi ~init ~f in
  let init = fold_linear_eqs_vars tableau ~init ~f in
  let init = fold_constant_var_map intervals ~init ~f in
  Atom.Set.fold (fun atom acc -> Atom.fold_variables atom ~init:acc ~f) atoms init


let is_neq_zero phi t =
  Term.get_as_var t
  |> Option.exists ~f:(fun v ->
         Var.Map.find_opt v phi.intervals |> Option.exists ~f:CItv.is_not_equal_to_zero )
  || Atom.Set.mem (NotEqual (t, Term.zero)) phi.atoms
  || Atom.Set.mem (LessThan (Term.zero, t)) phi.atoms


let is_non_pointer {var_eqs; linear_eqs; intervals; atoms} var =
  let repr = (VarUF.find var_eqs var :> Var.t) in
  Option.exists (Var.Map.find_opt repr linear_eqs) ~f:(fun v ->
      Option.is_some (LinArith.get_as_const v) )
  || Option.exists (Var.Map.find_opt repr intervals) ~f:CItv.is_non_pointer
  || Atom.Set.mem (Equal (IsInt (Var repr), Term.one)) atoms


(** module that breaks invariants more often that the rest, with an interface that is safer to use
*)
module Normalizer : sig
  val and_var_linarith : Var.t -> LinArith.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val and_var_term : Var.t -> Term.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val and_var_var : Var.t -> Var.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val normalize_atom : t -> Atom.t -> Atom.t list SatUnsat.t

  val and_normalized_atoms :
    t * new_eqs -> Atom.t list -> orig_atom:Atom.t list -> add_term:bool -> (t * new_eqs) SatUnsat.t
  (** use with the result of {!normalize_atom} in place of {!and_atom} *)

  val and_atom : Atom.t -> t * new_eqs -> add_term:bool -> (t * new_eqs) SatUnsat.t

  val and_dynamic_type :
    Var.t -> Typ.t -> ?source_file:SourceFile.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val and_below : Var.t -> Typ.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val and_notbelow : Var.t -> Typ.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val propagate_atom : Atom.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  (** [and_atom atom (phi, new_eqs)] is
      [SatUnsat.(normalize_atom phi atom >>= and_normalized_atoms (phi, new_eqs))] *)
end = struct
  (* Use the monadic notations when normalizing formulas. *)
  open SatUnsat.Import

  (** OVERVIEW: the best way to think about this is as a (somewhat half-assed) Shostak technique.

      The [var_eqs] and [linear_eqs] parts of a formula are kept in a normal form of sorts. We apply
      some deduction every time a new equality is discovered. Where this is incomplete is that we
      stop discovering new consequences of facts after some fixed number of steps (the [fuel]
      argument of some of the functions of this module). saturating the consequences of what we know
      implies keeping track of *all* the consequences (to avoid diverging by re-discovering the same
      facts over and over), which would be expensive. *)

  exception OutOfFuel of (t * new_eqs * (F.formatter -> unit))

  (* the only way to initialize fuel: no functions in the interface of this module
       take fuel as argument, and no functions in this module pass concrete fuel values,
       so this will always catch [OutOfFuel] exceptions and these exceptions will not
       escape this module *)
  let with_base_fuel f =
    (* an arbitrary value *)
    let base_fuel = 10 in
    try f ~fuel:base_fuel
    with OutOfFuel (phi, new_eqs, why) ->
      L.d_printfln "%t" why ;
      Sat (phi, new_eqs)


  let normalize_linear_ phi linear_eqs l =
    LinArith.subst_variables l ~f:(fun v ->
        let repr = (get_repr phi v :> Var.t) in
        match Var.Map.find_opt repr linear_eqs with None -> VarSubst repr | Some l' -> LinSubst l' )


  let get_var_as_const phi v =
    Var.Map.find_opt v phi.linear_eqs |> Option.bind ~f:LinArith.get_as_const


  let subst_target_of_term phi t =
    match Term.to_subst_target t with
    | (Term.QSubst _ | ConstantSubst _ | LinSubst _) as subst ->
        subst
    | VarSubst v as subst -> (
      match get_var_as_const phi v with Some q -> Term.QSubst q | None -> subst )
    | NonLinearTermSubst t' as subst -> (
      match get_term_eq phi t' |> Option.bind ~f:(get_var_as_const phi) with
      | Some q ->
          Term.QSubst q
      | None ->
          subst )


  (** substitute vars in [l] *once* with their linear form to discover more simplification
      opportunities *)
  let normalize_linear phi l = normalize_linear_ phi phi.linear_eqs l

  (** same as [normalize_linear] but for slack variables, used for inequalities *)
  let normalize_restricted phi l = normalize_linear_ phi phi.tableau l

  let normalize_var_const phi t =
    Debug.p "normalize_var_const initial term is %a@\n" (Term.pp Var.pp) t ;
    let t' =
      Term.subst_variables t ~f:(fun v ->
          let v_canon = (get_repr phi v :> Var.t) in
          match Var.Map.find_opt v_canon phi.linear_eqs with
          | None -> (
            match Var.Map.find_opt v_canon phi.const_eqs with
            | None ->
                VarSubst v_canon
            | Some c ->
                ConstantSubst (c, Some v_canon) )
          | Some l -> (
            match LinArith.get_as_const l with
            | None ->
                (* OPTIM: don't make the term bigger *) VarSubst v_canon
            | Some q ->
                (* replace vars by constants when available to possibly trigger further
                     simplifications in atoms. This is not actually needed for [term_eqs]. *)
                QSubst q ) )
    in
    Debug.p "normalized term is %a@\n" (Term.pp Var.pp) t' ;
    t'


  let add_lin_eq_to_new_eqs v l new_eqs =
    match LinArith.get_as_const l with
    | Some q when Q.is_zero q ->
        RevList.cons (EqZero v) new_eqs
    | _ ->
        new_eqs


  (** add [l1 = l2] to [phi.linear_eqs] and resolves consequences of that new fact

      [l1] and [l2] should have already been through {!normalize_linear} (w.r.t. [phi]) *)
  let rec solve_normalized_lin_eq ~fuel ?(force_no_tableau = false) new_eqs l1 l2 phi =
    Debug.p "solve_normalized_lin_eq: %a=%a@\n" (LinArith.pp Var.pp) l1 (LinArith.pp Var.pp) l2 ;
    LinArith.solve_eq l1 l2
    >>= function
    | None ->
        Sat (phi, new_eqs)
    | Some (v, l) -> (
      match LinArith.get_as_var l with
      | Some v' ->
          merge_vars ~fuel new_eqs v v' phi
      | None -> (
          let* phi, new_eqs =
            if (not force_no_tableau) && Var.is_restricted v && LinArith.is_restricted l then
              (* linear equalities between restricted variables can be forwarded to [tableau] *)
              solve_tableau_restricted_eq ~fuel new_eqs v l phi
            else Sat (phi, new_eqs)
          in
          match Var.Map.find_opt v phi.linear_eqs with
          | None ->
              (* add to the [term_eqs] relation only when we also add to [linear_eqs] *)
              let* phi, new_eqs =
                solve_normalized_term_eq_no_lin ~fuel new_eqs (Term.Linear l) v phi
              in
              (* the rep might be changed by [solve_normalized_term_eq_no_lin] *)
              let v = (get_repr phi v :> Var.t) in
              let new_eqs = add_lin_eq_to_new_eqs v l new_eqs in
              (* this can break the invariant that variables in the domain of [linear_eqs] do not
                   appear in the range of [linear_eqs], restore it *)
              add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v l phi
              >>= propagate_linear_eq ~fuel v l
          | Some l' ->
              (* This is the only step that consumes fuel: discovering an equality [l = l']: because we
                   do not record these anywhere (except when their consequence can be recorded as [y =
                   l''] or [y = y']), we could potentially discover the same equality over and over and
                   diverge otherwise. Or could we?) *)
              let* phi, new_eqs = propagate_linear_eq ~fuel v l (phi, new_eqs) in
              if fuel > 0 then (
                L.d_printfln "Consuming fuel solving linear equality (from %d)" fuel ;
                solve_normalized_lin_eq ~fuel:(fuel - 1) new_eqs l l' phi )
              else
                (* [fuel = 0]: give up simplifying further for fear of diverging *)
                raise
                  (OutOfFuel
                     ( phi
                     , new_eqs
                     , fun fmt -> F.fprintf fmt "Ran out of fuel solving linear equality" ) ) ) )


  and discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi =
    match new_eq_opt with
    | None ->
        Sat (phi, new_eqs)
    | Some v' ->
        merge_vars ~fuel new_eqs v v' phi


  (** add [t = v] to [phi.term_eqs] and resolves consequences of that new fact; don't use directly
      as it doesn't do any checks on what else should be done about [t = v] *)
  and add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi =
    Debug.p "add_term_eq_and_solve_new_eq_opt %a->%a@\n" (Term.pp Var.pp) t Var.pp v ;
    let phi, new_eq_opt = add_term_eq t v phi in
    discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi


  and add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v l phi =
    Debug.p "add_linear_eq_and_solve_new_eq_opt %a->%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
    let phi, new_eq_opt = add_linear_eq v l phi in
    discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi


  (** TODO: at the moment this doesn't try to discover and return new equalities implied by the
      tableau (see Chapter 5 in \[2\]) *)
  and solve_tableau_restricted_eq ?(force_no_lin_arith = false) ~fuel new_eqs w l phi =
    Debug.p "tableau %a = %a@\n" Var.pp w (LinArith.pp Var.pp) l ;
    let l_c = LinArith.get_constant_part l in
    let l_c_sign =
      if Q.(l_c > zero) then `Positive else if Q.(l_c = zero) then `Zero else `Negative
    in
    match (l_c_sign, LinArith.classify_minimized_maximized l) with
    | `Zero, (`Maximized | `Constant) ->
        (* [w = k1·v1 + ... + kn·vn], all coeffs [ki] are ≤0, so [w] = [v1] = ... = [vn] = 0. *)
        let* phi, new_eqs =
          if force_no_lin_arith then Sat (phi, new_eqs)
          else
            solve_normalized_lin_eq ~force_no_tableau:true ~fuel new_eqs (LinArith.of_var w)
              (LinArith.of_q Q.zero) phi
        in
        LinArith.get_variables l
        |> SatUnsat.seq_fold ~init:(phi, new_eqs) ~f:(fun (phi, new_eqs) v ->
               merge_vars ~fuel new_eqs w v phi )
    | (`Positive | `Zero), (`Minimized | `Constant) ->
        (* [w = l_c + k1·v1 + ... + kn·vn], all coeffs [ki] are ≥0 (and so are all possible
             values of all [vi]s since they are restricted variables), hence any possible value
             of [w] is ≥0 so [w ≥ 0] is a tautologie and we can just discard the atom *)
        Debug.p "Tautology@\n" ;
        Sat (phi, new_eqs)
    | (`Positive | `Zero), (`Maximized | `Neither) ->
        (* [w = l] is feasible and not a tautologie (and [l] is restricted), add to the
             tableau *)
        let phi = add_tableau_eq w l phi in
        Debug.p "Add to tableau@\n" ;
        Sat (phi, new_eqs)
    | `Negative, (`Maximized | `Constant) ->
        (* [l_c < 0], [w = l_c + k1·v1 + ... + kn·vn], all coeffs [ki] are ≤0 so [l] denotes
             only negative values, hence cannot be ≥0: contradiction *)
        Debug.p "Contradiction!@\n" ;
        let reason () = F.asprintf "tableau" in
        Unsat {reason; source= __POS__}
    | `Negative, (`Minimized | `Neither) -> (
      (* stuck, let's pivot to try to add a feasible equality to the tableau; there are two ways
           to pivot in \[2\] *)
      match Tableau.pivot_unbounded_with_positive_coeff phi.tableau w l with
      | Some tableau ->
          Sat (set_tableau tableau phi, new_eqs)
      | None ->
          if fuel > 0 then (
            Debug.p "PIVOT %d in %a@\n" fuel (Tableau.pp Var.pp) phi.tableau ;
            match Tableau.pivot l phi.tableau with
            | None ->
                (* Huho, we cannot put the equality in a feasible form (i.e. [u = c + k1·v1 +
                     ... + kn·vn] with [c≥0]). This isn't supposed to happen in theory but because
                     we're a bit sloppy with normalization we can exceptionally get there in
                     practice. Store it as an unrestricted linear equality to avoid losing
                     completeness and hope a later re-normalization will take care of it better. *)
                L.debug Analysis Verbose "No pivot found for %a in %a@\n" (LinArith.pp Var.pp) l
                  (Tableau.pp Var.pp) phi.tableau ;
                (* set [force_no_tableau] so that the equality won't just ping-pong back to here *)
                solve_normalized_lin_eq ~fuel ~force_no_tableau:true new_eqs (LinArith.of_var w)
                  (normalize_restricted phi l) phi
            | Some tableau ->
                let phi = set_tableau tableau phi in
                Debug.p "pivoted tableau: %a@\n" (Tableau.pp Var.pp) phi.tableau ;
                solve_tableau_restricted_eq ~fuel:(fuel - 1) new_eqs w (normalize_restricted phi l)
                  phi )
          else
            raise
              (OutOfFuel
                 ( phi
                 , new_eqs
                 , fun fmt ->
                     F.fprintf fmt "Ran out of fuel pivoting the tableau %a@\n" (Tableau.pp Var.pp)
                       phi.tableau ) ) )


  (** add [t = v] to [phi.term_eqs] and resolves consequences of that new fact; assumes that linear
      facts have been or will be added to [phi.linear_eqs] separately

      [t] should have already been through {!normalize_var_const} and [v] should be a representative
      from {!get_repr} (w.r.t. [phi]) *)
  and solve_normalized_term_eq_no_lin ~fuel new_eqs (t : Term.t) v phi =
    Debug.p "solve_normalized_term_eq_no_lin %a->%a@\n" (Term.pp Var.pp) t Var.pp v ;
    match t with
    | Linear l when LinArith.get_as_var l |> Option.is_some ->
        (* [v1=v2] is already taken care of by [var_eqs] *)
        Sat (phi, new_eqs)
    | _ ->
        let t =
          match t with
          | Linear l -> (
            match LinArith.get_as_const l with Some c -> Term.Const c | None -> t )
          | _ ->
              t
        in
        add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi


  (** same as {!solve_normalized_eq_no_lin} but also adds linear to [phi.linear_eqs] *)
  and solve_normalized_term_eq ~fuel new_eqs (t : Term.t) v phi =
    Debug.p "solve_normalized_term_eq: %a=%a in %a, new_eqs=%a@\n" (Term.pp Var.pp) t Var.pp v
      (pp_with_pp_var Var.pp) phi pp_new_eqs new_eqs ;
    match t with
    | Var v' ->
        merge_vars ~fuel new_eqs v v' phi
    | Linear l when LinArith.get_as_var l |> Option.is_some ->
        let v' = Option.value_exn (LinArith.get_as_var l) in
        merge_vars ~fuel new_eqs v v' phi
    | Linear l -> (
        (* [l = v]: need to first solve it to get [l' = v'] such that [v' < vars(l')], and [l'] is
             normalized wrt [phi.linear_eqs] (to get a canonical form), add this to [term_eqs], then
             in order to know which equality should be added to [linear_eqs] we still need to
             substitute [v'] with [linear_eqs] and solve again *)
        LinArith.solve_eq (normalize_linear phi l) (LinArith.of_var v)
        >>= function
        | None ->
            (* [v = v], we can drop this tautology *)
            Sat (phi, new_eqs)
        | Some (v', l') ->
            (* [l'] could contain [v], which hasn't been through [linear_eqs], so normalize again *)
            let l' = normalize_linear phi l' in
            let* phi, new_eqs =
              add_term_eq_and_solve_new_eq_opt ~fuel new_eqs (Term.Linear l') v' phi
            in
            solve_normalized_lin_eq ~fuel new_eqs l' (normalize_linear phi (LinArith.of_var v')) phi
        )
    | Const c ->
        (* same as above but constants ([c]) are always normalized so it's simpler *)
        let* phi, new_eqs = add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi in
        solve_normalized_lin_eq ~fuel new_eqs (LinArith.of_q c) (LinArith.of_var v) phi
    | String _ ->
        (* same as above for non-numeric constants *)
        let* phi = add_const_eq v t phi in
        add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi >>= propagate_term_eq ~fuel t v
    | _ ->
        solve_normalized_term_eq_no_lin ~fuel new_eqs t v phi >>= propagate_term_eq ~fuel t v


  and merge_vars ~fuel new_eqs v1 v2 phi =
    Debug.p "merge_vars: %a=%a@\n" Var.pp v1 Var.pp v2 ;
    let var_eqs, subst_opt = VarUF.union phi.var_eqs v1 v2 in
    let phi = set_var_eqs var_eqs phi in
    match subst_opt with
    | None ->
        (* we already knew the equality *)
        Debug.p "we already knew %a=%a@\n" Var.pp v1 Var.pp v2 ;
        Sat (phi, new_eqs)
    | Some (v_old, v_new) ->
        (* new equality [v_old = v_new]: we need to propagate this fact to the various domains,
             especially [linear_eqs]: we update a potential [v_old = l_old] to be [v_new = l_old],
             and if [v_new = l_new] was known we add [l_old = l_new] *)
        let v_new = (v_new :> Var.t) in
        Debug.p "new eq: %a->%a@\n" Var.pp v_old Var.pp v_new ;
        L.d_printfln "new eq: %a = %a" Var.pp v_old Var.pp v_new ;
        let new_eqs = RevList.cons (Equal (v_old, v_new)) new_eqs in
        (* substitute [v_old -> v_new] in [phi.linear_eqs] while maintaining the [linear_eqs]
             invariant *)
        propagate_var_eq ~fuel v_old v_new (phi, new_eqs)


  and propagate_in_const_eqs x y (phi, new_eqs) =
    Debug.p "[propagate_in_const_eqs] %a=%a@\n  @[" Var.pp x Var.pp y ;
    let r =
      match Var.Map.find_opt x phi.const_eqs with
      | None ->
          Sat (phi, new_eqs)
      | Some c -> (
          let phi = remove_const_eq x phi in
          match Var.Map.find_opt y phi.const_eqs with
          | None ->
              let+ phi = add_const_eq y c phi in
              (phi, new_eqs)
          | Some c' ->
              if Term.equal_syntax c c' then Sat (phi, new_eqs)
              else
                let reason () =
                  F.asprintf "propagating equates distinct const terms %a and %a" (Term.pp Var.pp) c
                    (Term.pp Var.pp) c'
                in
                Unsat {reason; source= __POS__} )
    in
    Debug.p "@]end [propagate_in_const_eqs] %a=%a@\n" Var.pp x Var.pp y ;
    r


  and propagate_in_linear_eqs_domain ~fuel v_old l (phi, new_eqs) =
    match LinArith.get_as_var l with
    | None ->
        Sat (phi, new_eqs)
    | Some v_new -> (
        Debug.p "[propagate_in_linear_eqs_domain] %a->%a@\n" Var.pp v_old Var.pp v_new ;
        let l_new = Var.Map.find_opt v_new phi.linear_eqs in
        let phi, l_old =
          match Var.Map.find_opt v_old phi.linear_eqs with
          | None ->
              (phi, None)
          | Some l_old ->
              (remove_linear_eq v_old l_old phi, Some l_old)
        in
        match (l_old, l_new) with
        | None, None ->
            Sat (phi, new_eqs)
        | None, Some l ->
            let new_eqs = add_lin_eq_to_new_eqs v_new l new_eqs in
            Sat (phi, new_eqs)
        | Some l, None ->
            let new_eqs = add_lin_eq_to_new_eqs v_new l new_eqs in
            add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v_new l phi
            >>= propagate_linear_eq ~fuel v_new l
        | Some l1, Some l2 ->
            let new_eqs = add_lin_eq_to_new_eqs v_new l1 new_eqs in
            let new_eqs = add_lin_eq_to_new_eqs v_new l2 new_eqs in
            (* no need to consume fuel here as we can only go through this branch finitely many
                 times because there are finitely many variables in a given formula *)
            (* TODO: we may want to keep the "simpler" representative for [v_new] between [l1] and [l2] *)
            solve_normalized_lin_eq ~fuel new_eqs l1 l2 phi )


  and propagate_in_linear_eqs_range ~fuel x lx (phi, new_eqs) =
    Debug.p "[propagate_in_linear_eqs_range] %a=%a@\n  @[" Var.pp x (LinArith.pp Var.pp) lx ;
    let r =
      match Var.Map.find_opt x phi.linear_eqs_occurrences with
      | None ->
          Sat (phi, new_eqs)
      | Some in_linear_eqs ->
          (* [x=l] has been added to the linear equalities so by the invariant (that we are about to
               restore) there are no further occurrences of [x] in [phi.linear_eqs] *)
          let phi = remove_from_linear_eqs_occurrences x phi in
          Var.Set.fold
            (fun v phi_new_eqs_sat ->
              let* phi, new_eqs = phi_new_eqs_sat in
              match Var.Map.find_opt v phi.linear_eqs with
              | None ->
                  Debug.p "huh? no %a in linear eqs but %a claimed it was in a linear eq for %a"
                    Var.pp v Var.pp x Var.pp v ;
                  phi_new_eqs_sat
              | Some lv ->
                  (* renormalize [lx] in case we have discovered new equalities since the beginning
                       of the fold(!!) *)
                  let lx = normalize_linear phi lx in
                  Debug.p "substituting %a->%a in %a->%a@\n" Var.pp x (LinArith.pp Var.pp) lx Var.pp
                    v (LinArith.pp Var.pp) lv ;
                  let r =
                    let lv' = LinArith.subst_variable x (LinSubst lx) lv in
                    (* check the invariant that [v] is (strictly) simpler than any variable in
                         [lv']; because the invariant was true before it's enough to check that it's
                         simpler than any variable in [lx] *)
                    let needs_pivot =
                      match LinArith.get_simplest lx with
                      | None ->
                          false
                      | Some min ->
                          Var.is_simpler_or_equal min v
                    in
                    Debug.p "needs_pivot= %b@\n" needs_pivot ;
                    if needs_pivot then
                      (* need to pivot the equality [v = lv'] to restore the above invariant since
                           it's not in the right form *)
                      let phi = remove_linear_eq v lv phi in
                      solve_normalized_lin_eq ~fuel:(fuel - 1) new_eqs
                        (LinArith.of_var v |> normalize_linear phi)
                        lv' phi
                    else
                      (* To restore the linarith invariants just subst [x->lx] in [lv],
                           i.e. [lv']. Detect if this is a variable equality and propagate it as such
                           if so instead of adding it to [linear_eqs] (which aren't supposed to
                           contain plain variable equalities). *)
                      match LinArith.get_as_var lv' with
                      | Some v' ->
                          let phi = remove_linear_eq v lv phi in
                          merge_vars ~fuel new_eqs v v' phi
                      | _ ->
                          add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v lv' phi
                  in
                  Debug.p "@\nResult of %a->%a in %a->%a=@\n  @[%a@]@\n" Var.pp x
                    (LinArith.pp Var.pp) lx Var.pp v (LinArith.pp Var.pp) lv
                    (SatUnsat.pp (fun fmt (phi, _) -> pp_with_pp_var Var.pp fmt phi))
                    r ;
                  r )
            in_linear_eqs
            (Sat (phi, new_eqs))
    in
    Debug.p "@]end [propagate_in_linear_eqs_range] %a=%a@\n" Var.pp x (LinArith.pp Var.pp) lx ;
    r


  and propagate_in_tableau ~fuel x lx (phi, new_eqs) =
    Debug.p "[propagate_in_tableau] %a=%a@\n  @[" Var.pp x (LinArith.pp Var.pp) lx ;
    let r =
      if not (Var.is_restricted x && LinArith.is_restricted lx) then Sat (phi, new_eqs)
      else
        let* phi, new_eqs =
          match Var.Map.find_opt x phi.tableau_occurrences with
          | None ->
              Sat (phi, new_eqs)
          | Some in_tableau ->
              (* [x=l] has been added to the tableau so by the invariant (that we are about to
                   restore) there are no further occurrences of [x] in [phi.tableau] *)
              let phi = remove_from_tableau_occurrences x phi in
              Var.Set.fold
                (fun v phi_new_eqs_sat ->
                  let* phi, new_eqs = phi_new_eqs_sat in
                  match Var.Map.find_opt v phi.tableau with
                  | None ->
                      Debug.p "huh? no %a in tableau but %a claimed it was" Var.pp v Var.pp x ;
                      phi_new_eqs_sat
                  | Some lv ->
                      (* renormalize [lx] in case we have discovered new equalities since the beginning
                           of the fold(!!) *)
                      let lx = normalize_restricted phi lx in
                      Debug.p "tableau substituting %a->%a in %a->%a@\n" Var.pp x
                        (LinArith.pp Var.pp) lx Var.pp v (LinArith.pp Var.pp) lv ;
                      let r =
                        let lv' = LinArith.subst_variable x (LinSubst lx) lv in
                        remove_tableau_eq v lv phi
                        |> solve_tableau_restricted_eq ~force_no_lin_arith:true ~fuel new_eqs v lv'
                      in
                      Debug.p "@\nResult of %a->%a in %a->%a=@\n  @[%a@]@\n" Var.pp x
                        (LinArith.pp Var.pp) lx Var.pp v (LinArith.pp Var.pp) lv
                        (SatUnsat.pp (fun fmt (phi, _) -> pp_with_pp_var Var.pp fmt phi))
                        r ;
                      r )
                in_tableau
                (Sat (phi, new_eqs))
        in
        match Var.Map.find_opt x phi.tableau with
        | None ->
            Sat (phi, new_eqs)
        | Some l -> (
            let phi = remove_tableau_eq x l phi in
            let* tableau_eq_opt = LinArith.solve_eq l lx in
            match tableau_eq_opt with
            | None ->
                Sat (phi, new_eqs)
            | Some (w, lw) ->
                solve_tableau_restricted_eq ~force_no_lin_arith:true ~fuel new_eqs w lw phi )
    in
    Debug.p "@]end [propagate_in_tableau] %a=%a@\n" Var.pp x (LinArith.pp Var.pp) lx ;
    r


  and propagate_in_term_eqs ~fuel (tx : Term.t) x ((phi, new_eqs) as phi_new_eqs) =
    match subst_target_of_term phi tx with
    | LinSubst _ | NonLinearTermSubst _ ->
        Debug.p "prop in term eqs tx=%a, x=%a being ignored@\n" (Term.pp Var.pp) tx Var.pp x ;
        Sat phi_new_eqs
    | (VarSubst _ | QSubst _ | ConstantSubst _) as subst_target_x -> (
      match Var.Map.find_opt x phi.term_eqs_occurrences with
      | None ->
          Sat phi_new_eqs
      | Some in_term_eqs ->
          ( (* [tx=x] with [tx] a constant or a variable has been added to the term equalities so by
                 the invariant (that we are about to restore) there are no further occurrences of [x]
                 on the LHS in [phi.term_eqs] (and occurrences on the RHS are dealt on the fly by
                 [Formula.Unsafe]) *)
            Debug.p "term_eq propagating %a = %a in %a@\n" (Term.pp Var.pp) tx Var.pp x
              (pp_with_pp_var Var.pp) phi ;
            let phi = remove_from_term_eqs_occurrences x phi in
            TermDomainOrRange.Set.fold (fun (t, domain_or_range) phi_new_eqs_sat ->
                if Term.equal t tx then phi_new_eqs_sat
                else
                  match get_term_eq phi t with
                  | None ->
                      Debug.p "huh? %a was supposed to appear in %a@\n" Var.pp x (Term.pp Var.pp) t ;
                      phi_new_eqs_sat
                  | Some y -> (
                    match domain_or_range with
                    | Range -> (
                        let* phi, new_eqs = phi_new_eqs_sat in
                        (* If t is an IsInstanceOf formula, and we've just found its truth value, propagate
                             the information into the below/notbelow type constraints on the relevant
                             variable, and also add >0 facts where appropriate *)
                        let* phi, new_eqs =
                          match Term.get_as_isinstanceof t with
                          | Some (var, typ, nullable) ->
                              if is_neq_zero phi tx then (
                                Debug.p "prop in term_eq adding below with nullable=%b\n" nullable ;
                                let* phi, new_eqs = and_below var typ (phi, new_eqs) in
                                if not nullable then (
                                  Debug.p "adding %a not equal to zero" Var.pp var ;
                                  let* atoms =
                                    Atom.eval ~is_neq_zero:(is_neq_zero phi)
                                      (NotEqual (Var var, Term.zero))
                                  in
                                  and_normalized_atoms (phi, new_eqs) atoms ~orig_atom:atoms
                                    ~add_term:true
                                  >>| snd )
                                else Sat (phi, new_eqs) )
                              else if
                                match tx with
                                | Linear l ->
                                    LinArith.is_zero l
                                | Const c ->
                                    Q.is_zero c
                                | _ ->
                                    false
                              then (
                                Debug.p "prop in term_eq adding notbelow@\n" ;
                                let* phi, new_eqs = and_notbelow var typ (phi, new_eqs) in
                                if nullable then
                                  let* atoms =
                                    Atom.eval ~is_neq_zero:(is_neq_zero phi)
                                      (NotEqual (Var var, Term.zero))
                                  in
                                  and_normalized_atoms (phi, new_eqs) atoms ~orig_atom:atoms
                                    ~add_term:true
                                  >>| snd
                                else Sat (phi, new_eqs) )
                              else (
                                Debug.p "%a is neither zero nor non-zero, leaving phi alone@\n"
                                  (Term.pp Var.pp) tx ;
                                Sat (phi, new_eqs) )
                          | None ->
                              Sat (phi, new_eqs)
                        in
                        (* Now check if the new equality on [x] introduced contradictions in [t=x] or new atoms *)
                        let* atoms_opt =
                          Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi)
                            (Equal (t, Term.simplify_linear tx))
                        in
                        match atoms_opt with
                        | None ->
                            (* need to add back that [x] occurs in [term_eqs(t)] since we removed [x] from
                                 [term_eqs_occurrences] altogether before the fold; the repr of [x] might have
                                 changed from [subst_target_x] *)
                            let x' = get_repr phi x in
                            Debug.p "no relevant atoms, adding dependency %a->%a back@\n" Var.pp
                              (x' :> Var.t)
                              (Term.pp Var.pp) t ;
                            Sat (add_occurrence_to_range_of_term_eq t x' phi, new_eqs)
                        | Some atoms ->
                            Debug.p "Found new atoms %a@\n"
                              (Pp.seq ~sep:"," (Atom.pp_with_pp_var Var.pp))
                              atoms ;
                            and_normalized_atoms (phi, new_eqs) atoms ~orig_atom:atoms
                              ~add_term:true
                            >>| snd )
                    | Domain | DomainAndRange -> (
                        let* phi, new_eqs = phi_new_eqs_sat in
                        let subst_target_x =
                          match subst_target_x with
                          | VarSubst v ->
                              Term.VarSubst (get_repr phi v :> Var.t)
                          | _ ->
                              subst_target_x
                        in
                        let* t' =
                          let exception Unsat of unsat_info in
                          try
                            Sat
                              (Term.subst_variables t
                                 ~f:(fun v -> if Var.equal v x then subst_target_x else VarSubst v)
                                 ~f_post:(fun ~prev () sub_t ->
                                   let sub_t' =
                                     if phys_equal prev sub_t then sub_t
                                     else
                                       match
                                         sub_t |> Term.eval_const_shallow >>= Term.simplify_shallow
                                         >>| Term.linearize >>| Term.simplify_linear
                                       with
                                       | Sat sub_t' ->
                                           sub_t'
                                       | Unsat unsat_info ->
                                           raise (Unsat unsat_info)
                                   in
                                   ((), sub_t') ) )
                          with Unsat unsat_info -> Unsat unsat_info
                        in
                        let phi = remove_term_eq t y phi in
                        Debug.p "phi=%a@\n" (pp_with_pp_var Var.pp) phi ;
                        match Term.get_as_var t' with
                        | Some y' when Var.equal y y' ->
                            Debug.p "Discarding tautology %a -> %a@\n" (Term.pp Var.pp) t' Var.pp y ;
                            Sat (phi, new_eqs)
                        | Some y' ->
                            merge_vars ~fuel new_eqs y y' phi
                        | None -> (
                            (* resolve whether the term is some atoms in disguise *)
                            let ty = normalize_var_const phi (Var y) in
                            let* atoms_opt =
                              Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi)
                                (Equal (t', ty))
                            in
                            match atoms_opt with
                            | Some atoms ->
                                Debug.p "adding atoms %a instead of term_eq@\n"
                                  (Pp.seq ~sep:"," (Atom.pp_with_pp_var Var.pp))
                                  atoms ;
                                and_normalized_atoms (phi, new_eqs) atoms ~orig_atom:atoms
                                  ~add_term:true
                                >>| snd
                            | None -> (
                              match get_term_eq phi t' with
                              | None -> (
                                  Debug.p "New term_eq %a -> %a@\n" (Term.pp Var.pp) t' Var.pp y ;
                                  match Term.get_as_linear t' with
                                  | Some l' ->
                                      Debug.p "delegating to [solve_normalized_lin_eq]@\n" ;
                                      solve_normalized_lin_eq ~fuel new_eqs
                                        (LinArith.of_var y |> normalize_linear phi)
                                        l' phi
                                  | None ->
                                      if Term.is_non_numeric_constant t' then
                                        let+ phi = add_const_eq y t' phi in
                                        (phi, new_eqs)
                                      else add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t' y phi )
                              | Some y' ->
                                  Debug.p "Existing term_eq %a -> %a, merging %a=%a@\n"
                                    (Term.pp Var.pp) t' Var.pp y' Var.pp y Var.pp y' ;
                                  merge_vars ~fuel new_eqs y y' phi ) ) ) ) ) )
            in_term_eqs
            (Sat (phi, new_eqs)) )


  and propagate_in_atoms ~fuel:_ tx x ((phi, new_eqs) as phi_new_eqs) =
    match Var.Map.find_opt x phi.atoms_occurrences with
    | None ->
        Sat phi_new_eqs
    | Some in_atoms ->
        (* [tx=x] has been added to the term equalities so by the invariant (that we are about
             to restore) there are no further occurrences of [x] in [phi.atoms] as we are going to
             substitute them with [tx] to get maximally-expanded atoms *)
        Debug.p "propagating %a = %a in atoms@\n" (Term.pp Var.pp) tx Var.pp x ;
        let phi = remove_from_atoms_occurrences x phi in
        let subst_target_x = subst_target_of_term phi tx in
        (* TODO: could be more efficient to Atom.Set.map + linearly follow along in in_atoms,
             raising Unsat as needed and accumulating in a ref (no fold_map...) *)
        Atom.Set.fold
          (fun atom phi_new_eqs_sat ->
            let* phi, new_eqs = phi_new_eqs_sat in
            if Atom.Set.mem atom phi.atoms then
              let phi = remove_atom atom phi in
              and_atom
                (Atom.subst_variables
                   ~f:(fun x' -> if Var.equal x' x then subst_target_x else VarSubst x')
                   atom )
                (phi, new_eqs) ~add_term:false
              >>| snd
            else phi_new_eqs_sat )
          in_atoms
          (Sat (phi, new_eqs))


  and propagate_term_eq ~fuel tx x phi_new_eqs =
    propagate_in_term_eqs ~fuel tx x phi_new_eqs >>= propagate_in_atoms ~fuel tx x


  and propagate_linear_eq ~fuel x lx phi_new_eqs =
    propagate_in_linear_eqs_range ~fuel x lx phi_new_eqs
    >>= propagate_in_linear_eqs_domain ~fuel x lx
    >>= propagate_in_tableau ~fuel x lx
    >>= propagate_term_eq ~fuel (Term.Linear lx) x


  and propagate_var_eq ~fuel x y phi_new_eqs =
    propagate_in_const_eqs x y phi_new_eqs >>= propagate_linear_eq ~fuel x (LinArith.of_var y)


  and propagate_atom atom phi_new_eqs =
    Debug.p "propagate atom called on %a@\n" (Atom.pp_with_pp_var Var.pp) atom ;
    match Atom.get_as_var_neq_zero atom with
    | None ->
        Sat phi_new_eqs
    | Some v -> (
        Debug.p "got as var neq zero with v=%a@\n" Var.pp v ;
        match Var.Map.find_opt v (fst phi_new_eqs).term_eqs_occurrences with
        | None ->
            Debug.p "failed to find in term_eqs@\n" ;
            Sat phi_new_eqs
        | Some in_term_eqs ->
            Debug.p "found in term_eqs@\n" ;
            TermDomainOrRange.Set.fold
              (fun (t, domain_or_range) phi_new_eqs_sat ->
                Debug.p "found var maps to %a@\n" (Term.pp Var.pp) t ;
                let* phi, new_eqs = phi_new_eqs_sat in
                match domain_or_range with
                | Domain ->
                    Debug.p "domain@\n" ;
                    phi_new_eqs_sat
                | Range | DomainAndRange -> (
                    Debug.p "range or both\n" ;
                    let* phi, new_eqs =
                      match Term.get_as_isinstanceof t with
                      | Some (var, typ, nullable) ->
                          Debug.p "prop atom in term_eq adding below, nullable=%b\n" nullable ;
                          let* phi, new_eqs =
                            if not nullable then (
                              Debug.p "also adding greater than zero" ;
                              let* atoms =
                                Atom.eval ~is_neq_zero:(is_neq_zero phi)
                                  (LessThan (Term.zero, Var var))
                              in
                              and_normalized_atoms (phi, new_eqs) atoms ~orig_atom:atoms
                                ~add_term:true
                              >>| snd )
                            else Sat (phi, new_eqs)
                          in
                          and_below var typ (phi, new_eqs)
                      | None ->
                          Sat (phi, new_eqs)
                    in
                    let* atoms_opt =
                      (* TODO: double-check eval_with_normalized_terms does the intended thing here *)
                      Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi)
                        (Equal (t, Var v))
                    in
                    match atoms_opt with
                    | None ->
                        Sat (phi, new_eqs)
                    | Some atoms ->
                        Debug.p "Found new atoms thanks to %a≠0: [%a]@\n" Var.pp v
                          (Pp.seq ~sep:"," (Atom.pp_with_pp_var Var.pp))
                          atoms ;
                        and_normalized_atoms (phi, new_eqs) atoms ~orig_atom:atoms ~add_term:true
                        >>| snd ) )
              in_term_eqs (Sat phi_new_eqs) )


  (* Assumes [w] is restricted, [l] is normalized. This is called [addlineq] in \[2\]. *)
  and solve_tableau_eq ~fuel new_eqs w l phi =
    Debug.p "Solving %a = %a@\n" Var.pp w (LinArith.pp Var.pp) l ;
    match LinArith.solve_for_unrestricted w l with
    | Some (v, l_v) ->
        (* there is at least one variable [v] whose value is not known to always be non-negative;
             we cannot add it to the tableau, let's add the equality [v=l_v <=> w=l] to the "normal"
             linear equalities for now *)
        Debug.p "Unrestricted %a = %a@\n" Var.pp v (LinArith.pp Var.pp) l_v ;
        solve_normalized_lin_eq ~fuel new_eqs l_v (LinArith.of_var v) phi
    | None ->
        (* [l] can go into the tableau as it contains only restricted (non-negative) variables *)
        let* phi, new_eqs =
          solve_normalized_lin_eq ~force_no_tableau:true ~fuel new_eqs l (LinArith.of_var w) phi
        in
        solve_tableau_restricted_eq ~fuel new_eqs w l phi


  and solve_lin_ineq new_eqs l1 l2 phi =
    (* [l1 ≤ l2] becomes [(l2-l1) ≥ 0], encoded as [l = w] with [w] a fresh restricted variable *)
    let l = LinArith.subtract l2 l1 |> normalize_linear phi |> normalize_restricted phi in
    let l_c_sign = Q.sign (LinArith.get_constant_part l) in
    let l_restricted = LinArith.is_restricted l in
    match LinArith.classify_minimized_maximized l with
    | (`Minimized | `Constant) when l_restricted && l_c_sign >= 0 ->
        Debug.p "Skip adding %a≥0: known@\n" (LinArith.pp Var.pp) l ;
        Sat (phi, new_eqs) (* already trivially known: don't add to formula to avoid divergence *)
    | (`Maximized | `Constant) when l_restricted && l_c_sign < 0 ->
        Debug.p "Skip adding %a≥0: unsat@\n" (LinArith.pp Var.pp) l ;
        let reason () = F.asprintf "%a≥0 is false@\n" (LinArith.pp Var.pp) l in
        Unsat {reason; source= __POS__}
    | _ ->
        let w = Var.mk_fresh_restricted () in
        with_base_fuel (solve_tableau_eq new_eqs w l phi)


  and solve_lin_eq new_eqs t1 t2 phi =
    with_base_fuel
      (solve_normalized_lin_eq new_eqs (normalize_linear phi t1) (normalize_linear phi t2) phi)


  and and_var_linarith v l (phi, new_eqs) = solve_lin_eq new_eqs l (LinArith.of_var v) phi

  (* TODO: should we check if [φ ⊢ atom] (i.e. whether [φ ∧ ¬atom] is unsat) in [normalize_atom],
       or is [normalize_atom] already just as strong? *)
  and normalize_atom phi (atom : Atom.t) =
    let atom' = Atom.map_terms atom ~f:(fun t -> normalize_var_const phi t) in
    Debug.p "Normalizer.normalize_atom atom'=%a@\n" (Atom.pp_with_pp_var Var.pp) atom' ;
    Atom.eval ~is_neq_zero:(is_neq_zero phi) atom'


  (** return [(new_linear_equalities, phi ∧ atom)], where [new_linear_equalities] is [true] if
      [phi.linear_eqs] was changed as a result *)
  and and_normalized_atom (phi, new_eqs) atom =
    match Atom.var_terms_to_linear atom with
    | Atom.Equal (Linear _, Linear _) ->
        assert false
    | Atom.Equal (Linear l, Const c) | Atom.Equal (Const c, Linear l) ->
        (* NOTE: {!normalize_atom} calls {!Atom.eval}, which normalizes linear equalities so
             they end up only on one side, hence only this match case is needed to detect linear
             equalities *)
        let+ phi', new_eqs = solve_lin_eq new_eqs l (LinArith.of_q c) phi in
        (true, (phi', new_eqs))
    | (Atom.Equal (Linear l, t) | Atom.Equal (t, Linear l))
      when Option.is_some (LinArith.get_as_var l) ->
        let v = Option.value_exn (LinArith.get_as_var l) in
        let+ phi_new_eqs' = with_base_fuel (solve_normalized_term_eq new_eqs t v phi) in
        (false, phi_new_eqs')
    | Atom.LessEqual (Linear l, Const c) ->
        let+ phi', new_eqs = solve_lin_ineq new_eqs l (LinArith.of_q c) phi in
        (true, (phi', new_eqs))
    | Atom.LessEqual (Const c, Linear l) ->
        let+ phi', new_eqs = solve_lin_ineq new_eqs (LinArith.of_q c) l phi in
        (true, (phi', new_eqs))
    | Atom.LessThan (Linear l, Const c) ->
        let+ phi', new_eqs = solve_lin_ineq new_eqs l (LinArith.of_q (Q.sub c Q.one)) phi in
        (true, (phi', new_eqs))
    | Atom.LessThan (Const c, Linear l) ->
        let+ phi', new_eqs = solve_lin_ineq new_eqs (LinArith.of_q (Q.add c Q.one)) l phi in
        (true, (phi', new_eqs))
    | atom' ->
        (* the previous normalization has "simplified" [Var] terms into [Linear] ones, revert
             this *)
        let atom = Atom.simplify_linear atom' in
        let+ phi_new_eqs = (add_atom atom phi, new_eqs) |> propagate_atom atom in
        (false, phi_new_eqs)


  and and_normalized_atoms (phi, new_eqs) atoms ~orig_atom ~add_term =
    let upd_phi =
      if
        add_term
        && ( Config.pulse_experimental_infinite_loop_checker
           || Config.pulse_experimental_infinite_loop_checker_v2 )
      then and_termcond_atoms phi orig_atom
      else phi
    in
    SatUnsat.list_fold atoms
      ~init:(false, (upd_phi, new_eqs))
      ~f:(fun (linear_changed, (phi, new_eqs)) atom ->
        let+ changed', phi_new_eqs = and_normalized_atom (phi, new_eqs) atom in
        (linear_changed || changed', phi_new_eqs) )


  and and_atom atom (phi, new_eqs) ~add_term =
    normalize_atom phi atom >>= and_normalized_atoms (phi, new_eqs) ~orig_atom:[atom] ~add_term


  and and_var_is_zero v (phi, neweqs) =
    if Language.curr_language_is Erlang then
      (* No null pointers in Erlang *)
      let reason () = F.asprintf "%a=0 but %a is an Erlang pointer, hence ≠0" Var.pp v Var.pp v in
      Unsat {reason; source= __POS__}
    else solve_lin_eq neweqs (LinArith.of_var v) (LinArith.of_q Q.zero) phi


  and and_below v t (phi, new_eqs) =
    let phi, should_zero = add_below v t phi in
    if should_zero then and_var_is_zero v (phi, new_eqs) else Sat (phi, new_eqs)


  and and_notbelow v t (phi, new_eqs) =
    let phi, should_zero = add_notbelow v t phi in
    if should_zero then and_var_is_zero v (phi, new_eqs) else Sat (phi, new_eqs)


  (* [and_dynamic_type] wraps [add_dynamic_type]. In particular, if the call to the former
        returns [(phi, true)], then [and_dynamic_type] also adds and propagates the assertion that
        the value to which we added the type must actually be null.
    *)
  let and_dynamic_type v t ?source_file (phi, new_eqs) =
    let phi, should_zero = add_dynamic_type v t ?source_file phi in
    if should_zero then and_var_is_zero v (phi, new_eqs) else Sat (phi, new_eqs)


  let and_var_term ~fuel v t (phi, new_eqs) =
    let* (t' : Term.t) =
      normalize_var_const phi t |> Atom.eval_term ~is_neq_zero:(is_neq_zero phi)
    in
    let v' = (get_repr phi v :> Var.t) in
    let* t_v =
      normalize_var_const phi (Term.Var v') |> Atom.eval_term ~is_neq_zero:(is_neq_zero phi)
    in
    let* phi, new_eqs =
      let* atoms_opt =
        Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi) (Equal (t', t_v))
      in
      match atoms_opt with
      | None ->
          Sat (phi, new_eqs)
      | Some atoms ->
          and_normalized_atoms (phi, new_eqs) atoms ~orig_atom:atoms ~add_term:true >>| snd
    in
    solve_normalized_term_eq ~fuel new_eqs t' v' phi


  (* interface *)

  let and_atom atom phi_new_eqs ~add_term =
    Debug.p "BEGIN and_atom %a@\n" (Atom.pp_with_pp_var Var.pp) atom ;
    let phi_new_eqs' = and_atom atom phi_new_eqs ~add_term >>| snd in
    Debug.p "END and_atom %a -> %a@\n" (Atom.pp_with_pp_var Var.pp) atom
      (SatUnsat.pp (Pp.pair ~fst:(pp_with_pp_var Var.pp) ~snd:pp_new_eqs))
      phi_new_eqs' ;
    phi_new_eqs'


  let and_normalized_atoms phi_new_eqs atoms ~orig_atom ~add_term =
    let phi_new_eqs' = and_normalized_atoms phi_new_eqs atoms ~orig_atom ~add_term >>| snd in
    Debug.p "and_normalized_atoms [@[<v>%a@]] -> %a@\n"
      (Pp.seq ~sep:";" (Atom.pp_with_pp_var Var.pp))
      atoms
      (SatUnsat.pp (pp_with_pp_var Var.pp))
      (SatUnsat.map fst phi_new_eqs') ;
    phi_new_eqs'


  let and_var_term v t phi_new_eqs = with_base_fuel (and_var_term v t phi_new_eqs)

  let and_var_var v1 v2 (phi, new_eqs) = with_base_fuel (merge_vars new_eqs v1 v2 phi)
end
