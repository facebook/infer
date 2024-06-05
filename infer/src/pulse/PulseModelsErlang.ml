(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module F = Format

(** Represents the result of a transfer function that may (a) nondeterministically split the state,
    and (b) some of the nondeterministic branches may be errors. Goes well with [let>] defined later
    in this file. *)
type 'ok result = 'ok AccessResult.t list

(** A type for transfer functions that make an object, add it to abstract state
    ([AbductiveDomain.t]), and return a handle to it ([(AbstractValue.t * ValueHistory.t)]). Note
    that the type is simlar to that of [PulseOperations.eval]. *)
type maker =
     AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t SatUnsat.t

(** special case of {!maker} when the result is known to be satisfiable *)
type sat_maker =
  AbductiveDomain.t -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

(** Similar to {!maker} but can return a disjunction of results. *)
type disjunction_maker =
  AbductiveDomain.t -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t list

(** A type similar to {!maker} for transfer functions that only return an abstract value without any
    history attached to it. *)
type value_maker =
  AbductiveDomain.t -> (AbductiveDomain.t * AbstractValue.t) AccessResult.t SatUnsat.t

let write_field_and_deref path location ~struct_addr ~field_addr ~field_val field_name astate =
  let* astate =
    PulseOperations.write_field path location ~ref:struct_addr field_name ~obj:field_addr astate
  in
  PulseOperations.write_deref path location ~ref:field_addr ~obj:field_val astate


(** Returns the erlang type of an abstract value, extracted from its dynamic type (pulse) attribute.
    Returns [Any] if the value has no dynamic type, or if no erlang type can be extracted from it.
    Note that it may be the case for some encoded Erlang values (such as strings, floats or closures
    at the first implementation time). *)
let get_erlang_type_or_any val_ astate =
  let open IOption.Let_syntax in
  let typename =
    let* {Formula.typ} = PulseArithmetic.get_dynamic_type val_ astate in
    Typ.name typ
  in
  match typename with Some (Typ.ErlangType erlang_type) -> erlang_type | _ -> ErlangTypeName.Any


let write_dynamic_type_and_return (addr_val, hist) typ ret_id location astate =
  let typ = Typ.mk_struct (ErlangType typ) in
  let astate = PulseArithmetic.and_dynamic_type_is_unsafe addr_val typ location astate in
  PulseOperations.write_id ret_id (addr_val, hist) astate


(** A simple helper that wraps destination-passing-style evaluation functions that also return a
    handler to their result into a function that allocates the destination under the hood and simply
    return that handler.

    This allows to transform this (somehow recurring) pattern:

    [let dest = AbstractValue.mk_fresh () in let (astate, dest) = eval dest arg1 ... argN in ....]

    into the simpler: [let (astate, dest) = eval_into_fresh eval arg1 ... argN in ...] *)
let eval_into_fresh eval =
  let symbol = AbstractValue.mk_fresh () in
  eval symbol


(** Use for chaining functions of the type ('a->('b,'err) result list). The idea of such functions
    is that they can both fan-out into a (possibly empty) disjunction *and* signal errors. For
    example, consider [f] of type ['a->('b,'err) result list] and [g] of type
    ['b->('c,'err) result list] and [a] is some value of type ['a]. Note that the type of error is
    the same, so they can be propagated forward. To chain the application of these functions, you
    can write [let> x=f a in let> y=g x in [Ok y]].

    In several places, we have to compose with functions of the type ['a->('b,'err) result], which
    don't produce a list. One way to handle this is to wrap those functions in a list. For example,
    if [f] and [a] have the same type as before but [g] has type ['b->('c,'err) result], then we can
    write [let> =f a in let> y=[g x] in [Ok y].] *)
let ( let> ) x f =
  List.concat_map
    ~f:(function
      | FatalError _ as error ->
          [error]
      | Ok ok ->
          f ok
      | Recoverable (ok, errors) ->
          f ok |> List.map ~f:(fun result -> PulseResult.append_errors errors result) )
    x


let result_fold list ~init ~f =
  let init = [Ok init] in
  let f result x =
    let> ok = result in
    f ok x
  in
  List.fold list ~init ~f


let result_foldi list ~init ~f =
  let init = [Ok init] in
  let f index result x =
    let> ok = result in
    f index ok x
  in
  List.foldi list ~init ~f


let value_die message opt =
  match opt with Some value -> value | None -> L.die InternalError message


(** Builds as an abstract value the truth value of the predicate "The value given as an argument as
    the erlang type given as the other argument" *)
let has_erlang_type value typ : value_maker =
 fun astate ->
  let instanceof_val = AbstractValue.mk_fresh () in
  let sil_type = Typ.mk_struct (ErlangType typ) in
  let++ astate = PulseArithmetic.and_equal_instanceof instanceof_val value sil_type astate in
  (astate, instanceof_val)


let prune_type path location (value, hist) typ astate : AbductiveDomain.t result =
  (let open SatUnsat.Import in
   let* astate =
     (* If check_addr_access fails, we stop exploring this path by marking it [Unsat] *)
     PulseOperations.check_addr_access path Read location (value, hist) astate
     |> PulseResult.ok |> SatUnsat.of_option
   in
   let** astate, instanceof_val = has_erlang_type value typ astate in
   PulseArithmetic.prune_positive instanceof_val astate )
  |> SatUnsat.to_list


(** Loads a field from a struct, assuming that it has the correct type (should be checked by
    [prune_type]). *)
let load_field path field location obj astate =
  match PulseModelsJava.load_field path field location obj astate with
  | Recoverable _ | FatalError _ ->
      L.die InternalError "@[<v>@[%s@]@;@[%a@]@;@]"
        "Could not load field. Did you call this function without calling prune_type?"
        AbductiveDomain.pp astate
  | Ok result ->
      result


let get_module_attribute tenv ~tag =
  let open IOption.Let_syntax in
  let typ = Typ.ErlangType ModuleInfo in
  let* field_info =
    Tenv.resolve_field_info tenv typ (Fieldname.make typ ErlangTypeName.module_info_field_name)
  in
  let* annot =
    List.find field_info.Struct.annotations ~f:(function ({class_name} : Annot.t) ->
        String.equal class_name ErlangTypeName.module_info_attributes_class_name )
  in
  match Annot.find_parameter annot ~name:tag with
  | Some (Annot.Str module_name) ->
      Some module_name
  | _ ->
      None


module type ERRORS = sig
  val badarg : model_no_non_disj

  val badgenerator : model_no_non_disj

  val badkey : model_no_non_disj

  val badmap : model_no_non_disj

  val badmatch : model_no_non_disj

  val badrecord : model_no_non_disj

  val badreturn : model_no_non_disj

  val case_clause : model_no_non_disj

  val else_clause : model_no_non_disj

  val function_clause : model_no_non_disj

  val if_clause : model_no_non_disj

  val try_clause : model_no_non_disj
end

module ErrorsReport : ERRORS = struct
  let error err astate = [FatalError (ReportableError {astate; diagnostic= ErlangError err}, [])]

  let badarg : model_no_non_disj =
   fun {location} astate -> error (Badarg {calling_context= []; location}) astate


  let badgenerator : model_no_non_disj =
   fun {location} astate -> error (Badgenerator {calling_context= []; location}) astate


  let badkey : model_no_non_disj =
   fun {location} astate -> error (Badkey {calling_context= []; location}) astate


  let badmap : model_no_non_disj =
   fun {location} astate -> error (Badmap {calling_context= []; location}) astate


  let badmatch : model_no_non_disj =
   fun {location} astate -> error (Badmatch {calling_context= []; location}) astate


  let badrecord : model_no_non_disj =
   fun {location} astate -> error (Badrecord {calling_context= []; location}) astate


  let badreturn : model_no_non_disj =
   fun {location} astate -> error (Badreturn {calling_context= []; location}) astate


  let case_clause : model_no_non_disj =
   fun {location} astate -> error (Case_clause {calling_context= []; location}) astate


  let else_clause : model_no_non_disj =
   fun {location} astate -> error (Else_clause {calling_context= []; location}) astate


  let function_clause : model_no_non_disj =
   fun {location} astate -> error (Function_clause {calling_context= []; location}) astate


  let if_clause : model_no_non_disj =
   fun {location} astate -> error (If_clause {calling_context= []; location}) astate


  let try_clause : model_no_non_disj =
   fun {location} astate -> error (Try_clause {calling_context= []; location}) astate
end

module ErrorsSilent : ERRORS = struct
  let stuck : model_no_non_disj = fun _data _astate -> []

  let badarg = stuck

  let badgenerator = stuck

  let badkey = stuck

  let badmap = stuck

  let badmatch = stuck

  let badrecord = stuck

  let badreturn = stuck

  let case_clause = stuck

  let else_clause = stuck

  let function_clause = stuck

  let if_clause = stuck

  let try_clause = stuck
end

module Errors : ERRORS =
  (val if Config.erlang_reliability then (module ErrorsReport) else (module ErrorsSilent) : ERRORS)

module Atoms = struct
  let name_field = Fieldname.make (ErlangType Atom) ErlangTypeName.atom_name

  let hash_field = Fieldname.make (ErlangType Atom) ErlangTypeName.atom_hash

  let get_name path location var astate : string option =
    let _astate, _addr, (name, _) = load_field path name_field location var astate in
    PulseArithmetic.as_constant_string astate name


  let make_raw location path name hash : sat_maker =
   fun astate ->
    let hist = Hist.single_alloc path location "atom" in
    let addr_atom = (AbstractValue.mk_fresh (), hist) in
    let* astate =
      write_field_and_deref path location ~struct_addr:addr_atom
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:name name_field astate
    in
    let+ astate =
      write_field_and_deref path location ~struct_addr:addr_atom
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:hash hash_field astate
    in
    ( PulseArithmetic.and_dynamic_type_is_unsafe (fst addr_atom) (Typ.mk_struct (ErlangType Atom))
        location astate
    , addr_atom )


  let make name hash : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let<+> astate, ret = make_raw location path name hash astate in
    PulseOperations.write_id ret_id ret astate


  let of_string location path (name : string) : maker =
   fun astate ->
    (* Note: This should correspond to [ErlangTranslator.mk_atom_call]. *)
    let** astate, hash =
      let hash_exp : Exp.t = Const (Cint (IntLit.of_int (ErlangTypeName.calculate_hash name))) in
      PulseOperations.eval path Read location hash_exp astate
    in
    let+* astate, name =
      let name_exp : Exp.t = Const (Cstr name) in
      PulseOperations.eval path Read location name_exp astate
    in
    make_raw location path name hash astate


  (* Converts [bool_value] into true/false, and write it to [addr_atom]. *)
  let of_bool path location bool_value astate =
    let astate_true =
      let** astate = PulseArithmetic.prune_positive bool_value astate in
      of_string location path ErlangTypeName.atom_true astate
    in
    let astate_false :
        (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t SatUnsat.t =
      let** astate = PulseArithmetic.prune_eq_zero bool_value astate in
      of_string location path ErlangTypeName.atom_false astate
    in
    let> astate, (addr, hist) = SatUnsat.to_list astate_true @ SatUnsat.to_list astate_false in
    let typ = Typ.mk_struct (ErlangType Atom) in
    let astate = PulseArithmetic.and_dynamic_type_is_unsafe addr typ location astate in
    [Ok (astate, (addr, hist))]


  (** Takes a boolean value, converts it to true/false atom and writes to return value. *)
  let write_return_from_bool path location bool_value ret_id astate =
    let> astate, ret_val = of_bool path location bool_value astate in
    PulseOperations.write_id ret_id ret_val astate |> Basic.ok_continue
end

module Integers = struct
  let value_field = Fieldname.make (ErlangType Integer) ErlangTypeName.integer_value

  let typ = Typ.mk_struct (ErlangType Integer)

  let make_raw location path value : sat_maker =
   fun astate ->
    let hist = Hist.single_alloc path location "integer" in
    let addr = (AbstractValue.mk_fresh (), hist) in
    let+ astate =
      write_field_and_deref path location ~struct_addr:addr
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:value value_field astate
    in
    let astate = PulseArithmetic.and_dynamic_type_is_unsafe (fst addr) typ location astate in
    (astate, addr)


  let make value : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let<+> astate, ret = make_raw location path value astate in
    PulseOperations.write_id ret_id ret astate


  let of_intlit location path (intlit : IntLit.t) : sat_maker =
   fun astate ->
    let* astate, name =
      let intlit_exp : Exp.t = Const (Cint intlit) in
      PulseOperations.eval path Read location intlit_exp astate
      |> SatUnsat.sat
      |> value_die "intlit failed evaluation"
    in
    make_raw location path name astate


  let of_string location path (intlit : string) : sat_maker =
    of_intlit location path (IntLit.of_string intlit)
end

module Comparison = struct
  module Comparator = struct
    (** Records that define how to compare values according to their types.

        These records define a few functions that correspond to a comparison on values that have a
        specific type. For instance, the [integer] function can assume that both compared values are
        indeed of the integer type, and that function will be called by the global comparison
        function on these cases.

        Comparators must also define an [unsupported] function, that the dispatching function will
        call on types that it does not support or cannot determine, and an [incompatible] function
        that will be called when both compared values are known to be of a different type. *)

    (** {1 Helper functions} *)

    (** These functions are provided as helpers to define the comparator functions. *)

    (** Compares two objects by comparing one specific field. No type checking is made and the user
        should take care that the field is indeed a valid one for both arguments. *)
    let from_fields sil_op field x y location path : value_maker =
     fun astate ->
      let astate, _addr, (x_field, _) = load_field path field location x astate in
      let astate, _addr, (y_field, _) = load_field path field location y astate in
      eval_into_fresh PulseArithmetic.eval_binop_absval sil_op x_field y_field astate


    (** A trivial comparison that is always false. Can be used eg. for equality on incompatible
        types. *)
    let const_false _x _y _location _path : value_maker =
     fun astate ->
      let const_false = AbstractValue.mk_fresh () in
      let++ astate = PulseArithmetic.prune_eq_zero const_false astate in
      (astate, const_false)


    (** A trivial comparison that is always true. Can be used eg. for inequality on incompatible
        types. *)
    let const_true _x _y _location _path : value_maker =
     fun astate ->
      let const_true = AbstractValue.mk_fresh () in
      let++ astate = PulseArithmetic.prune_positive const_true astate in
      (astate, const_true)


    (** Returns an unconstrained value. Can be used eg. for overapproximation or for unsupported
        comparisons.

        Note that, as an over-approximation, it can lead to some false positives, that we generally
        try to avoid. However, the only solution for comparisons that we do not support (such as
        ordering on atoms) would then be to consider the result as unreachable (that is, return an
        empty abstract state). This would lead to code depending on such comparisons not being
        analysed at all, which may be less desirable than analysing it without gaining information
        from the comparison itself. *)
    let unknown _x _y _location _path : value_maker =
     fun astate ->
      let result = AbstractValue.mk_fresh () in
      Sat (Ok (astate, result))


    (** Takes as parameters the types of two values [x] and [y] known to be incompatible and returns
        a comparator for [x < y] based on this type. Currently, this only supports comparisons with
        at least one integer: namely, when [x] is known to be an integer, then the comparison is
        always true, and when [y] is, the comparison is always false. Otherwise it is unknown.

        Reference: {:https://www.erlang.org/doc/reference_manual/expressions.html#term-comparisons}. *)
    let incompatible_lt ty_x ty_y x y location path : value_maker =
     fun astate ->
      match (ty_x, ty_y) with
      | ErlangTypeName.Integer, _ ->
          const_true x y location path astate
      | _, ErlangTypeName.Integer ->
          const_false x y location path astate
      | _ ->
          unknown x y location path astate


    (** Takes as parameters the types of two values [x] and [y] known to be incompatible and returns
        a comparator for [x > y] based on this type.

        See also {!incompatible_lt}. *)
    let incompatible_gt ty_x ty_y x y location path : value_maker =
     fun astate ->
      match (ty_x, ty_y) with
      | ErlangTypeName.Integer, _ ->
          const_false x y location path astate
      | _, ErlangTypeName.Integer ->
          const_true x y location path astate
      | _ ->
          unknown x y location path astate


    (** Adapt {!const_false} to have the expected type for the equality of incompatible values (cf
        {!type:t}, {!recfield:t.incompatible}). *)
    let incompatible_eq _ty_x _ty_y = const_false

    (** Cf. {!incompatible_eq} *)
    let incompatible_exactly_not_eq _ty_x _ty_y = const_true

    (** {1 Comparators as records of functions} *)

    (** The type of the functions that compare two values based on a specific type combination. They
        take the two values as parameters and build the abstract result that holds the comparison
        value. *)
    type monotyped_comparison =
         AbstractValue.t * ValueHistory.t
      -> AbstractValue.t * ValueHistory.t
      -> Location.t
      -> PathContext.t
      -> value_maker

    type t =
      { unsupported: monotyped_comparison
      ; incompatible: ErlangTypeName.t -> ErlangTypeName.t -> monotyped_comparison
            (** [incompatible] takes as first parameters the types of the values being compared in
                order to implement type-based ordering *)
      ; integer: monotyped_comparison
      ; atom: monotyped_comparison }

    let eq =
      { unsupported= unknown
      ; incompatible= incompatible_eq
      ; integer= from_fields Binop.Eq Integers.value_field
      ; atom= from_fields Binop.Eq Atoms.hash_field }


    let xne =
      (* exactly_not_equal. *)
      { unsupported= unknown
      ; incompatible= incompatible_exactly_not_eq
      ; integer= from_fields Binop.Ne Integers.value_field
      ; atom= from_fields Binop.Ne Atoms.hash_field }


    (** Makes an ordering comparator given an operator to be used for comparing integer values and a
        function to compare incompatible values. *)
    let ordering int_binop incompatible =
      { unsupported= unknown
      ; incompatible
      ; integer= from_fields int_binop Integers.value_field
      ; atom= unknown }


    let gt = ordering Gt incompatible_gt

    let ge = ordering Ge incompatible_gt

    let lt = ordering Lt incompatible_lt

    let le = ordering Le incompatible_lt
  end

  (** Makes an abstract value holding the comparison result of two parameters. We perform a case
      analysis of the dynamic type of these parameters.

      See the documentation of {!Comparator} values for the meaning of the [cmp] parameter. It will
      be given as an argument by specific comparisons functions and should define a few functions
      that return a result for comparisons on specific types.

      Note that we here say that two values are "incompatible" if they have separate types. That
      does not mean that the comparison is invalid, as in erlang all comparisons are properly
      defined even on differently-typed values:
      {:https://www.erlang.org/doc/reference_manual/expressions.html#term-comparisons}.

      We say that two values have an "unsupported" type if they both share a type on which we don't
      do any precise comparison. Not that two values of *distinct* unsupported types are still
      incompatible, and therefore might be precisely compared.

      Current supported types are integers and atoms.

      The final result is computed as follows:

      - If the parameters are both of a supported type, eg. integers, then we compare them
        accordingly (eg. the [cmp.integer] function will then typically compare their value fields).
      - If the parameters have incompatible types, then we return the result of a comparison of
        incompatible types (eg. equality would be false, and inequality would be true).
      - If both parameters have the same unsupported type, then the comparison is unsupported and we
        use the [cmp.unsupported] function (that could for instance return an - overapproximating -
        unconstrained result).
      - If one parameter has a supported type, say [Integer] and the other has no known dynamic type
        (or, equivalently, its type is [Any]), then we will assume that the unknown-typed value is
        an [Integer] and use the appropriate comparison. Note: this is strictly under-approximating
        in some cases, but seems to be the better alternative (for instance, case-splitting on the
        [Any] type yields too many disjunctions).
      - If both parameters have no known dynamic type (or, equivalently, their type is [Any]), or
        one has an unsupported type and the other one has [Any] type, then the comparison is also
        unsupported.

      Note that, on supported types (eg. integers), it is important that the [cmp] functions decide
      themselves if they should compare some specific fields or not, instead of getting these fields
      in the global function and have the methods work on the field values. This is because, when we
      extend this code to work on other more complex types, which field is used or not may depend on
      the actual comparison operator that we're computing. For instance the equality of atoms can be
      decided on their hash, but their relative ordering should check their names as
      lexicographically ordered strings. *)
  let make_raw (cmp : Comparator.t) location path ((x_val, _) as x) ((y_val, _) as y) :
      disjunction_maker =
   fun astate ->
    let x_typ = get_erlang_type_or_any x_val astate in
    let y_typ = get_erlang_type_or_any y_val astate in
    match (x_typ, y_typ) with
    (* When supporting more types, this should probably be refactored to avoid N² cases. *)
    | Integer, Integer ->
        let<**> astate, result = cmp.integer x y location path astate in
        let hist = Hist.single_call path location "integer_comparison" in
        [Ok (astate, (result, hist))]
    | Atom, Atom ->
        let<**> astate, result = cmp.atom x y location path astate in
        let hist = Hist.single_call path location "atom_comparison" in
        [Ok (astate, (result, hist))]
    | Integer, Any ->
        let> astate = prune_type path location y Integer astate in
        let<**> astate, result = cmp.integer x y location path astate in
        let hist = Hist.single_call path location "integer_any_comparison" in
        [Ok (astate, (result, hist))]
    | Any, Integer ->
        let> astate = prune_type path location x Integer astate in
        let<**> astate, result = cmp.integer x y location path astate in
        let hist = Hist.single_call path location "any_integer_comparison" in
        [Ok (astate, (result, hist))]
    | Atom, Any ->
        let> astate = prune_type path location y Atom astate in
        let<**> astate, result = cmp.atom x y location path astate in
        let hist = Hist.single_call path location "atom_any_comparison" in
        [Ok (astate, (result, hist))]
    | Any, Atom ->
        let> astate = prune_type path location x Atom astate in
        let<**> astate, result = cmp.atom x y location path astate in
        let hist = Hist.single_call path location "any_atom_comparison" in
        [Ok (astate, (result, hist))]
    | Nil, Nil | Cons, Cons | Tuple _, Tuple _ | Map, Map | Any, _ | _, Any ->
        let<**> astate, result = cmp.unsupported x y location path astate in
        let hist = Hist.single_alloc path location "unsupported_comparison" in
        [Ok (astate, (result, hist))]
    | _ ->
        let<**> astate, result = cmp.incompatible x_typ y_typ x y location path astate in
        let hist = Hist.single_alloc path location "incompatible_comparison" in
        [Ok (astate, (result, hist))]


  (** A model of comparison operators where we store in the destination the result of comparing two
      parameters. *)
  let make cmp x y : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let> astate, (result, _hist) = make_raw cmp location path x y astate in
    Atoms.write_return_from_bool path location result ret_id astate


  (** Returns an abstract state that has been pruned on the comparison result being true. *)
  let prune cmp location path x y astate : AbductiveDomain.t AccessResult.t list =
    let> astate, (comparison, _hist) = make_raw cmp location path x y astate in
    PulseArithmetic.prune_positive comparison astate |> SatUnsat.to_list


  (** {1 Specific comparison operators} *)

  let equal = make Comparator.eq

  let prune_equal = prune Comparator.eq

  let exactly_not_equal = make Comparator.xne

  let greater = make Comparator.gt

  let greater_or_equal = make Comparator.ge

  let lesser = make Comparator.lt

  let lesser_or_equal = make Comparator.le
end

module Lists = struct
  let head_field = Fieldname.make (ErlangType Cons) ErlangTypeName.cons_head

  let tail_field = Fieldname.make (ErlangType Cons) ErlangTypeName.cons_tail

  (** Helper function to create a Nil structure without assigning it to return value *)
  let make_nil_raw location path : sat_maker =
   fun astate ->
    let event = Hist.alloc_event path location "[]" in
    let addr_nil_val = AbstractValue.mk_fresh () in
    let addr_nil = (addr_nil_val, Hist.single_event path event) in
    let astate =
      PulseArithmetic.and_dynamic_type_is_unsafe addr_nil_val (Typ.mk_struct (ErlangType Nil))
        location astate
    in
    Ok (astate, addr_nil)


  (** Create a Nil structure and assign it to return value *)
  let make_nil : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let<+> astate, addr_nil = make_nil_raw location path astate in
    PulseOperations.write_id ret_id addr_nil astate


  (** Helper function to create a Cons structure without assigning it to return value *)
  let make_cons_raw path location hd tl : sat_maker =
   fun astate ->
    let hist = Hist.single_alloc path location "[X|Xs]" in
    let addr_cons_val = AbstractValue.mk_fresh () in
    let addr_cons = (addr_cons_val, hist) in
    let* astate =
      write_field_and_deref path location ~struct_addr:addr_cons
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:hd head_field astate
    in
    let+ astate =
      write_field_and_deref path location ~struct_addr:addr_cons
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:tl tail_field astate
    in
    let astate =
      PulseArithmetic.and_dynamic_type_is_unsafe addr_cons_val (Typ.mk_struct (ErlangType Cons))
        location astate
    in
    (astate, addr_cons)


  (* helper funtion to add a set of field to a cons cell. The fields are passed as a non-empty list of pairs (field name, filed value) *)
  let rec add_field_to_cons path location addr_cons hist fld_ls : sat_maker =
   fun astate ->
    match fld_ls with
    | [] ->
        L.die InternalError "[ERROR] function add_field_to_state cannot add an empty field."
    | [(fname, fval)] ->
        let field = Fieldname.make (ErlangType Cons) fname in
        let+ astate' =
          write_field_and_deref path location ~struct_addr:addr_cons
            ~field_addr:(AbstractValue.mk_fresh (), hist)
            ~field_val:fval field astate
        in
        let astate' =
          PulseArithmetic.and_dynamic_type_is_unsafe (fst addr_cons)
            (Typ.mk_struct (ErlangType Cons)) location astate'
        in
        (astate', addr_cons)
    | (fname, fval) :: fld_ls' ->
        let field = Fieldname.make (ErlangType Cons) fname in
        let* astate' =
          write_field_and_deref path location ~struct_addr:addr_cons
            ~field_addr:(AbstractValue.mk_fresh (), hist)
            ~field_val:fval field astate
        in
        add_field_to_cons path location addr_cons hist fld_ls' astate'


  (* add a set of field to a cons cell. The set of field are passed in a list *)
  let make_cons_raw_general path location fld_ls : sat_maker =
   fun astate ->
    let hist = Hist.single_alloc path location "[X|Xs]" in
    let addr_cons_val = AbstractValue.mk_fresh () in
    let addr_cons = (addr_cons_val, hist) in
    add_field_to_cons path location addr_cons hist fld_ls astate


  (** Create a Cons structure and assign it to return value *)
  let make_cons head tail : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let<+> astate, addr_cons = make_cons_raw path location head tail astate in
    PulseOperations.write_id ret_id addr_cons astate


  (** Assumes that the argument is a Cons and loads the head and tail *)
  let load_head_tail cons astate path location =
    let> astate = prune_type path location cons Cons astate in
    let astate, _, head = load_field path head_field location cons astate in
    let astate, _, tail = load_field path tail_field location cons astate in
    [Ok (head, tail, astate)]


  (** Assumes that a list is of given length and reads the elements *)
  let rec assume_and_deconstruct list length astate path location =
    match length with
    | 0 ->
        let> astate = prune_type path location list Nil astate in
        [Ok ([], astate)]
    | _ ->
        let> hd, tl, astate = load_head_tail list astate path location in
        let> elems, astate = assume_and_deconstruct tl (length - 1) astate path location in
        [Ok (hd :: elems, astate)]


  let make_astate_badarg (list_val, _list_hist) data astate =
    (* arg is not a list if its type is neither Cons nor Nil *)
    let typ_cons = Typ.mk_struct (ErlangType Cons) in
    let typ_nil = Typ.mk_struct (ErlangType Nil) in
    let instanceof_val_cons = AbstractValue.mk_fresh () in
    let instanceof_val_nil = AbstractValue.mk_fresh () in
    let<**> astate =
      PulseArithmetic.and_equal_instanceof instanceof_val_cons list_val typ_cons astate
    in
    let<**> astate =
      PulseArithmetic.and_equal_instanceof instanceof_val_nil list_val typ_nil astate
    in
    let<**> astate = PulseArithmetic.prune_eq_zero instanceof_val_cons astate in
    let<**> astate = PulseArithmetic.prune_eq_zero instanceof_val_nil astate in
    Errors.badarg data astate


  let append2 ~reverse list1 list2 : model_no_non_disj =
   fun ({location; path; ret= ret_id, _} as data) astate ->
    let mk_astate_badarg = make_astate_badarg list1 data astate in
    (* Makes an abstract state corresponding to appending to a list of given length *)
    let mk_good_astate_concat length =
      let> elems, astate = assume_and_deconstruct list1 length astate path location in
      let elems = if reverse then elems else List.rev elems in
      let> astate, result_list =
        [ PulseResult.list_fold ~init:(astate, list2)
            ~f:(fun (astate, tl) hd -> make_cons_raw path location hd tl astate)
            elems ]
      in
      [Ok (PulseOperations.write_id ret_id result_list astate)]
    in
    mk_astate_badarg
    @ ( List.concat (List.init Config.erlang_list_unfold_depth ~f:mk_good_astate_concat)
      |> List.map ~f:Basic.map_continue )


  let reverse list : model_no_non_disj =
   fun ({location; path; _} as data) astate ->
    let<*> astate, nil = make_nil_raw location path astate in
    append2 ~reverse:true list nil data astate


  let rec make_raw location path elements : sat_maker =
   fun astate ->
    match elements with
    | [] ->
        make_nil_raw location path astate
    | head :: tail ->
        let* astate, tail_val = make_raw location path tail astate in
        make_cons_raw path location head tail_val astate


  (** Approximation: we don't actually do the side-effect, just assume the return value. *)
  let foreach _fun _list : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let<**> astate, ret = Atoms.of_string location path "ok" astate in
    PulseOperations.write_id ret_id ret astate |> Basic.ok_continue
end

module Tuples = struct
  let typ size = Typ.ErlangType (Tuple size)

  let field_name size index =
    if not (1 <= index && index <= size) then L.die InternalError "Erlang tuples are 1-indexed"
    else Fieldname.make (typ size) (ErlangTypeName.tuple_elem index)


  (** Helper: Like [Tuples.make] but with a more precise/composable type. *)
  let make_raw (location : Location.t) (path : PathContext.t)
      (args : (AbstractValue.t * ValueHistory.t) list) : sat_maker =
   fun astate ->
    let tuple_size = List.length args in
    let tuple_typ_name : Typ.name = ErlangType (Tuple tuple_size) in
    let hist = Hist.single_alloc path location "{}" in
    let addr_tuple = (AbstractValue.mk_fresh (), hist) in
    let addr_elems = List.map ~f:(function _ -> (AbstractValue.mk_fresh (), hist)) args in
    let mk_field = Fieldname.make tuple_typ_name in
    let field_names = ErlangTypeName.tuple_field_names tuple_size in
    let addr_elems_fields_payloads = List.zip_exn addr_elems (List.zip_exn field_names args) in
    let write_tuple_field astate (addr_elem, (field_name, payload)) =
      write_field_and_deref path location ~struct_addr:addr_tuple ~field_addr:addr_elem
        ~field_val:payload (mk_field field_name) astate
    in
    let+ astate =
      PulseResult.list_fold addr_elems_fields_payloads ~init:astate ~f:write_tuple_field
    in
    let astate =
      PulseArithmetic.and_dynamic_type_is_unsafe (fst addr_tuple) (Typ.mk_struct tuple_typ_name)
        location astate
    in
    (astate, addr_tuple)


  let make (args : 'a ProcnameDispatcher.Call.FuncArg.t list) : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let get_payload (arg : 'a ProcnameDispatcher.Call.FuncArg.t) = arg.arg_payload in
    let arg_payloads = List.map ~f:get_payload args in
    let<+> astate, ret = make_raw location path arg_payloads astate in
    PulseOperations.write_id ret_id ret astate
end

(** Maps are currently approximated to store only the latest key/value *)
module Maps = struct
  let mk_field = Fieldname.make (ErlangType Map)

  let key_field = mk_field "__infer_model_backing_map_key"

  let value_field = mk_field "__infer_model_backing_map_value"

  let is_empty_field = mk_field "__infer_model_backing_map_is_empty"

  let make (args : 'a ProcnameDispatcher.Call.FuncArg.t list) : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let hist = Hist.single_alloc path location "#{}" in
    let addr_map = (AbstractValue.mk_fresh (), hist) in
    let addr_is_empty = (AbstractValue.mk_fresh (), hist) in
    let is_empty_value = AbstractValue.mk_fresh () in
    let fresh_val = (is_empty_value, hist) in
    let is_empty_lit = match args with [] -> IntLit.one | _ -> IntLit.zero in
    (* Reverse the list so we can get last key/value *)
    let<*> astate =
      match List.rev args with
      (* Non-empty map: we just consider the last key/value, rest is ignored (approximation) *)
      | value_arg :: key_arg :: _ ->
          let addr_key = (AbstractValue.mk_fresh (), hist) in
          let addr_value = (AbstractValue.mk_fresh (), hist) in
          write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_key
            ~field_val:key_arg.arg_payload key_field astate
          >>= write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_value
                ~field_val:value_arg.arg_payload value_field
      | _ :: _ ->
          L.die InternalError "Map create got one argument (requires even number)"
      (* Empty map *)
      | [] ->
          Ok astate
    in
    let<*> astate =
      write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_is_empty
        ~field_val:fresh_val is_empty_field astate
    in
    let<++> astate = PulseArithmetic.and_eq_int is_empty_value is_empty_lit astate in
    write_dynamic_type_and_return addr_map Map ret_id location astate


  let new_ : model_no_non_disj = make []

  let make_astate_badmap (map_val, _map_hist) data astate =
    let typ = Typ.mk_struct (ErlangType Map) in
    let instanceof_val = AbstractValue.mk_fresh () in
    let<**> astate = PulseArithmetic.and_equal_instanceof instanceof_val map_val typ astate in
    let<**> astate = PulseArithmetic.prune_eq_zero instanceof_val astate in
    Errors.badmap data astate


  let make_astate_goodmap path location map astate = prune_type path location map Map astate

  let is_key key map : model_no_non_disj =
   fun ({location; path; ret= ret_id, _} as data) astate ->
    (* Return 3 cases:
     * - Error & assume not map
     * - Ok & assume map & assume empty & return false
     * - Ok & assume map & assume not empty & assume key is the tracked key & return true
     *)
    let astate_badmap = make_astate_badmap map data astate in
    let astate_empty =
      let ret_val_false = AbstractValue.mk_fresh () in
      let> astate = make_astate_goodmap path location map astate in
      let astate, _isempty_addr, (is_empty, _isempty_hist) =
        load_field path is_empty_field location map astate
      in
      let> astate = PulseArithmetic.prune_positive is_empty astate |> SatUnsat.to_list in
      let> astate =
        PulseArithmetic.and_eq_int ret_val_false IntLit.zero astate |> SatUnsat.to_list
      in
      Atoms.write_return_from_bool path location ret_val_false ret_id astate
    in
    let astate_haskey =
      let ret_val_true = AbstractValue.mk_fresh () in
      let> astate = make_astate_goodmap path location map astate in
      let astate, _isempty_addr, (is_empty, _isempty_hist) =
        load_field path is_empty_field location map astate
      in
      let> astate = PulseArithmetic.prune_eq_zero is_empty astate |> SatUnsat.to_list in
      let astate, _key_addr, tracked_key = load_field path key_field location map astate in
      let> astate = Comparison.prune_equal location path key tracked_key astate in
      let> astate = PulseArithmetic.and_eq_int ret_val_true IntLit.one astate |> SatUnsat.to_list in
      Atoms.write_return_from_bool path location ret_val_true ret_id astate
    in
    astate_empty @ astate_haskey @ astate_badmap


  let get (key, key_history) map : model_no_non_disj =
   fun ({location; path; ret= ret_id, _} as data) astate ->
    (* Return 3 cases:
     * - Error & assume not map
     * - Error & assume map & assume empty;
     * - Ok & assume map & assume nonempty & assume key is the tracked key & return tracked value
     *)
    let astate_badmap = make_astate_badmap map data astate in
    let astate_ok =
      let> astate = make_astate_goodmap path location map astate in
      let astate, _isempty_addr, (is_empty, _isempty_hist) =
        load_field path is_empty_field location map astate
      in
      let> astate = PulseArithmetic.prune_eq_zero is_empty astate |> SatUnsat.to_list in
      let astate, _key_addr, tracked_key = load_field path key_field location map astate in
      let> astate = Comparison.prune_equal location path (key, key_history) tracked_key astate in
      let astate, _value_addr, tracked_value = load_field path value_field location map astate in
      [Ok (PulseOperations.write_id ret_id tracked_value astate)]
    in
    let astate_badkey =
      let> astate = make_astate_goodmap path location map astate in
      let astate, _isempty_addr, (is_empty, _isempty_hist) =
        load_field path is_empty_field location map astate
      in
      let> astate = PulseArithmetic.prune_positive is_empty astate |> SatUnsat.to_list in
      Errors.badkey data astate
    in
    List.map ~f:Basic.map_continue astate_ok @ astate_badkey @ astate_badmap


  let put key value map : model_no_non_disj =
   fun ({location; path; ret= ret_id, _} as data) astate ->
    (* Ignore old map. We only store one key/value so we can simply create a new map. *)
    (* Return 2 cases:
     * - Error & assume not map
     * - Ok & assume map & return new map
     *)
    let hist = Hist.single_alloc path location "maps_put" in
    let astate_badmap = make_astate_badmap map data astate in
    let astate_ok =
      let addr_map = (AbstractValue.mk_fresh (), hist) in
      let addr_is_empty = (AbstractValue.mk_fresh (), hist) in
      let is_empty_value = AbstractValue.mk_fresh () in
      let fresh_val = (is_empty_value, hist) in
      let addr_key = (AbstractValue.mk_fresh (), hist) in
      let addr_value = (AbstractValue.mk_fresh (), hist) in
      let> astate = make_astate_goodmap path location map astate in
      let> astate =
        [ write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_key
            ~field_val:key key_field astate ]
      in
      let> astate =
        [ write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_value
            ~field_val:value value_field astate ]
      in
      let> astate =
        write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_is_empty
          ~field_val:fresh_val is_empty_field astate
        >>>= PulseArithmetic.and_eq_int is_empty_value IntLit.zero
        |> SatUnsat.to_list
      in
      [Ok (write_dynamic_type_and_return addr_map Map ret_id location astate)]
    in
    List.map ~f:Basic.map_continue astate_ok @ astate_badmap


  let to_list ?(check_badmap = true) map : model_no_non_disj =
   fun ({location; path; ret= ret_id, _} as data) astate ->
    let astate_badmap = if check_badmap then make_astate_badmap map data astate else [] in
    let astate_empty =
      let> astate = make_astate_goodmap path location map astate in
      let astate, _isempty_addr, (is_empty, _isempty_hist) =
        load_field path is_empty_field location map astate
      in
      let> astate = PulseArithmetic.prune_positive is_empty astate |> SatUnsat.to_list in
      let<+> astate, addr_nil = Lists.make_nil_raw location path astate in
      PulseOperations.write_id ret_id addr_nil astate
    in
    let astate_nonempty =
      let> astate = make_astate_goodmap path location map astate in
      let astate, _isempty_addr, (is_empty, _isempty_hist) =
        load_field path is_empty_field location map astate
      in
      let> astate = PulseArithmetic.prune_eq_zero is_empty astate |> SatUnsat.to_list in
      let astate, _key_addr, key = load_field path key_field location map astate in
      let astate, _value_addr, value = load_field path value_field location map astate in
      let<*> astate, addr_tuple = Tuples.make_raw location path [key; value] astate in
      let<*> astate, addr_nil = Lists.make_nil_raw location path astate in
      let<+> astate, addr_cons = Lists.make_cons_raw path location addr_tuple addr_nil astate in
      PulseOperations.write_id ret_id addr_cons astate
    in
    astate_nonempty @ astate_empty @ astate_badmap
end

module Strings = struct
  (** This is a temporary solution for strings to avoid false positives. For now, we consider that
      the type of strings is list and compute this information whenever a string is created. Strings
      should be fully supported in future. T93361792 *)

  let of_const_string location path (const_str : String.t) : sat_maker =
   fun astate ->
    let const_str_exp : Exp.t = Const (Cstr const_str) in
    PulseOperations.eval path Read location const_str_exp astate
    |> SatUnsat.sat
    |> value_die "'of_const_string' failed evaluation"


  (** recurse character by character of the string and build suitable heap allocated data structure *)
  let rec handle_string_content location path value str_lst : sat_maker =
   fun astate ->
    match str_lst with
    | [] ->
        Lists.make_nil_raw location path astate
    | c :: str_lst' ->
        let ascii_code = IntLit.of_int (Char.to_int c) in
        let* astate, tail = handle_string_content location path value str_lst' astate in
        let* astate, head = Integers.of_intlit location path ascii_code astate in
        let* astate, str_val = of_const_string location path (String.of_char_list str_lst) astate in
        let fld_ls = [("head", head); ("tail", tail); ("strval", str_val)] in
        Lists.make_cons_raw_general path location fld_ls astate


  let make_raw location path (value, _) : sat_maker =
   fun astate ->
    let string_value =
      PulseArithmetic.as_constant_string astate value |> value_die "expected string value attribute"
    in
    let ls_str = String.to_list string_value in
    handle_string_content location path value ls_str astate


  let make value : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let> astate, (result, _hist) = [make_raw location path value astate] in
    PulseOperations.write_id ret_id (result, _hist) astate |> Basic.ok_continue
end

module BIF = struct
  let has_type (value, _hist) type_ : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let typ = Typ.mk_struct (ErlangType type_) in
    let is_typ = AbstractValue.mk_fresh () in
    let<**> astate = PulseArithmetic.and_equal_instanceof is_typ value typ astate in
    Atoms.write_return_from_bool path location is_typ ret_id astate


  let is_atom x : model_no_non_disj = has_type x Atom

  let is_boolean ((atom_val, _atom_hist) as atom) : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let astate_not_atom =
      (* Assume not atom: just return false *)
      let typ = Typ.mk_struct (ErlangType Atom) in
      let is_atom = AbstractValue.mk_fresh () in
      let<**> astate = PulseArithmetic.and_equal_instanceof is_atom atom_val typ astate in
      let<**> astate = PulseArithmetic.prune_eq_zero is_atom astate in
      Atoms.write_return_from_bool path location is_atom ret_id astate
    in
    let astate_is_atom =
      (* Assume atom: return hash==hashof(true) or hash==hashof(false) *)
      let> astate = prune_type path location atom Atom astate in
      let astate, _hash_addr, (hash, _hash_hist) =
        load_field path
          (Fieldname.make (ErlangType Atom) ErlangTypeName.atom_hash)
          location atom astate
      in
      let is_true = AbstractValue.mk_fresh () in
      let is_false = AbstractValue.mk_fresh () in
      let is_bool = AbstractValue.mk_fresh () in
      let<**> astate, is_true =
        PulseArithmetic.eval_binop is_true Binop.Eq (AbstractValueOperand hash)
          (ConstOperand
             (Cint (IntLit.of_int (ErlangTypeName.calculate_hash ErlangTypeName.atom_true))) )
          astate
      in
      let<**> astate, is_false =
        PulseArithmetic.eval_binop is_false Binop.Eq (AbstractValueOperand hash)
          (ConstOperand
             (Cint (IntLit.of_int (ErlangTypeName.calculate_hash ErlangTypeName.atom_false))) )
          astate
      in
      let<**> astate, is_bool =
        PulseArithmetic.eval_binop is_bool Binop.LOr (AbstractValueOperand is_true)
          (AbstractValueOperand is_false) astate
      in
      Atoms.write_return_from_bool path location is_bool ret_id astate
    in
    astate_not_atom @ astate_is_atom


  let is_integer x : model_no_non_disj = has_type x Integer

  let is_list (list_val, _list_hist) : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let cons_typ = Typ.mk_struct (ErlangType Cons) in
    let nil_typ = Typ.mk_struct (ErlangType Nil) in
    let astate_is_cons =
      let is_cons = AbstractValue.mk_fresh () in
      let<**> astate = PulseArithmetic.and_equal_instanceof is_cons list_val cons_typ astate in
      let<**> astate = PulseArithmetic.prune_positive is_cons astate in
      Atoms.write_return_from_bool path location is_cons ret_id astate
    in
    let astate_is_nil =
      let is_nil = AbstractValue.mk_fresh () in
      let<**> astate = PulseArithmetic.and_equal_instanceof is_nil list_val nil_typ astate in
      let<**> astate = PulseArithmetic.prune_positive is_nil astate in
      Atoms.write_return_from_bool path location is_nil ret_id astate
    in
    let astate_not_list =
      let is_cons = AbstractValue.mk_fresh () in
      let is_nil = AbstractValue.mk_fresh () in
      let<**> astate = PulseArithmetic.and_equal_instanceof is_cons list_val cons_typ astate in
      let<**> astate = PulseArithmetic.prune_eq_zero is_cons astate in
      let<**> astate = PulseArithmetic.and_equal_instanceof is_nil list_val nil_typ astate in
      let<**> astate = PulseArithmetic.prune_eq_zero is_nil astate in
      (* At this point, both [is_cons] and [is_nil] are false so we can return any of them. *)
      Atoms.write_return_from_bool path location is_nil ret_id astate
    in
    astate_is_cons @ astate_is_nil @ astate_not_list


  let is_map x : model_no_non_disj = has_type x Map
end

(** Custom models, specified by Config.pulse_models_for_erlang. *)
module Custom = struct
  (* TODO: see T110841433 *)

  (** Note: [None] means unknown/nondeterministic. *)
  type erlang_value = known_erlang_value option [@@deriving of_yojson, compare]

  and known_erlang_value =
    | Atom of string option
    | IntLit of string option
    | List of erlang_value list
    | Tuple of erlang_value list
    | GenServer of {module_name: string option}

  type selector =
    | ModuleFunctionArity of
        {module_: string [@key "module"]; function_: string [@key "function"]; arity: int}
        [@name "MFA"]
  [@@deriving of_yojson]

  type arguments_return = {arguments: erlang_value list; return: erlang_value}
  [@@deriving of_yojson, compare]

  type behavior = ReturnValue of erlang_value | ArgumentsReturnList of arguments_return list
  [@@deriving of_yojson]

  type rule = {selector: selector; behavior: behavior} [@@deriving of_yojson]

  type spec = rule list [@@deriving of_yojson]

  let max_nesting_level_bastraction = 3

  let max_concrete_list_lenght = 3

  (* Used as non deterministic erlang value in abstraction *)
  let non_det_erlang_value = Some (List [None])

  let rec pp_erlang_value fmt ev =
    match ev with
    | Some kev -> (
      match kev with
      | Atom (Some s) ->
          F.fprintf fmt "Atom(%s)" s
      | Atom None ->
          F.pp_print_string fmt "Atom(None)"
      | IntLit None ->
          F.fprintf fmt "IntLit(None)"
      | IntLit (Some s) ->
          F.fprintf fmt "IntLit(%s)" s
      | List [None] ->
          (* corresponds to non_det_erlang_value *)
          F.pp_print_string fmt "NonDetErlangValue"
      | List evl ->
          F.fprintf fmt "List[ %a ]" pp_erlang_value_list evl
      | Tuple evl ->
          F.fprintf fmt "Tuple[ %a ]" pp_erlang_value_list evl
      | GenServer {module_name= Some s} ->
          F.fprintf fmt "GenServer( %s )" s
      | GenServer {module_name= None} ->
          F.pp_print_string fmt "GenServer()" )
    | None ->
        F.pp_print_string fmt "NoneErlangValue"


  and pp_erlang_value_list fmt evl =
    List.iter ~f:(fun ev -> Format.fprintf fmt " %a,  " pp_erlang_value ev) evl


  let pp_behavior fmt b =
    match b with
    | ReturnValue ev ->
        F.fprintf fmt "@\n ReturnValue ( %a )@\n@\n@\n" pp_erlang_value ev
    | ArgumentsReturnList arl ->
        let helpf fmt arl =
          List.iter arl ~f:(fun {arguments= evl; return= ev} ->
              F.fprintf fmt "Arguments: %a ; @\n Return: %a @\n@\n@\n" pp_erlang_value_list evl
                pp_erlang_value ev )
        in
        F.fprintf fmt "@\n ArgumentReturnList ( %a )@\n@\n@\n" helpf arl


  let rec abstract_erlang_value nesting_level ev =
    let abs_max_nesting evl =
      if nesting_level <= max_nesting_level_bastraction then
        List.map evl ~f:(abstract_erlang_value (nesting_level + 1))
      else []
    in
    match ev with
    | None ->
        None
    | Some ev' ->
        let ev'' =
          match ev' with
          | Atom None ->
              Atom None
          | Atom (Some s) ->
              let s' =
                if
                  List.mem
                    ["false"; "true"; "timeout"; "return"; "error"; "exit"; "undefined"]
                    ~equal:String.( = ) s
                then Some s
                else None
              in
              Atom s'
          | (IntLit None | IntLit (Some "0") | IntLit (Some "1")) as il ->
              il
          | IntLit (Some _) ->
              IntLit None
          | List evs ->
              let abs_evs = abs_max_nesting evs in
              let abs_evs = fst (List.split_n abs_evs max_concrete_list_lenght) in
              let abs_evs = List.append abs_evs [non_det_erlang_value] in
              List abs_evs
          | Tuple evs ->
              let abs_evs = abs_max_nesting evs in
              let abs_evs = List.append abs_evs [non_det_erlang_value] in
              Tuple abs_evs
          | GenServer _mn ->
              GenServer _mn
        in
        Some ev''


  let join_behaviour_arguments_list abs_arl =
    List.dedup_and_sort abs_arl ~compare:compare_arguments_return


  let abstract_behavior mfa behavior =
    let do_argument_return ar =
      let abs_arg_val = List.map ar.arguments ~f:(abstract_erlang_value 0) in
      let abs_ret_val = abstract_erlang_value 0 ar.return in
      {arguments= abs_arg_val; return= abs_ret_val}
    in
    match behavior with
    | ReturnValue nev ->
        ReturnValue (abstract_erlang_value 0 nev)
    | ArgumentsReturnList args ->
        let abs_arguments_return_list = List.map args ~f:do_argument_return in
        let joined_arguments_return_list =
          join_behaviour_arguments_list abs_arguments_return_list
        in
        L.log_task "\n Function '%s'   Argument-Return List size =%i, After Abs+Join size = %i\n"
          mfa (List.length args)
          (List.length joined_arguments_return_list) ;
        ArgumentsReturnList joined_arguments_return_list


  let make_selector selector model =
    let l0 f = f [] in
    let l1 f x0 = f [x0] in
    let l2 f x0 x1 = f [x0; x1] in
    let l3 f x0 x1 x2 = f [x0; x1; x2] in
    let open ProcnameDispatcher.Call in
    match selector with
    | ModuleFunctionArity {module_; function_; arity= 0} ->
        -module_ &:: function_ <>$$--> l0 model
    | ModuleFunctionArity {module_; function_; arity= 1} ->
        -module_ &:: function_ <>$ capt_arg $--> l1 model
    | ModuleFunctionArity {module_; function_; arity= 2} ->
        -module_ &:: function_ <>$ capt_arg $+ capt_arg $--> l2 model
    | ModuleFunctionArity {module_; function_; arity= 3} ->
        -module_ &:: function_ <>$ capt_arg $+ capt_arg $+ capt_arg $--> l3 model
    | ModuleFunctionArity {module_; function_; arity} ->
        L.user_warning "@[<v>@[model for %s:%s/%d may match other arities (tool limitation)@]@;@]"
          module_ function_ arity ;
        -module_ &:: function_ &++> model


  let return_value_helper location path =
    (* Implementation note: [return_value_helper] groups two mutually recursive functions, [one] and
       [many], both of which may access [location] and [path]. *)
    let rec one (ret_val : erlang_value) : maker =
     fun astate ->
      match ret_val with
      | None ->
          let ret_addr = AbstractValue.mk_fresh () in
          let ret_hist = Hist.single_alloc path location "nondet" in
          Sat (Ok (astate, (ret_addr, ret_hist)))
      | Some (Atom None) ->
          let ret_addr = AbstractValue.mk_fresh () in
          let ret_hist = Hist.single_alloc path location "nondet_atom" in
          let astate =
            PulseArithmetic.and_dynamic_type_is_unsafe ret_addr (Typ.mk_struct (ErlangType Atom))
              location astate
          in
          Sat (Ok (astate, (ret_addr, ret_hist)))
      | Some (Atom (Some name)) ->
          Atoms.of_string location path name astate
      | Some (GenServer {module_name}) ->
          let ret_addr = AbstractValue.mk_fresh () in
          let ret_hist = Hist.single_alloc path location "gen_server_pid" in
          let astate =
            PulseArithmetic.and_dynamic_type_is_unsafe ret_addr
              (Typ.mk_struct (ErlangType (GenServerPid {module_name})))
              location astate
          in
          Sat (Ok (astate, (ret_addr, ret_hist)))
      | Some (IntLit (Some intlit)) ->
          Sat (Integers.of_string location path intlit astate)
      | Some (IntLit None) ->
          let ret_addr = AbstractValue.mk_fresh () in
          let ret_hist = Hist.single_alloc path location "nondet_abstr_intlit" in
          let astate =
            PulseArithmetic.and_dynamic_type_is_unsafe ret_addr
              (Typ.mk_struct (ErlangType Integer))
              location astate
          in
          Sat (Ok (astate, (ret_addr, ret_hist)))
      | Some (List elements) ->
          let mk = Lists.make_raw location path in
          many mk elements astate
      | Some (Tuple elements) ->
          let mk = Tuples.make_raw location path in
          many mk elements astate
    and many (mk : (AbstractValue.t * ValueHistory.t) list -> sat_maker)
        (elements : erlang_value list) : maker =
     fun astate ->
      let mk_arg (args, astate) element =
        let++ astate, arg = one element astate in
        (arg :: args, astate)
      in
      let+* args, astate = PulseOperationResult.list_fold ~init:([], astate) ~f:mk_arg elements in
      mk (List.rev args) astate
    in
    fun ret_val astate -> one ret_val astate


  let return_value_model (ret_val : erlang_value) : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let<++> astate, ret = return_value_helper location path ret_val astate in
    PulseOperations.write_id ret_id ret astate


  let rec argument_value_helper path location actual_arg (pre_arg : erlang_value) astate :
      AbductiveDomain.t result =
    let of_atom_like typ value_opt =
      match value_opt with
      | None ->
          prune_type path location actual_arg typ astate
      | Some value ->
          let> astate = prune_type path location actual_arg typ astate in
          let astate, _, (arg_hash, _hist) =
            load_field path Atoms.hash_field location actual_arg astate
          in
          let name_hash : Const.t = Cint (IntLit.of_int (ErlangTypeName.calculate_hash value)) in
          PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand arg_hash)
            (ConstOperand name_hash) astate
          |> SatUnsat.to_list
    in
    let of_intlit_like typ value_opt =
      match value_opt with
      | None ->
          prune_type path location actual_arg typ astate
      | Some intlit ->
          let> astate = prune_type path location actual_arg typ astate in
          let astate, _, (value, _hist) =
            load_field path Integers.value_field location actual_arg astate
          in
          PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand value)
            (Formula.ConstOperand (Cint (IntLit.of_string intlit)))
            astate
          |> SatUnsat.to_list
    in
    match pre_arg with
    | None ->
        [Ok astate]
    | Some (Atom value_opt) ->
        of_atom_like Atom value_opt
    | Some (GenServer {module_name}) ->
        of_atom_like (GenServerPid {module_name}) module_name
    | Some (IntLit intlit_opt) ->
        of_intlit_like Integer intlit_opt
    | Some (Tuple elements) ->
        let size = List.length elements in
        let> astate = prune_type path location actual_arg (Tuple size) astate in
        let one_element index astate pattern =
          let field = Tuples.field_name size (index + 1) in
          let astate, _, argi = load_field path field location actual_arg astate in
          argument_value_helper path location argi pattern astate
        in
        result_foldi elements ~init:astate ~f:one_element
    | Some (List elements) ->
        let rec go value elements astate =
          match elements with
          | [] ->
              prune_type path location value Nil astate
          | element :: rest ->
              let> head, tail, astate = Lists.load_head_tail value astate path location in
              let> astate = argument_value_helper path location head element astate in
              go tail rest astate
        in
        go actual_arg elements astate


  let arguments_return_model args (summaries : arguments_return list) : model_no_non_disj =
   fun {location; path; ret= ret_id, _} astate ->
    let get_payload (arg : 'a ProcnameDispatcher.Call.FuncArg.t) =
      arg.arg_payload |> ValueOrigin.addr_hist
    in
    let actual_arguments = args |> List.map ~f:get_payload in
    let one_summary {arguments; return} =
      let one_arg astate (actual_arg, pre_arg) =
        argument_value_helper path location actual_arg pre_arg astate
      in
      let paired =
        match List.zip actual_arguments arguments with
        | Unequal_lengths ->
            L.internal_error "Matched wrong arity (or model has wrong arity)." ;
            []
        | Ok result ->
            result
      in
      let> astate = result_fold paired ~init:astate ~f:one_arg in
      let> astate, ret = return_value_helper location path return astate |> SatUnsat.to_list in
      [Ok (PulseOperations.write_id ret_id ret astate)]
    in
    List.concat_map ~f:one_summary summaries |> List.map ~f:Basic.map_continue


  let make_model behavior args : model_no_non_disj =
    match behavior with
    | ReturnValue ret_val ->
        return_value_model ret_val
    | ArgumentsReturnList arguments_return_list ->
        arguments_return_model args arguments_return_list


  let matcher_of_rule {selector; behavior} = make_selector selector (make_model behavior)

  let load_files suffix loader =
    let maybe_load models path =
      if Filename.check_suffix path suffix then loader path :: models else models
    in
    List.fold Config.pulse_models_for_erlang ~init:[] ~f:(fun models path ->
        match (Unix.stat path).st_kind with
        | S_DIR ->
            Utils.fold_files ~init:models ~f:maybe_load ~path
        | S_REG ->
            maybe_load models path
        | _ ->
            models
        | exception Unix.Unix_error (ENOENT, _, _) ->
            models )


  let matchers () : matcher list =
    let load_spec path =
      try spec_of_yojson (Yojson.Safe.from_file path) with
      | Yojson.Json_error what ->
          L.user_error
            "@[<v>Failed to parse json from %s: %s@;\
             Continuing with no custom models imported from this file.@;\
             @]"
            path what ;
          []
      | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (what, json) ->
          let details = match what with Failure what -> Printf.sprintf " (%s)" what | _ -> "" in
          L.user_error
            "@[<v>Failed to parse --pulse-models-for-erlang from %s %s:@;\
             %a@;\
             Continuing with no custom models imported from this file.@;\
             @]"
            path details Yojson.Safe.pp json ;
          []
    in
    let convert_rule rule =
      matcher_of_rule rule |> ProcnameDispatcher.Call.map_matcher ~f:lift_model
    in
    List.concat_map (load_files ".json" load_spec) ~f:(List.map ~f:convert_rule)


  let mfa_to_db_map =
    let load_db path =
      let db =
        Sqlite3.db_open ~mode:`READONLY ~cache:`PRIVATE ~mutex:`NO ?vfs:Config.sqlite_vfs path
      in
      let stmt = Sqlite3.prepare db "SELECT mfa FROM models" in
      let mfas =
        SqliteUtils.result_fold_rows db stmt ~log:"Erlang models" ~init:[] ~f:(fun acc stmt ->
            Sqlite3.column_text stmt 0 :: acc )
      in
      (db, mfas)
    in
    let db_mfas = load_files ".db" load_db in
    List.fold db_mfas
      ~init:(Map.empty (module String))
      ~f:(fun map (db, mfas) ->
        List.fold mfas ~init:map ~f:(fun map mfa -> Map.set map ~key:mfa ~data:db) )


  let dynamic_behavior_models = Hashtbl.create (module String)

  let fetch_model db mfa =
    match Hashtbl.find dynamic_behavior_models mfa with
    | None ->
        let query = "SELECT behavior FROM models WHERE mfa = ? LIMIT 1" in
        let stmt = Sqlite3.prepare db query in
        Sqlite3.bind_text stmt 1 mfa |> SqliteUtils.check_result_code db ~log:"Erlang models" ;
        let model_opt =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              let behavior_json = Sqlite3.column_text stmt 0 in
              match behavior_of_yojson (Yojson.Safe.from_string behavior_json) with
              | behavior ->
                  let abs_behavior =
                    if Config.abstract_pulse_models_for_erlang then abstract_behavior mfa behavior
                    else behavior
                  in
                  L.debug Analysis Quiet "Function '%s' ABS BEHAVIOR:[  %a ] @\n@\n" mfa pp_behavior
                    abs_behavior ;
                  let model_abs_behavior = make_model abs_behavior in
                  Some model_abs_behavior
              | exception (Yojson.Json_error _ | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error _)
                ->
                  L.user_error "@[<v>Failed to parse json from db for %s .@;@]" mfa ;
                  None )
          | _ ->
              None
        in
        Sqlite3.finalize stmt |> SqliteUtils.check_result_code db ~log:"Erlang models" ;
        Hashtbl.add_exn dynamic_behavior_models ~key:mfa ~data:model_opt ;
        model_opt
    | Some model_opt ->
        model_opt


  let get_model_from_db proc_name args =
    let mfa = Fmt.to_to_string Procname.pp_verbose proc_name in
    let open IOption.Let_syntax in
    let* db = Map.find mfa_to_db_map mfa in
    let+ m = fetch_model db mfa in
    m args


  let exists_db_model proc_name =
    let mfa = Fmt.to_to_string Procname.pp_verbose proc_name in
    match Map.find mfa_to_db_map mfa with None -> false | Some _ -> true
end

module GenServer = struct
  type request = Call | Cast

  let start_link module_atom args _ : model =
   (* gen_server:start_link(Module, _, _) -> {ok, Pid}
      where Pid is `GenServerPid of Module` *)
   fun ({path; analysis_data; location; ret} as data) astate non_disj ->
    let module_name_opt =
      match get_erlang_type_or_any (fst module_atom) astate with
      | Atom ->
          Atoms.get_name path location module_atom astate
      | typ ->
          L.debug Analysis Verbose
            "@[First argument of gen_server:start_link is of unsupported type %a@."
            ErlangTypeName.pp typ ;
          None
    in
    let res_list, non_disj =
      match module_name_opt with
      | None ->
          ([Ok (ContinueProgram astate)], non_disj)
      | Some module_name ->
          let procname = Procname.make_erlang ~module_name ~function_name:"init" ~arity:1 in
          let actuals =
            [(args, Typ.mk_struct (ErlangType (get_erlang_type_or_any (fst args) astate)))]
          in
          let res_list, non_disj, _, _ =
            PulseCallOperations.call analysis_data path location procname ~ret ~actuals
              ~formals_opt:None ~call_kind:`ResolvedProcname astate non_disj
          in
          (res_list, non_disj)
    in
    let post_process_handle_init res =
      (*
        Convert Module:init Result = {ok,State}
          | {ok,State,Timeout}
          | {ok,State,hibernate}
          | {ok,State,{continue,Continue}}
          | {stop,Reason}
          | {error,Reason}
          | ignore
        to
          gen_server:start_link start_ret() = {ok, Pid :: pid()}
          | ignore
          | {error, Reason :: term()}
        https://www.erlang.org/doc/man/gen_server.html#Module:init-1
        https://www.erlang.org/doc/man/gen_server.html#type-start_ret
      *)
      let start_ret, astate =
        match PulseResult.ok res with
        | None ->
            (* Module:init crashes returns {error, _} *)
            (Some (Custom.Tuple [Some (Custom.Atom (Some "error")); None]), astate)
        | Some exec_state -> (
          match exec_state with
          | ContinueProgram astate -> (
              let _, ret_val = PulseOperations.eval_ident (fst ret) astate in
              match get_erlang_type_or_any (fst ret_val) astate with
              | Tuple n when n >= 2 && n <= 3 -> (
                  let _, _, first_element =
                    load_field path (Tuples.field_name n 1) location ret_val astate
                  in
                  match get_erlang_type_or_any (fst first_element) astate with
                  | Atom -> (
                    match Atoms.get_name path location first_element astate with
                    | Some "ok" ->
                        (* the second element is the server state - not considered for now *)
                        ( Some
                            (Custom.Tuple
                               [ Some (Custom.Atom (Some "ok"))
                               ; Some (Custom.GenServer {module_name= module_name_opt}) ] )
                        , astate )
                    | Some "stop" | Some "error" ->
                        (Some (Custom.Tuple [Some (Custom.Atom (Some "error")); None]), astate)
                    | _ ->
                        (None, astate) )
                  | typ ->
                      L.debug Analysis Verbose
                        "@[<v>gen_server:init returned tuple with first element unexpected type %a@;\
                         @]"
                        ErlangTypeName.pp typ ;
                      (None, astate) )
              | Atom -> (
                match Atoms.get_name path location ret_val astate with
                | Some "ignore" ->
                    (Some (Custom.Atom (Some "ignore")), astate)
                | _ ->
                    (None, astate) )
              | typ ->
                  L.debug Analysis Verbose "@[<v>gen_server:init returned unexpected type %a@;@]"
                    ErlangTypeName.pp typ ;
                  (None, astate) )
          | _ ->
              (* Module:init exceptions returns {error, _} *)
              (Some (Custom.Tuple [Some (Custom.Atom (Some "error")); None]), astate) )
      in
      Custom.return_value_model start_ret data astate
    in
    (List.concat_map res_list ~f:post_process_handle_init, non_disj)


  let handle_request req_type server_ref request {path; analysis_data; location; ret} astate
      non_disj =
    let astate = AbductiveDomain.add_need_dynamic_type_specialization (fst server_ref) astate in
    let module_name_opt =
      (* Cf. https://www.erlang.org/doc/man/gen_server.html#type-server_ref :
               server_ref() =
                   pid() |
                   (LocalName :: atom()) |
                   {Name :: atom(), Node :: atom()} |
                   {global, GlobalName :: term()} |
         {via, RegMod :: module(), ViaName :: term()}
      *)
      match get_erlang_type_or_any (fst server_ref) astate with
      | GenServerPid {module_name= Some module_name} ->
          Some module_name
      | GenServerPid {module_name= None} ->
          L.debug Analysis Verbose "@[gen_server:call/cast called with unnamed GenServerPid@." ;
          None
      (* The following cases assume that the server is registered with its name. *)
      | Atom ->
          (* case `server_ref() = LocalName :: atom()` *)
          Atoms.get_name path location server_ref astate
      | Tuple 2 -> (
          (* case `server_ref() = {Name :: atom(), Node :: atom()} | {global, GlobalName :: term()}
             We model global and local registration on another node as local registration in the current node *)
          let _, _, first_element =
            load_field path (Tuples.field_name 2 1) location server_ref astate
          in
          match Atoms.get_name path location first_element astate with
          | Some "global" ->
              (* case `server_ref() = {global, GlobalName :: term()}` *)
              let _, _, second_element =
                load_field path (Tuples.field_name 2 2) location server_ref astate
              in
              Atoms.get_name path location second_element astate
          | module_name_opt ->
              (* case `server_ref() = {Name :: atom(), Node :: atom()}` *)
              module_name_opt )
      | Tuple 3 ->
          (* unsupported case: `server_ref() = {via, RegMod :: module(), ViaName :: term()}`.
                                                 Note: we don't check that the first element is the atom `via` as we don't support that case yet anyway. *)
          L.debug Analysis Verbose
            "@[gen_server:call/cast called with unsupported `gen_server:server_ref()`: `{via, \
             RegMod :: module(), ViaName :: term()}`@." ;
          None
      | typ ->
          L.debug Analysis Verbose
            "@[gen_server:call/cast called with Invalid server_ref of type: %a@." ErlangTypeName.pp
            typ ;
          None
    in
    match module_name_opt with
    | Some module_name ->
        let arg_nondet () =
          ( (AbstractValue.mk_fresh (), Hist.single_alloc path location "nondet")
          , Typ.mk_struct (ErlangType Any) )
        in
        let arg_req =
          (request, Typ.mk_struct (ErlangType (get_erlang_type_or_any (fst request) astate)))
        in
        let procname, actuals =
          match req_type with
          | Call ->
              ( Procname.make_erlang ~module_name ~function_name:"handle_call" ~arity:3
              , [arg_req; arg_nondet (); arg_nondet ()] )
          | Cast ->
              ( Procname.make_erlang ~module_name ~function_name:"handle_cast" ~arity:2
              , [arg_req; arg_nondet ()] )
        in
        let execs, non_disj, _, _ =
          PulseCallOperations.call analysis_data path location procname ~ret ~actuals
            ~formals_opt:None ~call_kind:`ResolvedProcname astate non_disj
        in
        (execs, non_disj)
    | None ->
        L.debug Analysis Verbose "@[gen_server:call/cast failed to derive gen_server module name@." ;
        ([Ok (ContinueProgram astate)], non_disj)


  let call2 server_ref request : model =
   fun ({ret; path; location} as data) astate non_disj ->
    let res_list, non_disj = handle_request Call server_ref request data astate non_disj in
    let post_process_handle_call res =
      match PulseResult.ok res with
      | None ->
          res
      | Some exec_state -> (
        match exec_state with
        | ContinueProgram astate ->
            let _, ret_val = PulseOperations.eval_ident (fst ret) astate in
            let val_nondet () =
              (AbstractValue.mk_fresh (), Hist.single_alloc path location "nondet")
            in
            let proc_ret_val =
              match get_erlang_type_or_any (fst ret_val) astate with
              | Tuple n when n > 1 -> (
                  let _, _, first_element =
                    load_field path (Tuples.field_name n 1) location ret_val astate
                  in
                  match get_erlang_type_or_any (fst first_element) astate with
                  | Atom -> (
                    (*
                      valid atoms are `reply`, `noreply` and `stop`
                      https://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3
                    *)
                    match Atoms.get_name path location first_element astate with
                    | Some "reply" ->
                        (* the second element is sent back to the client request and there becomes its return value *)
                        let _, _, second_element =
                          load_field path (Tuples.field_name n 2) location ret_val astate
                        in
                        second_element
                    | Some "noreply" | Some "stop" ->
                        val_nondet ()
                    | Some value ->
                        L.debug Analysis Verbose
                          "@[<v>gen_server:handle_call returned tuple with unexpected first \
                           element Atom `%s`@;\
                           @]"
                          value ;
                        val_nondet ()
                    | None ->
                        L.debug Analysis Verbose
                          "@[<v>gen_server:handle_call returned tuple with unknown first element \
                           Atom @;\
                           @]" ;
                        val_nondet () )
                  | typ ->
                      L.debug Analysis Verbose
                        "@[<v>gen_server:handle_call returned tuple with first element unexpected \
                         type %a@;\
                         @]"
                        ErlangTypeName.pp typ ;
                      val_nondet () )
              | typ ->
                  L.debug Analysis Verbose
                    "@[<v>gen_server:handle_call returned unexpected type %a@;@]" ErlangTypeName.pp
                    typ ;
                  val_nondet ()
            in
            Ok (PulseOperations.write_id (fst ret) proc_ret_val astate) |> Basic.map_continue
        | _ ->
            res )
    in
    (List.map res_list ~f:post_process_handle_call, non_disj)


  let call3 server_ref request _timeout : model = call2 server_ref request

  let cast server_ref request : model =
   fun data astate non_disj ->
    let res_list, non_disj = handle_request Cast server_ref request data astate non_disj in
    let post_process_handle_call res =
      match PulseResult.ok res with
      | None ->
          [res]
      | Some exec_state -> (
        match exec_state with
        | ContinueProgram astate ->
            Custom.return_value_model (Some (Custom.Atom (Some "ok"))) data astate
        | _ ->
            [res] )
    in
    (List.concat_map res_list ~f:post_process_handle_call, non_disj)
end

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let arg = capt_arg_payload in
  let erlang_ns = ErlangTypeName.erlang_namespace in
  Custom.matchers ()
  @ List.map
      ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
      [ +BuiltinDecl.(match_builtin __erlang_error_badgenerator)
        <>--> Errors.badgenerator |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_badkey) <>--> Errors.badkey |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_badmap) <>--> Errors.badmap |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_badmatch) <>--> Errors.badmatch |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_badrecord)
        <>--> Errors.badrecord |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_badreturn)
        <>--> Errors.badreturn |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_case_clause)
        <>--> Errors.case_clause |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_else_clause)
        <>--> Errors.else_clause |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_function_clause)
        <>--> Errors.function_clause |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_if_clause)
        <>--> Errors.if_clause |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_error_try_clause)
        <>--> Errors.try_clause |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_make_atom)
        <>$ arg $+ arg $--> Atoms.make |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_make_integer)
        <>$ arg $--> Integers.make |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_make_nil) <>--> Lists.make_nil |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_make_cons)
        <>$ arg $+ arg $--> Lists.make_cons |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_make_str_const)
        <>$ arg $--> Strings.make |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_equal)
        <>$ arg $+ arg $--> Comparison.equal |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_exactly_equal)
        <>$ arg $+ arg $--> Comparison.equal |> with_non_disj
        (* TODO: proper modeling of equal vs exactly equal T95767672 *)
      ; +BuiltinDecl.(match_builtin __erlang_not_equal)
        <>$ arg $+ arg $--> Comparison.exactly_not_equal |> with_non_disj
        (* TODO: proper modeling of equal vs exactly equal T95767672 *)
      ; +BuiltinDecl.(match_builtin __erlang_exactly_not_equal)
        <>$ arg $+ arg $--> Comparison.exactly_not_equal |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_greater)
        <>$ arg $+ arg $--> Comparison.greater |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_greater_or_equal)
        <>$ arg $+ arg $--> Comparison.greater_or_equal |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_lesser)
        <>$ arg $+ arg $--> Comparison.lesser |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_lesser_or_equal)
        <>$ arg $+ arg $--> Comparison.lesser_or_equal |> with_non_disj
      ; -"lists" &:: "append" <>$ arg $+ arg $--> Lists.append2 ~reverse:false |> with_non_disj
      ; -"lists" &:: "foreach" <>$ arg $+ arg $--> Lists.foreach |> with_non_disj
      ; -"lists" &:: "reverse" <>$ arg $--> Lists.reverse |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_make_map) &++> Maps.make |> with_non_disj
      ; +BuiltinDecl.(match_builtin __erlang_map_to_list)
        <>$ arg $--> Maps.to_list ~check_badmap:false |> with_non_disj
      ; -"maps" &:: "is_key" <>$ arg $+ arg $--> Maps.is_key |> with_non_disj
      ; -"maps" &:: "get" <>$ arg $+ arg $--> Maps.get |> with_non_disj
      ; -"maps" &:: "put" <>$ arg $+ arg $+ arg $--> Maps.put |> with_non_disj
      ; -"maps" &:: "to_list" <>$ arg $--> Maps.to_list ~check_badmap:true |> with_non_disj
      ; -"maps" &:: "new" <>$$--> Maps.new_ |> with_non_disj
      ; -"gen_server" &:: "start_link" <>$ arg $+ arg $+ arg $--> GenServer.start_link
      ; -"gen_server" &:: "call" <>$ arg $+ arg $--> GenServer.call2
      ; -"gen_server" &:: "call" <>$ arg $+ arg $+ arg $--> GenServer.call3
      ; -"gen_server" &:: "cast" <>$ arg $+ arg $--> GenServer.cast
      ; +BuiltinDecl.(match_builtin __erlang_make_tuple) &++> Tuples.make |> with_non_disj
      ; -erlang_ns &:: "is_atom" <>$ arg $--> BIF.is_atom |> with_non_disj
      ; -erlang_ns &:: "is_boolean" <>$ arg $--> BIF.is_boolean |> with_non_disj
      ; -erlang_ns &:: "is_integer" <>$ arg $--> BIF.is_integer |> with_non_disj
      ; -erlang_ns &:: "is_list" <>$ arg $--> BIF.is_list |> with_non_disj
      ; -erlang_ns &:: "is_map" <>$ arg $--> BIF.is_map |> with_non_disj ]


let get_model_from_db = Custom.get_model_from_db
