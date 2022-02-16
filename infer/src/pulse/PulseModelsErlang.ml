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
open PulseOperations.Import
open PulseModelsImport

(** A type for transfer functions that make an object, add it to abstract state
    ([AbductiveDomain.t]), and return a handle to it ([(AbstractValue.t * ValueHistory.t)]). Note
    that the type is simlar to that of [PulseOperations.eval]. *)
type maker =
  AbductiveDomain.t -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

let write_field_and_deref path location ~struct_addr ~field_addr ~field_val field_name astate =
  let* astate =
    PulseOperations.write_field path location ~ref:struct_addr field_name ~obj:field_addr astate
  in
  PulseOperations.write_deref path location ~ref:field_addr ~obj:field_val astate


let write_dynamic_type_and_return (addr_val, hist) typ ret_id astate =
  let typ = Typ.mk_struct (ErlangType typ) in
  let astate = PulseOperations.add_dynamic_type typ addr_val astate in
  PulseOperations.write_id ret_id (addr_val, hist) astate


(** Use for chaining functions of the type ('a->('b,'err) result list). The idea of such functions
    is that they can both fan-out into a (possibly empty) disjunction *and* signal errors. For
    example, consider [f] of type ['a->('b,'err) result list] and [g] of type
    ['b->('c,'err) result list] and [a] is some value of type ['a]. Note that the type of error is
    the same, so they can be propagated forward. To chain the application of these functions, you
    can write [let> x=f a in let> y=g x in \[Ok y\]].

    In several places, we have to compose with functions of the type ['a->('b,'err) result], which
    don't produce a list. One way to handle this is to wrap those functions in a list. For example,
    if [f] and [a] have the same type as before but [g] has type ['b->('c,'err) result], then we can
    write [let> =f a in let> y=\[g x\] in \[Ok y\].] *)
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


let prune_type path location (value, hist) typ astate : AbductiveDomain.t AccessResult.t list =
  (* If check_addr_access fails, we stop exploring this path. *)
  let ( let^ ) x f = match x with Recoverable _ | FatalError _ -> [] | Ok astate -> [f astate] in
  let^ astate = PulseOperations.check_addr_access path Read location (value, hist) astate in
  let typ = Typ.mk_struct (ErlangType typ) in
  let instanceof_val = AbstractValue.mk_fresh () in
  let* astate = PulseArithmetic.and_equal_instanceof instanceof_val value typ astate in
  let+ astate = PulseArithmetic.prune_positive instanceof_val astate in
  astate


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


module Errors = struct
  let error err astate = [FatalError (ReportableError {astate; diagnostic= ErlangError err}, [])]

  let badkey : model =
   fun {location} astate -> error (Badkey {calling_context= []; location}) astate


  let badmap : model =
   fun {location} astate -> error (Badmap {calling_context= []; location}) astate


  let badmatch : model =
   fun {location} astate -> error (Badmatch {calling_context= []; location}) astate


  let badrecord : model =
   fun {location} astate -> error (Badrecord {calling_context= []; location}) astate


  let case_clause : model =
   fun {location} astate -> error (Case_clause {calling_context= []; location}) astate


  let function_clause : model =
   fun {location} astate -> error (Function_clause {calling_context= []; location}) astate


  let if_clause : model =
   fun {location} astate -> error (If_clause {calling_context= []; location}) astate


  let try_clause : model =
   fun {location} astate -> error (Try_clause {calling_context= []; location}) astate
end

module Atoms = struct
  let value_field = Fieldname.make (ErlangType Atom) ErlangTypeName.atom_value

  let hash_field = Fieldname.make (ErlangType Atom) ErlangTypeName.atom_hash

  let make_raw location path value hash : maker =
   fun astate ->
    let hist = Hist.single_alloc path location "atom" in
    let addr_atom = (AbstractValue.mk_fresh (), hist) in
    let* astate =
      write_field_and_deref path location ~struct_addr:addr_atom
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:value value_field astate
    in
    let+ astate =
      write_field_and_deref path location ~struct_addr:addr_atom
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:hash hash_field astate
    in
    ( PulseOperations.add_dynamic_type (Typ.mk_struct (ErlangType Atom)) (fst addr_atom) astate
    , addr_atom )


  let make value hash : model =
   fun {location; path; ret= ret_id, _} astate ->
    let<+> astate, ret = make_raw location path value hash astate in
    PulseOperations.write_id ret_id ret astate


  let of_string location path (name : string) : maker =
   fun astate ->
    (* Note: This should correspond to [ErlangTranslator.mk_atom_call]. *)
    let* astate, hash =
      let hash_exp : Exp.t = Const (Cint (IntLit.of_int (ErlangTypeName.calculate_hash name))) in
      PulseOperations.eval path Read location hash_exp astate
    in
    let* astate, name =
      let name_exp : Exp.t = Const (Cstr name) in
      PulseOperations.eval path Read location name_exp astate
    in
    make_raw location path name hash astate


  (* Converts [bool_value] into true/false, and write it to [addr_atom]. *)
  let of_bool path location bool_value astate =
    let astate_true =
      let* astate = PulseArithmetic.prune_positive bool_value astate in
      of_string location path ErlangTypeName.atom_true astate
    in
    let astate_false : (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t =
      let* astate = PulseArithmetic.prune_eq_zero bool_value astate in
      of_string location path ErlangTypeName.atom_false astate
    in
    let> astate, (addr, hist) = [astate_true; astate_false] in
    let typ = Typ.mk_struct (ErlangType Atom) in
    [Ok (PulseOperations.add_dynamic_type typ addr astate, (addr, hist))]


  (** Takes a boolean value, converts it to true/false atom and writes to return value. *)
  let write_return_from_bool path location bool_value ret_id astate =
    let> astate, ret_val = of_bool path location bool_value astate in
    PulseOperations.write_id ret_id ret_val astate |> Basic.ok_continue
end

module Lists = struct
  let head_field = Fieldname.make (ErlangType Cons) ErlangTypeName.cons_head

  let tail_field = Fieldname.make (ErlangType Cons) ErlangTypeName.cons_tail

  (** Helper function to create a Nil structure without assigning it to return value *)
  let make_nil_raw location path : maker =
   fun astate ->
    let event = Hist.alloc_event path location "[]" in
    let addr_nil_val = AbstractValue.mk_fresh () in
    let addr_nil = (addr_nil_val, Hist.single_event path event) in
    let astate =
      PulseOperations.add_dynamic_type (Typ.mk_struct (ErlangType Nil)) addr_nil_val astate
    in
    Ok (astate, addr_nil)


  (** Create a Nil structure and assign it to return value *)
  let make_nil : model =
   fun {location; path; ret= ret_id, _} astate ->
    let<+> astate, addr_nil = make_nil_raw location path astate in
    PulseOperations.write_id ret_id addr_nil astate


  (** Helper function to create a Cons structure without assigning it to return value *)
  let make_cons_raw path location hd tl : maker =
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
      PulseOperations.add_dynamic_type (Typ.mk_struct (ErlangType Cons)) addr_cons_val astate
    in
    (astate, addr_cons)


  (** Create a Cons structure and assign it to return value *)
  let make_cons head tail : model =
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


  let append2 ~reverse list1 list2 : model =
   fun {location; path; ret= ret_id, _} astate ->
    (* Makes an abstract state corresponding to appending to a list of given length *)
    let mk_astate_concat length =
      let> elems, astate = assume_and_deconstruct list1 length astate path location in
      let elems = if reverse then elems else List.rev elems in
      let> astate, result_list =
        [ PulseResult.list_fold ~init:(astate, list2)
            ~f:(fun (astate, tl) hd -> make_cons_raw path location hd tl astate)
            elems ]
      in
      [Ok (PulseOperations.write_id ret_id result_list astate)]
    in
    List.concat (List.init Config.erlang_list_unfold_depth ~f:mk_astate_concat)
    |> List.map ~f:Basic.map_continue


  let reverse list : model =
   fun ({location; path; _} as data) astate ->
    let<*> astate, nil = make_nil_raw location path astate in
    append2 ~reverse:true list nil data astate


  let rec make_raw location path elements : maker =
   fun astate ->
    match elements with
    | [] ->
        make_nil_raw location path astate
    | head :: tail ->
        let* astate, tail_val = make_raw location path tail astate in
        make_cons_raw path location head tail_val astate


  (** Approximation: we don't actually do the side-effect, just assume the return value. *)
  let foreach _fun _list : model =
   fun {location; path; ret= ret_id, _} astate ->
    let<*> astate, ret = Atoms.of_string location path "ok" astate in
    PulseOperations.write_id ret_id ret astate |> Basic.ok_continue
end

module Tuples = struct
  (** Helper: Like [Tuples.make] but with a more precise/composable type. *)
  let make_raw (location : Location.t) (path : PathContext.t)
      (args : (AbstractValue.t * ValueHistory.t) list) : maker =
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
    ( PulseOperations.add_dynamic_type (Typ.mk_struct tuple_typ_name) (fst addr_tuple) astate
    , addr_tuple )


  let make (args : 'a ProcnameDispatcher.Call.FuncArg.t list) : model =
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

  let make (args : 'a ProcnameDispatcher.Call.FuncArg.t list) : model =
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
    let<+> astate = PulseArithmetic.and_eq_int is_empty_value is_empty_lit astate in
    write_dynamic_type_and_return addr_map Map ret_id astate


  let new_ : model = make []

  let make_astate_badmap (map_val, _map_hist) data astate =
    let typ = Typ.mk_struct (ErlangType Map) in
    let instanceof_val = AbstractValue.mk_fresh () in
    let<*> astate = PulseArithmetic.and_equal_instanceof instanceof_val map_val typ astate in
    let<*> astate = PulseArithmetic.prune_eq_zero instanceof_val astate in
    Errors.badmap data astate


  let make_astate_goodmap path location map astate = prune_type path location map Map astate

  let is_key (key, _key_history) map : model =
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
      let> astate = [PulseArithmetic.prune_positive is_empty astate] in
      let> astate = [PulseArithmetic.and_eq_int ret_val_false IntLit.zero astate] in
      Atoms.write_return_from_bool path location ret_val_false ret_id astate
    in
    let astate_haskey =
      let ret_val_true = AbstractValue.mk_fresh () in
      let> astate = make_astate_goodmap path location map astate in
      let astate, _isempty_addr, (is_empty, _isempty_hist) =
        load_field path is_empty_field location map astate
      in
      let> astate = [PulseArithmetic.prune_eq_zero is_empty astate] in
      let astate, _key_addr, (tracked_key, _hist) = load_field path key_field location map astate in
      let> astate =
        [ PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand key)
            (AbstractValueOperand tracked_key) astate ]
      in
      let> astate = [PulseArithmetic.and_eq_int ret_val_true IntLit.one astate] in
      Atoms.write_return_from_bool path location ret_val_true ret_id astate
    in
    astate_empty @ astate_haskey @ astate_badmap


  let get (key, _key_history) map : model =
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
      let> astate = [PulseArithmetic.prune_eq_zero is_empty astate] in
      let astate, _key_addr, (tracked_key, _hist) = load_field path key_field location map astate in
      let> astate =
        [ PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand key)
            (AbstractValueOperand tracked_key) astate ]
      in
      let astate, _value_addr, tracked_value = load_field path value_field location map astate in
      [Ok (PulseOperations.write_id ret_id tracked_value astate)]
    in
    let astate_badkey =
      let> astate = make_astate_goodmap path location map astate in
      let astate, _isempty_addr, (is_empty, _isempty_hist) =
        load_field path is_empty_field location map astate
      in
      let> astate = [PulseArithmetic.prune_positive is_empty astate] in
      Errors.badkey data astate
    in
    List.map ~f:Basic.map_continue astate_ok @ astate_badkey @ astate_badmap


  let put key value map : model =
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
        [ write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_is_empty
            ~field_val:fresh_val is_empty_field astate
          >>= PulseArithmetic.and_eq_int is_empty_value IntLit.zero ]
      in
      [Ok (write_dynamic_type_and_return addr_map Map ret_id astate)]
    in
    List.map ~f:Basic.map_continue astate_ok @ astate_badmap


  let return_nondet_map name path location ret_id astate =
    let hist = Hist.single_alloc path location name in
    let addr_map = (AbstractValue.mk_fresh (), hist) in
    write_dynamic_type_and_return addr_map Map ret_id astate |> Basic.ok_continue


  let merge _m1 _m2 : model =
   fun {location; path; ret= ret_id, _} astate ->
    (* This is just a very rough approximation, we just assume that the return type. *)
    return_nondet_map "maps:merge" path location ret_id astate


  let filter _pred _map : model =
   fun {location; path; ret= ret_id, _} astate ->
    (* This is just a very rough approximation, we just assume that the return type. *)
    return_nondet_map "maps:filter" path location ret_id astate
end

module BIF = struct
  let is_atom (atom_val, _atom_hist) : model =
   fun {location; path; ret= ret_id, _} astate ->
    let typ = Typ.mk_struct (ErlangType Atom) in
    let is_atom = AbstractValue.mk_fresh () in
    let<*> astate = PulseArithmetic.and_equal_instanceof is_atom atom_val typ astate in
    Atoms.write_return_from_bool path location is_atom ret_id astate


  let is_boolean ((atom_val, _atom_hist) as atom) : model =
   fun {location; path; ret= ret_id, _} astate ->
    let astate_not_atom =
      (* Assume not atom: just return false *)
      let typ = Typ.mk_struct (ErlangType Atom) in
      let is_atom = AbstractValue.mk_fresh () in
      let<*> astate = PulseArithmetic.and_equal_instanceof is_atom atom_val typ astate in
      let<*> astate = PulseArithmetic.prune_eq_zero is_atom astate in
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
      let<*> astate, is_true =
        PulseArithmetic.eval_binop is_true Binop.Eq (AbstractValueOperand hash)
          (ConstOperand
             (Cint (IntLit.of_int (ErlangTypeName.calculate_hash ErlangTypeName.atom_true))) )
          astate
      in
      let<*> astate, is_false =
        PulseArithmetic.eval_binop is_false Binop.Eq (AbstractValueOperand hash)
          (ConstOperand
             (Cint (IntLit.of_int (ErlangTypeName.calculate_hash ErlangTypeName.atom_false))) )
          astate
      in
      let<*> astate, is_bool =
        PulseArithmetic.eval_binop is_bool Binop.LOr (AbstractValueOperand is_true)
          (AbstractValueOperand is_false) astate
      in
      Atoms.write_return_from_bool path location is_bool ret_id astate
    in
    astate_not_atom @ astate_is_atom


  let is_list (list_val, _list_hist) : model =
   fun {location; path; ret= ret_id, _} astate ->
    let cons_typ = Typ.mk_struct (ErlangType Cons) in
    let nil_typ = Typ.mk_struct (ErlangType Nil) in
    let is_cons = AbstractValue.mk_fresh () in
    let is_nil = AbstractValue.mk_fresh () in
    let is_list = AbstractValue.mk_fresh () in
    let<*> astate = PulseArithmetic.and_equal_instanceof is_cons list_val cons_typ astate in
    let<*> astate = PulseArithmetic.and_equal_instanceof is_nil list_val nil_typ astate in
    let<*> astate, is_list =
      PulseArithmetic.eval_binop is_list Binop.LOr (AbstractValueOperand is_cons)
        (AbstractValueOperand is_nil) astate
    in
    Atoms.write_return_from_bool path location is_list ret_id astate


  let is_map (map_val, _map_hist) : model =
   fun {location; path; ret= ret_id, _} astate ->
    let typ = Typ.mk_struct (ErlangType Map) in
    let is_map = AbstractValue.mk_fresh () in
    let<*> astate = PulseArithmetic.and_equal_instanceof is_map map_val typ astate in
    Atoms.write_return_from_bool path location is_map ret_id astate
end

(** Custom models, specified by Config.pulse_models_for_erlang. *)
module Custom = struct
  (* TODO: see T110841433 *)

  (** Note: [None] means unknown/nondeterministic. *)
  type erlang_value = known_erlang_value option [@@deriving of_yojson]

  and known_erlang_value =
    | Atom of string option
    | IntLit of string
    | List of erlang_value list
    | Tuple of erlang_value list

  type selector =
    | ModuleFunctionArity of
        {module_: string [@key "module"]; function_: string [@key "function"]; arity: int}
        [@name "MFA"]
  [@@deriving of_yojson]

  type behavior = ReturnValue of erlang_value [@@deriving of_yojson]

  type rule = {selector: selector; behavior: behavior} [@@deriving of_yojson]

  type spec = rule list [@@deriving of_yojson]

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
          Ok (astate, (ret_addr, ret_hist))
      | Some (Atom None) ->
          let ret_addr = AbstractValue.mk_fresh () in
          let ret_hist = Hist.single_alloc path location "nondet_atom" in
          Ok
            ( PulseOperations.add_dynamic_type (Typ.mk_struct (ErlangType Atom)) ret_addr astate
            , (ret_addr, ret_hist) )
      | Some (Atom (Some name)) ->
          Atoms.of_string location path name astate
      | Some (IntLit intlit) ->
          let intlit_exp : Exp.t = Const (Cint (IntLit.of_string intlit)) in
          let+ astate, intlit = PulseOperations.eval path Read location intlit_exp astate in
          (astate, intlit)
      | Some (List elements) ->
          let mk = Lists.make_raw location path in
          many mk elements astate
      | Some (Tuple elements) ->
          let mk = Tuples.make_raw location path in
          many mk elements astate
    and many (mk : (AbstractValue.t * ValueHistory.t) list -> maker) (elements : erlang_value list)
        : maker =
     fun astate ->
      let mk_arg (args, astate) element =
        let+ astate, arg = one element astate in
        (arg :: args, astate)
      in
      let* args, astate = PulseResult.list_fold ~init:([], astate) ~f:mk_arg elements in
      mk (List.rev args) astate
    in
    fun ret_val astate -> one ret_val astate


  let return_value_model (ret_val : erlang_value) : model =
   fun {location; path; ret= ret_id, _} astate ->
    let<+> astate, ret = return_value_helper location path ret_val astate in
    PulseOperations.write_id ret_id ret astate


  let make_model behavior _args : model =
    match behavior with ReturnValue ret_val -> return_value_model ret_val


  let matcher_of_rule {selector; behavior} = make_selector selector (make_model behavior)

  let matchers () : matcher list =
    let spec =
      try spec_of_yojson (Config.pulse_models_for_erlang :> Yojson.Safe.t)
      with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (what, json) ->
        let details = match what with Failure what -> Printf.sprintf " (%s)" what | _ -> "" in
        L.user_error
          "@[<v>Failed to parse --pulse-models-for-erlang%s:@;\
           %a@;\
           Continuing with no custom models.@;\
           @]"
          details Yojson.Safe.pp json ;
        []
    in
    List.map ~f:matcher_of_rule spec
end

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let arg = capt_arg_payload in
  let erlang_ns = ErlangTypeName.erlang_namespace in
  Custom.matchers ()
  @ [ +BuiltinDecl.(match_builtin __erlang_error_badkey) <>--> Errors.badkey
    ; +BuiltinDecl.(match_builtin __erlang_error_badmap) <>--> Errors.badmap
    ; +BuiltinDecl.(match_builtin __erlang_error_badmatch) <>--> Errors.badmatch
    ; +BuiltinDecl.(match_builtin __erlang_error_badrecord) <>--> Errors.badrecord
    ; +BuiltinDecl.(match_builtin __erlang_error_case_clause) <>--> Errors.case_clause
    ; +BuiltinDecl.(match_builtin __erlang_error_function_clause) <>--> Errors.function_clause
    ; +BuiltinDecl.(match_builtin __erlang_error_if_clause) <>--> Errors.if_clause
    ; +BuiltinDecl.(match_builtin __erlang_error_try_clause) <>--> Errors.try_clause
    ; +BuiltinDecl.(match_builtin __erlang_make_atom) <>$ arg $+ arg $--> Atoms.make
    ; +BuiltinDecl.(match_builtin __erlang_make_nil) <>--> Lists.make_nil
    ; +BuiltinDecl.(match_builtin __erlang_make_cons) <>$ arg $+ arg $--> Lists.make_cons
    ; -"lists" &:: "append" <>$ arg $+ arg $--> Lists.append2 ~reverse:false
    ; -"lists" &:: "foreach" <>$ arg $+ arg $--> Lists.foreach
    ; -"lists" &:: "reverse" <>$ arg $--> Lists.reverse
    ; +BuiltinDecl.(match_builtin __erlang_make_map) &++> Maps.make
    ; -"maps" &:: "filter" <>$ arg $+ arg $--> Maps.filter
    ; -"maps" &:: "is_key" <>$ arg $+ arg $--> Maps.is_key
    ; -"maps" &:: "get" <>$ arg $+ arg $--> Maps.get
    ; -"maps" &:: "put" <>$ arg $+ arg $+ arg $--> Maps.put
    ; -"maps" &:: "merge" <>$ arg $+ arg $--> Maps.merge
    ; -"maps" &:: "new" <>$$--> Maps.new_
    ; +BuiltinDecl.(match_builtin __erlang_make_tuple) &++> Tuples.make
    ; -erlang_ns &:: "is_atom" <>$ arg $--> BIF.is_atom
    ; -erlang_ns &:: "is_boolean" <>$ arg $--> BIF.is_boolean
    ; -erlang_ns &:: "is_list" <>$ arg $--> BIF.is_list
    ; -erlang_ns &:: "is_map" <>$ arg $--> BIF.is_map ]
