(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module Collection = PulseModelsGenericArrayBackedCollection
module GenericMapCollection = PulseModelsCpp.GenericMapCollection

(* Models of [std::deque] and of the node-based containers of libc++ ([std::list], [std::map],
   [std::set], [std::unordered_map], ...) and of their iterators.

   The elements of a container are the cells of its backing array so that [clear()] can invalidate
   all the elements known to the analysis. An iterator stores the address of its element, or the
   past-the-end value of the container, in [node_field], and the backing array of the container in
   [Collection.field]. Insertions do not invalidate the elements of the node-based containers; the
   invalidation of the iterators of an unordered container by rehashing is not modelled.

   [std::deque] iterators also store the current [token_field] of the container: insertions into a
   deque invalidate the token, that is all the iterators, but no element (insertions in the middle of
   a deque also invalidate references, which is not modelled).

   [begin()] and [find()] return the end iterator only when the container is known to be empty so
   that a lookup is not reported for lack of a comparison with [end()]. *)

let node_field = Fieldname.make PulseOperations.pulse_model_type "__infer_node"

let token_field = Fieldname.make PulseOperations.pulse_model_type "__infer_iterators_token"

let desc container name = Format.asprintf "%a::%s()" Invalidation.pp_std_container container name

let has_token (container : Invalidation.std_container) =
  match container with Deque -> true | _ -> false


let load_field path location obj field astate =
  PulseOperations.eval_deref_access path Read location obj (FieldAccess field) astate


let store_field path location event obj field (value, hist) astate =
  PulseOperations.write_deref_field path location ~ref:obj field
    ~obj:(value, Hist.add_event event hist)
    astate


(* The element is added to the post only, and the array is not marked as written to since reading
   a container does not modify it. An element abduced in the pre and compared with [end()], also
   read from the pre, would be an assumption on the heap of the caller that makes all the paths
   after a loop over a container parameter latent. *)
let add_fresh_element arr astate =
  let elem = (AbstractValue.mk_fresh (), snd arr) in
  let edges =
    AbductiveDomain.find_post_cell_opt (fst arr) astate
    |> Option.value_map ~default:UnsafeMemory.Edges.empty ~f:fst
    |> UnsafeMemory.Edges.add (ArrayAccess (StdTyp.void, AbstractValue.mk_fresh ())) elem
  in
  (AbductiveDomain.set_post_edges (fst arr) edges astate, elem)


let fresh_element path location container astate =
  let+ astate, arr = Collection.eval path Read location container astate in
  add_fresh_element arr astate


let set_size_positive path location event container astate =
  let size = AbstractValue.mk_fresh () in
  let=* astate =
    PulseOperations.write_deref_field path location ~ref:container Collection.size_field
      ~obj:(size, Hist.single_event event)
      astate
  in
  PulseArithmetic.and_positive size astate


let make_iterator ~has_token path location event container iter elem astate =
  let* astate, arr = Collection.eval path Read location container astate in
  let* astate = store_field path location event iter Collection.field arr astate in
  let* astate = store_field path location event iter node_field elem astate in
  if has_token then
    let* astate, token = load_field path location container token_field astate in
    store_field path location event iter token_field token astate
  else Ok astate


let copy_iterator ~has_token path location event ~src ~dst astate =
  let fields = Collection.field :: node_field :: (if has_token then [token_field] else []) in
  PulseResult.list_fold fields ~init:astate ~f:(fun astate field ->
      let* astate, value = load_field path location src field astate in
      store_field path location event dst field value astate )


(** check that [iter] is valid, and that it is not the end iterator unless [allow_end], then return
    its element and the backing array of its container *)
let check_iterator ~has_token ~allow_end path location iter astate =
  let* astate, elem = load_field path location iter node_field astate in
  let* astate =
    if allow_end then Ok astate else Collection.Iterator.check_not_end location elem astate
  in
  let* astate =
    if has_token then
      let* astate, token = load_field path location iter token_field astate in
      PulseOperations.check_addr_access path NoAccess location token astate
    else Ok astate
  in
  let* astate = PulseOperations.check_addr_access path NoAccess location elem astate in
  (* [clear()] invalidates the backing array, hence the iterators obtained before *)
  let* astate, arr = load_field path location iter Collection.field astate in
  let+ astate = PulseOperations.check_addr_access path NoAccess location arr astate in
  (astate, elem, arr)


(* the fields of the element are invalidated too, e.g. the mapped value of a map entry *)
let invalidate_element path location cause elem astate =
  let rec invalidate_fields (visited, astate) addr =
    Memory.fold_edges (fst addr) astate ~init:(visited, astate)
      ~f:(fun (visited, astate) (access, field_addr) ->
        match (access : Access.t) with
        | FieldAccess _ when not (AbstractValue.Set.mem (fst field_addr) visited) ->
            let astate =
              PulseOperations.invalidate path
                (MemoryAccess {pointer= addr; access; hist_obj_default= snd field_addr})
                location cause field_addr astate
            in
            invalidate_fields (AbstractValue.Set.add (fst field_addr) visited, astate) field_addr
        | _ ->
            (visited, astate) )
  in
  let astate = PulseOperations.invalidate path UntraceableAccess location cause elem astate in
  invalidate_fields (AbstractValue.Set.singleton (fst elem), astate) elem |> snd


module Iterator = struct
  let constructor ~has_token ~desc this other : model_no_non_disj =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate = copy_iterator ~has_token path location event ~src:other ~dst:this astate in
    astate


  let operator_assign ~has_token ~desc this other : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate = copy_iterator ~has_token path location event ~src:other ~dst:this astate in
    PulseOperations.write_id ret_id (fst this, Hist.add_event event (snd this)) astate


  let operator_star ~has_token ~desc iter : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate, (elem, hist), _ =
      check_iterator ~has_token ~allow_end:false path location iter astate
    in
    PulseOperations.write_id ret_id (elem, Hist.add_event event hist) astate


  let step ~has_token step path location event iter astate =
    let allow_end = match step with `PlusPlus -> false | `MinusMinus -> true in
    let* astate, _, arr = check_iterator ~has_token ~allow_end path location iter astate in
    let astate, elem = add_fresh_element arr astate in
    store_field path location event iter node_field elem astate


  let operator_step ~has_token step_ ~desc iter : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate = step ~has_token step_ path location event iter astate in
    PulseOperations.write_id ret_id (fst iter, Hist.add_event event (snd iter)) astate


  (** [it++] and [it--] return a copy of the iterator before the step *)
  let operator_step_postfix ~has_token step_ ~desc iter ret_iter : model_no_non_disj =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate =
      let* astate = copy_iterator ~has_token path location event ~src:iter ~dst:ret_iter astate in
      step ~has_token step_ path location event iter astate
    in
    astate


  let operator_compare comparison ~desc iter_lhs iter_rhs : model_no_non_disj =
   fun ({path; location} as model_data) astate ->
    let<*> astate, (elem_lhs, _) = load_field path location iter_lhs node_field astate in
    let<*> astate, (elem_rhs, _) = load_field path location iter_rhs node_field astate in
    Collection.Iterator.compare_values comparison ~desc elem_lhs elem_rhs model_data astate
end

let begin_or_find container ~desc this iter : model_no_non_disj =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let<+> astate =
    let* astate, (size, _) = Collection.to_internal_size_deref path Read location this astate in
    let* astate, elem =
      if PulseArithmetic.is_known_zero astate size then
        Collection.eval_pointer_to_last_element path location this astate
      else fresh_element path location this astate
    in
    make_iterator ~has_token:(has_token container) path location event this iter elem astate
  in
  astate


let end_ container ~desc this iter : model_no_non_disj =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let<+> astate =
    let* astate, end_value = Collection.eval_pointer_to_last_element path location this astate in
    make_iterator ~has_token:(has_token container) path location event this iter end_value astate
  in
  astate


let erase container ~desc this pos ret_iter : model_no_non_disj =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let has_token = has_token container in
  let<++> astate =
    let=* astate, elem, _ = check_iterator ~has_token ~allow_end:false path location pos astate in
    let astate = invalidate_element path location (StdContainer (container, Erase)) elem astate in
    let+* astate = Collection.decrease_size path location this ~desc astate in
    let* astate, next = fresh_element path location this astate in
    make_iterator ~has_token path location event this ret_iter next astate
  in
  astate


let clear container ~desc this : model_no_non_disj =
 fun {path; location} astate ->
  let cause = Invalidation.StdContainer (container, Clear) in
  let<++> astate =
    let=* astate, arr = Collection.eval path NoAccess location this astate in
    let astate =
      Memory.fold_edges (fst arr) astate ~init:astate ~f:(fun astate (access, elem) ->
          match (access : Access.t) with
          (* an element equal to [end()] is the past-the-end value, which [end()] keeps returning *)
          | ArrayAccess _ when not (AddressAttributes.is_end_of_collection (fst elem) astate) ->
              invalidate_element path location cause elem astate
          | _ ->
              astate )
      |> PulseOperations.invalidate_deref_access path location cause this Collection.access
    in
    let=* astate =
      PulseOperations.havoc_deref_field path location this Collection.field
        (Hist.single_call path location desc)
        astate
    in
    Collection.assign_size_constant path location this ~constant:IntLit.zero ~desc astate
  in
  astate


(** [count()] and [contains()] return 0 when the container is known to be empty *)
let count ~desc this : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let ret_val = AbstractValue.mk_fresh () in
  let<++> astate =
    let=* astate, (size, _) = Collection.to_internal_size_deref path Read location this astate in
    let astate =
      PulseOperations.write_id ret_id (ret_val, Hist.single_call path location desc) astate
    in
    if PulseArithmetic.is_known_zero astate size then
      PulseArithmetic.and_eq_int ret_val IntLit.zero astate
    else PulseArithmetic.and_nonnegative ret_val astate
  in
  astate


(* [at()], [front()], and [back()] succeed only on non-empty containers; this is assumed rather
   than pruned, since a pruned size coming from the caller would make all the caller's later issues
   latent *)
let assume_non_empty path location this astate =
  let=* astate, (size, _) = Collection.to_internal_size_deref path Read location this astate in
  PulseArithmetic.and_positive size astate


(** [front()] and [back()] *)
let element_reference ~desc this : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    let** astate = assume_non_empty path location this astate in
    let=+ astate, (elem, hist) = fresh_element path location this astate in
    Ok (PulseOperations.write_id ret_id (elem, Hist.add_event event hist) astate)
  in
  astate


(** [operator[]] and [at()] of maps return a reference to the mapped value of an element;
    [operator[]] inserts an element if needed *)
let mapped_value_reference ~inserts ~desc this : model_no_non_disj =
 fun {path; location; callee_procname; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    let=* astate, elem = fresh_element path location this astate in
    let=* astate, (value, hist) =
      match Procname.get_class_type_name callee_procname with
      | Some (CppClass {template_spec_info= Template {args= TType key_t :: TType value_t :: _}}) ->
          PulseOperations.eval_access path Read location elem
            (GenericMapCollection.pair_second_access key_t value_t)
            astate
      | _ ->
          Ok (astate, elem)
    in
    let astate = PulseOperations.write_id ret_id (value, Hist.add_event event hist) astate in
    if inserts then set_size_positive path location event this astate
    else assume_non_empty path location this astate
  in
  astate


(** insertions into a [std::deque] invalidate all its iterators; references to its elements are not
    invalidated, which is only right for insertions at either end *)
let deque_insertion deque_f ~ret ~desc this args : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    let astate =
      PulseOperations.invalidate_deref_access path location
        (StdContainer (Deque, deque_f))
        this (FieldAccess token_field) astate
    in
    let=* astate =
      PulseOperations.havoc_deref_field path location this token_field (Hist.single_event event)
        astate
    in
    let** astate = set_size_positive path location event this astate in
    match (ret, List.last args) with
    | `Reference, _ ->
        let=+ astate, (elem, hist) = fresh_element path location this astate in
        Ok (PulseOperations.write_id ret_id (elem, Hist.add_event event hist) astate)
    | `Iterator, Some {FuncArg.arg_payload= ret_iter} ->
        let=* astate, elem = fresh_element path location this astate in
        Sat (make_iterator ~has_token:true path location event this ret_iter elem astate)
    | `Iterator, None | `Nothing, _ ->
        Sat (Ok astate)
  in
  astate


(* the user-facing iterator classes of the modelled libc++ containers *)
let iterators =
  [ "__deque_iterator"
  ; "__hash_const_iterator"
  ; "__hash_map_const_iterator"
  ; "__hash_map_iterator"
  ; "__list_const_iterator"
  ; "__list_iterator"
  ; "__map_const_iterator"
  ; "__map_iterator"
  ; "__tree_const_iterator" ]


let containers : (string * Invalidation.std_container) list =
  [ ("deque", Deque)
  ; ("list", List)
  ; ("map", Map)
  ; ("multimap", Multimap)
  ; ("multiset", Multiset)
  ; ("set", Set)
  ; ("unordered_map", UnorderedMap)
  ; ("unordered_multimap", UnorderedMultimap)
  ; ("unordered_multiset", UnorderedMultiset)
  ; ("unordered_set", UnorderedSet) ]


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let iterator_typs () = List.map iterators ~f:(fun iterator -> -"std" &:: iterator) in
  let iterator_matchers =
    List.concat_map iterators ~f:(fun iterator ->
        let has_token = String.equal iterator "__deque_iterator" in
        let desc name = Printf.sprintf "std::%s::%s()" iterator name in
        [ -"std" &:: iterator &:: iterator $ capt_arg_payload
          $+ capt_arg_payload_of_typ_exists (iterator_typs ())
          $--> Iterator.constructor ~has_token ~desc:(desc iterator)
        ; -"std" &:: iterator &:: "operator=" <>$ capt_arg_payload $+ capt_arg_payload
          $--> Iterator.operator_assign ~has_token ~desc:(desc "operator=")
        ; -"std" &:: iterator &:: "operator*" <>$ capt_arg_payload
          $--> Iterator.operator_star ~has_token ~desc:(desc "operator*")
        ; -"std" &:: iterator &:: "operator->" <>$ capt_arg_payload
          $--> Iterator.operator_star ~has_token ~desc:(desc "operator->")
        ; -"std" &:: iterator &:: "operator++" <>$ capt_arg_payload
          $--> Iterator.operator_step ~has_token `PlusPlus ~desc:(desc "operator++")
        ; -"std" &:: iterator &:: "operator++" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload
          $--> Iterator.operator_step_postfix ~has_token `PlusPlus ~desc:(desc "operator++")
        ; -"std" &:: iterator &:: "operator--" <>$ capt_arg_payload
          $--> Iterator.operator_step ~has_token `MinusMinus ~desc:(desc "operator--")
        ; -"std" &:: iterator &:: "operator--" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload
          $--> Iterator.operator_step_postfix ~has_token `MinusMinus ~desc:(desc "operator--") ] )
  in
  let comparison_matchers =
    [ -"std" &:: "operator=="
      $ capt_arg_payload_of_typ_exists (iterator_typs ())
      $+ capt_arg_payload_of_typ_exists (iterator_typs ())
      $--> Iterator.operator_compare `Equal ~desc:"iterator operator=="
    ; -"std" &:: "operator!="
      $ capt_arg_payload_of_typ_exists (iterator_typs ())
      $+ capt_arg_payload_of_typ_exists (iterator_typs ())
      $--> Iterator.operator_compare `NotEqual ~desc:"iterator operator!=" ]
  in
  let container_matchers =
    List.concat_map containers ~f:(fun (name, container) ->
        let desc = desc container in
        let common =
          [ -"std" &:: name &:: name <>$ capt_arg_payload
            $--> Collection.default_constructor ~desc:(desc name)
          ; -"std" &:: name &:: "begin" <>$ capt_arg_payload $+ capt_arg_payload
            $--> begin_or_find container ~desc:(desc "begin")
          ; -"std" &:: name &:: "cbegin" <>$ capt_arg_payload $+ capt_arg_payload
            $--> begin_or_find container ~desc:(desc "cbegin")
          ; -"std" &:: name &:: "end" <>$ capt_arg_payload $+ capt_arg_payload
            $--> end_ container ~desc:(desc "end")
          ; -"std" &:: name &:: "cend" <>$ capt_arg_payload $+ capt_arg_payload
            $--> end_ container ~desc:(desc "cend")
          ; -"std" &:: name &:: "erase" <>$ capt_arg_payload
            $+ capt_arg_payload_of_typ_exists (iterator_typs ())
            $+ capt_arg_payload
            $--> erase container ~desc:(desc "erase")
          ; -"std" &:: name &:: "clear" <>$ capt_arg_payload
            $--> clear container ~desc:(desc "clear")
          ; -"std" &:: name &:: "empty" <>$ capt_arg_payload
            $--> Collection.empty ~desc:(desc "empty")
          ; -"std" &:: name &:: "size" <>$ capt_arg_payload $--> Collection.size ~desc:(desc "size")
          ]
        in
        let lookups =
          [ -"std" &:: name &:: "find" $ capt_arg_payload $+ any_arg $+ capt_arg_payload
            $--> begin_or_find container ~desc:(desc "find")
          ; -"std" &:: name &:: "count" $ capt_arg_payload $+ any_arg
            $--> count ~desc:(desc "count")
          ; -"std" &:: name &:: "contains" $ capt_arg_payload $+ any_arg
            $--> count ~desc:(desc "contains") ]
        in
        let front_back =
          [ -"std" &:: name &:: "front" <>$ capt_arg_payload
            $--> element_reference ~desc:(desc "front")
          ; -"std" &:: name &:: "back" <>$ capt_arg_payload
            $--> element_reference ~desc:(desc "back") ]
        in
        let specific =
          match (container : Invalidation.std_container) with
          | Deque ->
              front_back
              @ [ -"std" &:: name &:: "operator[]" <>$ capt_arg_payload $+ capt_arg_payload
                  $--> PulseModelsCpp.Vector.at ~desc:(desc "operator[]")
                ; -"std" &:: name &:: "at" <>$ capt_arg_payload $+ capt_arg_payload
                  $--> PulseModelsCpp.Vector.at ~desc:(desc "at")
                ; -"std" &:: name &:: "push_back" <>$ capt_arg_payload
                  $++$--> deque_insertion PushBack ~ret:`Nothing ~desc:(desc "push_back")
                ; -"std" &:: name &:: "push_front" <>$ capt_arg_payload
                  $++$--> deque_insertion PushFront ~ret:`Nothing ~desc:(desc "push_front")
                ; -"std" &:: name &:: "emplace_back" $ capt_arg_payload
                  $++$--> deque_insertion EmplaceBack ~ret:`Reference ~desc:(desc "emplace_back")
                ; -"std" &:: name &:: "emplace_front" $ capt_arg_payload
                  $++$--> deque_insertion EmplaceFront ~ret:`Reference ~desc:(desc "emplace_front")
                ; -"std" &:: name &:: "insert" $ capt_arg_payload
                  $++$--> deque_insertion Insert ~ret:`Iterator ~desc:(desc "insert")
                ; -"std" &:: name &:: "emplace" $ capt_arg_payload
                  $++$--> deque_insertion Emplace ~ret:`Iterator ~desc:(desc "emplace") ]
          | List ->
              front_back
          | Map | UnorderedMap ->
              lookups
              @ [ -"std" &:: name &:: "operator[]" <>$ capt_arg_payload $+ any_arg
                  $--> mapped_value_reference ~inserts:true ~desc:(desc "operator[]")
                ; -"std" &:: name &:: "at" <>$ capt_arg_payload $+ any_arg
                  $--> mapped_value_reference ~inserts:false ~desc:(desc "at") ]
          | Multimap | Multiset | Set | UnorderedMultimap | UnorderedMultiset | UnorderedSet ->
              lookups
        in
        common @ specific )
  in
  iterator_matchers @ comparison_matchers @ container_matchers
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
  |> List.map ~f:(ProcnameDispatcher.Call.map_matcher ~f:lift_model)
