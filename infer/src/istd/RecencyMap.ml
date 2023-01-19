(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
module F = Format

module type Config = sig
  val limit : int
end

module type S = sig
  type t [@@deriving compare, equal]

  type key

  type value

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit

  val empty : t

  val add : key -> value -> t -> t

  val bindings : t -> (key * value) list

  val exists : t -> f:(key * value -> bool) -> bool

  val for_all : t -> f:(key * value -> bool) -> bool

  val filter : t -> f:(key * value -> bool) -> t

  val find_opt : key -> t -> value option

  val iter : t -> f:(key * value -> unit) -> unit

  val fold : t -> init:'acc -> f:('acc -> key * value -> 'acc) -> 'acc

  val fold_map : t -> init:'acc -> f:('acc -> key -> value -> 'acc * value) -> 'acc * t

  val is_empty : t -> bool

  val map : t -> f:(value -> value) -> t

  val mapi : t -> f:(key -> value -> value) -> t

  val mem : t -> key -> bool

  val union_left_biased : t -> t -> t

  val to_seq : t -> (key * value) Seq.t
end

module Make
    (Key : PrettyPrintable.PrintableEquatableOrderedType)
    (Value : PrettyPrintable.PrintableOrderedType)
    (Config : Config) : S with type key = Key.t and type value = Value.t = struct
  type key = Key.t [@@deriving compare]

  type value = Value.t [@@deriving compare]

  (* suppress warnings about using {!List.Assoc.compare} since our own compare function also ignores
     that different representations of a [t] can have the same meaning *)
  [@@@alert "-deprecated"]

  (** [new_] and [old] together represent the map. Keys may be present in both [old] and [new_], in
      which case bindings in [new_] take precendence.

      The idea is to add to [new_] whenever an element is modified. When the size of [new_] passes
      the limit, throw away [old] and move [new_] to [old] (an O(1) cleanup operation). This way the
      [Config.limit] most recently modified elements will always be in the map, but we may
      temporarily have up to [2*Config.limit] elements in memory.

      Adding elements requires scanning [new_] to possibly remove it, which is [O(Config.limit)].
      This is assumed small (eg, around 50). *)
  type t =
    { count_new: int  (** invariant: [count_new] is [M.length new_] *)
    ; new_: (key, value) List.Assoc.t  (** invariant: [List.length new_ ≤ Config.limit] *)
    ; old: (key, value) List.Assoc.t  (** invariant: [List.length old ≤ Config.limit] *) }
  [@@deriving compare]

  [@@@alert "+deprecated"]

  let equal = [%compare.equal: t]

  let empty = {count_new= 0; old= []; new_= []}

  let rec remove_aux front_rev key = function
    | [] ->
        List.rev front_rev
    | (key', _) :: rest when Key.equal key key' ->
        List.rev_append front_rev rest
    | binding :: rest ->
        remove_aux (binding :: front_rev) key rest


  (** optimised: unroll [map.new_] a few times to try and avoid allocating unecessary intermediate
      lists *)
  let remove_from_new key map =
    match map.new_ with
    | [] ->
        (false, [])
    | (key', _) :: rest when Key.equal key key' ->
        (true, rest)
    | binding :: (key', _) :: rest when Key.equal key key' ->
        (true, binding :: rest)
    | binding1 :: binding2 :: (key', _) :: rest when Key.equal key key' ->
        (true, binding1 :: binding2 :: rest)
    | [_] | [_; _] | [_; _; _] ->
        (* not found in the unrollings and no more elements *) (false, map.new_)
    | binding1 :: binding2 :: binding3 :: rest ->
        if List.exists rest ~f:(fun (key', _) -> Key.equal key key') then
          (true, binding1 :: binding2 :: binding3 :: remove_aux [] key rest)
        else (false, map.new_)


  (** {2 External API} *)

  let is_empty map = map.count_new = 0 && List.is_empty map.old

  let find_opt key map =
    match List.Assoc.find ~equal:Key.equal map.new_ key with
    | Some _ as value_yes ->
        value_yes
    | None ->
        (* Ideally we would want to put the binding in [new_] to reflect that it was recently
           accessed but that would imply returning the new map, which is odd for a [find_opt]
           function. Could be done in the future. *)
        List.Assoc.find ~equal:Key.equal map.old key


  let mem map key = find_opt key map |> Option.is_some

  let add key value map =
    let removed_from_new, new_without_key = remove_from_new key map in
    (* compute the final count first *)
    let next_count_new = if removed_from_new then map.count_new else map.count_new + 1 in
    (* shrink if we are about to go past the limit size of [new_] *)
    if next_count_new > Config.limit then {count_new= 1; new_= [(key, value)]; old= new_without_key}
    else {count_new= next_count_new; new_= (key, value) :: new_without_key; old= map.old}


  let to_seq map =
    Seq.append (Caml.List.to_seq map.new_)
      ( (* this is quadratic time but the lists are at most [Config.limit] long, assumed small *)
        Caml.List.to_seq map.old
      |> Seq.filter (fun binding -> not (List.Assoc.mem ~equal:Key.equal map.new_ (fst binding))) )


  let fold map ~init ~f = Seq.fold_left f init (to_seq map)

  let iter map ~f = fold map ~init:() ~f:(fun () x -> f x)

  let bindings map = to_seq map |> Caml.List.of_seq

  let exists map ~f =
    List.exists map.new_ ~f
    || List.exists map.old ~f:(fun binding ->
           if List.Assoc.mem ~equal:Key.equal map.new_ (fst binding) then false else f binding )


  let for_all map ~f =
    List.for_all map.new_ ~f
    && List.for_all map.old ~f:(fun binding ->
           List.Assoc.mem ~equal:Key.equal map.new_ (fst binding) || f binding )


  let filter map ~f =
    let count_new = ref map.count_new in
    let f_update_count_new binding =
      let r = f binding in
      if not r then decr count_new ;
      r
    in
    let new_ = List.filter map.new_ ~f:f_update_count_new in
    let old = List.filter map.old ~f in
    {new_; count_new= !count_new; old}


  let pp fmt map =
    let pp_item fmt (k, v) = F.fprintf fmt "%a -> %a" Key.pp k Value.pp v in
    IContainer.pp_collection ~pp_item fmt map ~fold


  let mapi m ~f =
    let new_ = List.map m.new_ ~f:(fun (key, value) -> (key, f key value)) in
    let old =
      (* garbage-collect since we are building a new list anyway *)
      List.filter_map m.old ~f:(fun (key, value) ->
          if List.Assoc.mem ~equal:Key.equal m.new_ key then None else Some (key, f key value) )
    in
    {m with new_; old}


  let map m ~f = mapi m ~f:(fun _ value -> f value)

  let fold_map m ~init ~f =
    let acc_ref = ref init in
    let m' =
      mapi m ~f:(fun key value ->
          let acc, value' = f !acc_ref key value in
          acc_ref := acc ;
          value' )
    in
    (!acc_ref, m')


  (** return [List.take (l1 @ l2) Config.limit], the length thereof, and the rest of [l1 @ l2] *)
  let concat_and_spill l1 count1 l2 =
    if Int.equal count1 0 then (l2, List.length l2, [])
    else if Int.equal count1 Config.limit then (l1, count1, l2)
    else
      (* take elements from the start of [l2] to add to [l1], in order to preserve recency ordering
         *)
      let l2_rev, count_l2, rest_rev =
        List.fold l2 ~init:([], 0, [])
          ~f:(fun ((l2_rev, count_l2, rest_rev) as acc) ((key2, _) as binding) ->
            if count_l2 + count1 >= Config.limit then (l2_rev, count_l2, binding :: rest_rev)
            else if List.Assoc.mem ~equal:Key.equal l1 key2 then acc
            else (binding :: l2_rev, count_l2 + 1, rest_rev) )
      in
      (l1 @ List.rev l2_rev, count1 + count_l2, List.rev rest_rev)


  let union_left_biased map1 map2 =
    (* first take everything from [map1] *)
    let new_, count_new, rest =
      (* hack: reverse because [bindings] reverses the recency order (clients do not need to know
         that but we care) *)
      let bindings1 = List.rev (bindings map1) in
      let count = List.length bindings1 in
      if count <= Config.limit then (bindings1, count, [])
      else
        let new_, rest = List.split_n bindings1 Config.limit in
        (new_, Config.limit, rest)
    in
    let new_, count_new, rest =
      if count_new < Config.limit then
        (* still room: fill with [map2.new_] *)
        concat_and_spill new_ count_new map2.new_
      else (new_, count_new, rest)
    in
    let new_, count_new, rest =
      if count_new < Config.limit then
        (* still room: fill with [map2.old] *)
        concat_and_spill new_ count_new map2.old
      else (new_, count_new, rest)
    in
    (* Note: we haven't bothered trying to fill [rest] as much as possible because the contract is
       to keep between [Config.limit] and [2*Config.limit] elements so this is good enough. *)
    {new_; count_new; old= rest}
end
