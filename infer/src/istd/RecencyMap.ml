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
  type t [@@deriving compare]

  type key

  type value

  val equal : (value -> value -> bool) -> t -> t -> bool

  val pp : F.formatter -> t -> unit

  val empty : t

  val add : key -> value -> t -> t

  val find_opt : key -> t -> value option

  val fold : t -> init:'acc -> f:('acc -> key -> value -> 'acc) -> 'acc

  val fold_bindings : t -> init:'acc -> f:('acc -> key * value -> 'acc) -> 'acc

  val fold_map : t -> init:'acc -> f:('acc -> value -> 'acc * value) -> 'acc * t

  val is_empty : t -> bool

  val bindings : t -> (key * value) list

  val union : f:(key -> value -> value -> value option) -> t -> t -> t

  val merge : f:(key -> value option -> value option -> value option) -> t -> t -> t
end

module Make
    (Key : PrettyPrintable.PrintableOrderedType)
    (Value : PrettyPrintable.PrintableOrderedType)
    (Config : Config) : S with type key = Key.t and type value = Value.t = struct
  type key = Key.t

  type value = Value.t [@@deriving compare]

  module M = Caml.Map.Make (Key)

  (** [new_] and [old] together represent the map. Keys may be present in both [old] and [new_], in
      which case bindings in [new_] take precendence.

      The idea is to add to [new_] whenever an element is modified. When the size of [new_] passes
      the limit, throw away [old] and move [new_] to [old] (an O(1) cleanup operation). This way the
      [Config.limit] most recently modified elements will always be in the map, but we may
      temporarily have up to [2*Config.limit] elements in memory.

      Using [Caml.Map]s as our backing data-structure allows us to avoid having to scan the [new_]
      collection before adding an element in order to avoid adding it multiple times (which would
      force us to either go over the [2*Config.limit] max number of elements or risk us losing some
      of the [N] most recently-used keys because of repeating keys). *)
  type t =
    { count_new: int  (** invariant: [count_new] is [M.length new_] *)
    ; new_: value M.t  (** invariant: [M.length new_ ≤ Config.limit] *)
    ; old: value M.t
          (** invariant: [M.length old ≤ Config.limit]. Actually, the length of [old] is always
              either [0] or [N], except possibly after a call to [merge]. *) }
  [@@deriving compare]

  let empty = {count_new= 0; old= M.empty; new_= M.empty}

  let edges map =
    M.union
      (fun _addr edge_new _edge_old -> (* bindings in [new_] take precedence *) Some edge_new)
      map.new_ map.old


  (** {2 External API} *)

  let is_empty map = map.count_new = 0 && M.is_empty map.old

  let find_opt key map =
    match M.find_opt key map.new_ with
    | Some _ as value_yes ->
        value_yes
    | None ->
        (* Ideally we would want to put the binding in [new_] to reflect that it was recently
           accessed but that would imply returning the new map, which is odd for a [find_opt]
           function. Could be done in the future. *)
        M.find_opt key map.old


  let add key value map =
    (* compute the final count first *)
    let next_count_new = if M.mem key map.new_ then map.count_new else map.count_new + 1 in
    (* shrink if we are about to go past the limit size of [new_] *)
    if next_count_new > Config.limit then {count_new= 1; new_= M.singleton key value; old= map.new_}
    else {count_new= next_count_new; new_= M.add key value map.new_; old= map.old}


  let equal equal_value map1 map2 =
    let edges1 = edges map1 in
    let edges2 = edges map2 in
    M.equal equal_value edges1 edges2


  let fold map ~init ~f =
    let acc = M.fold (fun key value acc -> f acc key value) map.new_ init in
    M.fold (fun key value acc -> if M.mem key map.new_ then acc else f acc key value) map.old acc


  let fold_bindings map ~init ~f = fold map ~init ~f:(fun acc k v -> f acc (k, v))

  let pp fmt map =
    let pp_item fmt (k, v) = F.fprintf fmt "%a -> %a" Key.pp k Value.pp v in
    IContainer.pp_collection ~pp_item fmt map ~fold:fold_bindings


  let map m ~f =
    let new_ = M.map f m.new_ in
    let old = M.mapi (fun key value -> if M.mem key m.new_ then value else f value) m.old in
    {m with new_; old}


  let fold_map m ~init ~f =
    let acc_ref = ref init in
    let m' =
      map m ~f:(fun value ->
          let acc, value' = f !acc_ref value in
          acc_ref := acc ;
          value' )
    in
    (!acc_ref, m')


  let bindings map = M.bindings (edges map)

  (** [merge ~f map1 map2] has at least all the elements [M.merge f map1.new_ map2.new_] but if
      there are more than [N] then some will be arbitrarily demoted to [old]. Elements of
      [M.merge f map1.old map2.old] may not make it into the final map if [old] then overflows too.
      In other words: merge new first, overflow arbitrarily into old, then overflow old arbitrarily
      into nothingness. *)
  let merge ~f map1 map2 =
    let count_new = ref 0 in
    let count_old = ref 0 in
    let old = ref M.empty in
    let new_ =
      M.merge
        (fun addr val1_opt val2_opt ->
          match f addr val1_opt val2_opt with
          | None ->
              None
          | Some value as value_opt ->
              incr count_new ;
              if !count_new > Config.limit then (
                incr count_old ;
                old := M.add addr value !old ;
                None )
              else value_opt )
        map1.new_ map2.new_
    in
    (* save [old] as it was after having added just the [new_] components *)
    let old_from_new_ = !old in
    let old =
      M.merge
        (fun addr val1_opt val2_opt ->
          (* Check if we have already seen [addr] when merging the [new_] maps. As bindings in
             [new_] override those in [old] we don't want to add it again in case it was spilled
             into [old_from_new_], but it's fine to add it again if it made it to [new_].

             Also if [!old] is already full then we cannot add any more elements so return [None].
          *)
          if !count_old >= Config.limit || M.mem addr old_from_new_ then None
          else
            match f addr val1_opt val2_opt with
            | None ->
                None
            | Some _ as value_opt ->
                incr count_old ;
                value_opt )
        map1.old map2.old
    in
    {count_new= !count_new; old= M.union (fun _k _v1 _v2 -> assert false) old_from_new_ old; new_}


  (** standard-looking implementation of [union] based on [merge] *)
  let union ~f map1 map2 =
    merge
      ~f:(fun addr val1_opt val2_opt ->
        match (val1_opt, val2_opt) with
        | None, None ->
            None
        | (Some _ as val_), None | None, (Some _ as val_) ->
            val_
        | Some val1, Some val2 ->
            f addr val1 val2 )
      map1 map2
end
