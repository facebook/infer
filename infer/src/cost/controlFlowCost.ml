(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module BasicCost = CostDomain.BasicCost
module Node = ProcCfg.DefaultNode

module Item = struct
  type t = [`Node of Node.id | `Edge of Node.id * Node.id] [@@deriving equal]

  let compare : t -> t -> int =
   fun x y ->
    match (x, y) with
    | `Node id1, `Node id2 ->
        Node.compare_id id1 id2
    | `Node _, `Edge _ ->
        -1
    | `Edge _, `Node _ ->
        1
    | `Edge (f1, t1), `Edge (f2, t2) ->
        [%compare: Node.id * Node.id] (f1, t1) (f2, t2)


  let pp : F.formatter -> t -> unit =
   fun fmt -> function
    | `Node id ->
        F.fprintf fmt "Node(%a)" Node.pp_id id
    | `Edge (f, t) ->
        F.fprintf fmt "Edge(%a -> %a)" Node.pp_id f Node.pp_id t


  let normalize ~(normalizer : t -> [> t]) (x : t) : t =
    match normalizer x with #t as x -> x | _ -> assert false
end

module Sum = struct
  type 'a set = (* non-empty sorted list *) 'a list [@@deriving compare, equal]

  type t = [`Sum of int * Item.t set] [@@deriving compare, equal]

  let of_list l =
    let length = List.length l in
    let set = List.sort ~compare:Item.compare l in
    `Sum (length, set)


  let pp : F.formatter -> t -> unit = fun fmt (`Sum (_, set)) -> Pp.seq ~sep:" + " Item.pp fmt set

  let items (`Sum (_, l)) = l

  let normalized_items ~normalizer (`Sum (_, l)) =
    let normalizer = (normalizer :> Item.t -> [> Item.t]) in
    l |> List.rev_map ~f:(Item.normalize ~normalizer)


  let normalize ~normalizer sum = sum |> normalized_items ~normalizer |> of_list

  (* Given a sum and an item, remove one occurence of the item in the sum. Returns [None] if the item is not present in the sum.
     [remove_one_item ~item:A (A + B)] = B
     [remove_one_item ~item:A (A + B + C)] = B + C
     [remove_one_item ~item:A (A + A + B)] = A + B
     [remove_one_item ~item:A (B + C)] = None
  *)
  let remove_one_item ~item (`Sum (len, l)) =
    match IList.remove_first l ~f:(Item.equal item) with
    | None ->
        None
    | Some [e] ->
        Some (e :> [Item.t | t])
    | Some l ->
        Some (`Sum (len - 1, l))


  let cost ~of_item (`Sum (_, l)) =
    List.fold l ~init:BasicCost.zero ~f:(fun cost item -> BasicCost.plus cost (of_item item))
end

type t = [Item.t | Sum.t] [@@deriving equal]

let compare : t -> t -> int =
 fun x y ->
  match (x, y) with
  | (#Item.t as x), (#Item.t as y) ->
      Item.compare x y
  | #Item.t, #Sum.t ->
      -1
  | #Sum.t, #Item.t ->
      1
  | (#Sum.t as x), (#Sum.t as y) ->
      Sum.compare x y


let make_node node = `Node node

let make_pred_edge succ pred = `Edge (pred, succ)

let make_succ_edge pred succ = `Edge (pred, succ)

let pp : F.formatter -> t -> unit =
 fun fmt -> function #Item.t as item -> Item.pp fmt item | #Sum.t as sum -> Sum.pp fmt sum


let sum : Item.t list -> t = function [] -> assert false | [e] -> (e :> t) | l -> Sum.of_list l

module Set = struct
  type elt = t [@@deriving compare, equal]

  type t =
    { mutable size: int
    ; mutable items: Item.t ARList.t
    ; mutable sums: Sum.t ARList.t
    ; mutable cost: BasicCost.t }

  let create e =
    let items, sums =
      match e with
      | #Item.t as item ->
          (ARList.singleton item, ARList.empty)
      | #Sum.t as sum ->
          (ARList.empty, ARList.singleton sum)
    in
    {size= 1; items; sums; cost= BasicCost.top}


  let compare_size {size= size1} {size= size2} = Int.compare size1 size2

  (* Invalidation is just a sanity check, union-find already takes care of it. *)
  let is_valid {size} = size >= 1

  let cost {cost} = cost

  (* move semantics, should not be called with aliases *)
  let merge ~from ~to_ =
    assert (not (phys_equal from to_)) ;
    assert (is_valid from) ;
    assert (is_valid to_) ;
    to_.size <- to_.size + from.size ;
    to_.items <- ARList.append to_.items from.items ;
    to_.sums <- ARList.append to_.sums from.sums ;
    from.size <- 0


  let pp_equalities fmt t =
    ARList.append (t.items :> elt ARList.t) (t.sums :> elt ARList.t)
    |> IContainer.to_rev_list ~fold:ARList.fold_unordered
    |> List.sort ~compare |> Pp.seq ~sep:" = " pp fmt


  let normalize_sums : normalizer:(elt -> elt) -> t -> unit =
   fun ~normalizer t ->
    t.sums <-
      t.sums
      |> IContainer.rev_map_to_list ~fold:ARList.fold_unordered ~f:(Sum.normalize ~normalizer)
      |> List.dedup_and_sort ~compare:Sum.compare
      |> ARList.of_list


  let infer_equalities_by_removing_item ~on_infer t item =
    t.sums
    |> IContainer.rev_filter_map_to_list ~fold:ARList.fold_unordered ~f:(Sum.remove_one_item ~item)
    |> IContainer.iter_consecutive ~fold:List.fold ~f:on_infer


  let sum_items t =
    t.sums
    |> ARList.fold_unordered ~init:ARList.empty ~f:(fun acc sum ->
           sum |> Sum.items |> ARList.of_list |> ARList.append acc )
    |> IContainer.to_rev_list ~fold:ARList.fold_unordered
    |> List.dedup_and_sort ~compare:Item.compare


  let infer_equalities_from_sums :
      on_infer:(elt -> elt -> unit) -> normalizer:(elt -> elt) -> t -> unit =
   fun ~on_infer ~normalizer t ->
    normalize_sums ~normalizer t ;
    (* Keep in mind that [on_infer] can modify [t].
       It happens only if we merge a node while infering equalities from it, i.e. in the case an item appears in an equality class both alone and in two sums, i.e. X = A + X = A + B.
       This is not a problem here (we could stop if it happens but it is not necessary as existing equalities still remain true after merges) *)
    (* Also keep in mind that the current version, in the worst-case scenario, is quadratic-ish in the size of the CFG *)
    sum_items t |> List.iter ~f:(fun item -> infer_equalities_by_removing_item ~on_infer t item)


  let init_cost : of_node:(Node.id -> BasicCost.t) -> t -> unit =
   fun ~of_node t ->
    let min_if_node cost item =
      match item with `Node node -> BasicCost.min_default_left cost (of_node node) | _ -> cost
    in
    t.cost <- ARList.fold_unordered t.items ~init:t.cost ~f:min_if_node


  let improve_cost_from_sums :
         on_improve:(Sum.t -> BasicCost.t -> BasicCost.t -> unit)
      -> of_item:(Item.t -> BasicCost.t)
      -> t
      -> unit =
   fun ~on_improve ~of_item t ->
    let f sum =
      let cost_of_sum = Sum.cost ~of_item sum in
      let new_cost = BasicCost.min_default_left t.cost cost_of_sum in
      if not (BasicCost.leq ~lhs:t.cost ~rhs:new_cost) then (
        on_improve sum cost_of_sum new_cost ;
        t.cost <- new_cost )
    in
    Container.iter t.sums ~fold:ARList.fold_unordered ~f


  let improve_cost_with t cost' =
    let old_cost = t.cost in
    let new_cost = BasicCost.min_default_left old_cost cost' in
    if not (BasicCost.leq ~lhs:old_cost ~rhs:new_cost) then (
      t.cost <- new_cost ;
      Some old_cost )
    else None
end
