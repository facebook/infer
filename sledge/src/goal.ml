(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type t [@@deriving compare, equal, sexp_of]
  type status [@@deriving compare]

  val status : t -> Llair.block option -> status
  val pp : t pp
  val reached : t -> bool
  val update_after_call : Llair.Function.t -> t -> t
  val initialize : pgm:Llair.program -> entry:Llair.block -> t -> unit
end

module Undirected = struct
  type t = unit [@@deriving compare, equal, sexp_of]
  type status = unit [@@deriving compare]

  let status _ _ = ()
  let pp _ppf _ = ()
  let reached _ = false
  let update_after_call _ _ = ()
  let initialize ~pgm:_ ~entry:_ _ = ()
end

module Sparse_trace = struct
  type t = {cursor: int; trace: Llair.Function.t iarray}
  [@@deriving compare, equal, sexp_of]

  let pp ppf {cursor; trace} =
    Format.fprintf ppf "[" ;
    IArray.iteri trace ~f:(fun idx fn ->
        let arrow = if Int.equal cursor idx then "*->" else "->" in
        Format.fprintf ppf "%s@,%a" arrow Llair.Function.pp fn ) ;
    Format.fprintf ppf "]"

  let reached {cursor; trace} = Int.equal cursor (IArray.length trace)

  let update_after_call fn ({cursor; trace} as goal) =
    if
      cursor < IArray.length trace
      && Llair.Function.equal fn (IArray.get trace cursor)
    then {goal with cursor= cursor + 1}
    else goal

  exception Failed_lookup of string

  let of_file_exn file pgm =
    let lookup x =
      match Llair.(Func.find x pgm.functions) with
      | Some f -> f.name
      | None ->
          raise
            (Failed_lookup
               ("Unable to resolve function " ^ x ^ " in goal trace " ^ file)
            )
    in
    List.map ~f:lookup (In_channel.read_lines file)
    |> IArray.of_list
    |> fun trace -> {cursor= 0; trace}

  let initialize ~pgm ~entry {trace; _} =
    Llair.Program.compute_distances ~entry ~trace pgm

  let dist_to_next_checkpoint (({trace; cursor} as goal), block) =
    if reached goal then 0
    else
      let next_fn = IArray.get trace cursor in
      Option.value ~default:Int.max_int
        Llair.(Function.Map.find next_fn block.checkpoint_dists)

  type status = t * Llair.block option

  let status t b = (t, b)

  (** Compare using trace progress first, then using distance to next
      checkpoint if trace progress is equal. Note that [(g,blk)] <
      [(g',blk')] if we should prioritize a worklist element at [blk] with
      goal [g] over one at [blk'] with [g']. So, this effects a depth-first
      search currently, greedily prioritizing whichever element is closest
      to reaching the goal. *)
  let compare_status =
    let open Ord.Infix in
    (* [dist_to_next_checkpoint] yields [Int.max_int] when we don't know of
       any path to the next checkpoint, and [b]/[b'] are [None] when a
       thread is terminated. So, this orders (1) states with shorter paths
       to the checkpoint <= (2) states with longer paths to the checkpoint
       <= (3) states with no known paths to the checkpoint <= (4) states in
       terminated threads. *)
    let compare_dist_to_next_checkpoint (g, b) (g', b') =
      match (b, b') with
      | None, None -> 0
      | None, _ -> 1
      | _, None -> -1
      | Some b, Some b' ->
          if b == b' then 0
          else
            let d = dist_to_next_checkpoint (g, b) in
            let d' = dist_to_next_checkpoint (g', b') in
            Int.compare d d'
    in
    (Int.compare >|= fun (g, _) -> g.cursor)
    @? compare_dist_to_next_checkpoint
end
