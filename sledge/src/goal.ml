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
  val update_after_call : Llair.FuncName.t -> t -> t
  val update_after_retn : Llair.FuncName.t -> t -> t
  val initialize : pgm:Llair.program -> entry:Llair.block -> t -> unit
end

module Undirected = struct
  type t = unit [@@deriving compare, equal, sexp_of]
  type status = unit [@@deriving compare]

  let status _ _ = ()
  let pp _ppf _ = ()
  let reached _ = false
  let update_after_call _ _ = ()
  let update_after_retn _ _ = ()
  let initialize ~pgm:_ ~entry:_ _ = ()
end

module Sparse_trace = struct
  type direction = Call | Retn [@@deriving compare, equal, sexp_of]

  type checkpoint = direction * Llair.FuncName.t
  [@@deriving compare, equal, sexp_of]

  let pp_checkpoint ppf (dir, fn) =
    let dir_string = match dir with Call -> "" | Retn -> "retn:" in
    Format.fprintf ppf "%s%a" dir_string Llair.FuncName.pp fn

  let is_call (dir, _) = match dir with Call -> true | Retn -> false
  let is_ret = is_call >> not
  let function_of_cp (_, f) = f

  type t = {cursor: int; trace: checkpoint iarray}
  [@@deriving compare, equal, sexp_of]

  let pp ppf {cursor; trace} =
    Format.fprintf ppf "[" ;
    IArray.iteri trace ~f:(fun idx cp ->
        let arrow = if Int.equal cursor idx then "*->" else "->" in
        Format.fprintf ppf "%s@,%a" arrow pp_checkpoint cp ) ;
    Format.fprintf ppf "]"

  let reached {cursor; trace} = Int.equal cursor (IArray.length trace)

  let update_after_call fn ({cursor; trace} as goal) =
    if
      cursor < IArray.length trace
      &&
      match IArray.get trace cursor with
      | Call, fn' -> Llair.FuncName.equal fn fn'
      | Retn, _ -> false
    then (
      [%Dbg.info "reached %a in %a" Llair.FuncName.pp fn pp goal] ;
      {goal with cursor= cursor + 1} )
    else goal

  let update_after_retn fn ({cursor; trace} as goal) =
    if
      cursor < IArray.length trace
      &&
      match IArray.get trace cursor with
      | Call, _ -> false
      | Retn, fn' -> Llair.FuncName.equal fn fn'
    then (
      [%Dbg.info "reached %a in %a" pp_checkpoint (Retn, fn) pp goal] ;
      {goal with cursor= cursor + 1} )
    else goal

  exception Invalid_trace of string

  let of_fns_exn fns pgm =
    let lookup x =
      match Llair.(Func.find x pgm.functions) with
      | Some f -> f.name
      | None ->
          raise
            (Invalid_trace
               ("Unable to resolve function " ^ x ^ " in goal trace") )
    in
    let src_trace, snk_trace =
      List.take_drop_while
        ~f:(not << String.equal "__sledge_trace_separator__")
        fns
    in
    if List.is_empty src_trace then
      raise (Invalid_trace "Malformed trace: src_trace must not be empty") ;
    let src_trace_fns = List.map ~f:lookup src_trace in
    let src_calls = List.map ~f:(fun f -> (Call, f)) src_trace_fns in
    let trace =
      IArray.of_list
      @@
      if List.is_empty snk_trace then src_calls
      else
        List.(
          let src_rets =
            drop 1 src_trace_fns |> map ~f:(fun f -> (Retn, f)) |> rev
          in
          let snk_calls =
            drop 1 snk_trace |> map ~f:(fun f -> (Call, lookup f))
          in
          src_calls @ src_rets @ snk_calls)
    in
    {cursor= 0; trace}

  let src_trace =
    IArray.to_iter
    >> Iter.(take_while ~f:is_call >> map ~f:function_of_cp)
    >> IArray.of_iter

  let snk_trace =
    IArray.to_iter
    >> Iter.(
         drop_while ~f:is_call
         >> drop_while ~f:is_ret
         >> map ~f:function_of_cp)
    >> IArray.of_iter

  let initialize ~pgm ~entry {trace; _} =
    let src_trace = src_trace trace in
    let snk_trace = snk_trace trace in
    match
      Llair.Program.compute_distances ~entry ~src_trace ~snk_trace pgm
    with
    | Result.Ok _ -> ()
    | Result.Error dp_path -> Report.unreachable_goal ~dp_path

  let dist_to_next_checkpoint (({cursor; _} as goal), block) =
    if reached goal then 0
    else
      Option.value ~default:Int.max_int
        (Int.Map.find cursor block.Llair.checkpoint_dists)

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
