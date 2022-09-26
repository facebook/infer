(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** An execution history is a current instruction pointer and some
    predecessors. [preds] are empty iff this is an entrypoint. *)
type t =
  | Init
  | Step of {curr: Llair.IP.t; preds: t iarray}
  | Goal_progress of t
[@@deriving sexp_of]

let init = Init
let extend curr preds = Step {curr; preds= IArray.of_list preds}
let progress_goal x = Goal_progress x

let dump ?(show_root = false) h fs =
  (* todo: output nicely-formatted DAG; just printing a single
     arbitrarily-chosen witness path from the root for now. *)
  let path =
    let rec path_impl = function
      | Init -> []
      | Goal_progress path -> path_impl path
      | Step {curr; preds} ->
          let tail =
            if IArray.is_empty preds then []
            else path_impl (IArray.get preds 0)
          in
          if Llair.IP.index curr = 0 || IArray.length preds > 1 then
            let goal_progress =
              IArray.exists preds ~f:(function
                | Goal_progress _ -> true
                | _ -> false )
            in
            (curr, goal_progress) :: tail
          else tail
    in
    path_impl >> List.rev
  in
  let pp_ip fs ip =
    let open Llair in
    let block = IP.block ip in
    Format.fprintf fs "#%-8i%a%a" block.sort_index FuncName.pp
      block.parent.name Loc.pp (IP.loc ip)
  in
  let pp_path fs path =
    List.foldi path (not show_root)
      ~f:(fun idx (ip, goal_progress) seen_root ->
        let is_root = goal_progress && not seen_root in
        Format.fprintf fs "@ %6i: %-8s%a" idx
          (if is_root then "ROOT:" else "")
          pp_ip ip ;
        seen_root || goal_progress )
    |> (ignore : bool -> unit)
  in
  Format.fprintf fs "@[<v 2>Witness Trace:%a@]" pp_path (path h)
