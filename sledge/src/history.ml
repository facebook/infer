(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** An execution history is a current instruction pointer and some
    predecessors. [preds] are empty iff this is an entrypoint. *)
type t = {curr: Llair.IP.t; preds: t iarray} [@@deriving sexp_of]

let init ip = {curr= ip; preds= IArray.empty}
let extend curr preds = {curr; preds= IArray.of_list preds}

let dump h fs =
  (* todo: output nicely-formatted DAG; just printing a single
     arbitrarily-chosen witness path from the root for now. *)
  let path =
    let rec path_impl h =
      let tail =
        if IArray.is_empty h.preds then []
        else path_impl (IArray.get h.preds 0)
      in
      if Llair.IP.index h.curr = 0 || IArray.length h.preds > 1 then
        h.curr :: tail
      else tail
    in
    path_impl >> List.rev
  in
  let pp_ip fs ip =
    let open Llair in
    Format.fprintf fs "%a%a%a" FuncName.pp (IP.block ip).parent.name IP.pp
      ip Loc.pp (IP.loc ip)
  in
  Format.fprintf fs "@[<v 2>Witness Trace:@ %a@]" (List.pp "@ " pp_ip)
    (path h)
