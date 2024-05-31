(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

type call = {proc_name: Procname.t; location: Location.t} [@@deriving compare, equal]

type t = {chain: call list; innermost: call} [@@deriving compare, equal]

let mk location proc_name = {chain= []; innermost= {proc_name; location}}

let get_inner_call cycle = cycle.innermost.proc_name

let get_outer_location cycle =
  match cycle.chain with [] -> cycle.innermost.location | {location} :: _ -> location


let add_call proc_name location cycle = {cycle with chain= {proc_name; location} :: cycle.chain}

let pp fmt cycle =
  let rec pp_chain fmt = function
    | [] ->
        CallEvent.pp fmt (Call cycle.innermost.proc_name)
    | {proc_name} :: chain ->
        F.fprintf fmt "%a -> " CallEvent.pp (CallEvent.Call proc_name) ;
        pp_chain fmt chain
  in
  pp_chain fmt cycle.chain


let get_error_message cycle =
  let pp_cycle fmt cycle =
    match cycle.chain with
    | [] ->
        F.fprintf fmt "recursive call to %a" pp cycle
    | _ :: _ ->
        F.fprintf fmt "mutual recursion cycle: %a -> %a" CallEvent.pp
          (CallEvent.Call cycle.innermost.proc_name) pp cycle
  in
  F.asprintf
    "%a; make sure this is intentional and cannot lead to non-termination or stack overflow"
    pp_cycle cycle


let iter_rotations cycle ~f =
  let rotation = ref cycle in
  for _ = 0 to List.length cycle.chain do
    f !rotation ;
    rotation :=
      match !rotation.chain with
      | [] ->
          (* length is invariant and >0 *) assert false
      | last_call :: chain ->
          {innermost= last_call; chain= chain @ [!rotation.innermost]}
  done


let to_errlog cycle =
  let rec chain_to_errlog prev_call = function
    | [] ->
        Errlog.make_trace_element 0 cycle.innermost.location
          (F.asprintf "%a makes a recursive call to %a" CallEvent.pp (CallEvent.Call prev_call)
             CallEvent.pp (CallEvent.Call cycle.innermost.proc_name) )
          []
        :: []
    | call :: chain ->
        Errlog.make_trace_element 0 call.location
          (F.asprintf "%a calls %a" CallEvent.pp (CallEvent.Call prev_call) CallEvent.pp
             (CallEvent.Call call.proc_name) )
          []
        :: chain_to_errlog call.proc_name chain
  in
  chain_to_errlog cycle.innermost.proc_name cycle.chain


module Set = PrettyPrintable.MakePPSet (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)
