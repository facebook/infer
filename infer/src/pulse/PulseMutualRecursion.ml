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

type inner_call = {call: call; actuals: AbstractValue.t list} [@@deriving compare, equal]

type t = {chain: call list; innermost: inner_call} [@@deriving compare, equal]

let mk location proc_name actuals = {chain= []; innermost= {call= {proc_name; location}; actuals}}

let get_inner_call cycle = cycle.innermost.call.proc_name

let get_outer_location cycle =
  match cycle.chain with [] -> cycle.innermost.call.location | {location} :: _ -> location


let add_call subst proc_name location cycle =
  let exception ArgumentValueNotFound in
  match
    List.map cycle.innermost.actuals ~f:(fun v_callee ->
        match AbstractValue.Map.find_opt v_callee subst with
        | None ->
            raise_notrace ArgumentValueNotFound
        | Some (v_caller, _) ->
            v_caller )
  with
  | exception ArgumentValueNotFound ->
      None
  | innermost_actuals ->
      Some
        { chain= {proc_name; location} :: cycle.chain
        ; innermost= {call= cycle.innermost.call; actuals= innermost_actuals} }


let pp fmt cycle =
  let rec pp_chain fmt = function
    | [] ->
        CallEvent.pp fmt (Call cycle.innermost.call.proc_name)
    | {proc_name} :: chain ->
        F.fprintf fmt "%a -> " CallEvent.pp (CallEvent.Call proc_name) ;
        pp_chain fmt chain
  in
  pp_chain fmt cycle.chain


let get_error_message cycle ~is_call_with_same_values =
  let pp_cycle fmt cycle =
    match cycle.chain with
    | [] ->
        F.fprintf fmt "recursive call to %a" pp cycle
    | _ :: _ ->
        F.fprintf fmt "mutual recursion cycle: %a -> %a" CallEvent.pp
          (CallEvent.Call cycle.innermost.call.proc_name) pp cycle
  in
  F.asprintf "%a; %s" pp_cycle cycle
    ( if is_call_with_same_values then
        "moreover, the same values are passed along the cycle so there is a high chance of an \
         infinite recursion"
      else "make sure this is intentional and cannot lead to non-termination or stack overflow" )


let iter_rotations cycle ~f =
  let rotation = ref cycle in
  for _ = 0 to List.length cycle.chain do
    f !rotation ;
    rotation :=
      match !rotation.chain with
      | [] ->
          (* empty list: no further loop iterations (= no rotations) *)
          !rotation
      | last_call :: chain ->
          { innermost=
              { call= last_call
              ; actuals=
                  []
                  (* HACK: not recording the real actuals but these are not used at the call
                     sites *) }
          ; chain= chain @ [!rotation.innermost.call] }
  done


let to_errlog cycle ~is_call_with_same_values =
  let rec chain_to_errlog prev_call = function
    | [] ->
        Errlog.make_trace_element 0 cycle.innermost.call.location
          (F.asprintf "%a makes a recursive call to %a%s" CallEvent.pp (CallEvent.Call prev_call)
             CallEvent.pp (CallEvent.Call cycle.innermost.call.proc_name)
             (if is_call_with_same_values then " with the same argument values" else "") )
          []
        :: []
    | call :: chain ->
        Errlog.make_trace_element 0 call.location
          (F.asprintf "%a calls %a" CallEvent.pp (CallEvent.Call prev_call) CallEvent.pp
             (CallEvent.Call call.proc_name) )
          []
        :: chain_to_errlog call.proc_name chain
  in
  chain_to_errlog cycle.innermost.call.proc_name cycle.chain


module Set = PrettyPrintable.MakePPSet (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)
