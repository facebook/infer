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

type astate = AbductiveDomain.t

type 'a result = 'a PulseOperationResult.t

type aval = AbstractValue.t * ValueHistory.t

(* we work on a single execution only in ContinueProgram mode *)
type 'a monad = model_data -> astate -> ('a * astate) result

module Syntax = struct
  let ret (a : 'a) : 'a monad = fun _data astate -> Sat (Ok (a, astate))

  let unreachable : 'a monad = fun _ _ -> Unsat

  let bind (x : 'a monad) (f : 'a -> 'b monad) : 'b monad =
   fun data astate ->
    let** a, astate = x data astate in
    f a data astate


  let ( let* ) a f = bind a f

  let list_fold (l : 'a list) ~(init : 'accum) ~(f : 'accum -> 'a -> 'accum monad) : 'accum monad =
    List.fold l ~init:(ret init) ~f:(fun monad a ->
        let* acc = monad in
        f acc a )


  let list_iter (l : 'a list) ~(f : 'a -> unit monad) : unit monad =
    list_fold l ~init:() ~f:(fun () a -> f a)


  let list_filter_map (l : 'a list) ~(f : 'a -> 'b option monad) : 'b list monad =
    let* rev_res =
      list_fold l ~init:[] ~f:(fun acc a ->
          let* opt = f a in
          match opt with None -> ret acc | Some b -> b :: acc |> ret )
    in
    List.rev rev_res |> ret


  let get_data : model_data monad = fun data astate -> ret data data astate

  let sat x = Sat x

  let sat_ok x = Sat (Ok x)

  let ( >> ) f g x = f x |> g

  let start_model (m : unit monad) : model =
   fun data astate ->
    match m data astate with
    | Sat result ->
        [PulseResult.map result ~f:(fun ((), astate) -> ContinueProgram astate)]
    | Unsat ->
        []


  let exec_partial_command (f : astate -> astate result) : unit monad =
   fun _data astate ->
    let++ astate = f astate in
    ((), astate)


  let exec_command (f : astate -> astate) : unit monad = fun _data astate -> sat_ok ((), f astate)

  let exec_partial_operation (f : astate -> (astate * 'a) result) : 'a monad =
   fun _data astate ->
    let++ astate, a = f astate in
    (a, astate)


  let exec_operation (f : astate -> 'a) : 'a monad = fun data astate -> ret (f astate) data astate

  let return_value aval : unit monad =
    let* {ret= ret_id, _} = get_data in
    PulseOperations.write_id ret_id aval |> exec_command


  let eval_deref_access access_mode aval access : aval monad =
    let* {path; location} = get_data in
    PulseOperations.eval_deref_access path access_mode location aval access
    >> sat |> exec_partial_operation


  let add_dynamic_type typ (addr, _) : unit monad =
    PulseOperations.add_dynamic_type typ addr |> exec_command


  let get_dynamic_type ~ask_specialization (addr, _) : Typ.t option monad =
   fun data astate ->
    let res = AbductiveDomain.AddressAttributes.get_dynamic_type addr astate in
    let astate =
      if ask_specialization && Option.is_none res then
        AbductiveDomain.add_need_dynamic_type_specialization addr astate
      else astate
    in
    ret res data astate


  let and_eq_int (size_addr, _) i : unit monad =
    PulseArithmetic.and_eq_int size_addr i |> exec_partial_command


  let mk_fresh ~model_desc : aval monad =
    let* {path; location} = get_data in
    let addr = AbstractValue.mk_fresh () in
    let hist = Hist.single_call path location model_desc in
    ret (addr, hist)


  let write_deref_field ~ref ~obj field : unit monad =
    let* {path; location} = get_data in
    PulseOperations.write_deref_field path location ~ref ~obj field >> sat |> exec_partial_command


  let deep_copy ?depth_max source : aval monad =
    let* {path; location} = get_data in
    PulseOperations.deep_copy ?depth_max path location source >> sat |> exec_partial_operation


  let dynamic_dispatch ~(cases : (Typ.name * 'a monad) list) aval : 'a monad =
    let* opt_typ = get_dynamic_type ~ask_specialization:true aval in
    match opt_typ with
    | Some {Typ.desc= Tstruct type_name} -> (
      match List.find cases ~f:(fun case -> fst case |> Typ.Name.equal type_name) with
      | Some (_, case_fun) ->
          case_fun
      | None ->
          Logging.d_printfln "[ocaml model] dynamic_dispatch: no case for type %a" Typ.Name.pp
            type_name ;
          unreachable )
    | _ ->
        unreachable
end
