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

  let bind (x : 'a monad) (f : 'a -> 'b monad) : 'b monad =
   fun data astate ->
    let** a, astate = x data astate in
    f a data astate


  let ( let* ) a f = bind a f

  let list_iter (l : 'a list) ~(f : 'a -> unit monad) : unit monad =
    List.fold l ~init:(ret ()) ~f:(fun monad a ->
        let* () = f a in
        monad )


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


  let return_value aval : unit monad =
    let* {ret= ret_id, _} = get_data in
    PulseOperations.write_id ret_id aval |> exec_command


  let eval_deref_access access_mode aval access : aval monad =
    let* {path; location} = get_data in
    PulseOperations.eval_deref_access path access_mode location aval access
    >> sat |> exec_partial_operation


  let add_dynamic_type typ (addr, _) : unit monad =
    PulseOperations.add_dynamic_type typ addr |> exec_command


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
end
