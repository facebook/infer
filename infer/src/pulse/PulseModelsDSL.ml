(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport

type astate = AbductiveDomain.t

type 'a result = 'a AccessResult.t

type aval = AbstractValue.t * ValueHistory.t

(* we work on a disjunction of executions *)
type 'a model_monad = model_data -> astate -> 'a execution result list

and 'a execution =
  | ContinueProgram of 'a * astate
  | Other of ExecutionDomain.t (* should never contain a ExecutionDomain.ContinueProgram *)

module Syntax = struct
  let ret (a : 'a) : 'a model_monad = fun _data astate -> [Ok (ContinueProgram (a, astate))]

  let unreachable : 'a model_monad = fun _ _ -> []

  let bind (x : 'a model_monad) (f : 'a -> 'b model_monad) : 'b model_monad =
   fun data astate ->
    List.concat_map (x data astate) ~f:(function
      | Ok (ContinueProgram (a, astate)) ->
          f a data astate
      | Recoverable (ContinueProgram (a, astate), err) ->
          f a data astate |> List.map ~f:(PulseResult.append_errors err)
      | Ok (Other (ContinueProgram _)) | Recoverable (Other (ContinueProgram _), _) ->
          L.die InternalError "DSL.Other should never contains ContinueProgram"
      | Ok (Other exec) ->
          [Ok (Other exec)]
      | Recoverable (Other exec, err) ->
          [Recoverable (Other exec, err)]
      | FatalError _ as err ->
          [err] )


  let ( let* ) a f = bind a f

  let list_fold (list : 'a list) ~(init : 'accum) ~(f : 'accum -> 'a -> 'accum model_monad) :
      'accum model_monad =
    List.fold list ~init:(ret init) ~f:(fun monad a ->
        let* acc = monad in
        f acc a )


  let list_iter (list : 'a list) ~(f : 'a -> unit model_monad) : unit model_monad =
    list_fold list ~init:() ~f:(fun () a -> f a)


  let list_filter_map (list : 'a list) ~(f : 'a -> 'b option model_monad) : 'b list model_monad =
    let* rev_res =
      list_fold list ~init:[] ~f:(fun acc a ->
          let* opt = f a in
          match opt with None -> ret acc | Some b -> b :: acc |> ret )
    in
    List.rev rev_res |> ret


  let option_iter (o : 'a option) ~(f : 'a -> unit model_monad) : unit model_monad =
    Option.value_map o ~default:(ret ()) ~f


  let get_data : model_data model_monad = fun data astate -> ret data data astate

  let ok x = Ok x

  let sat x = Sat x

  let ( >> ) f g x = f x |> g

  let start_model (monad : unit model_monad) : model =
   fun data astate ->
    List.map (monad data astate)
      ~f:
        (PulseResult.map ~f:(function
          | ContinueProgram ((), astate) ->
              ExecutionDomain.ContinueProgram astate
          | Other exec ->
              exec ) )


  let lift_to_monad (model : model) : unit model_monad =
   fun data astate ->
    List.map (model data astate)
      ~f:
        (PulseResult.map ~f:(function
          | ExecutionDomain.ContinueProgram astate ->
              ContinueProgram ((), astate)
          | exec ->
              Other exec ) )


  let disjuncts (list : 'a model_monad list) : 'a model_monad =
   fun data astate -> List.concat_map list ~f:(fun m -> m data astate)


  let exec_partial_command (f : astate -> astate PulseOperationResult.t) : unit model_monad =
   fun _data astate ->
    match f astate with
    | Unsat ->
        []
    | Sat res ->
        [PulseResult.map res ~f:(fun astate -> ContinueProgram ((), astate))]


  let exec_command (f : astate -> astate) : unit model_monad =
   fun _data astate -> [ok (ContinueProgram ((), f astate))]


  let exec_partial_operation (f : astate -> (astate * 'a) PulseOperationResult.t) : 'a model_monad =
   fun _data astate ->
    match f astate with
    | Unsat ->
        []
    | Sat res ->
        [PulseResult.map res ~f:(fun (astate, a) -> ContinueProgram (a, astate))]


  let exec_operation (f : astate -> 'a * astate) : 'a model_monad =
   fun data astate ->
    let a, astate = f astate in
    ret a data astate


  let assign_ret aval : unit model_monad =
    let* {ret= ret_id, _} = get_data in
    PulseOperations.write_id ret_id aval |> exec_command


  let eval_deref_access access_mode aval access : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.eval_deref_access path access_mode location aval access
    >> sat |> exec_partial_operation


  let add_dynamic_type typ (addr, _) : unit model_monad =
    PulseOperations.add_dynamic_type typ addr |> exec_command


  let get_dynamic_type ~ask_specialization (addr, _) : Typ.t option model_monad =
   fun data astate ->
    let res = AbductiveDomain.AddressAttributes.get_dynamic_type addr astate in
    let astate =
      if ask_specialization && Option.is_none res then
        AbductiveDomain.add_need_dynamic_type_specialization addr astate
      else astate
    in
    ret res data astate


  let add_static_type typ_name (addr, _) : unit model_monad =
   fun ({analysis_data= {tenv}} as data) astate ->
    let astate = AbductiveDomain.AddressAttributes.add_static_type tenv typ_name addr astate in
    ret () data astate


  let get_const_string (addr, _) : string option model_monad =
    let op astate = (AddressAttributes.get_const_string addr astate, astate) in
    exec_operation op


  let tenv_resolve_fieldname typ_name field_name : Struct.field_info option model_monad =
   fun ({analysis_data= {tenv}} as data) astate ->
    let info = Tenv.resolve_fieldname tenv typ_name field_name in
    ret info data astate


  let and_eq_int (size_addr, _) i : unit model_monad =
    PulseArithmetic.and_eq_int size_addr i |> exec_partial_command


  let mk_fresh ~model_desc : aval model_monad =
    let* {path; location} = get_data in
    let addr = AbstractValue.mk_fresh () in
    let hist = Hist.single_call path location model_desc in
    ret (addr, hist)


  let write_deref_field ~ref ~obj field : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.write_deref_field path location ~ref ~obj field >> sat |> exec_partial_command


  let deep_copy ?depth_max source : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.deep_copy ?depth_max path location source >> sat |> exec_partial_operation


  let aval_operand (addr, _) = PulseArithmetic.AbstractValueOperand addr

  let prune_binop ~negated binop operand1 operand2 =
    PulseArithmetic.prune_binop ~negated binop operand1 operand2 |> exec_partial_command


  let dynamic_dispatch ~(cases : (Typ.name * 'a model_monad) list)
      ?(default : 'a model_monad option) aval : 'a model_monad =
    let* opt_typ = get_dynamic_type ~ask_specialization:true aval in
    match opt_typ with
    | Some {Typ.desc= Tstruct type_name} -> (
      match (List.find cases ~f:(fun case -> fst case |> Typ.Name.equal type_name), default) with
      | Some (_, case_fun), _ ->
          case_fun
      | None, Some default ->
          default
      | None, None ->
          Logging.d_printfln "[ocaml model] dynamic_dispatch: no case for type %a" Typ.Name.pp
            type_name ;
          disjuncts [] )
    | _ ->
        Logging.d_printfln "[ocaml model] No dynamic type found!" ;
        Option.value_map default ~default:(disjuncts []) ~f:Fn.id
end
