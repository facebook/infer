(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module Hist = PulseModelsImport.Hist
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

type model_data = {path: PathContext.t; location: Location.t}

type model =
     model_data
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) PulseOperationResult.t

let std_is_same_v t1 t2 : model =
 fun {path; location} astate ->
  if Typ.equal_template_arg t1 t2 then
    let astate, one = PulseArithmetic.absval_of_int astate IntLit.one in
    Sat (Ok (astate, (one, Hist.single_call path location "std_is_same_v is true")))
  else
    let astate, zero = PulseArithmetic.absval_of_int astate IntLit.zero in
    Sat (Ok (astate, (zero, Hist.single_call path location "std_is_same_v is false")))


let dispatch ~load:load_exp =
  let open IOption.Let_syntax in
  (* very ad-hoc for now as there is only one model *)
  let* pvar = match (load_exp : Exp.t) with Lvar pvar -> Some pvar | _ -> None in
  if Pvar.get_name pvar |> Mangled.to_string |> String.equal "std::is_same_v" then (
    L.d_printfln "found std::is_same_v model" ;
    let+ t1, t2 =
      match Pvar.get_template_args pvar with
      | Typ.Template {args= [t1; t2]} ->
          Some (t1, t2)
      | template_args ->
          L.d_printfln
            "Strange case: variable %a: was expecting two templates arguments but got '%a'"
            Pvar.pp_value pvar
            (Typ.pp_template_spec_info Pp.text)
            template_args ;
          None
    in
    std_is_same_v t1 t2 )
  else None
