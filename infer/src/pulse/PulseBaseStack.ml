(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface

(** Stacks: map addresses of variables to values and histoy. *)

module VarAddress = struct
  include Var

  let pp f var =
    let pp_ampersand f = function ProgramVar _ -> F.pp_print_string f "&" | LogicalVar _ -> () in
    F.fprintf f "%a%a" pp_ampersand var Var.pp var
end

module AddrHistPair = struct
  type t = AbstractValue.t * ValueHistory.t [@@deriving compare]

  let pp f addr_trace =
    if Config.debug_level_analysis >= 3 then
      Pp.pair ~fst:AbstractValue.pp ~snd:ValueHistory.pp f addr_trace
    else AbstractValue.pp f (fst addr_trace)
end

include PrettyPrintable.MakePPMonoMap (VarAddress) (AddrHistPair)

let pp fmt m =
  let pp_item fmt (var_address, v) =
    F.fprintf fmt "%a=%a" VarAddress.pp var_address AddrHistPair.pp v
  in
  PrettyPrintable.pp_collection ~pp_item fmt (bindings m)


let compare = compare AddrHistPair.compare
