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
  type t = AbstractValue.t * ValueHistory.t [@@deriving compare, equal, yojson_of]

  let pp f addr_trace =
    if Config.debug_level_analysis >= 3 then
      Pp.pair ~fst:AbstractValue.pp ~snd:ValueHistory.pp f addr_trace
    else AbstractValue.pp f (fst addr_trace)
end

module M = PrettyPrintable.MakePPMonoMap (VarAddress) (AddrHistPair)

let yojson_of_t m = [%yojson_of: (VarAddress.t * AddrHistPair.t) list] (M.bindings m)

let canonicalize ~get_var_repr stack =
  (* TODO: detect contradictions when the addresses of two different program variables are equal *)
  let changed = ref false in
  let stack' =
    M.map
      (fun ((addr, hist) as addr_hist) ->
        let addr' = get_var_repr addr in
        if phys_equal addr addr' then addr_hist
        else (
          changed := true ;
          (addr', hist) ) )
      stack
  in
  if !changed then stack' else stack


let subst_var (v, v') stack =
  canonicalize stack ~get_var_repr:(fun addr -> if AbstractValue.equal v addr then v' else addr)


include M

let compare = M.compare AddrHistPair.compare

let equal = M.equal AddrHistPair.equal

let pp fmt m =
  let pp_item fmt (var_address, v) =
    F.fprintf fmt "%a=%a" VarAddress.pp var_address AddrHistPair.pp v
  in
  PrettyPrintable.pp_collection ~pp_item fmt (M.bindings m)
