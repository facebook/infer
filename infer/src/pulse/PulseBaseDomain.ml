(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface
module Memory = PulseBaseMemory
module Stack = PulseBaseStack
module AddressAttributes = PulseBaseAddressAttributes

(* {2 Abstract domain description } *)

type t = {heap: Memory.t; stack: Stack.t; attrs: AddressAttributes.t}
[@@deriving compare, equal, yojson_of]

let empty =
  { heap=
      Memory.empty
      (* TODO: we could record that 0 is an invalid address at this point but this makes the
         analysis go a bit overboard with the Nullptr reports. *)
  ; stack= Stack.empty
  ; attrs= AddressAttributes.empty }


type cell = Memory.Edges.t * Attributes.t

let pp fmt {heap; stack; attrs} =
  F.fprintf fmt "{@[<v1> roots=@[<hv>%a@];@;mem  =@[<hv>%a@];@;attrs=@[<hv>%a@];@]}" Stack.pp stack
    Memory.pp heap AddressAttributes.pp attrs


let subst_var ~for_summary subst ({heap; stack; attrs} as astate) =
  let open SatUnsat.Import in
  let* stack' = if for_summary then Stack.subst_var subst stack else Sat stack in
  let+ heap' = Memory.subst_var ~for_summary subst heap in
  let attrs' = AddressAttributes.subst_var subst attrs in
  if phys_equal heap heap' && phys_equal stack stack' && phys_equal attrs attrs' then astate
  else {heap= heap'; stack= stack'; attrs= attrs'}
