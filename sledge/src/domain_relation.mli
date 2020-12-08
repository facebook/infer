(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Relational abstract domain, elements of which are interpreted as Hoare
    triples over a base domain *)

module type State_domain_sig = sig
  include Domain_intf.Dom

  val create_summary :
       locals:Llair.Reg.Set.t
    -> formals:Llair.Reg.t iarray
    -> entry:t
    -> current:t
    -> summary * t
end

module Make (_ : State_domain_sig) : Domain_intf.Dom
