(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Transfer functions that push abstract states across instructions. A typical client should
    implement the Make signature to allow the transfer functions to be used with any kind of CFG. *)

module type S = sig
  module CFG : ProcCfg.S
  module Domain : AbstractDomain.S (* abstract domain whose state we propagate *)
  type extras (* read-only extra state (results of previous analyses, globals, etc.) *)

  (* {A} instr {A'}. [node] is the node of the current instruction *)
  val exec_instr : Domain.astate -> extras ProcData.t -> CFG.node -> Sil.instr -> Domain.astate
end

module type Make = functor (C : ProcCfg.S) -> sig
  include (S with module CFG = C)
end
