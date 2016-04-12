(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils


module type S = sig
  type astate

  (* {A} instr {A'}. [caller_pdesc] is the procdesc of the current procedure *)
  val exec_instr : astate -> ProcData.t -> Sil.instr -> astate

end
