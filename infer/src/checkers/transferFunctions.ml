(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


module type TransferFunctions = sig
  type astate

  (* {A} instr {A'} *)
  val exec_instr : astate -> Sil.instr -> astate

end
