(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
let erase_backtrace exn =
  match exn with
  | Logging.InferUserError (msg, _)
   -> Logging.InferUserError (msg, "")
  | Logging.InferExternalError (msg, _)
   -> Logging.InferExternalError (msg, "")
  | Logging.InferInternalError (msg, _)
   -> Logging.InferInternalError (msg, "")
  | _
   -> exn

let assert_raises ?msg exn f =
  OUnit2.assert_raises ?msg (erase_backtrace exn) (fun () ->
      try f ()
      with exn -> raise (erase_backtrace exn) )
