(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Process

let main =
  let pid, ic =
    fork (fun oc ->
        output_string oc "This is a test\n" ;
        true )
  in
  let zipunzip = compose gzip gunzip in
  ignore
    (diff_on_same_input
       (fun ic oc ->
         copy ic oc ;
         flush_all () ;
         true )
       zipunzip ic stderr) ;
  close_in ic ;
  ignore (wait pid)
