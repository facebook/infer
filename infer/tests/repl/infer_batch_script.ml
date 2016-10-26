(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Example of ocaml script starting with infer code. To execute a scipt run:
   ./scripts/infer_repl <path/to/this/script.ml>
   It's used as a basic integration test *)

(* "import" infer code *)
#use "toplevel_init";;

let _ = Ident.create_fresh Ident.knormal in
let ident = Ident.create_fresh Ident.knormal in
let e = Exp.Var ident in
print_endline (Exp.to_string e);
(* pass --flavors flag to change the value *)
print_endline (string_of_bool Config.flavors)
