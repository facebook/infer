(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This is the driver-side entry point for the ToyLang tutorial frontend (step 7 of             *)
(* infer/src/textual/TutoFrontend.ml). The [Driver] dispatches here when the user passes        *)
(* [--capture-toylang]. All the real work -- parsing, translation to Textual, verification,     *)
(* transforms, lowering to SIL and capturing -- lives in [TutoFrontend] (in the [Textuallib]    *)
(* library, which this [Integration] library depends on). Here we merely iterate over the       *)
(* requested files. A production frontend would additionally merge each file's type environment *)
(* into a single global one before storing it; we keep that simple and let                      *)
(* [TutoFrontend.capture_file] store the global tenv itself.                                    *)

open! IStd
module L = Logging

let capture ~files =
  List.iter files ~f:(fun file ->
      L.progress "Capturing ToyLang file %s@\n" file ;
      TutoFrontend.capture_file file )
