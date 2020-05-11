(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let die () =
  prerr_endline "ERROR: infer was built without clang support." ;
  Die.exit 1


module ClangQuotes = struct
  type style = EscapedDoubleQuotes | SingleQuotes | EscapedNoQuotes

  let mk_arg_file _ _ _ = die ()
end

module ClangTests = struct
  let tests = [OUnitTest.TestList []]
end

module ClangWrapper = struct
  let exe ~prog:_ ~args:_ = ()
end

module CTLParserHelper = struct
  let validate_al_files () = die ()
end

module RegisterCallback = struct
  let register_frontend_checks () = ()
end
