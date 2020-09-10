(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let main = Yojson_utils.make_yojson_validator Clang_ast_j.read_decl Clang_ast_j.write_decl Sys.argv
