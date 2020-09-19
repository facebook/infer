(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let () = Yojson_utils.run_converter_tool Clang_ast_j.read_decl Clang_ast_j.write_decl
