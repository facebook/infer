(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val decl_name_is_contained_in_name_of_decl :
  Ctl_parser_types.ast_node -> Ctl_parser_types.ast_node -> bool
