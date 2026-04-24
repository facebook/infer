(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let message procname =
  F.asprintf
    "Method `%a` returns a pointer without `_Nullable` or `_Nonnull` annotations. When called from \
     Swift the return type is imported as an Implicitly Unwrapped Optional (`T!`), which traps at \
     runtime if the method ever returns `nil`. Annotate the declaration with `_Nullable` or \
     `_Nonnull` so the contract is explicit at the Swift call site."
    Procname.pp procname
