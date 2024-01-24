/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <optional>

// Binary comparison, e.g. "!=", should not introduce the `__infer_skip`
// instruction.
// https://clang.llvm.org/doxygen/classclang_1_1CXXRewrittenBinaryOperator.html
int rewritten_expr_ok() {
  std::optional<int> x{std::nullopt};
  if (x != std::nullopt) {
    return x.value();
  }
  return 42;
}
