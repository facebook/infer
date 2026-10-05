/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "decls.h"

// clang rejects the nested functions of GCC: this file includes decls.h but
// cannot be captured
int call_nested_function(int* p) {
  int nested_function(void) { return declared_only(p); }
  return nested_function();
}
