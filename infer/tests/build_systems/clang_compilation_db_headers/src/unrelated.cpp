/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "decls.h"

// reported only when this file is in the changed files index: it does not
// include widget.h, and decls.h defines no procedure
int unrelated_bad() {
  int* p = nullptr;
  return *p;
}

int call_declared_only() { return declared_only(nullptr); }
