/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "gadget.h"

int use_gadget_in_client() { return gadget_bad(); }

// not reported when only gadget.h changes: the procedures of gadget.h are
// analyzed as part of gadget.cpp, even though the capture database keeps the
// copy of gadget_bad from this file
int gadget_client_bad() {
  int* p = nullptr;
  return *p;
}
