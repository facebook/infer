/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdlib.h>

// We should report here no NPE in the first dereference, but also we
// should report the second NPE as it is reachable
int report_on_line_offset_6_bad(int* p) {
  assert(p);
  if (!p) {
    *p = 42; // unreachable
  }
  int* q = NULL;
  *q = 42;
}
