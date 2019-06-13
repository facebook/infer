/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>

// We should report here no NPE, but also we should report div0 to show that we
// get the
// non-assertion branch.
int report_div0_and_no_npe(int* p) {
  assert(p);
  return *p / 0;
}
