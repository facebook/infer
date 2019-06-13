/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void foo_defined_in_subtarget1() {
  int* t = NULL;
  *t = 42;
}
