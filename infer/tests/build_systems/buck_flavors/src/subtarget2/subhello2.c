/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void null_deref_spec_defined_in_subtarget1();

void null_deref_after_error_spec_ok() {
  null_deref_spec_defined_in_subtarget1();
  int* s = NULL;
  *s = 42;
}
