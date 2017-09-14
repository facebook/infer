/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// we should only report one dead store warning on this file

template <class T>
T* unused_var_template_bad() {
  int i = 42;
  return 0;
}

int* unused_var_template1_bad() { return unused_var_template_bad<int>(); }

char* unused_var_template2_bad() { return unused_var_template_bad<char>(); }

long* unused_var_template3_bad() { return unused_var_template_bad<long>(); }
