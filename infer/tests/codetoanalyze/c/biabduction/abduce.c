/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
void external_func(int* const*);

int const_local_no_abduce(int* p) {
  external_func(&p);
  return p ? *p : 0;
  // We shouldn't get a stack address escape warning here
}
