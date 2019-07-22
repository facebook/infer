/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <cstdint>

void casting_void_ptr(void* p) {
  uint8_t* q = (uint8_t*)p;
  q[14] = 0;
}

void FP_call_casting_void_ptr_Ok() {
  uint64_t p[2];
  casting_void_ptr(p);
}
