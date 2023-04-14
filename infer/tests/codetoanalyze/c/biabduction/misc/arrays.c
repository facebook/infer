/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdint.h>

const char* GetVarint64Ptr(const char* p, const char* limit) {
  for (uint32_t shift = 0; shift <= 63 && p < limit; shift += 7) {
    uint64_t byte = *p;
    p++;
    if (!(byte & 128)) {
      return p;
    }
  }
  return 0;
}

char* data_;

void DecodeCurrentValue() {
  const char* limit = data_ + 1;
  const char* newp = GetVarint64Ptr(data_, limit);
  while (!newp) {
  }
  newp = GetVarint64Ptr(newp, limit);
  while (!newp) {
  }
  // ensure this does not trigger assertion failure in Absarray (see T42274983)
}
