/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <memory.h>
#define size_t int
#define off_t int

struct foo { // globals are treated differntly

  char* memory_;
  size_t memory_size_;

  bool ReadBytes(void* buffer, size_t size, off_t offset) {
    if (memory_) {
      if (offset < 0) {
        return false;
      }
      if (offset + size >= memory_size_) {
        return false;
      }
      memcpy(buffer, memory_ + offset, size);
      return true;
    } else {
      return false;
    }
  }

  void FindHeader(off_t& offset) {
    int magic;
    ReadBytes(&magic, sizeof(magic), 0);
    ReadBytes(&magic, sizeof(magic), 0);
  }
};

void fail(char* x) {
  if (x == 0) {
    *x = 'a';
  }
}
