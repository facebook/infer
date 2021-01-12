/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstddef>
#include <cstdint>

extern "C" int mallctl(const char* name, void* oldp, size_t* oldlenp,
                       void* newp, size_t newlen) __attribute__((__weak__));

int main() {
  volatile uint64_t* counter;
  size_t counterLen = sizeof(uint64_t*);

  if (mallctl("thread.allocatedp", static_cast<void*>(&counter), &counterLen,
              nullptr, 0) != 0) {
    return 1;
  }

  if (counterLen != sizeof(uint64_t*)) {
    return 1;
  }

  // false alarm: the spec of mallctl does not special case
  // thread.allocatedp and set counter to a valid pointer
  uint64_t origAllocated = *counter;

  return 0;
}
