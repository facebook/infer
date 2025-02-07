/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdint.h>
#include <stdlib.h>

int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size);

int _llair_main() {
  size_t Size = 13;
  uint8_t Data[Size];
  return LLVMFuzzerTestOneInput(Data, Size);
}
