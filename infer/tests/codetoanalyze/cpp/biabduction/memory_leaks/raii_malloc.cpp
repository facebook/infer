/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct MyResourceManager {
  void* p;
  MyResourceManager(size_t n) : p(malloc(sizeof(int) * n)) {}
  ~MyResourceManager() { free(p); }
};

void no_memory_leak() { MyResourceManager resource(1); }

void memory_leak() { void* p = malloc(sizeof(int)); }
