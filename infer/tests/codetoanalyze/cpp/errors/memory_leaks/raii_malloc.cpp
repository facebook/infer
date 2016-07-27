/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>

struct MyResourceManager {
  void* p;
  MyResourceManager(size_t n) : p(malloc(sizeof(int) * n)) {}
  ~MyResourceManager() { free(p); }
};

void no_memory_leak() { MyResourceManager resource(1); }

void memory_leak() { void* p = malloc(sizeof(int)); }
