/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma once

#include "project_lib.h"

// even though it's unused, it should be translated if compiled source is
// main.cpp (or a symbolic link pointing to it)
int unused_deref_in_header(int* a) {
  int x = internal::used_in_main_header(0);
  return *a;
}
