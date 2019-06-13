/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "project_lib.h"

// even though it's unused, it should be translated if compiled source is
// main.cpp (or a symbolic link pointing to it)
int unused_deref_in_header(int* a) {
  int x = internal::used_in_main_header(0);
  return *a;
}
