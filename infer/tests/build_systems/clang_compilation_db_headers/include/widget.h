/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

// there is no widget.cpp: these procedures are captured in the files that
// include this header

inline int header_function_bad() {
  int* p = nullptr;
  return *p;
}

class Widget {
 public:
  int header_method_bad() {
    int* p = nullptr;
    return *p;
  }

  int header_method_ok(int* p) { return p == nullptr ? 0 : *p; }
};
