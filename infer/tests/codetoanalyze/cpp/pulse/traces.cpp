/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <string>

void access_destructed_string() {
  const char* ptr;
  {
    std::string s = "blah";
    ptr = s.data();
  }
  char first_char = *ptr;
}
