/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int deref(int* p) {
  if (p == 0) {
    throw "Null pointer!";
  }
  return *p;
}

int deref_null(int* p) {
  try {
    return *p;
  } catch (const char* msg) {
  }
}

int call_deref_with_null() { deref_null(nullptr); }

int main() {
  try {
    return deref(0);
  } catch (const char* msg) {
    return -1;
  }
}
