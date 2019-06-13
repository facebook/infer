/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <exception>
#include <stdexcept>

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

void basic_throw_ok() { throw std::runtime_error("throwing!"); }

int dead_deref_null_after_throw_ok() {
  int* i = nullptr;
  throw std::runtime_error("throwing!");
  return *i;
}

int FN_deref_null_in_catch_bad() {
  int* i = nullptr;
  try {
    throw std::runtime_error("error");
  } catch (...) {
    return *i;
  }
  return 0;
}

int FN_deref_null_after_catch_bad(int* i) {
  try {
    *i = 2;
    throw std::runtime_error("error");
  } catch (...) {
    i = nullptr;
  }
  return *i;
}

int FN_multiple_catches_bad(bool b) {
  int* i = nullptr;
  int* j = nullptr;
  try {
    if (b) {
      throw std::length_error("error");
    } else {
      throw std::range_error("error");
    }
  } catch (std::length_error& msg) {
    return *i;
  } catch (std::range_error& msg) {
    return *j;
  }
  return 0;
}

int main() {
  try {
    return deref(0);
  } catch (const char* msg) {
    return -1;
  }
}
