/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>

struct B {
  B() { printf("  Base default constructor, this=%p\n", this); }
  B(const B& other) {
    printf("  Base copy-constructor, this=%p from that=%p\n", this, &other);
  }
  virtual ~B() { printf("  Base destructor, this=%p\n", this); }
};

struct D : public B {
  D() { printf("  Derived default constructor, this=%p\n", this); }
  D(const D& other) {
    printf("  Derived copy-constructor, this=%p from that=%p\n", this, &other);
  }
  virtual ~D() { printf("  Derived destructor, this=%p\n", this); }
};

void f() {
  D e;
  throw e;
}

int main() {
  try {
    try {
      f();
    } catch (B& err) {
      printf("A Inner catch, &err=%p\n", &err);
      throw;
    }
  } catch (B& err) {
    printf("A Outer catch, &err=%p\n", &err);
  }
  printf("---\n");
  try {
    try {
      D e;
      throw e;
    } catch (B& err) {
      printf("B Inner catch, &err=%p\n", &err);
      throw err;
    }
  } catch (B& err) {
    printf("B Outer catch, &err=%p\n", &err);
  }
  return 0;
}
