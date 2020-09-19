/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>

struct E {
  E() { printf("  E default constructor, this=%p\n", this); }
  E(const E& other) {
    printf("  E copy-constructor, this=%p from that=%p\n", this, &other);
  }
  virtual ~E() { printf("  E destructor, this=%p\n", this); }
};

struct F : public E {
  F() { printf("  F default constructor, this=%p\n", this); }
  F(const F& other) {
    printf("  F copy-constructor, this=%p from that=%p\n", this, &other);
  }
  virtual ~F() { printf("  F destructor, this=%p\n", this); }
};

__attribute__((noinline)) void f() { throw 20; }

void g() throw(E) {
  F e;
  throw e;
}

int main() {
  try {
    f();
    throw 20;
  } catch (int e) {
    if (e > 0) {
      try {
        g();
      } catch (E e2) {
        throw;
      }
    } else
      return e;
  }
  return 0;
}
