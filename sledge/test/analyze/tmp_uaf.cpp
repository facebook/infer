/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <iostream>
#include <memory>

// just a struct
struct A {
  int f;
  ~A() {}
};

// a function that returns an object, here a unique_ptr
std::unique_ptr<A> return_object() { return std::unique_ptr<A>(new A()); }

int main() {
  // the compiler creates a C++ temporary to hold the result
  // of the function call
  const A& a_ref = *return_object();
  // the lifetime of the temporary is only the expression
  // above, so the pointer inside a_ref has been deleted
  // by unique_ptr's destructor
  std::cout << a_ref.f; // a_ref is garbage now; boom.
  return 0;
}
