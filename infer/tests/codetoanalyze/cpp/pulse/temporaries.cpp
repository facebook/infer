/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <iostream>

namespace temporaries {

struct A {
  int f;
};

std::unique_ptr<A> some_f();

void FN_call_some_f_deref_bad() {
  const A& a_ref = *some_f(); // temporary unique_ptr returned by `some_f` is
                              // destroyed at the end of the statement
  std::cout << a_ref.f;
}

void call_some_f_ok() {
  auto local = some_f(); // ok, as ownership of a temporary unique_ptr is passed
                         // to `local`
  const A& a_ref = *local;
  std::cout << a_ref.f;
}

void call_some_f_copy_object_ok() {
  auto a = *some_f().get(); // ok, as value is copied before temporary
                            // unique_prt is destroyed
  std::cout << a.f;
}

} // namespace temporaries
