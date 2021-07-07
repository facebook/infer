/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

struct lol {
  int f;
};

typedef const std::shared_ptr<lol>& const_lol;

void skip_const(const std::shared_ptr<lol>& foo);
void skip_const2(int x, const std::shared_ptr<lol>& foo, int y, bool z);
void skip_no_const(std::shared_ptr<lol>& foo);
void skip_typedef(const_lol foo);

void test_pointer(const std::shared_ptr<lol>& foo) {
  // create a case split where one post-condition has foo == null
  if (foo) {
    foo->f = 4;
  }
}

void FP_skip_then_split_case_bad() {
  auto foo = std::make_shared<lol>();
  skip_no_const(foo); // Infer havocs foo here since it's not const
  test_pointer(foo); // this call creates a case split, foo can be null in one
                     // branch
  foo->f = 12; // error
}

void FP_const_skip_then_split_case_ok() {
  auto foo = std::make_shared<lol>();
  skip_const(foo); // Infer shouldn't havoc foo here since it's const...
  test_pointer(foo); /* ...so foo cannot be null here, even if there is an
                        explicit null post... */
  foo->f = 12; // error
}

// same as above but make sure infer pinpoints the correct const argument
void FP_const_skip2_then_split_case_ok() {
  auto foo = std::make_shared<lol>();
  skip_const2(0, foo, 0, 0);
  test_pointer(foo);
  foo->f = 12; // error
}

// same as above but hide the type under a typedef
void FP_typedef_skip_then_split_case_ok() {
  auto foo = std::make_shared<lol>();
  skip_typedef(foo);
  test_pointer(foo);
  foo->f = 12; // error
}
