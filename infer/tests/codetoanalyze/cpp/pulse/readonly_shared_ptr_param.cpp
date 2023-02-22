/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <functional>
#include <memory>

int read_shared_ptr_param_bad(std::shared_ptr<int> x) { return *x; }

int read_shared_ptr_param_cond_bad(std::shared_ptr<int> x) {
  if (x) {
    return *x;
  } else {
    return 42;
  }
}

// x should be captured with move.
std::function<void(void)> captured_shared_ptr_param_bad_FN(
    std::shared_ptr<int> x) {
  return [x]() {};
}

class SharedPtrField1_Bad {
  std::shared_ptr<int> field;

 public:
  // x should be moved.
  SharedPtrField1_Bad(std::shared_ptr<int> x) { field = x; }
};

int* global_ptr;

// It reports PULSE_READONLY_SHARED_PTR_PARAM. While copying raw pointer to
// global is dangerous in general, changing the function to get a raw
// pointer does not make it more dangerous.
void copy_raw_ptr_to_global_bad(std::shared_ptr<int> x) {
  global_ptr = x.get();
}

std::shared_ptr<int> global_shared_ptr;

// x should be moved.
void copy_shared_ptr_to_global_bad(std::shared_ptr<int> x) {
  global_shared_ptr = x;
}
