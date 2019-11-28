/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>
void insert_impure(std::vector<int>& vec) { vec.insert(vec.begin(), 2); }

void push_back_impure(std::vector<int>& vec) { vec.push_back(32); }

void fresh_push_back_pure() {
  std::vector<int> vec = {0, 0};
  push_back_impure(vec);
}

// modifies vec
void push_back_in_loop_impure(std::vector<int>& vec,
                              std::vector<int>& vec_other) {
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
}

// modifies vec
void clear_impure(std::vector<int>& vec) { vec.clear(); }

// modifies vec
void assign_impure(std::vector<int>& vec) { vec.assign(11, 7); }

struct A {
  int i;
};

int set_zero_impure(std::vector<A>& numbers) {
  for (auto& num : numbers) {
    num.i = 0;
  }
  return 0;
}
