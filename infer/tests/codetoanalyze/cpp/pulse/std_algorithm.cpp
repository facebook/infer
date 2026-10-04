/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>

namespace std_algorithm {

int find_then_push_back_bad(std::vector<int>& vec) {
  auto it = std::find(vec.begin(), vec.end(), 3);
  if (it == vec.end()) {
    return 0;
  }
  vec.push_back(1);
  return *it;
}

int find_if_then_push_back_bad(std::vector<int>& vec) {
  auto it = std::find_if(vec.begin(), vec.end(), [](int x) { return x > 3; });
  if (it == vec.end()) {
    return 0;
  }
  vec.push_back(1);
  return *it;
}

int lower_bound_then_push_back_bad(std::vector<int>& vec) {
  auto it = std::lower_bound(vec.begin(), vec.end(), 3);
  vec.push_back(1);
  return *it;
}

int max_element_then_push_back_bad(std::vector<int>& vec) {
  auto it = std::max_element(vec.begin(), vec.end());
  vec.push_back(1);
  return *it;
}

int next_then_push_back_bad(std::vector<int>& vec) {
  auto it = std::next(vec.begin());
  vec.push_back(1);
  return *it;
}

int advance_then_push_back_bad(std::vector<int>& vec) {
  auto it = vec.begin();
  std::advance(it, 1);
  vec.push_back(1);
  return *it;
}

struct Elem {
  int x;
};

int find_if_arrow_then_push_back_bad(std::vector<Elem>& vec) {
  auto it = std::find_if(
      vec.begin(), vec.end(), [](const Elem& e) { return e.x > 3; });
  if (it == vec.end()) {
    return 0;
  }
  vec.push_back(Elem{1});
  return it->x;
}

std::vector<int>::iterator find_three(std::vector<int>& vec) {
  return std::find(vec.begin(), vec.end(), 3);
}

int find_interproc_then_push_back_bad(std::vector<int>& vec) {
  auto it = find_three(vec);
  if (it == vec.end()) {
    return 0;
  }
  vec.push_back(1);
  return *it;
}

int find_assigned_then_push_back_bad(std::vector<int>& vec) {
  auto it = vec.end();
  it = std::find(vec.begin(), vec.end(), 3);
  if (it == vec.end()) {
    return 0;
  }
  vec.push_back(1);
  return *it;
}

int find_checked_ok(std::vector<int>& vec) {
  auto it = std::find(vec.begin(), vec.end(), 3);
  if (it != vec.end()) {
    return *it;
  }
  return 0;
}

int find_checked_saved_end_ok(std::vector<int>& vec) {
  auto end = vec.end();
  auto it = std::find(vec.begin(), end, 3);
  if (it != end) {
    return *it;
  }
  return 0;
}

int find_checked_cend_ok(const std::vector<int>& vec) {
  auto it = std::find(vec.cbegin(), vec.cend(), 3);
  if (it == vec.cend()) {
    return 0;
  }
  return *it;
}

int find_present_element_ok(std::vector<int>& vec) {
  vec.push_back(3);
  return *std::find(vec.begin(), vec.end(), 3);
}

// the models have no case for "not found"
int FN_find_on_empty_bad() {
  std::vector<int> vec;
  return *std::find(vec.begin(), vec.end(), 3);
}

int find_after_reserve_ok(std::vector<int>& vec) {
  vec.reserve(vec.size() + 1);
  auto it = std::find(vec.begin(), vec.end(), 3);
  if (it == vec.end()) {
    return 0;
  }
  vec.push_back(1);
  return *it;
}

int find_again_after_push_back_ok(std::vector<int>& vec) {
  auto it = std::find(vec.begin(), vec.end(), 3);
  if (it == vec.end()) {
    vec.push_back(3);
    it = std::find(vec.begin(), vec.end(), 3);
  }
  return *it;
}

int prev_end_ok(std::vector<int>& vec) {
  if (vec.empty()) {
    return 0;
  }
  return *std::prev(vec.end());
}

int prev_then_push_back_bad(std::vector<int>& vec) {
  auto it = std::prev(vec.end());
  vec.push_back(1);
  return *it;
}

int next_prev_end_bad(std::vector<int>& vec) {
  return *std::next(std::prev(vec.end()));
}

int next_prev_round_trip_ok(std::vector<int>& vec) {
  auto it = vec.begin();
  if (std::prev(std::next(it, 2), 2) != it) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int next_same_as_plus_ok(std::vector<int>::iterator it) {
  if (std::next(it, 2) != it + 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int advance_base_ok(std::vector<int>::iterator it) {
  std::vector<int>::iterator it_copy;
  it_copy = it;
  std::advance(it_copy, 2);
  if (it_copy.base() != it.base() + 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

// erase invalidates the elements before the erased range too
int FP_erase_after_found_ok(std::vector<int>& vec) {
  auto it = std::find(vec.begin(), vec.end(), 3);
  if (it == vec.end()) {
    return 0;
  }
  vec.erase(std::next(it), vec.end());
  return *it;
}

void erase_remove_if_ok(std::vector<int>& vec) {
  vec.erase(std::remove_if(vec.begin(), vec.end(), [](int x) { return x < 0; }),
            vec.end());
}

std::vector<int> rotate_copy_ok(const std::vector<int>& vec) {
  auto copy = vec;
  std::rotate(copy.begin(), copy.begin() + 1, copy.end());
  return copy;
}

int partition_elements_ok(std::vector<int*>& vec) {
  int x = 0;
  if (vec.size() != 2) {
    return 0;
  }
  vec[0] = nullptr;
  vec[1] = &x;
  std::partition(vec.begin(), vec.end(), [](int* p) { return p != nullptr; });
  return *vec[0];
}

char find_in_string_ok(const std::string& s) {
  auto it = std::find(s.begin(), s.end(), 'a');
  if (it != s.end()) {
    return *it;
  }
  return 'b';
}

} // namespace std_algorithm
