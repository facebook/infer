/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>
#include <string>

struct Arr {
  int arr[2];
};

Arr& get_a_ref() {
  static Arr a;
  return a;
}

int copy_decl_bad() {
  auto a = get_a_ref(); // unnecessary copy, use a ref
  // call to copy constructor A::A(a, n$0)
  return a.arr[0];
}

int source_mod_ok() {
  Arr source;
  auto copy = source;
  source.arr[0] = 9; // source is modified, so copy is not unnecessary as we
                     // can't just add &
  return copy.arr[0];
}

int source_mod_param_ok(Arr source) {
  auto copy = source;
  source.arr[0] = 9; // source is modified, so copy is not unnecessary as we
                     // can't just add &
  return copy.arr[0];
}

void copy_in_branch_bad(bool b) {
  if (b) {
    Arr a;
    auto copy = a;
  }
}

void copy_in_branch_mod_ok(bool b) {
  if (b) {
    Arr a;
    auto copy = a;
    copy.arr[0] = 8;
  }
}

void copy_outside_branch_mod_ok(bool b) {
  Arr a;
  auto copy = a;
  if (b) {
    copy.arr[0] = 8;
  }
}

void multiple_copies_bad(bool b) {
  Arr a;
  auto copy1 = a; // unnecessary copy
  if (b) {
    auto copy2 = copy1; // unnecessary copy
  }
}

void modified_copy_decl_ok() {
  auto a = get_a_ref();
  a.arr[0] = 9; // copy modified here, it is not unnecessary
}

// Clang frontend doesn't properly translate array copy constructor
void copy_via_constructor_bad_FN() {
  auto my_arr = Arr{{1, 2}}; // call to constructor
  auto c = my_arr; // copy
  c.arr[0] = 9;
}

int get_first_elem(Arr a) { return a.arr[0]; }

void copy_via_constructor_read_bad() {
  auto my_arr = Arr{{1, 2}}; // call to constructor
  auto copy_arr = my_arr; // copy
  get_first_elem(copy_arr);
}

Arr get_a() {
  Arr a;
  return a;
}

void set_to_zero(int arr[]) { arr[0] = 0; }

void modified_interproc_copy_decl_ok() {
  auto a = get_a_ref();
  set_to_zero(a.arr); // copy modified here by callee, it is not unnecessary
}

void copy_vec_bad(std::vector<int> vec) { auto copy_vec = vec; }

void copy_vec_mod_ok(std::vector<int> vec) {
  auto copy_vec = vec;
  copy_vec.push_back(0);
}

class Vec {

 public:
  std::vector<int> vec;
  Vec() {
    for (int i = 1; i <= 3; i) {
      vec.push_back(i);
    }
  }

  Vec(const Vec& v) {
    for (int i = 1; i < 3; i)
      vec.push_back(v.get(i));
  }

  int get(int i) const { return vec[i]; }
};

// Although underlying vectors are the same for the copy and the source,
// __end_cap_,__begin__, and __end__ are different. TODO: investigate
void copy_own_vec_bad_FN() {
  Vec vec;
  auto copied_own_vec = vec; // copy
}

// copy created only in one branch
void loop_via_copy_bad(std::vector<std::string>& namesOfTheEntirePopulation) {
  for (const auto name :
       namesOfTheEntirePopulation) { // copies the name unnecessarily
                                     // use const auto&
  }
}

// as opposed to the above one, there is no copy here
void loop_no_copy_ok(std::vector<std::string>& namesOfTheEntirePopulation) {
  for (const auto& name : namesOfTheEntirePopulation) {
  }
}
