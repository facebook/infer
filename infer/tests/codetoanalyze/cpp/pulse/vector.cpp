/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <iostream>
#include <vector>
#include <memory>

struct Vector {
  std::vector<std::unique_ptr<int>> u_vector;
  std::vector<std::shared_ptr<int>> s_vector;
  int add(std::unique_ptr<int> u_ptr) {
    u_vector.push_back(std::move(u_ptr));
    return 0;
  }
  int add(std::shared_ptr<int> s_ptr) {
    s_vector.push_back(s_ptr);
    return 0;
  }
  static Vector* getInstance() {
    static Vector instance;
    return &instance;
  }
};

// missing a more precise model for vector::push_back
int push_back0_ok(int* value) {
  std::unique_ptr<int> ptr(value);
  Vector* v = Vector::getInstance();
  v->add(std::move(ptr));
  // value should not be deallocated: it is owned by the first element of
  // u_vector
  return *value;
}

// missing a more precise model for vector::push_back
int FP_push_back1_ok(int* value) {
  {
    std::shared_ptr<int> ptr(value);
    Vector* v = Vector::getInstance();
    v->add(ptr);
  }
  // value should not be deallocated: it is owned by the first element of
  // s_vector
  return *value;
}

int push_back0_bad() {
  std::vector<int> v;
  int n = 42;
  v.push_back(n);
  if (v.back() == 42) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int size0_ok() {
  std::vector<int> v;
  if (v.size() != 0) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int size1_ok() {
  std::vector<int> v;
  v.push_back(0);
  v.push_back(42);
  if (v.size() != 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int size2_ok() {
  std::vector<int> v;
  v.push_back(0);
  v.push_back(42);
  std::vector<int> v_copy{v};
  if (v_copy.size() != 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int size3_ok() {
  std::vector<int> v;
  v.push_back(0);
  std::vector<int> v_copy{v};
  v.push_back(0);
  if (v.size() != 2 || v_copy.size() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

// missing a more precise model for std::initializer_list
int FP_size4_ok() {
  std::vector<int> v{0, 42};
  if (v.size() != 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int size5_ok() {
  std::vector<int> v;
  v.push_back(0);
  v.push_back(0);
  v.pop_back();
  if (v.size() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int size0_bad() {
  std::vector<int> v;
  if (v.size() == 0) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int size1_bad() {
  std::vector<int> v;
  v.push_back(0);
  v.push_back(42);
  if (v.size() == 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int size2_bad() {
  std::vector<int> v;
  v.push_back(0);
  std::vector<int> v_copy{v};
  v_copy.push_back(0);
  if (v.size() == 1 && v_copy.size() == 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int empty0_ok() {
  std::vector<int> v;
  if (!v.empty()) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int empty1_ok() {
  std::vector<int> v;
  v.push_back(0);
  if (v.empty()) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int empty2_ok() {
  std::vector<int> v;
  v.push_back(0);
  v.pop_back();
  if (!v.empty()) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int empty0_bad() {
  std::vector<int> v;
  if (v.empty()) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

void deref_vector_element_after_push_back_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  int* y = elt;
  vec.push_back(42);
  std::cout << *y << "\n";
}

// slight variation of above, in particular use vector::at()
void deref_vector_pointer_element_after_push_back_bad(std::vector<int>* vec) {
  int* elt = &vec->at(1);
  int* y = elt;
  vec->push_back(42);
  std::cout << *y << "\n";
}

void deref_local_vector_element_after_push_back_bad() {
  std::vector<int> vec = {0, 0};
  int* elt = &vec[1];
  vec.push_back(42);
  std::cout << *elt << "\n";
}

void deref_null_local_vector_element_bad() {
  std::vector<int*> vec = {nullptr};
  std::cout << *vec[0] << "\n";
}

void two_push_back_ok(std::vector<int>& vec) {
  vec.push_back(32);
  vec.push_back(52);
}

void push_back_in_loop_ok(std::vector<int>& vec, std::vector<int>& vec_other) {
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
}

void reserve_then_push_back_ok(std::vector<int>& vec) {
  vec.reserve(vec.size() + 1);
  int* elt = &vec[1];
  vec.push_back(42);
  std::cout << *elt << "\n";
}

void FN_reserve_too_small_bad() {
  std::vector<int> vec;
  vec.reserve(1);
  vec.push_back(32);
  int* elt = &vec[0];
  vec.push_back(52);
  std::cout << *elt << "\n";
}

void reserve_then_push_back_loop_ok(std::vector<int>& vec,
                                    std::vector<int>& vec_other) {
  vec.reserve(vec.size() + vec_other.size());
  int* elt = &vec[1];
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
  std::cout << *elt << "\n";
}

void FP_init_fill_then_push_back_ok(std::vector<int>& vec_other) {
  std::vector<int> vec(vec_other.size());
  int* elt = &vec[1];
  vec.push_back(0);
  vec.push_back(0);
  vec.push_back(0);
  vec.push_back(0);
  std::cout << *elt << "\n";
}

void push_back_loop_bad(std::vector<int>& vec_other) {
  std::vector<int> vec(2);
  int* elt = &vec[1];
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
  std::cout << *elt << "\n";
}

void reserve_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.reserve(vec.size() + 1);
  std::cout << *elt << "\n";
}

void clear_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.clear();
  std::cout << *elt << "\n";
}

void assign_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.assign(11, 7);
  std::cout << *elt << "\n";
}

void shrink_to_fit_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.shrink_to_fit();
  std::cout << *elt << "\n";
}

void insert_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.insert(vec.begin(), 7);
  std::cout << *elt << "\n";
}

void emplace_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.emplace(vec.begin(), 7);
  std::cout << *elt << "\n";
}

void emplace_back_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.emplace_back(7);
  std::cout << *elt << "\n";
}

void f(int&);

void push_back_value_ok(std::vector<int>& vec) {
  int x = vec[0];
  vec.push_back(7);
  f(x);
}

struct VectorA {
  int x;

  void push_back_value_field_ok(std::vector<int>& vec) {
    x = vec[0];
    vec.push_back(7);
    f(x);
  }
};

void push_back_wrapper() {
  static std::vector<int> v{};
  v.push_back(7);
}

void call_push_back_wrapper_ok() {
  push_back_wrapper();
  push_back_wrapper();
}

int emplace_back_size_ok() {
  std::vector<int> v;
  v.emplace_back(42);
  if (v.size() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int emplace_back_size_bad() {
  std::vector<int> v;
  v.emplace_back(42);
  if (v.size() == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

void push_back_in_callee(std::vector<int>& vec) { vec.push_back(42); }

void push_back_in_nested_callee(std::vector<int>& vec) {
  push_back_in_callee(vec);
}

void clear_in_callee(std::vector<int>& vec) { vec.clear(); }

void push_back_if_in_callee(std::vector<int>& vec, bool b) {
  if (b) {
    vec.push_back(42);
  }
}

void unknown_vector_function(std::vector<int>& vec);

void push_back_then_unknown_call_in_callee(std::vector<int>& vec) {
  vec.push_back(42);
  unknown_vector_function(vec);
}

int size_in_callee(std::vector<int>& vec) { return vec.size(); }

void push_back_in_by_value_callee(std::vector<int> vec) { vec.push_back(42); }

void deref_vector_element_after_push_back_in_callee_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  push_back_in_callee(vec);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_push_back_in_nested_callee_bad(
    std::vector<int>& vec) {
  int* elt = &vec[1];
  push_back_in_nested_callee(vec);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_clear_in_callee_bad(std::vector<int>& vec) {
  int& elt = vec.at(1);
  clear_in_callee(vec);
  std::cout << elt << "\n";
}

void deref_local_vector_element_after_push_back_in_callee_bad() {
  std::vector<int> vec = {0, 0};
  int* elt = &vec[1];
  push_back_in_callee(vec);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_push_back_if_true_in_callee_bad(
    std::vector<int>& vec) {
  int* elt = &vec[1];
  push_back_if_in_callee(vec, true);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_push_back_then_unknown_call_in_callee_bad(
    std::vector<int>& vec) {
  int* elt = &vec[1];
  push_back_then_unknown_call_in_callee(vec);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_push_back_if_false_in_callee_ok(
    std::vector<int>& vec) {
  int* elt = &vec[1];
  push_back_if_in_callee(vec, false);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_size_in_callee_ok(std::vector<int>& vec) {
  int* elt = &vec[1];
  size_in_callee(vec);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_push_back_other_in_callee_ok(
    std::vector<int>& vec, std::vector<int>& vec_other) {
  int* elt = &vec[1];
  push_back_in_callee(vec_other);
  std::cout << *elt << "\n";
}

void get_vector_element_after_push_back_in_callee_ok(std::vector<int>& vec) {
  push_back_in_callee(vec);
  int* elt = &vec[1];
  std::cout << *elt << "\n";
}

void copy_vector_element_before_push_back_in_callee_ok(std::vector<int>& vec) {
  int x = vec[1];
  push_back_in_callee(vec);
  std::cout << x << "\n";
}

void deref_vector_element_after_push_back_in_by_value_callee_ok(
    std::vector<int>& vec) {
  int* elt = &vec[1];
  push_back_in_by_value_callee(vec);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_push_back_on_copy_ok(std::vector<int>& vec) {
  int* elt = &vec[1];
  std::vector<int> copy{vec};
  copy.push_back(42);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_push_back_on_moved_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  std::vector<int> moved{std::move(vec)};
  moved.push_back(42);
  std::cout << *elt << "\n";
}

void deref_null_element_of_vector_copy_bad() {
  std::vector<int*> vec = {nullptr};
  std::vector<int*> copy{vec};
  std::cout << *copy[0] << "\n";
}

void reserve_then_push_back_in_callee_ok(std::vector<int>& vec) {
  vec.reserve(vec.size() + 1);
  int* elt = &vec[1];
  push_back_in_callee(vec);
  std::cout << *elt << "\n";
}

void deref_vector_element_after_reserve_then_clear_in_callee_bad(
    std::vector<int>& vec) {
  vec.reserve(vec.size() + 1);
  int* elt = &vec[1];
  clear_in_callee(vec);
  std::cout << *elt << "\n";
}

void erase_last_in_callee(std::vector<int>& vec) { vec.erase(vec.end() - 1); }

void erase_last_in_callee_keeps_first_ok(std::vector<int>& vec) {
  if (vec.size() < 2) {
    return;
  }
  int& first = vec[0];
  erase_last_in_callee(vec);
  std::cout << first << "\n";
}

void resize_to_one_in_callee(std::vector<int>& vec) { vec.resize(1); }

void resize_smaller_in_callee_keeps_first_ok(std::vector<int>& vec) {
  int& first = vec[0];
  resize_to_one_in_callee(vec);
  std::cout << first << "\n";
}

struct Point {
  int x;
  int y;
};

void push_back_point_in_callee(std::vector<Point>& points) {
  points.push_back(Point{0, 0});
}

// only the elements are invalidated, not their fields
void FN_element_field_ptr_after_grow_bad(std::vector<Point>& points) {
  int* x = &points[0].x;
  push_back_point_in_callee(points);
  std::cout << *x << "\n";
}

void move_and_push_back_in_callee(std::vector<int>&& vec) {
  std::vector<int> moved{std::move(vec)};
  moved.push_back(42);
}

// the callee only invalidates the internal array of the vector it moved to
void FN_deref_vector_element_after_move_and_push_back_in_callee_bad(
    std::vector<int>& vec) {
  int* elt = &vec[1];
  move_and_push_back_in_callee(std::move(vec));
  std::cout << *elt << "\n";
}

// the capture by value does not call the copy constructor, so the lambda's copy
// shares the elements of vec
void FP_deref_vector_element_after_push_back_on_captured_copy_ok(
    std::vector<int>& vec) {
  int* elt = &vec[1];
  auto f = [vec]() mutable { vec.push_back(42); };
  f();
  std::cout << *elt << "\n";
}

struct VectorRegistry {
  std::vector<int> items;

  int& create(int x) {
    items.push_back(x);
    return items[items.size() - 1];
  }

  int create_twice_bad() {
    int& a = create(1);
    int& b = create(2);
    return a + b;
  }

  int create_once_ok() {
    int& a = create(1);
    return a;
  }
};
