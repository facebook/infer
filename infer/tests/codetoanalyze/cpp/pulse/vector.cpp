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

void push_back_loop_latent(std::vector<int>& vec_other) {
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
