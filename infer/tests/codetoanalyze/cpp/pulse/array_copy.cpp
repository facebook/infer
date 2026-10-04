/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace array_copy {

struct S {
  int x;
};

struct WithArray {
  S* ptrs[2];
};

struct WithArray2D {
  S* ptrs[2][2];
};

struct Elt {
  S* p;
  Elt() : p(nullptr) {}
  Elt(const Elt& other) : p(other.p) {}
};

struct WithObjectArray {
  Elt elts[2];
};

struct WithLargeArray {
  S* ptrs[6];
};

struct WithDefaultedAssignment {
  S* ptrs[2];
  WithDefaultedAssignment& operator=(const WithDefaultedAssignment&) = default;
};

int implicit_copy_ctor_bad() {
  WithArray a{{nullptr, nullptr}};
  WithArray b = a;
  return b.ptrs[0]->x;
}

int implicit_move_ctor_bad() {
  WithArray a{{nullptr, nullptr}};
  WithArray b = static_cast<WithArray&&>(a);
  return b.ptrs[1]->x;
}

int copy_is_independent_ok(S* s) {
  WithArray a{{s, s}};
  WithArray b = a;
  b.ptrs[0] = nullptr;
  return a.ptrs[0]->x;
}

int implicit_copy_ctor_2d_array_bad() {
  WithArray2D a{{{nullptr, nullptr}, {nullptr, nullptr}}};
  WithArray2D b = a;
  return b.ptrs[1][0]->x;
}

// the sources of copies of objects are modified after the copy so that the
// copies are not reported as unnecessary
int implicit_copy_ctor_object_array_bad() {
  WithObjectArray a;
  a.elts[1].p = nullptr;
  WithObjectArray b = a;
  a.elts[0].p = nullptr;
  return b.elts[1].p->x;
}

int implicit_copy_ctor_object_array_ok(S* s) {
  WithObjectArray a;
  a.elts[1].p = s;
  WithObjectArray b = a;
  a.elts[0].p = nullptr;
  return b.elts[1].p->x;
}

struct Holder {
  WithArray w;
  explicit Holder(const WithArray& w) : w(w) {}
};

int member_copy_init_bad() {
  WithArray a{{nullptr, nullptr}};
  Holder h(a);
  return h.w.ptrs[0]->x;
}

// arrays longer than --clang-compound-literal-init-limit are not copied
int FN_implicit_copy_ctor_large_array_bad() {
  WithLargeArray a{};
  a.ptrs[0] = nullptr;
  WithLargeArray b = a;
  return b.ptrs[0]->x;
}

int implicit_copy_assignment_bad() {
  WithArray a{{nullptr, nullptr}};
  WithArray b{};
  b = a;
  return b.ptrs[0]->x;
}

int implicit_move_assignment_bad() {
  WithArray a{{nullptr, nullptr}};
  WithArray b{};
  b = static_cast<WithArray&&>(a);
  return b.ptrs[1]->x;
}

int implicit_copy_assignment_overwrites_ok(S* s) {
  WithArray a{{s, s}};
  WithArray b{{nullptr, nullptr}};
  b = a;
  return b.ptrs[0]->x;
}

int implicit_copy_assignment_object_array_bad() {
  WithObjectArray a;
  a.elts[1].p = nullptr;
  WithObjectArray b;
  b = a;
  a.elts[0].p = nullptr;
  return b.elts[1].p->x;
}

int defaulted_copy_assignment_bad() {
  WithDefaultedAssignment a{{nullptr, nullptr}};
  WithDefaultedAssignment b{};
  b = a;
  return b.ptrs[0]->x;
}

int lambda_capture_by_value_bad() {
  S* arr[2] = {nullptr, nullptr};
  auto f = [arr]() { return arr[0]->x; };
  return f();
}

int lambda_implicit_capture_by_value_bad() {
  S* arr[2] = {nullptr, nullptr};
  auto f = [=]() { return arr[1]->x; };
  return f();
}

int lambda_capture_by_value_is_a_copy_ok(S* s) {
  S* arr[2] = {s, s};
  auto f = [arr]() { return arr[0]->x; };
  arr[0] = nullptr;
  return f();
}

int lambda_capture_2d_array_by_value_bad() {
  S* arr[2][2] = {{nullptr, nullptr}, {nullptr, nullptr}};
  auto f = [arr]() { return arr[1][0]->x; };
  return f();
}

int lambda_capture_object_array_by_value_bad() {
  Elt arr[2];
  arr[1].p = nullptr;
  auto f = [arr]() { return arr[1].p->x; };
  arr[0].p = nullptr;
  return f();
}

int lambda_capture_ref_to_array_by_value_bad() {
  S* arr[2] = {nullptr, nullptr};
  S*(&ref)[2] = arr;
  auto f = [ref]() { return ref[0]->x; };
  return f();
}

// copies of a closure share the arrays that it captures by value
int FP_copied_mutable_lambda_capture_by_value_ok(S* s) {
  S* arr[2] = {s, s};
  auto f = [arr]() mutable {
    int x = arr[0]->x;
    arr[0] = nullptr;
    return x;
  };
  auto g = f;
  f();
  return g();
}

// arrays longer than --clang-compound-literal-init-limit are not copied
int FN_lambda_capture_large_array_by_value_bad() {
  S* arr[6] = {nullptr};
  auto f = [arr]() { return arr[0]->x; };
  return f();
}

struct Owner {
  int* p;
  int n;
  Owner() : p(new int(0)), n(0) {}
  Owner(const Owner& other) : p(new int(*other.p)), n(other.n) {}
  ~Owner() { delete p; }
};

struct WithOwners {
  Owner owners[2];
};

int member_array_copy_ok(const WithOwners& w) {
  WithOwners copy = w;
  copy.owners[0].n = 1;
  return copy.owners[1].n;
}

// the copy of an array captured by value would not be destroyed so arrays of
// elements with a destructor are not copied
int lambda_capture_owner_array_by_value_ok(const Owner (&owners)[2]) {
  auto f = [owners]() { return owners[1].n; };
  return f();
}

struct Tracked {
  S* p;
  Tracked(const Tracked& other) : p(other.p) {}
  ~Tracked() {}
};

// arrays of elements with a destructor captured by value are not copied
int FN_lambda_capture_tracked_array_by_value_bad(Tracked (&arr)[2]) {
  arr[1].p = nullptr;
  auto f = [arr]() { return arr[1].p->x; };
  return f();
}

struct WithManyArrayElements {
  WithArray2D elts[5];
};

// array copies of more than 16 values are not done element by element
int FN_implicit_copy_ctor_many_array_elements_bad() {
  WithManyArrayElements a{};
  a.elts[0].ptrs[0][0] = nullptr;
  WithManyArrayElements b = a;
  return b.elts[0].ptrs[0][0]->x;
}

struct With16ArrayValues {
  S* ptrs[4];
  int a[4];
  int b[4];
  int c[4];
};

int implicit_copy_ctor_16_array_values_bad() {
  With16ArrayValues a{};
  a.ptrs[0] = nullptr;
  With16ArrayValues b = a;
  return b.ptrs[0]->x;
}

struct WithManyArrays {
  S* ptrs[4];
  int a[4];
  int b[4];
  int c[4];
  int d[2];
};

// the array members of a class are not copied element by element when they
// copy more than 16 values together
int FN_implicit_copy_ctor_many_arrays_bad() {
  WithManyArrays a{};
  a.ptrs[0] = nullptr;
  WithManyArrays b = a;
  return b.ptrs[0]->x;
}

// the array members of a class are not copied element by element when they
// copy more than 16 values together
int FN_implicit_copy_assignment_many_arrays_bad(WithManyArrays& b) {
  WithManyArrays a{};
  a.ptrs[0] = nullptr;
  b = a;
  return b.ptrs[0]->x;
}

struct WithManyOwnerArrays {
  Owner a[2];
  Owner b[2];
  Owner c[2];
  Owner d[2];
  Owner e[2];
};

int many_member_arrays_copy_ok(const WithManyOwnerArrays& w) {
  WithManyOwnerArrays copy = w;
  copy.e[0].n = 1;
  return copy.e[1].n;
}
} // namespace array_copy
