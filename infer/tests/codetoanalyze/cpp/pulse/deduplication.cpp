/*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace deduplication {

struct A {
  int f;
};

template <typename T>
struct SomeTemplatedClass {
  void templated_wrapper_delete_ok(A* a);
  void templated_wrapper_access_ok(A* a);
  void lifetime_error_bad(A* a);
};

template <typename T>
void SomeTemplatedClass<T>::templated_wrapper_delete_ok(A* a) {
  delete a;
}

template <typename T>
void SomeTemplatedClass<T>::templated_wrapper_access_ok(A* a) {
  int x = a->f;
}

template <typename T>
void SomeTemplatedClass<T>::lifetime_error_bad(A* a) {
  SomeTemplatedClass<T>::templated_wrapper_delete_ok(a);
  SomeTemplatedClass<T>::templated_wrapper_access_ok(a);
}

void materialize_template_instances() {
  A* a1 = new A();
  SomeTemplatedClass<int> x1;
  x1.lifetime_error_bad(a1);

  A* a2 = new A();
  SomeTemplatedClass<int*> x2;
  x2.lifetime_error_bad(a2);
}

} // namespace deduplication
