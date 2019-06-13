/*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

void materialize_class_template_instances() {
  A* a1 = new A();
  SomeTemplatedClass<int> x1;
  x1.lifetime_error_bad(a1);

  A* a2 = new A();
  SomeTemplatedClass<int*> x2;
  x2.lifetime_error_bad(a2);
}

template <typename T>
void templated_delete_function(T t, A* a) {
  delete a;
}

template <typename T>
void templated_access_function(T t, A* a) {
  int x = a->f;
}

template <typename T>
void templated_function_bad(T t) {
  A* a = new A();
  templated_delete_function<T>(t, a);
  templated_access_function<T>(t, a);
}

template void templated_delete_function<int>(int, A*);
template void templated_delete_function<bool>(bool, A*);
template void templated_access_function<int>(int, A*);
template void templated_access_function<bool>(bool, A*);
template void templated_function_bad<int>(int);
template void templated_function_bad<bool>(bool);

void materialize_function_template_instances() {
  templated_function_bad<int>(42);
  templated_function_bad<bool>(true);
}

} // namespace deduplication
