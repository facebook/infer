/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace class_specialization {

template <typename T>
struct Base {
  T x;
};

template <typename T>
struct Derived : public Base<T> {
  void foo(T t) { this->x = t; }
};

template <typename T>
struct Derived<T*> : public Base<T*> {
  void foo2(T* t) { this->x = t; }
};

void foo_intptr() {
  Derived<int*> b;
  b.foo2(nullptr);
  int x = *b.x;
}

void foo_int() {
  Derived<int> b;
  b.foo(0);
  int z = 1 / b.x;
}

} // namespace class_specialization
