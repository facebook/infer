/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace N {
template <class T>
struct S {
  T field;
};

template <class T>
struct S<T *> {
  T field; /* not T* !!! */
};

template <>
struct S<int> {
  int field;
};

void test(S<int> p) {
  S<S<int> *> val;
  val.field = p;
}
} // namespace N
