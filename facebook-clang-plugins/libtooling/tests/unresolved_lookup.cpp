/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
template <typename T>
struct S {
  template <class U>
  static int foo(U *);
  static const unsigned int s = sizeof(S<T>::foo<T>(0));
};
