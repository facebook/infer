/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

template <class T>
struct AnotherClass {};

template <class T>
struct Y {
  friend class AnotherClass<T *>;
  template <class Z>
  friend class AnotherClass;
};

Y<int> y1;
