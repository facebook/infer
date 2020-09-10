/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
template <typename T, bool>
class __foo;

template <typename T>
class __foo<T, true> {
  int x = sizeof(T);

 public:
  __foo(){};
};

int y = sizeof(char);
