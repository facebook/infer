/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct Foo {
  int x;

  bool operator<(const Foo& rhs) const { return x < rhs.x; }
};

void call_lt_pure(Foo& lhs, Foo& rhs) { lhs < rhs; }
