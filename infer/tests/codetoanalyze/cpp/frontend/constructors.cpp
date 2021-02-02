/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class C {};

class D {
 public:
  D(C x) {}

  D() : D(C{}) {}
};
