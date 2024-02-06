// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

trait T {

  use TParent; // implements TParent::callee(int)

  public static function caller(int $i): int {
    return T::callee($i);
  }
}
