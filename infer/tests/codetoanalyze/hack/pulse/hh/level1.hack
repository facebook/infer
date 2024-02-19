// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Level1;

class A {
  public function myUnknownFun(): \Unknown\SensitiveClass {
    return new \Unknown\SensitiveClass();
  }
}

function unknownTaintSource(): A {
  return new A();
}
