// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// check we resolve constants in the right order

namespace AbstractConstants;

abstract class C {
  abstract const int MyC;
}

interface I {
  const int MyC = 1;
}

trait T {
  const int MyC = 2;
}

class D extends C implements I {

}

class E extends C {
  use T;
}

class Tester {
  public static function constant_from_interface_OK(): void {
    D::MyC;
  }

  public static function constant_from_trait_OK(): void {
    E::MyC;
  }
}
