// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace StaticTests;

class A {
  public static function source(): int {
    return \Level1\taintSource();
  }
}

class B extends A {}

class Main {
  public function fromABad(): void {
    $tainted = A::source();
    \Level1\taintSink($tainted);
  }

  public function fromBBad(): void {
    $tainted = B::source();
    \Level1\taintSink($tainted);
  }

}
