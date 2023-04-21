// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace ExtendsTests;

class A {
  public function source(): int {
    return \Level1\taintSource();
  }
}

class B extends A {}

class Main {
  function fromABad(): void {
    $tainted = (new A())->source();
    \Level1\taintSink($tainted);
  }

  function fromBBad(): void {
    $tainted = (new B())->source();
    \Level1\taintSink($tainted);
  }

}
