// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class A {
  public function source(): int {
    return taintSource();
  }
}

class B extends A {}

class Main {
  function fromABad(): void {
    $tainted = (new A())->source();
    taintSink($tainted);
  }

  function fromBBad(): void {
    $tainted = (new B())->source();
    taintSink($tainted);
  }

}
