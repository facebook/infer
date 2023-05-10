// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace BasicObject;

class A {
  private int $f;

  public function __construct(int $f) {
    $this->f = $f;
  }

  public function getField() : int {
    return $this->f;
  }

  public function setField(int $f) : void {
    $this->f = $f;
  }

}

class Main {

  public function bad(): void {
    $tainted = \Level1\taintSource();
    $a = new A(42);
    $a->setField(0);
    $i = $a->getField();
    if ($i == 0) {
      \Level1\taintSink($tainted);
    }
  }

  public function good(): void {
    $tainted = \Level1\taintSource();
    $a = new A(42);
    $a->setField(0);
    $i = $a->getField();
    if ($i != 0) {
      \Level1\taintSink($tainted);
    }
  }
}
