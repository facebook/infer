// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace RecursionLevel1;

interface A {
  public function compute(): int;
}

class CycleNoFieldBAD {
  protected function __construct(protected A $a) {
  }

  public function getA(): A {
    return $this->a;
  }

  public function step1(): void {
    $this->step2();
  }

  private function step2(): void {
    $this->getA()->compute();
    $this->step1();
  }
}

class FNCycleWithFieldBAD {
  protected function __construct(protected A $a) {
  }

  public function getA(): A {
    return $this->a;
  }

  public function step1(): void {
    $this->step2($this->getA());
  }

  private function step2(A $a): void {
    $a->compute();
    $this->step1();
  }
}
