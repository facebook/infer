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

class CycleWithFieldBAD {
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

class FNCycleNameOrderMattersBADV1 {
  public function foo1(int $i, int $_): void {
    $this->foo2($i);
  }
  public function foo2(int $i): void {
    $this->foo1($i, 0);
  }
}

class CycleNameOrderMattersBADV2 {
  public function foo2(int $i, int $_): void {
    $this->foo1($i);
  }
  public function foo1(int $i): void {
    $this->foo2($i, 0);
  }

}
