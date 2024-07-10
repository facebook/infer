// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class MyBuilder {
  private int $a = 0;
  private int $b = 0;

  public function __construct() {}

  public function setA(int $a): MyBuilder {
    $this->a = $a;
    return $this;
  }

  public function setB(int $b): MyBuilder {
    $this->b = $b;
    return $this;
  }

  public function getA(): int {
    return $this->a;
  }

  public function saveX(): vec<int> {
    return vec[$this->a, $this->b];
  }
}

class BuilderTester {
  public static function builderUserOK(): void {
    $b = new MyBuilder();
    $b->setA(42)->setB(97)->saveX();
  }

  public static function builderUserBad(): void {
    $b = new MyBuilder();
    $b->setA(42)->setB(97);
  }
}
