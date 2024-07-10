// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// TestBuilderBase is declared as a builder in .inferconfig
interface TestBuilderBase {
  public function setFoo(int $x): this;
  public function doFinalize(): void;
}

// so this should be recognised as a builder
// which it is, provided we have a known __construct
class NoBuilderSuffix implements TestBuilderBase {
  public function __construct() {}

  public function setFoo(int $a): this {
    return $this; // actually does nothing
  }

  public function doFinalize(): void {
    return; // also actually does nothing
  }
}

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

  public static function vectorOfBuildersOK(): void {
    $v = vec[new MyBuilder(), new MyBuilder()];
    foreach ($v as $b) {
      $b->setA(42);
    }
    foreach ($v as $b) {
      $x = $b->saveX();
    }
  }

  // Would be an FP except for deep_clean_hack_value
  public static function vectorOfBuilders2OK(): void {
    $v = vec[new MyBuilder(), new MyBuilder(), new MyBuilder()];
    foreach ($v as $b) {
      $b->setA(42);
    }
    foreach ($v as $b) {
      $x = $b->saveX();
    }
  }

  // now check builders recognised from the .inferconfig file
  public static function testConfigBad(): void {
    $b = new NoBuilderSuffix();
  }

  public static function testConfigBad2(): void {
    $b = new NoBuilderSuffix();
    $b->setFoo(42);
  }

  public static function testConfigGood(): void {
    $b = new NoBuilderSuffix();
    $b->setFoo(42);
    $b->doFinalize();
  }
}
