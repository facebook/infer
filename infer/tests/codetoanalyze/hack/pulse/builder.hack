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

class MyImmediatelyDiscardableBuilder implements TestBuilderBase {
  public function __construct() {}

  public function setFoo(int $a): this {
    return $this;
  }

  public function doFinalize(): void {
    return;
  }
}

<<__ConsistentConstruct>>
abstract class AbstractBuilder {
  abstract public function __construct(int $foo);
}

<<__ConsistentConstruct>>
class MyBuilder extends AbstractBuilder {
  private int $a = 0;
  private int $b = 0;

  public function __construct(int $foo) {}

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
    $b = new MyBuilder(0);
    $b->setA(42)->setB(97)->saveX();
  }

  public static function builderUserBad(): void {
    $b = new MyBuilder(0);
    $b->setA(42)->setB(97);
  }

  public static function vectorOfBuildersOK(): void {
    $v = vec[new MyBuilder(0), new MyBuilder(1)];
    foreach ($v as $b) {
      $b->setA(42);
    }
    foreach ($v as $b) {
      $x = $b->saveX();
    }
  }

  public static function maybeThrow(): void {
    throw new Exception();
  }

  public static function exceptionalBuilderOK(): void {
    try {
      $b = new MyBuilder(0);
      $b->setA(42);
      self::maybeThrow();
      $b->saveX();
    } catch (Exception $e) {
      // do nothing
    }
  }

  // Would be an FP except for deep_clean_hack_value
  public static function vectorOfBuilders2OK(): void {
    $v = vec[new MyBuilder(0), new MyBuilder(1), new MyBuilder(2)];
    foreach ($v as $b) {
      $b->setA(42);
    }
    foreach ($v as $b) {
      $x = $b->saveX();
    }
  }

  public static function builderWithoutCallsOk(bool $flag): void {
    $changes_made = false;
    $b = new MyBuilder(0);

    if ($flag) {
      $b->setA(42);
      $changes_made = true;
    }

    if ($changes_made) {
      $b->saveX();
    }
  }

  // now check builders recognised from the .inferconfig file
  public static function testConfigBad(): void {
    $b = new NoBuilderSuffix();
    $b->setFoo(42);
  }

  public static function testConfigGood(): void {
    $b = new NoBuilderSuffix();
    $b->setFoo(42);
    $b->doFinalize();
  }

  // check that we can recongnise immediately discardable builders
  // from the .inferconfig and handle them accordingly
  public static function testImmediatelyDiscardableBuilderBad(): void {
    $b = new MyImmediatelyDiscardableBuilder();
  }
}

// In real code, builders are often created via some icky reflection
// so see that works
final class BuilderTester2 {
  const type TB = MyBuilder;

  public static function create(int $arg): this::TB {
    $builder_cls = type_structure(static::class, 'TB')['classname'];
    return new $builder_cls($arg);
  }

  public static function testCreateOK(): void {
    $b = static::create(0);
    $b->setA(42);
    $b->saveX();
  }

  public static function testCreateBad(): void {
    $b = static::create(0);
    $b->setA(42);
  }

}

// Now wrap that pattern in a trait and a bit of hierarchy to make it even harder to spot
abstract class MutatorRealBase {
  abstract const type TB as AbstractBuilder;
}

abstract class MutatorBase extends MutatorRealBase {
  use MutatorTrait;
}

trait MutatorTrait {

  require extends MutatorRealBase;

  public static function create(int $arg): this::TB {
    $builder_cls = type_structure(static::class, 'TB')['classname'];
    return new $builder_cls($arg);
  }
}

final class ConcreteMutator extends MutatorBase {
  const type TB = MyBuilder;
}

final class BuilderTester3 {

  public static function testCreateOK(): void {
    $b = ConcreteMutator::create(42);
    $b->setA(42);
    $b->saveX();
  }

  public static function testCreateBad(): void {
    $b = ConcreteMutator::create(42);
    $b->setA(42);
  }
}
