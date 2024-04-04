// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace FunctionReference;

class Main {
  public static function foo(bool $x, bool $y): int {
    if ($x && $y) return \Level1\taintSource();
    else return 42;
  }
}

class Test1 {
  public static function runFooBad(): void {
    $f = Main::foo<>;
    \Level1\taintSink($f(true, true));
  }

  public static function runFooGood(): void {
    $f = Main::foo<>;
    \Level1\taintSink($f(true, false));
  }
}

class Test2 {
  public static function apply(
    (function(bool, bool): int) $f,
    bool $x,
    bool $y,
  ): int {
    return $f($x, $y);
  }

  public static function runFooUsingApplyBad(): void {
    $f = Main::foo<>;
    \Level1\taintSink(self::apply($f, true, true));
  }

  public static function runFooUsingApplyGood(): void {
    $f = Main::foo<>;
    \Level1\taintSink(self::apply($f, true, false));
  }
}

class Bundle {
  public function __construct(
    private (function(bool, bool): int) $fun,
    private bool $x,
  ) {}

  public function run(bool $y): int {
    return ($this->fun)($this->x, $y);
  }
}

class Test3 {
  public static function runFooWithBundleBad(): void {
    $f = Main::foo<>;
    \Level1\taintSink((new Bundle($f, true))->run(true));
  }

  public static function runFooWithBundleGood(): void {
    $f = Main::foo<>;
    \Level1\taintSink((new Bundle($f, true))->run(false));
  }
}

abstract class A {

  public static function lateBinding(bool $x, bool $y): int {
    return static::foo($x, $y);
  }

  public abstract static function foo(bool $x, bool $y): int;

}

class B extends A {

  public static function foo(bool $x, bool $y): int {
    return Main::foo($x, $y);
  }

}

class Test4 {
  public static function runFooWithLateBindingBad(): void {
    $f = B::lateBinding<>;
    \Level1\taintSink($f(true, true));
  }

  public static function runFooWithLateBindingGood(): void {
    $f = B::lateBinding<>;
    \Level1\taintSink($f(true, false));
  }
}

// function references & variadics args

abstract final class LogicalOps {

  public static function orVariadic(
    (function(): bool) $first_predicate,
    (function(): bool) $second_predicate,
    (function(): bool) ...$rest_predicates
  ): bool {
    if ($first_predicate()) {
      return true;
    }
    if ($second_predicate()) {
      return true;
    }
    foreach ($rest_predicates as $predicate) {
      if ($predicate()) {
        return true;
      }
    }
    return false;
  }

}

abstract class Predicates {

  public static function alwaysTrue(): bool {
    return true;
  }

  public static function alwaysFalse(): bool {
    return false;
  }

}

class TestVariadicAndFunctionReferences {

  public static function noVariadicBad(): void {
    $tainted = \Level1\taintSource();
    if (
      LogicalOps::orVariadic(
        Predicates::alwaysFalse<>,
        Predicates::alwaysTrue<>,
      )
    ) {
      \Level1\taintSink($tainted);
    }
  }

  public static function noVariadicGood(): void {
    $tainted = \Level1\taintSource();
    if (
      LogicalOps::orVariadic(
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
      )
    ) {
      \Level1\taintSink($tainted);
    }
  }

  public static function oneVariadicBad(): void {
    $tainted = \Level1\taintSource();
    if (
      LogicalOps::orVariadic(
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysTrue<>,
      )
    ) {
      \Level1\taintSink($tainted);
    }
  }

  public static function oneVariadicGood(): void {
    $tainted = \Level1\taintSource();
    if (
      LogicalOps::orVariadic(
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
      )
    ) {
      \Level1\taintSink($tainted);
    }
  }

  public static function twoVariadicBad(): void {
    $tainted = \Level1\taintSource();
    if (
      LogicalOps::orVariadic(
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysTrue<>,
      )
    ) {
      \Level1\taintSink($tainted);
    }
  }

  public static function twoVariadicGood(): void {
    $tainted = \Level1\taintSource();
    if (
      LogicalOps::orVariadic(
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
      )
    ) {
      \Level1\taintSink($tainted);
    }
  }

  public static function FN_threeVariadicBad(): void {
    $tainted = \Level1\taintSource();
    if (
      LogicalOps::orVariadic(
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysTrue<>,
      )
    ) {
      \Level1\taintSink($tainted);
    }
  }

  public static function threeVariadicGood(): void {
    $tainted = \Level1\taintSource();
    if (
      LogicalOps::orVariadic(
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
        Predicates::alwaysFalse<>,
      )
    ) {
      \Level1\taintSink($tainted);
    }
  }

}
