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
  public static function FN_runFooBad(): void {
    $f = Main::foo<>;
    \Level1\taintSink($f(true, true));
  }

  public static function runFooGood(): void {
    $f = Main::foo<>;
    \Level1\taintSink($f(true, false));
  }
}

class Test2 {
  public static function apply((function(bool, bool): int) $f, bool $x, bool $y): int {
    return $f($x, $y);
  }

  public static function FN_runFooUsingApplyBad(): void {
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
    private bool $x
  ) {}

  public function run(bool $y): int {
    return ($this->fun)($this->x, $y);
  }
}

class Test3 {
  public static function FN_runFooWithBundleBad(): void {
    $f = Main::foo<>;
    \Level1\taintSink((new Bundle($f, true))->run(true));
  }

  public static function runFooWithBundleGood(): void {
    $f = Main::foo<>;
    \Level1\taintSink((new Bundle($f, true))->run(false));
  }
}
