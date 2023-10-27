// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Variadic;

class Variadic {

  public static function variadicArgInSink(int $i, int ...$args): void {
    foreach ($args as $arg) {
      \Level1\taintSink($arg);
    }
  }

  public static function variadicArg0InSink(int $i, int ...$args): void {
    \Level1\taintSink($args[0]);
  }

  public static function callVariadic(int ...$args): void {
    self::variadicArgInSink(0, ...$args);
  }

  public static function callVariadicWith2ArgsBad(int $i): void {
    self::variadicArgInSink(0, \Level1\taintSource());
  }

  public static function callVariadicWith2ArgsOk(int $i): void {
    self::variadicArgInSink(\Level1\taintSource(), 0);
  }

  public static function callVariadicWith3ArgsBad(): void {
    self::variadicArgInSink(0, 0, \Level1\taintSource());
  }

  public static function callVariadicWith3ArgsBisBad(): void {
    self::variadicArgInSink(0, \Level1\taintSource(), 0);
  }

  public static function transitiveCallVariadicWith2ArgsBad(int $i): void {
    self::callVariadic(\Level1\taintSource());
  }

  public static function transitiveCallVariadicWith3ArgsBad(): void {
    self::callVariadic(0, \Level1\taintSource());
  }

  public static function transitiveCallVariadicWith3ArgsBisBad(): void {
    self::callVariadic(\Level1\taintSource(), 0);
  }

  public static function callVariadicArg0InSinkBad(): void {
    self::variadicArg0InSink(0, \Level1\taintSource());
  }

  public static function FP_callVariadicArg0InSinkOk(): void {
    self::variadicArg0InSink(0, 0, \Level1\taintSource());
  }

  public static function callVariadicArg0InSinkBisOk(): void {
    self::variadicArg0InSink(\Level1\taintSource());
  }
}

class WithoutVariadic {

  public static function variadicArgInSink(int $i, vec<int> $args): void {
    foreach ($args as $arg) {
      \Level1\taintSink($arg);
    }
  }

  public static function variadicArg0InSink(int $i, vec<int> $args): void {
    \Level1\taintSink($args[0]);
  }

  public static function callVariadic(vec<int> $args): void {
    self::variadicArgInSink(0, $args);
  }

  public static function callVariadicWith2ArgsBad(int $i): void {
    self::variadicArgInSink(0, vec[\Level1\taintSource()]);
  }

  public static function callVariadicWith2ArgsOk(int $i): void {
    self::variadicArgInSink(\Level1\taintSource(), vec[0]);
  }

  public static function callVariadicWith3ArgsBad(): void {
    self::variadicArgInSink(0, vec[0, \Level1\taintSource()]);
  }

  public static function callVariadicWith3ArgsBisBad(): void {
    self::variadicArgInSink(0, vec[\Level1\taintSource(), 0]);
  }

  public static function transitiveCallVariadicWith2ArgsBad(int $i): void {
    self::callVariadic(vec[\Level1\taintSource()]);
  }

  public static function transitiveCallVariadicWith3ArgsBad(): void {
    self::callVariadic(vec[0, \Level1\taintSource()]);
  }

  public static function transitiveCallVariadicWith3ArgsBisBad(): void {
    self::callVariadic(vec[\Level1\taintSource(), 0]);
  }

  public static function callVariadicArg0InSinkBad(): void {
    self::variadicArg0InSink(0, vec[\Level1\taintSource()]);
  }

  public static function FP_callVariadicArg0InSinkOk(): void {
    self::variadicArg0InSink(0, vec[0, \Level1\taintSource()]);
  }

  public static function callVariadicArg0InSinkBisOk(): void {
    self::variadicArg0InSink(\Level1\taintSource(), vec[]);
  }
}
