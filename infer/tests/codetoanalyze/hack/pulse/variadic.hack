// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace DictTests;

class Variadic {

  public static function variadicArgInSink(int $i, int ...$args) : void {
    foreach ($args as $arg) {
      \Level1\taintSink($arg);
    }
  }

  public static function variadicArg0InSink(int $i, int ...$args) : void {
    \Level1\taintSink($args[0]);
  }

  public static function callVariadic(int ...$args) : void {
    self::variadicArgInSink(0, ...$args);
  }

  public static function FN_callVariadicWith2ArgsBad(int $i) : void {
    self::variadicArgInSink(0, \Level1\taintSource());
  }

  public static function callVariadicWith2ArgsOk(int $i) : void {
    self::variadicArgInSink(\Level1\taintSource(), 0);
  }

  public static function FN_callVariadicWith3ArgsBad() : void {
    self::variadicArgInSink(0, 0, \Level1\taintSource());
  }

  public static function FN_callVariadicWith3ArgsBisBad() : void {
    self::variadicArgInSink(0, \Level1\taintSource(), 0);
  }

  public static function FN_transitiveCallVariadicWith2ArgsBad(int $i) : void {
    self::callVariadic(\Level1\taintSource());
  }

  public static function FN_transitiveCallVariadicWith3ArgsBad() : void {
    self::callVariadic(0, \Level1\taintSource());
  }

  public static function FN_transitiveCallVariadicWith3ArgsBisBad() : void {
    self::callVariadic(\Level1\taintSource(), 0);
  }

  public static function FN_callVariadicArg0InSinkBad() : void {
    self::variadicArg0InSink(0, \Level1\taintSource());
  }

  public static function callVariadicArg0InSinkOk() : void {
    self::variadicArg0InSink(0, 0, \Level1\taintSource());
  }

  public static function callVariadicArg0InSinkBisOk() : void {
    self::variadicArg0InSink(\Level1\taintSource());
  }
}
