// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace CallVariadic;

class CallVariadic {

  public static function callVariadic(int ...$args): void {
    \Variadic\Variadic::variadicArgInSink(0, ...$args);
  }

  public static function callVariadicWith2ArgsBad(int $i): void {
    \Variadic\Variadic::variadicArgInSink(0, \Level1\taintSource());
  }

  public static function callVariadicWith2ArgsOk(int $i): void {
    \Variadic\Variadic::variadicArgInSink(\Level1\taintSource(), 0);
  }

  public static function callVariadicWith3ArgsBad(): void {
    \Variadic\Variadic::variadicArgInSink(0, 0, \Level1\taintSource());
  }

  public static function callVariadicWith3ArgsBisBad(): void {
    \Variadic\Variadic::variadicArgInSink(0, \Level1\taintSource(), 0);
  }

  public static function transitiveCallVariadicWith2ArgsBad(int $i): void {
    \Variadic\Variadic::callVariadic(\Level1\taintSource());
  }

  public static function transitiveCallVariadicWith3ArgsBad(): void {
    \Variadic\Variadic::callVariadic(0, \Level1\taintSource());
  }

  public static function transitiveCallVariadicWith3ArgsBisBad(): void {
    \Variadic\Variadic::callVariadic(\Level1\taintSource(), 0);
  }

  public static function callVariadicArg0InSinkBad(): void {
    \Variadic\Variadic::variadicArg0InSink(0, \Level1\taintSource());
  }

  public static function FP_callVariadicArg0InSinkOk(): void {
    \Variadic\Variadic::variadicArg0InSink(0, 0, \Level1\taintSource());
  }

  public static function callVariadicArg0InSinkBisOk(): void {
    \Variadic\Variadic::variadicArg0InSink(\Level1\taintSource());
  }
}
