// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Sanitizers;

class Source {
  public function getTainted(): int {
    return 42;
  }
}

class Sink {
  public static function process(mixed $arg): void {}
}

class San {
  public static function sanitize(mixed $arg): void {
    Sink::process($arg);
  }
  public static async function sanitizeTest(mixed $arg): Awaitable<bool> {
    return \Unknown\UnknownClass::someBool();
  }
}

class Flows {
  public static function taintedSanitizedOk(Source $arg): void {
    $t = $arg->getTainted();
    San::sanitize($t);
    Sink::process($t);
  }

  public static function taintedNotSanitizedBad(Source $arg): void {
    $t = $arg->getTainted();
    Sink::process($t);
  }

  public static async function taintedIfSanitizedOk(
    Source $arg,
  ): Awaitable<void> {
    $t = $arg->getTainted();
    $b = await San::sanitizeTest((int)$t);
    if ($b) {
      Sink::process((int)$t);
    }
  }
  public static async function boolTest(mixed $arg): Awaitable<bool> {
    return true;
  }
  public static async function taintedIfSanitizedBad(
    Source $arg,
  ): Awaitable<void> {
    $t = $arg->getTainted();
    $b = await self::boolTest((int)$t);
    if ($b) {
      Sink::process((int)$t);
    }
  }

  // the examples below use chains of 2 or 3 calls
  public static async function FP_call_cast_and_call_sanitized_call3_ok(
    Source $arg,
  ): Awaitable<void> {
    $t = $arg->getTainted();
    await self::cast_and_call_sanitized_call3($t);
  }

  public static async function call_cast_and_call_not_sanitized_call3_bad(
    Source $arg,
  ): Awaitable<void> {
    $t = $arg->getTainted();
    await self::cast_and_call_not_sanitized_call3($t);
  }

  public static async function call_sanitized_call3_ok(
    Source $arg,
  ): Awaitable<void> {
    $t = $arg->getTainted();
    await self::sanitized_call3((string)$t);
  }

  public static async function call_not_sanitized_call3_bad(
    Source $arg,
  ): Awaitable<void> {
    $t = $arg->getTainted();
    await self::not_sanitized_call3((string)$t);
  }

  public static async function cast_and_call_sanitized_call3(
    int $t,
  ): Awaitable<void> {
    await self::sanitized_call3((string)$t);
  }

  public static async function cast_and_call_not_sanitized_call3(
    int $t,
  ): Awaitable<void> {
    await self::not_sanitized_call3((string)$t);
  }

  public static async function sanitized_call3(
    string $str,
  ): Awaitable<void> {
    $b = await San::sanitizeTest($str);
    if ($b) {
      Sink::process((int)$str);
    }
  }

  public static async function not_sanitized_call3(
    string $str,
  ): Awaitable<void> {
    $b = await self::boolTest($str);
    if ($b) {
      Sink::process((int)$str);
    }
  }

}
