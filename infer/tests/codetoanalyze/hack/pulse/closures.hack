// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Closures;

class SensitiveClass {}

class Delayed {
  public function startAndWait((function(): Awaitable<void>) $action): void {
    \HH\Asio\join($action());
  }
}

class Utils {
  public function logDelayed(mixed $data): void {
    (new Delayed())->startAndWait(async () ==> {
      \Level1\taintSink($data);
    });
  }
}

class C1 {
  public function f1Bad(SensitiveClass $sc): void {
    (new Utils())->logDelayed($sc);
  }
}

class A {
  public static function id_fst(int $x, int $y): int {
    $y = 0;
    return $x;
  }
}

class Main {

  public function id_fst_bad(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $x = 0;
    $y = $v;
    $f = (int $x): int ==> A::id_fst($x, $y);
    $i = $f($u);
    if ($i == $u && $y == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function id_fst_good(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $x = 0;
    $y = $v;
    $f = (int $x): int ==> A::id_fst($x, $y);
    $i = $f($u);
    if ($i != $u || $y != $v) {
      \Level1\taintSink($tainted);
    }
  }
}

class ClosuresAndDict1 {

  public static function main_bad(): void {
    $user = new User();
    $value = self::run(
      function($input) {
        return $input['arg']->getSource();
      },
      shape('arg' => $user->getUnsafe()),
    );
    \Level1\taintSink($value);
  }

  public static function main_ok(): void {
    $user = new User();
    $value = self::run(
      function($input) {
        return $input['arg']->getSource();
      },
      shape('arg' => $user->getSafe()),
    );
    \Level1\taintSink($value);
  }

  public static function run<TInput, TOutput>(
    (function(TInput): TOutput) $fun,
    TInput $input,
  ): TOutput {
    return $fun($input);
  }

}

class User {

  public Unsafe $unsafe;
  public Safe $safe;

  public function __construct() {
    $this->unsafe = new Unsafe();
    $this->safe = new Safe();
  }

  public function getSafe(): Safe {
    return $this->safe;
  }

  public function getUnsafe(): Unsafe {
    return $this->unsafe;
  }

}

interface I {
  public function getSource(): int;
}

class Safe implements I {

  public function getSource(): int {
    return 42;
  }
}

class Unsafe implements I {

  public function getSource(): int {
    return \Level1\taintSource();
  }
}

type object = shape(
  'safe' => int,
  'unsafe' => int,
  'get_unsafe' => (function(): int),
  'get_safe' => (function(): int),
);

class ClosuresAndDict2 {

  public static function get_unsafe_bad(): void {
    $o = self::init();
    $data = $o['get_unsafe']();
    \Level1\taintSink($data);
  }

  public static function get_safe_ok(): void {
    $o = self::init();
    $data = $o['get_safe']();
    \Level1\taintSink($data);
  }

  public static function get_unsafe_method(object $o): void {
    \Level1\taintSink($o['get_unsafe']());
  }
  public static function call_get_unsafe_method_bad(): void {
    $o = self::init();
    self::get_unsafe_method($o);
  }

  public static function get_safe_method(object $o): void {
    \Level1\taintSink($o['get_safe']());
  }
  public static function call_get_safe_method_ok(): void {
    $o = self::init();
    self::get_safe_method($o);
  }

  public static function init(): object {
    $o = shape('safe' => 0, 'unsafe' => \Level1\taintSource());
    $o['get_unsafe'] = () ==> ClosuresAndDict2::get_unsafe($o);
    $o['get_safe'] = () ==> ClosuresAndDict2::get_safe($o);
    return $o;
  }

  public static function get_safe(shape('safe' => int, ...) $self): int {
    return $self['safe'];
  }

  public static function get_unsafe(shape('unsafe' => int, ...) $self): int {
    return $self['unsafe'];
  }

}

class ClosuresAndDict2_with_self {

  public static function get_unsafe_bad(): void {
    $o = self::init();
    $data = $o['get_unsafe']();
    \Level1\taintSink($data);
  }

  public static function FP_get_safe_ok(): void {
    $o = self::init();
    $data = $o['get_safe']();
    \Level1\taintSink($data);
  }

  public static function init(): object {
    $o = shape('safe' => 0, 'unsafe' => \Level1\taintSource());
    $o['get_unsafe'] = () ==> self::get_unsafe($o);
    $o['get_safe'] = () ==> self::get_safe($o);
    return $o;
  }

  public static function get_safe(shape('safe' => int, ...) $self): int {
    return $self['safe'];
  }

  public static function get_unsafe(shape('unsafe' => int, ...) $self): int {
    return $self['unsafe'];
  }

}

class ThenAsyncPattern {
  public static async function gen(): Awaitable<void> {
    return;
  }

  public static async function then_async<TOut>(
    (function()[_]: Awaitable<TOut>) $fn,
  )[ctx $fn]: Awaitable<TOut> {
    return await $fn();
  }

  public static async function static_method_bad(): Awaitable<void> {
    await self::then_async(async () ==> self::gen());
    return;
  }

  public static async function static_method_ok(): Awaitable<void> {
    await self::then_async(async () ==> await self::gen());
    return;
  }

  public async function FN_method_bad(): Awaitable<void> {
    await self::then_async(async () ==> self::gen());
    return;
  }

  public async function method_ok(): Awaitable<void> {
    await self::then_async(async () ==> await self::gen());
    return;
  }
}
