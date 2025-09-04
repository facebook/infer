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

type object2 = shape(
  'safe' => int,
  'unsafe' => int,
  'get_unsafe' => (function(): int),
  'get_safe' => (function(): int),
  'get_key1' => (function(): string),
  'get_key2' => (function(): string),
);

class ClosuresAndDict2_with_self {

  public static function get_unsafe_taint_bad(): void {
    $o = self::init(0);
    $data = $o['get_unsafe']();
    \Level1\taintSink($data);
  }

  public static function get_safe_taint_ok(): void {
    $o = self::init(0);
    $data = $o['get_safe']();
    \Level1\taintSink($data);
  }

  public static function read_key1(dict<string, int> $d): int {
    $o = self::init(0);
    $key = $o['get_key1']();
    return $d[$key];
  }

  public static function read_key2(dict<string, int> $d): int {
    $o = self::init(0);
    $key = $o['get_key2']();
    return $d[$key];
  }

  public static function get_key1_dict_missing_key_ok(): int {
    return self::read_key1(dict['key1' => 1, 'key2typo' => 2]);
  }

  public static function get_key2_dict_missing_key_bad(): int {
    return self::read_key2(dict['key1' => 1, 'key2typo' => 2]);
  }

  public static function init(int $captured): object2 {
    $o = shape('safe' => 0, 'unsafe' => \Level1\taintSource());
    $o['get_unsafe'] = () ==> self::get_unsafe($captured, $o);
    $o['get_safe'] = () ==> self::get_safe($captured, $o);
    $o['get_key1'] = () ==> self::get_key1($captured);
    $o['get_key2'] = () ==> self::get_key2($captured);
    return $o;
  }

  public static function get_safe(
    int $i,
    shape('safe' => int, ...) $self,
  ): int {
    return $self['safe'];
  }

  public static function get_unsafe(
    int $i,
    shape('unsafe' => int, ...) $self,
  ): int {
    return $self['unsafe'];
  }

  public static function get_key1(int $i): string {
    return 'key1';
  }

  public static function get_key2(int $i): string {
    return 'key2';
  }

}

type object2_async = shape(
  'safe' => int,
  'unsafe' => int,
  'get_unsafe' => (function(): Awaitable<int>),
  'get_safe' => (function(): Awaitable<int>),
  'get_key1' => (function(): Awaitable<string>),
  'get_key2' => (function(): Awaitable<string>),
);

class ClosuresAndDict2_with_self_async {

  public static async function get_unsafe_taint_bad(): Awaitable<void> {
    $o = await self::init(0);
    $data = await $o['get_unsafe']();
    \Level1\taintSink($data);
  }

  public static async function FP_get_safe_taint_ok(): Awaitable<void> {
    $o = await self::init(0);
    $data = await $o['get_safe']();
    \Level1\taintSink($data);
  }

  public static async function read_key1(dict<string, int> $d): Awaitable<int> {
    $o = await self::init(0);
    $key = await $o['get_key1']();
    return $d[$key];
  }

  public static async function read_key2(dict<string, int> $d): Awaitable<int> {
    $o = await self::init(0);
    $key = await $o['get_key2']();
    return $d[$key];
  }

  public static async function get_key1_dict_missing_key_ok(): Awaitable<int> {
    return await self::read_key1(dict['key1' => 1, 'key2typo' => 2]);
  }

  public static async function FN_get_key2_dict_missing_key_bad(
  ): Awaitable<int> {
    return await self::read_key2(dict['key1' => 1, 'key2typo' => 2]);
  }

  public static async function init(int $captured): Awaitable<object2_async> {
    $o = shape('safe' => 0, 'unsafe' => \Level1\taintSource());
    $o['get_unsafe'] = async () ==> await self::get_unsafe($captured, $o);
    $o['get_safe'] = async () ==> await self::get_safe($captured, $o);
    $o['get_key1'] = async () ==> await self::get_key1($captured);
    $o['get_key2'] = async () ==> await self::get_key2($captured);
    return $o;
  }

  public static async function get_safe(
    int $i,
    shape('safe' => int, ...) $self,
  ): Awaitable<int> {
    return $self['safe'];
  }

  public static async function get_unsafe(
    int $i,
    shape('unsafe' => int, ...) $self,
  ): Awaitable<int> {
    return $self['unsafe'];
  }

  public static async function get_key1(int $i): Awaitable<string> {
    return 'key1';
  }

  public static async function get_key2(int $i): Awaitable<string> {
    return 'key2';
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
