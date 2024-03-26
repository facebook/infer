// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

function variadic(string $format, mixed ...$args): void {
}

function main1(string $format, string $x): void {
  variadic($format, $x);
}

function main2(string $format, string $x, string $y): void {
  variadic($format, $x, $y);
}

function variadic_with_optionals(
  string $format,
  string $opt1 = "",
  string $opt2 = "",
  mixed ...$args
): void {
}

function main_using_optionnals1(string $format, string $x): void {
  variadic_with_optionals($format, $x);
}

function main_using_optionnals2(string $format, string $x, string $y): void {
  variadic_with_optionals($format, $x, $y);
}

function main_using_optionnals3(
  string $format,
  string $x,
  string $y,
  string $z,
): void {
  variadic_with_optionals($format, $x, $y, $z);
}

function main_using_optionnals4(
  string $format,
  string $x,
  string $y,
  string $z,
  string $t,
): void {
  variadic_with_optionals($format, $x, $y, $z, $t);
}

function with_optional_and_reify<reify T>(
  ?string $msg = null,
  mixed ...$args
): void {}

function call_with_optional_and_reify(): void {
  with_optional_and_reify<int>();
}

class C {
  use T;

  public function call_trait(int $a, int $b, int $c): void {
    $this->foo($a, $b, $c);
    self::static_foo($a, $b, $c);
  }

  public function call_trait_with_optional_and_reify(
    int $a,
    int $b,
    int $c,
  ): void {
    $this->foo2<int>($a, $b, $c);
    self::static_foo2<int>($a, $b, $c);
  }
}

trait T {
  public function foo(int $x, int ...$rest): void {}
  public static function static_foo(int $x, int ...$rest): void {}
  public function foo2<reify T>(int $x = 0, int ...$rest): void {}
  public static function static_foo2<reify T>(int $x, int ...$rest): void {}
}

function arity3(int $arg1, int $arg2, int $arg3): void {}

function call_not_really_variadic_with_splat(): void {
  $args = tuple(1, 2, 3);
  arity3(...$args);
}
