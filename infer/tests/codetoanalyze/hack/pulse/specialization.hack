// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SpecializationTests;

interface I {

  public function foo(int $i): int;

}

class A implements I {
  public function foo(int $i): int {
    return $i;
  }
}

class B implements I {
  public function foo(int $i): int {
    return 0;
  }
}

final class Bfinal implements I {
  public function foo(int $i): int {
    return 0;
  }
}

class BoxedInt {
  public function __construct(public int $f) {}
}

class Main {

  // will need dynamic-type specialization
  public static function call_foo(int $i, I $o): int {
    return $o->foo($i);
  }

  // will need alias specialization
  public static function conditional_sink(
    int $i,
    BoxedInt $i1,
    BoxedInt $i2,
  ): void {
    $i1->f = 0;
    $i2->f = 1;
    if ($i1->f == 1) {
      \Level1\taintSink($i);
    }
  }

  // will need alias AND dynamic-type specialization at the same time
  public static function conditional_call_foo(
    int $i,
    I $o,
    BoxedInt $i1,
    BoxedInt $i2,
  ): int {
    $i1->f = 0;
    $i2->f = 1;
    if ($i1->f == 1) {
      return $o->foo($i);
    } else return 0;
  }

  public static function call_A_foo_with_new_bad(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new A();
    \Level1\taintSink(self::call_foo($tainted, $o));
  }

  public static function call_B_foo_with_new_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new B();
    \Level1\taintSink(self::call_foo($tainted, $o));
  }

  public static function call_A_foo_with_param_bad(int $i, A $o): void {
    $tainted = \Level1\taintSource();
    \Level1\taintSink(self::call_foo($tainted, $o));
  }

  // not a clear FP since it could exist a subclass of B with an ovveriding of foo
  public static function FP_call_B_foo_with_param_ok(int $i, B $o): void {
    $tainted = \Level1\taintSource();
    \Level1\taintSink(self::call_foo($tainted, $o));
  }

  public static function call_Bfinal_foo_with_param_ok(
    int $i,
    Bfinal $o,
  ): void {
    $tainted = \Level1\taintSource();
    \Level1\taintSink(self::call_foo($tainted, $o));
  }

  public static function without_alias_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $i1 = new BoxedInt(10);
    $i2 = new BoxedInt(100);
    self::conditional_sink($tainted, $i1, $i2);
  }

  public static function with_alias_bad(int $i): void {
    $tainted = \Level1\taintSource();
    $i1 = new BoxedInt(10);
    self::conditional_sink($tainted, $i1, $i1);
  }

  public static function without_alias_call_A_foo_with_new_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new A();
    $i1 = new BoxedInt(10);
    $i2 = new BoxedInt(100);
    \Level1\taintSink(self::conditional_call_foo($tainted, $o, $i1, $i2));
  }

  public static function without_alias_call_B_foo_with_new_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new B();
    $i1 = new BoxedInt(10);
    $i2 = new BoxedInt(100);
    \Level1\taintSink(self::conditional_call_foo($tainted, $o, $i1, $i2));
  }

  public static function with_alias_call_A_foo_with_new_bad(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new A();
    $i1 = new BoxedInt(10);
    \Level1\taintSink(self::conditional_call_foo($tainted, $o, $i1, $i1));
  }

  public static function with_alias_call_B_foo_with_new_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new B();
    $i1 = new BoxedInt(10);
    \Level1\taintSink(self::conditional_call_foo($tainted, $o, $i1, $i1));
  }

}
