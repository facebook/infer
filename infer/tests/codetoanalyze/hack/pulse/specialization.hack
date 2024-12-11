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

class Box1<T> {
  public function __construct(public T $f1) {}
}

class Box2<T> {
  public function __construct(public T $f2) {}
}

class Box3<T> {
  public function __construct(public T $f3) {}
}

class Main {

  // will need dynamic-type specialization
  public static function call_foo(int $i, I $o): int {
    return $o->foo($i);
  }

  // will need alias specialization
  public static function conditional_sink(
    int $i,
    Box1<int> $i1,
    Box1<int> $i2,
  ): void {
    $i1->f1 = 0;
    $i2->f1 = 1;
    if ($i1->f1 == 1) {
      \Level1\taintSink($i);
    }
  }

  // will need alias specialization on heap paths
  public static function conditional_with_paths_sink(
    int $i,
    Box1<Box2<int>> $i1,
    Box1<Box2<int>> $i2,
  ): void {
    $i1->f1->f2 = 0;
    $i2->f1->f2 = 1;
    if ($i1->f1->f2 == 1) {
      \Level1\taintSink($i);
    }
  }

  // will need alias specialization on heap paths
  public static function conditional_with_paths3_sink(
    int $i,
    Box1<Box2<Box3<int>>> $i1,
    Box1<Box2<Box3<int>>> $i2,
  ): void {
    $i1->f1->f2->f3 = 0;
    $i2->f1->f2->f3 = 1;
    if ($i1->f1->f2->f3 == 1) {
      \Level1\taintSink($i);
    }
  }

  // will need alias AND dynamic-type specialization at the same time
  public static function conditional_call_foo(
    int $i,
    I $o,
    Box1<int> $i1,
    Box1<int> $i2,
  ): int {
    $i1->f1 = 0;
    $i2->f1 = 1;
    if ($i1->f1 == 1) {
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
    $i1 = new Box1<int>(10);
    $i2 = new Box1<int>(100);
    self::conditional_sink($tainted, $i1, $i2);
  }

  public static function with_alias_bad(int $i): void {
    $tainted = \Level1\taintSource();
    $i1 = new Box1<int>(10);
    self::conditional_sink($tainted, $i1, $i1);
  }

  public static function without_heap_alias_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $i1 = new Box1<Box2<int>>(new Box2<int>(10));
    $i2 = new Box1<Box2<int>>(new Box2<int>(100));
    self::conditional_with_paths_sink($tainted, $i1, $i2);
  }

  public static function with_heap_alias1_bad(int $i): void {
    $tainted = \Level1\taintSource();
    $i = new Box1<Box2<int>>(new Box2<int>(10));
    self::conditional_with_paths_sink($tainted, $i, $i);
  }

  public static function with_heap_alias2_bad(int $i): void {
    $tainted = \Level1\taintSource();
    $i = new Box2<int>(10);
    $i1 = new Box1<Box2<int>>($i);
    $i2 = new Box1<Box2<int>>($i);
    self::conditional_with_paths_sink($tainted, $i1, $i2);
  }

  public static function with_heap_alias3_bad(int $i): void {
    $tainted = \Level1\taintSource();
    $i = new Box3<int>(10);
    $j1 = new Box2<Box3<int>>($i);
    $j2 = new Box2<Box3<int>>($i);
    $i1 = new Box1<Box2<Box3<int>>>($j1);
    $i2 = new Box1<Box2<Box3<int>>>($j2);
    self::conditional_with_paths3_sink($tainted, $i1, $i2);
  }

  public static function without_alias_call_A_foo_with_new_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new A();
    $i1 = new Box1<int>(10);
    $i2 = new Box1<int>(100);
    \Level1\taintSink(self::conditional_call_foo($tainted, $o, $i1, $i2));
  }

  public static function without_alias_call_B_foo_with_new_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new B();
    $i1 = new Box1<int>(10);
    $i2 = new Box1<int>(100);
    \Level1\taintSink(self::conditional_call_foo($tainted, $o, $i1, $i2));
  }

  public static function with_alias_call_A_foo_with_new_bad(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new A();
    $i1 = new Box1<int>(10);
    \Level1\taintSink(self::conditional_call_foo($tainted, $o, $i1, $i1));
  }

  public static function with_alias_call_B_foo_with_new_ok(int $i): void {
    $tainted = \Level1\taintSource();
    $o = new B();
    $i1 = new Box1<int>(10);
    \Level1\taintSink(self::conditional_call_foo($tainted, $o, $i1, $i1));
  }

  public static function test_alias_in_closure_specialization(
    (function(Box1<int>): void) $f,
    Box1<int> $counter,
  ): void {
    $f($counter);
  }

  public function test_alias_in_closure_specialization_bad(): void {
    $tainted = \Level1\taintSource();
    $counter = new Box1<int>(0);
    $incr_deref = (Box1<int> $counter2) ==> {
      $counter->f1 = $counter->f1 + 1;
      $counter2->f1 = $counter2->f1 + 1;
    };
    self::test_alias_in_closure_specialization($incr_deref, $counter);
    if ($counter->f1 == 2) {
      \Level1\taintSink($tainted);
    }
  }

  public function test_alias_in_closure_specialization_good(): void {
    $tainted = \Level1\taintSource();
    $counter = new Box1<int>(0);
    $incr_deref = (Box1<int> $counter2) ==> {
      $counter->f1 = $counter->f1 + 1;
      $counter2->f1 = $counter2->f1 + 1;
    };
    self::test_alias_in_closure_specialization($incr_deref, $counter);
    if ($counter->f1 != 2) {
      \Level1\taintSink($tainted);
    }
  }
}
