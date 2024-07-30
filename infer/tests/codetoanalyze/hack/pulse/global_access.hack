// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace GlobalAccess;

final class A {
  public function __construct(public int $GlobalVARIABLES) {}

  public function get(): int {
    $_ = new Unknown();
    return $this->GlobalVARIABLES;
  }

  public function double_get(): int {
    $_ = $this->get();
    return $this->get();
  }
}

class EventHandler {}
class Parent2 extends EventHandler {}
class Parent1 extends Parent2 {
  public function dangerous(): int {
    return 42;
  }

  public function suspicious(): int {
    return 42;
  }

  public function very_suspicious(): int {
    return 42;
  }

  public function extremely_suspicious(): int {
    return 42;
  }

  public function not_suspicious(): int {
    return 42;
  }

  public function fine(): int {
    return 42;
  }
}

class ExtendsVeryUnsafe extends VeryUnsafe {}

class C {
  public function skip_me(A $a): int {
    return $a->get();
  }
}

class B extends C {
  public function skip_me_too(A $a): int {
    return $a->get();
  }

  public function dont_skip_me(A $a): int {
    return $a->get();
  }
}

final class GlobalAccess extends Parent1 {

  public function basic_is_entry_bad(A $a): int {
    return $a->GlobalVARIABLES;
  }

  public function indirect_is_entry_bad(A $a): int {
    return $a->get();
  }

  public function indirect_other_is_entry_bad(A $a): int {
    return $a->get();
  }

  public function indirect_skip_me_is_entry_ok(A $a, B $b): int {
    return $b->skip_me($a);
  }

  public function indirect_skip_me_too_is_entry_ok(A $a, B $b): int {
    return $b->skip_me($a);
  }

  public function indirect_dont_skip_me_is_entry_bad(A $a, B $b): int {
    return $b->dont_skip_me($a);
  }

  public function indirect_is_entry_two_signals_bad(A $a1, A $a2): int {
    $_ = $a1->get();
    return $a2->get();
  }

  public function FN_indirect_is_entry_calls_double_get_two_signals_bad(
    A $a,
  ): int {
    return $a->double_get();
  }

  public function call1_is_entry_bad(Unsafe $a): int {
    return $a->dangerous();
  }

  public function call2_is_entry_bad(Unsafe $a): int {
    return $a->suspicious();
  }

  public function call3_is_entry_bad(ExtendsVeryUnsafe $a): int {
    return $a->suspicious();
  }

  public function call4_is_entry_ok(Fine $a): int {
    return $a->suspicious();
  }

  public function call5_is_entry_ok(Unsafe $a): int {
    return $a->fine();
  }

  public function call6_is_entry_bad(ContainsABadPatternInside $a): int {
    return $a->foo();
  }

  public function call7_is_entry_bad(ExtendsUnsafeInterface $a): int {
    return $a->very_suspicious();
  }

  public function call8_is_entry_bad(ExtendsUnsafeInterface $a): int {
    return $a->extremely_suspicious();
  }

  public function call9_is_entry_ok(ExtendsUnsafeInterface $a): int {
    return $a->not_suspicious();
  }

  public static function gen_closure1(A $a): (function(): int) {
    return () ==> $a->get();
  }

  public static function gen_closure2(A $a): (function(): int) {
    return () ==> $a->get();
  }

  public static function indirect_gen_closure2(A $a): (function(): int) {
    return self::gen_closure2($a);
  }

  public function call7_is_entry_with_closures_bad(
    A $a,
    (function(): int) $f,
  ): int {
    $f1 = self::gen_closure1($a);
    $f2 = self::indirect_gen_closure2($a);
    $res1 = $f1();
    $res2 = $f2();
    $res3 = $f();
    return $res1 + $res2;
  }
}

class EventNotHandler {}
final class DoesNotInheritEvenHandler extends EventNotHandler {
  public function indirect_other_is_not_entry_ok(A $a): int {
    return $a->get();
  }
}

interface Unsafe {
  public function dangerous(): int;
  public function suspicious(): int;
  public function extremely_suspicious(): int;
  public function very_suspicious(): int;
  public function not_suspicious(): int;
  public function fine(): int;
}
interface ExtendsUnsafeInterface extends Unsafe {}

final class ImplementUnsafe extends Parent1 implements Unsafe {
  public function indirect_other_is_not_entry_ok(A $a): int {
    return $a->get();
  }

}

class ImplementUnsafeByTransitivity
  extends Parent1
  implements ExtendsUnsafeInterface {
  public function indirect_other_is_not_entry_ok(A $a): int {
    return $a->get();
  }

}

class EventHandler2 {
  public function indirect_empty_does_not_extend_bad(A $a): int {
    return $a->get();
  }
}

class Helper {
  public static function gen_closure1(): (function(): int) {
    return () ==> 0;
  }

  public static function gen_closure2(): (function(): int) {
    return () ==> 1;
  }

  public static function indirect_gen_closure2(): (function(): int) {
    return self::gen_closure2();
  }

  public function bomb1(
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $activate,
    A $a,
  ): void {
    if ($choice0) {
    }
    if ($choice1) {
    }
    if ($choice2) {
      if ($activate && $choice0 && $choice1 && $choice2) {
        $_ = $a->get();
        $f1 = self::gen_closure1();
        $f2 = self::indirect_gen_closure2();
        $_ = $f1();
        $_ = $f2();
      }
      ;
      $_ =
        0; // trying to influence the CFG shape, we want a junction point with only 2 predecessors
    }
  }

  public function bomb2(
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $activate,
    A $a1,
    A $a2,
  ): void {
    if ($choice0) {
    }
    if ($choice1) {
    }
    if ($choice2) {
      if ($activate && $choice0 && $choice1 && $choice2) {
        $_ = $a1->get();
        $_ = $a2->get();
        $f1 = self::gen_closure1();
        $f2 = self::indirect_gen_closure2();
        $_ = $f1();
        $_ = $f2();
      }
      ;
      $_ =
        0; // trying to influence the CFG shape, we want a junction point with only 2 predecessors
    }
  }
}

final class TooMuchDisjuncts extends EventHandler {

  public function one_signal_bad(
    Helper $o,
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $choice3,
    A $a,
  ): void {
    $activate = $choice3 ? true : false;
    $o->bomb1($choice0, $choice1, $choice2, $activate, $a);
  }

  public function call_one_signal_bad(
    Helper $o,
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $choice3,
    A $a,
  ): void {
    $this->one_signal_bad($o, $choice0, $choice1, $choice2, $choice3, $a);
  }

  public function one_signal_bad_flipped(
    Helper $o,
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $choice3,
    A $a,
  ): void {
    $activate = $choice3 ? true : false;
    $o->bomb1($choice0, $choice1, $choice2, !$activate, $a);
  }

  public function FN_two_signals_bad(
    Helper $o,
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $choice3,
    A $a1,
    A $a2,
  ): void {
    $activate = $choice3 ? true : false;
    $o->bomb2($choice0, $choice1, $choice2, $activate, $a1, $a2);
  }

  public function FN_two_signals_bad_flipped(
    Helper $o,
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $choice3,
    A $a1,
    A $a2,
  ): void {
    $activate = $choice3 ? true : false;
    $o->bomb2($choice0, $choice1, $choice2, !$activate, $a1, $a2);
  }
}
