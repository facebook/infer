// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace GlobalAccess;

class A {
  public function __construct(public int $GlobalVARIABLES) {}

  public function get(): int {
    return $this->GlobalVARIABLES;
  }
}

class EventHandler {}
class Parent2 extends EventHandler {}
class Parent1 extends Parent2 {}

class ExtendsVeryUnsafe extends VeryUnsafe {}

class GlobalAccess extends Parent1 {

  public function basic_is_entry_bad(A $a): int {
    return $a->GlobalVARIABLES;
  }

  public function indirect_is_entry_bad(A $a): int {
    return $a->get();
  }

  public function indirect_other_is_entry_bad(A $a): int {
    return $a->get();
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
}

class EventNotHandler {}
class DoesNotInheritEvenHandler extends EventNotHandler {
  public function indirect_other_is_not_entry_ok(A $a): int {
    return $a->get();
  }
}

interface Unsafe {}
interface I extends Unsafe {}

class ImplementUnsafe extends Parent1 implements Unsafe {
  public function indirect_other_is_not_entry_ok(A $a): int {
    return $a->get();
  }

}

class ImplementUnsafeByTransitivity extends Parent1 implements I {
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
  public function bomb(
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
      }
      ;
    }
  }
}

class TooMuchDisjuncts extends EventHandler {

  public function bad(
    Helper $o,
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $choice3,
    A $a,
  ): void {
    $activate = $choice3 ? true : false;
    $o->bomb($choice0, $choice1, $choice2, $activate, $a);
  }

  public function bad_flipped(
    Helper $o,
    bool $choice0,
    bool $choice1,
    bool $choice2,
    bool $choice3,
    A $a,
  ): void {
    $activate = $choice3 ? true : false;
    $o->bomb($choice0, $choice1, $choice2, !$activate, $a);
  }
}
