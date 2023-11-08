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
