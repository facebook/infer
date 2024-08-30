// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace StaticField;

// A and B show the difference in compilation of static field/constant access depending on
// whether we're in the same class or not.
// function f() in the two classes use identical hhvm bytecodes to access A::MYCONST
// but the textual is different. That in class A makes use of $this being the class object
// but is assuming that this will always have already been initialized. Whilst that would be
// true for "real" execution, it's not true when analyzing A::f in isolation. The textual for B::f
// calls lazy_initialize explicitly
final class A {
  const int MYCONST = 42;

  public static function f(): int {
    return A::MYCONST;
  }
}

final class B {
  const int BCONST = A::MYCONST;
  public static function f(): int {
    return A::MYCONST;
  }
}

// Here's are real examples where it goes wrong
final class HasStaticField {
  private static int $field = 42;
  const int MYCONST = 42;

  public static async function fail(): Awaitable<int> {
    return 3;
  }

  // this is technically an FP but we can live with it because it depends
  // on the subtle-ish fact that no methods mutate the private field so it's really constant
  public async function checkStaticFieldFP(): Awaitable<void> {
    $v = HasStaticField::$field;
    if ($v == 42) {
      return;
    }
    $_ = HasStaticField::fail();
  }

  // this is a genuine FP because it's declared constant and we still don't know its value
  public async function checkConstantOK(): Awaitable<void> {
    $v = HasStaticField::MYCONST;
    if ($v == 42) {
      return;
    }
    $_ = HasStaticField::fail();
  }

  // still questionable FP, though we get the class object in
  // a different way because we're in a static method
  public static async function checkStaticFieldStaticFP(): Awaitable<void> {
    $v = HasStaticField::$field;
    if ($v == 42) {
      return;
    }
    $_ = HasStaticField::fail();
  }

  public static async function checkConstantStaticOK(): Awaitable<void> {
    $v = HasStaticField::MYCONST;
    if ($v == 42) {
      return;
    }
    $_ = HasStaticField::fail();
  }
}

// Now run the same constant tests, but from a different class
final class TestHasStaticFieldFromOutside {

  public async function checkConstantOK(): Awaitable<void> {
    $v = HasStaticField::MYCONST;
    if ($v == 42) {
      return;
    }
    $_ = HasStaticField::fail();
  }

  // still FP
  public static async function checkConstantStaticOK(): Awaitable<void> {
    $v = HasStaticField::MYCONST;
    if ($v == 42) {
      return;
    }
    $_ = HasStaticField::fail();
  }

}

// Now see what happens when the constant is defined in a trait
// this fails if we don't inject trait constant initialization
trait HasConstant {
  const int FortyTwo = 42;
}

class UseTrait {
  use HasConstant;

  public static async function checkConstantOK(): Awaitable<void> {
    if (self::FortyTwo == 42) {
      return;
    }
    $_ = HasStaticField::fail();
  }
}
