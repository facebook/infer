// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace UninitMethod;

interface MyInterface {
  public static function foo(): string;
}

trait TMyAbstractClass {
  public static function goo(): string {
    return "goo";
  }
}

abstract class MyAbstractClass {
  use TMyAbstractClass;

  public abstract static function foo(): string;
}

class MyClass implements MyInterface {
  public static function foo(): string {
    return "foo";
  }
}

final class MyFinalClass extends MyClass {
  public function closure_in_vector_ok(): void {
    foreach (vec[self::foo<>] as $f) {
      $_ = $f();
    }
  }
}

final class MyFinalUnknownClass extends UnknownClass {
}

function class_static_method_ok(): string {
  $c = MyClass::class;
  return $c::foo();
}

function final_class_static_method_ok(): string {
  $c = MyFinalClass::class;
  return $c::foo();
}

function final_unknown_class_static_method_ok(): string {
  $c = MyFinalUnknownClass::class;
  return $c::foo();
}

function interface_static_method_bad(): string {
  // Uncaught exception 'TypehintViolationException'
  $c = MyInterface::class;
  return $c::foo();
}

function abstract_class_static_method_bad(): string {
  // Fatal error: Cannot call abstract method
  $c = MyAbstractClass::class;
  return $c::foo();
}

function call_foo(classname<MyInterface> $c): string {
  return $c::foo();
}

function interface_call_foo_bad_FN(): string {
  return call_foo(MyInterface::class);
}

function fianl_class_call_foo_ok(): string {
  return call_foo(MyFinalClass::class);
}

function trait_method_ok(): string {
  return MyAbstractClass::goo();
}
