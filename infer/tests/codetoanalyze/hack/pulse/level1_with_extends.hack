// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace ExtendsTests;

class A {
  public function sourceIfA(): int {
    return \Level1\taintSource();
  }

  public function sourceIfC(): int {
    return 0;
  }
}

class B extends A {}

class C extends B {
  <<__Override>>
  public function sourceIfA(): int {
    return 0;
  }

  <<__Override>>
  public function sourceIfC(): int {
    return \Level1\taintSource();
  }
}

class Main {
  function fromABad(): void {
    $tainted = (new A())->sourceIfA();
    \Level1\taintSink($tainted);
  }

  function fromBBad(): void {
    $tainted = (new B())->sourceIfA();
    \Level1\taintSink($tainted);
  }

  function fromCGood(): void {
    $tainted = (new C())->sourceIfA();
    \Level1\taintSink($tainted);
  }

  function fromAGood(): void {
    $tainted = (new A())->sourceIfC();
    \Level1\taintSink($tainted);
  }

  function fromBGood(): void {
    $tainted = (new B())->sourceIfC();
    \Level1\taintSink($tainted);
  }

  function fromCBad(): void {
    $tainted = (new C())->sourceIfC();
    \Level1\taintSink($tainted);
  }

}

class NeedSpecialization {
  static function getSourceIfA(A $a): int {
    return $a->sourceIfA();
  }

  static function getSourceIfC(A $a): int {
    return $a->sourceIfC();
  }

  static function fromABad(): void {
    \Level1\taintSink(self::getSourceIfA(new A()));
  }

  static function fromBBad(): void {
    \Level1\taintSink(self::getSourceIfA(new B()));
  }

  static function fromCGood(): void {
    \Level1\taintSink(self::getSourceIfA(new C()));
  }

  static function fromAGood(): void {
    \Level1\taintSink(self::getSourceIfC(new A()));
  }

  static function fromBGood(): void {
    \Level1\taintSink(self::getSourceIfC(new B()));
  }

  static function fromCBad(): void {
    \Level1\taintSink(self::getSourceIfC(new C()));
  }

}
