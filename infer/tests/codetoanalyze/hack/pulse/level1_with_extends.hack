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
  public function fromABad(): void {
    $tainted = (new A())->sourceIfA();
    \Level1\taintSink($tainted);
  }

  public function fromBBad(): void {
    $tainted = (new B())->sourceIfA();
    \Level1\taintSink($tainted);
  }

  public function fromCGood(): void {
    $tainted = (new C())->sourceIfA();
    \Level1\taintSink($tainted);
  }

  public function fromAGood(): void {
    $tainted = (new A())->sourceIfC();
    \Level1\taintSink($tainted);
  }

  public function fromBGood(): void {
    $tainted = (new B())->sourceIfC();
    \Level1\taintSink($tainted);
  }

  public function fromCBad(): void {
    $tainted = (new C())->sourceIfC();
    \Level1\taintSink($tainted);
  }

}

class NeedSpecialization {
  public static function getSourceIfA(A $a): int {
    return $a->sourceIfA();
  }

  public static function getSourceIfC(A $a): int {
    return $a->sourceIfC();
  }

  public static function fromABad(): void {
    \Level1\taintSink(self::getSourceIfA(new A()));
  }

  public static function fromBBad(): void {
    \Level1\taintSink(self::getSourceIfA(new B()));
  }

  public static function fromCGood(): void {
    \Level1\taintSink(self::getSourceIfA(new C()));
  }

  public static function fromAGood(): void {
    \Level1\taintSink(self::getSourceIfC(new A()));
  }

  public static function fromBGood(): void {
    \Level1\taintSink(self::getSourceIfC(new B()));
  }

  public static function fromCBad(): void {
    \Level1\taintSink(self::getSourceIfC(new C()));
  }

}
