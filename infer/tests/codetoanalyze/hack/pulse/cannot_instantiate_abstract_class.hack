// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace CannotInstantiateAbstractClassTests;

class Main {

  public static function makeGeneric<T>(classname<T> $cls): void {
    new $cls();
  }
}

abstract class AbstractClass1 {}

class ConcreteClass1 extends AbstractClass1 {}

<<__ConsistentConstruct>>
abstract class AbstractClass2 {

  public static function makeStatic(): void {
    new static();
  }
}

class ConcreteClass2 extends AbstractClass2 {}

class Tests {

  public function FN_initAbstractClassViaGenericFunBad(): void {
    Main::makeGeneric(AbstractClass1::class);
  }

  public function initConcreteClassViaGenericFunOk(): void {
    Main::makeGeneric(ConcreteClass1::class);
  }

  public function FN_initAbstractClassViaStaticBad(): void {
    AbstractClass2::makeStatic();
  }

  public function initConcreteClassViaStaticOk(): void {
    ConcreteClass2::makeStatic();
  }
}
