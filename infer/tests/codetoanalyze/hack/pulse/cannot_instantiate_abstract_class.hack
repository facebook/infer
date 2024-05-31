// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace CannotInstantiateAbstractClassTests;

class Main {

  public static function makeGeneric<T>(classname<T> $cls): void {
    new $cls();
  }

  public static function passthrough<T>(classname<T> $cls): classname<T> {
    return $cls;
  }

  public static function getConcreteClass(): classname<ConcreteClass1> {
    return ConcreteClass1::class;
  }

  public static function getAbstractClass(): classname<AbstractClass1> {
    return AbstractClass1::class;
  }

  public static function makeGenericWithIndirection<T>(
    classname<T> $cls,
  ): void {
    Main::makeGeneric($cls);
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

abstract class AbstractClass3 extends AbstractClass2 {}

class Tests {

  public function initAbstractClassViaGenericFunBad(): void {
    Main::makeGeneric(AbstractClass1::class);
  }

  public function initConcreteClassViaGenericFunOk(): void {
    Main::makeGeneric(ConcreteClass1::class);
  }

  public function initAbstractClassViaStaticBad(): void {
    AbstractClass2::makeStatic();
  }

  public function initChildAbstractClassViaStaticBad(): void {
    AbstractClass3::makeStatic();
  }

  public function initConcreteClassViaStaticOk(): void {
    ConcreteClass2::makeStatic();
  }

  public function initAbstractClassViaGenericFunWithPassthroughBad(): void {
    Main::makeGeneric(Main::passthrough(AbstractClass1::class));
  }

  public function initConcreteClassViaGenericFunWithPassthroughOk(): void {
    Main::makeGeneric(Main::passthrough(ConcreteClass1::class));
  }

  public function initAbstractClassViaGenericFunWithExternalArgBad(): void {
    Main::makeGeneric(Main::getAbstractClass());
  }

  public function initConcreteClassViaGenericFunWithExternalArgOk(): void {
    Main::makeGeneric(Main::getConcreteClass());
  }

  public function initAbstractClassViaGenericWithIndirectionBad(): void {
    Main::makeGenericWithIndirection(AbstractClass1::class);
  }

  public function initConcreteClassViaGenericWithIndirectionOk(): void {
    Main::makeGenericWithIndirection(ConcreteClass1::class);
  }

  public function initAbstractClassWithSameUnrelatedSpecializationBad<T>(
    classname<T> $cls,
  ): void {
    Main::makeGeneric(AbstractClass1::class);
  }

  public function initAbstractClassWithSameUnrelatedSpecializationOk<T>(
  ): void {
    $this->initAbstractClassWithSameUnrelatedSpecializationBad(
      AbstractClass1::class,
    );
  }

  public function initAbstractClassInTheSameFunctionBad(): AbstractClass2 {
    $class = AbstractClass2::class;
    $object = new $class();

    return $object;
  }
}
