// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace TypePropagation;

class A {

  public function getTainted(): int {
    return \Level1\taintSource();
  }

  public function getUntainted(): int {
    return 1;
  }

}

abstract class C {

  public A $a;

  public static ?A $globalvar;

  abstract public function get(): A;
}

class D extends C {
  public function __construct() {
    $this->a = new A();
  }

  public function get(): A {
    return $this->a;
  }
}

class Main {

  public static function fromParamsBad(A $a): void {
    \Level1\taintSink($a->getTainted());
  }

  public static function fromParamsGood(A $a): void {
    \Level1\taintSink($a->getUntainted());
  }

  public static function fromPropertyThroughParamBad(C $c): void {
    \Level1\taintSink($c->a->getTainted());
  }

  public static function fromPropertyThroughParamGood(C $c): void {
    \Level1\taintSink($c->a->getUntainted());
  }

  public static function fromPropertyThroughNewBad(): void {
    $c = new D();
    \Level1\taintSink($c->a->getTainted());
  }

  public static function fromPropertyThroughNewGood(): void {
    $c = new D();
    \Level1\taintSink($c->a->getUntainted());
  }

  public static function fromGlobalBad(C $c): void {
    \Level1\taintSink(C::$globalvar?->getTainted());
  }

  public static function fromGlobalGood(C $c): void {
    \Level1\taintSink(C::$globalvar?->getUntainted());
  }

  public static function FN_fromCallResultBad(C $c): void {
    \Level1\taintSink($c->get()->getTainted());
  }

  public static function fromCallResultGood(C $c): void {
    \Level1\taintSink($c->get()->getUntainted());
  }
}
