// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace TypePropagation;

class A {

  public function getTainted() : int {
    return \Level1\taintSource();
  }

  public function getUntainted() {
    return 1;
  }

}

abstract class C {

  public A $a;

  public static A $globalvar;

  abstract public function get() : A;

}


class Main {

  public static function fromParamsBad(A $a) {
    \Level1\taintSink($a->getTainted());
  }

  public static function fromParamsGood(A $a) {
    \Level1\taintSink($a->getUntainted());
  }

  public static function fromPropertyThroughParamBad(C $c) {
    \Level1\taintSink($c->a->getTainted());
  }

  public static function fromPropertyThroughParamGood(C $c) {
    \Level1\taintSink($c->a->getUntainted());
  }

  public static function fromPropertyThroughNewBad() {
    $c = new C();
    \Level1\taintSink($c->a->getTainted());
  }

  public static function fromPropertyThroughNewGood() {
    $c = new C();
    \Level1\taintSink($c->a->getUntainted());
  }

  public static function fromGlobalBad(C $c) {
    \Level1\taintSink(C::$globalvar->getTainted());
  }

  public static function fromGlobalGood(C $c) {
    \Level1\taintSink(C::$globalvar->getUntainted());
  }

  public static function FN_fromCallResultBad(C $c) {
    \Level1\taintSink($c->get()->getTainted());
  }

  public static function fromCallResultGood(C $c) {
    \Level1\taintSink($c->get()->getUntainted());
  }
}
