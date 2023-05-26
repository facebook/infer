// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Equality;

class A {

  public function eqNullSimpleBad() {
    $a = null;
    $taint = \Level1\taintSource();
    if ($a === null) {
      \Level1\taintSink($taint);
    }
  }

  public function eqNullSimpleGood() {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a === null) {
      \Level1\taintSink($taint);
    }
  }

  public function neNullSimpleBad() {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a !== null) {
      \Level1\taintSink($taint);
    }
  }

  public function doubleEqNullBad(A $a) {
    $taint = \Level1\taintSource();
    if ($a === null) {
      if (null === $a) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function doubleEqNullGood(A $a1) {
    $taint = \Level1\taintSource();
    $a = new A();
    if ($a1 === null) {
      if ($a1 === $a) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function doubleEqNeNullGood(A $a) {
    $taint = \Level1\taintSource();
    if ($a === null) {
      if ($a !== null) {
        \Level1\taintSink($taint);
      }
    }
  }

}
