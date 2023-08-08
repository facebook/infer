// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Equality;

class A {

  public function eqNullSimpleBad(): void {
    $a = null;
    $taint = \Level1\taintSource();
    if ($a === null) {
      \Level1\taintSink($taint);
    }
  }

  public function eqNullSimpleGood(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a === null) {
      \Level1\taintSink($taint);
    }
  }

  public function neNullSimpleBad(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a !== null) {
      \Level1\taintSink($taint);
    }
  }

  public function doubleEqNullBad(A $a): void {
    $taint = \Level1\taintSource();
    if ($a === null) {
      if (null === $a) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function doubleEqNullGood(A $a1): void {
    $taint = \Level1\taintSource();
    $a = new A();
    if ($a1 === null) {
      if ($a1 === $a) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function doubleEqNeNullGood(A $a): void {
    $taint = \Level1\taintSource();
    if ($a === null) {
      if ($a !== null) {
        \Level1\taintSink($taint);
      }
    }
  }


  public function FN_eqIntSimpleBad(): void {
    $a = 0;
    $b = 0;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

 public function eqIntSimpleGood(): void {
    $a = 0;
    $b = 1;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FN_neIntSimpleBad(): void {
    $a = 0;
    $b = 1;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

 public function neIntSimpleGood(): void {
    $a = 0;
    $b = 0;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FN_eqBoolSimpleBad(): void {
    $a = true;
    $b = true;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

 public function eqBoolSimpleGood(): void {
    $a = true;
    $b = false;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FN_neBoolSimpleBad(): void {
    $a = true;
    $b = false;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

 public function neBoolSimpleGood(): void {
    $a = true;
    $b = true;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

}
