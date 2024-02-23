// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Equality;

class A {}

class CmpSame {

  public function cmpSameNullSimpleBad(): void {
    $a = null;
    $taint = \Level1\taintSource();
    if ($a === null) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameNullSimpleGood(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a === null) {
      \Level1\taintSink($taint);
    }
  }

  public function doubleCmpSameNullBad(A $a): void {
    $taint = \Level1\taintSource();
    if ($a === null) {
      if (null === $a) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function doubleCmpSameNullGood(A $a1): void {
    $taint = \Level1\taintSource();
    $a = new A();
    if ($a1 === null) {
      if ($a1 === $a) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function cmpSameIntSimpleBad(): void {
    $a = 0;
    $b = 0;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameIntSimpleGood(): void {
    $a = 0;
    $b = 1;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameFloatSimpleBad(): void {
    $a = 3.14;
    $b = 3.14;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameFloatSimpleGood(): void {
    $a = 3.14;
    $b = 42.0;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameBoolSimpleBad(): void {
    $a = true;
    $b = true;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameBoolSimpleGood(): void {
    $a = true;
    $b = false;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameObjectSimpleBad(): void {
    $a = new A();
    $b = $a;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FP_cmpSameObjectSimpleGood(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a === new A()) {
      \Level1\taintSink($taint);
    }
  }

  public function neObjectSimpleBad(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a !== new A()) {
      \Level1\taintSink($taint);
    }
  }

  public function neObjectSimpleGood(): void {
    $a = new A();
    $b = $a;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameDictSimpleBad(): void {
    $a = dict['a' => 1, 'b' => 3];
    // just writing $b = dict['a' => 1, 'b' => 3] because the compiler
    // reuses the same pointer.
    $b = $a;
    $b['a'] = 1;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FP_cmpSameDictSimpleGood(): void {
    $a = dict['a' => 1, 'b' => 3];
    $b = dict['a' => 1, 'b' => 4];
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpSameDictNestedBad(): void {
    $a = dict['a' => 1, 'b' => 2];
    $b = $a;
    $a['a'] = $a;
    $b['a'] = $b;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FP_cmpSameDictNestedGood(): void {
    $a = dict['a' => 1, 'b' => 2];
    $a['a'] = $a;
    $b = $a;
    $b['b'] = 3;
    $taint = \Level1\taintSource();
    if ($a === $b) {
      \Level1\taintSink($taint);
    }
  }

}

class CmpNsame {

  public function nsameNullSimpleBad(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a !== null) {
      \Level1\taintSink($taint);
    }
  }

  public function nsameDoubleNullGood(A $a): void {
    $taint = \Level1\taintSource();
    if ($a === null) {
      if ($a !== null) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function nsameIntSimpleBad(): void {
    $a = 0;
    $b = 1;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

  public function nsameIntSimpleGood(): void {
    $a = 0;
    $b = 0;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

  public function nsameFloatSimpleBad(): void {
    $a = 3.14;
    $b = 42.0;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

  public function nsameFloatSimpleGood(): void {
    $a = 3.14;
    $b = 3.14;
    $taint = \Level1\taintSource();
    if ($a !== $b) {
      \Level1\taintSink($taint);
    }
  }

  public function neBoolSimpleBad(): void {
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

class CmpEq {

  public function cmpEqNullSimpleBad(): void {
    $a = null;
    $taint = \Level1\taintSource();
    if ($a == null) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqNullSimpleGood(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a == null) {
      \Level1\taintSink($taint);
    }
  }

  public function neNullSimpleBad(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a != null) {
      \Level1\taintSink($taint);
    }
  }

  public function doubleCmpSameNullBad(A $a): void {
    $taint = \Level1\taintSource();
    if ($a == null) {
      if (null == $a) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function doubleCmpSameNullGood(A $a1): void {
    $taint = \Level1\taintSource();
    $a = new A();
    if ($a1 == null) {
      if ($a1 == $a) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function doubleCmpSameNeNullGood(A $a): void {
    $taint = \Level1\taintSource();
    if ($a == null) {
      if ($a != null) {
        \Level1\taintSink($taint);
      }
    }
  }

  public function cmpEqIntSimpleBad(): void {
    $a = 0;
    $b = 0;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqIntSimpleGood(): void {
    $a = 0;
    $b = 1;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function neIntSimpleBad(): void {
    $a = 0;
    $b = 1;
    $taint = \Level1\taintSource();
    if ($a != $b) {
      \Level1\taintSink($taint);
    }
  }

  public function neIntSimpleGood(): void {
    $a = 0;
    $b = 0;
    $taint = \Level1\taintSource();
    if ($a != $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqFloatSimpleBad(): void {
    $a = 3.14;
    $b = 3.14;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqFloatSimpleGood(): void {
    $a = 3.14;
    $b = 42.0;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function neFloatSimpleBad(): void {
    $a = 3.14;
    $b = 42.0;
    $taint = \Level1\taintSource();
    if ($a != $b) {
      \Level1\taintSink($taint);
    }
  }

  public function neFloatSimpleGood(): void {
    $a = 3.14;
    $b = 3.14;
    $taint = \Level1\taintSource();
    if ($a != $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqBoolSimpleBad(): void {
    $a = true;
    $b = true;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqBoolSimpleGood(): void {
    $a = true;
    $b = false;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function neBoolSimpleBad(): void {
    $a = true;
    $b = false;
    $taint = \Level1\taintSource();
    if ($a != $b) {
      \Level1\taintSink($taint);
    }
  }

  public function neBoolSimpleGood(): void {
    $a = true;
    $b = true;
    $taint = \Level1\taintSource();
    if ($a != $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqObjectSimpleBad(): void {
    $a = new A();
    $b = $a;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FP_cmpEqObjectSimpleGood(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a == new A()) {
      \Level1\taintSink($taint);
    }
  }

  public function neObjectSimpleBad(): void {
    $a = new A();
    $taint = \Level1\taintSource();
    if ($a != new A()) {
      \Level1\taintSink($taint);
    }
  }

  public function neObjectSimpleGood(): void {
    $a = new A();
    $b = $a;
    $taint = \Level1\taintSource();
    if ($a != $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqDictSimpleBad(): void {
    $a = dict['a' => 1, 'b' => 3];
    // just writing $b = dict['a' => 1, 'b' => 3] because the compiler
    // reuses the same pointer.
    $b = $a;
    $b['a'] = 1;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FP_cmpEqDictSimpleGood(): void {
    $a = dict['a' => 1, 'b' => 3];
    $b = dict['a' => 1, 'b' => 4];
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function cmpEqDictNestedBad(): void {
    $a = dict['a' => 1, 'b' => 2];
    $b = $a;
    $a['a'] = $a;
    $b['a'] = $b;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

  public function FP_cmpEqDictNestedGood(): void {
    $a = dict['a' => 1, 'b' => 2];
    $a['a'] = $a;
    $b = $a;
    $b['b'] = 3;
    $taint = \Level1\taintSource();
    if ($a == $b) {
      \Level1\taintSink($taint);
    }
  }

}
