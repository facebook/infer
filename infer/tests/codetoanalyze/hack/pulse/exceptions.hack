// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Exceptions;

final class Exception1 extends \Exception {}

final class Exception2 extends \Exception {}

class ExnTests {

  public function justThrow1OK(): void {
    throw new Exception1();
    $taint = \Level1\taintSource();
    \Level1\taintSink($taint);
  }

  public function justThrow2(): void {
    throw new Exception2();
  }

  public function callJustThrowOK(): void {
    $this->justThrow1OK();
    $taint = \Level1\taintSource();
    \Level1\taintSink($taint);
  }

  public function throwCatchBad(): void {
    try {
      $this->justThrow1OK();
    } catch (Exception1 $e) {
      $taint = \Level1\taintSource();
      \Level1\taintSink($taint);
    }
  }

  public function throwCatchGood(): void {
    try {
      $this->justThrow1OK();
    } catch (Exception2 $e) {
      $taint = \Level1\taintSource();
      \Level1\taintSink($taint);
    }
  }

  public function nested1OK(): void {
    try {
      $this->justThrow1OK();
    } catch (Exception2 $e) {
      $taint = \Level1\taintSource();
      \Level1\taintSink($taint);
    } catch (Exception1 $e) {
      try {
        $this->justThrow2();
      } catch (Exception1 $e) {
        $taint = \Level1\taintSource();
        \Level1\taintSink($taint);
      }
    }
  }

  public function nested2Bad(): void {
    try {
      $this->justThrow1OK();
    } catch (Exception2 $e) {
      $taint = \Level1\taintSource();
      \Level1\taintSink($taint);
    } catch (Exception1 $e) {
      try {
        $this->justThrow2();
      } catch (Exception2 $e) {
        $taint = \Level1\taintSource();
        \Level1\taintSink($taint);
      }
    }
  }

  public function leftNested1OK(): void {
    try {
      try {
        $this->justThrow1OK();
      } catch (Exception1 $e) {
        $this->justThrow2();
      }
      ;
      $taint = \Level1\taintSource();
      \Level1\taintSink($taint);
    } catch (Exception2 $e) {
    }
  }

  public function leftNested2Bad(): void {
    try {
      try {
        $this->justThrow1OK();
      } catch (Exception1 $e) {
        $this->justThrow2();
      }
      ;
      $taint = \Level1\taintSource();
      \Level1\taintSink($taint);
    } catch (Exception2 $e) {
      $taint = \Level1\taintSource();
      \Level1\taintSink($taint);
    } catch (Exception1 $e) {
    }
  }

}
