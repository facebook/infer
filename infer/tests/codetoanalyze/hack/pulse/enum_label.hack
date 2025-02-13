// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace EnumLabel;

enum class E: int {
  int A = 42;
  int B = 42;
}

class EnumLabel {

  public function labelParamBad(\HH\EnumClass\Label<E, int> $label): void {
    $taint = \Level1\taintSource();
    if ($label === E#A) {
      \Level1\taintSink($taint);
    }
  }

  public function nonLabelParamBad(E $value): void {
    $taint = \Level1\taintSource();
    if ($value === E::A) {
      \Level1\taintSink($taint);
    }
  }
}
