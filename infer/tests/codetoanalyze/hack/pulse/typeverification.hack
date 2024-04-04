// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class Unsafe {
  public function returnBogusInt(): ~int {
    $r = HH\FIXME\UNSAFE_CAST<string, int>("foo");
    return $r;
  }

  public function returnBogusBool(): ~bool {
    $r = HH\FIXME\UNSAFE_CAST<string, bool>("foo");
    return $r;
  }

  public function returnBogusVec(): ~vec<int> {
    $r = HH\FIXME\UNSAFE_CAST<int, vec<int>>(3);
    return $r;
  }

  public function returnBogusDict(): ~dict<int, string> {
    $r = HH\FIXME\UNSAFE_CAST<string, dict<int, string>>("foo");
    return $r;
  }

  public function maybeBogusString(bool $b): ~string {
    if ($b) {
      $r = HH\FIXME\UNSAFE_CAST<int, string>(42);
    } else {
      $r = "foo";
    }
    return $r;
  }

  // this should be OK because runtime type test prevents call from returning
  public function FP_callBogusIntOK(): void {
    $never_ret = $this->returnBogusInt();
    // note: this call is treated as un unknown call because
    // we use pulse-force-continue option
    $taint = \Level1\taintSource();
    \Level1\taintSink($taint);
  }

  public function FP_callBogusBoolOK(): void {
    $never_ret = $this->returnBogusBool();
    // note: this call is treated as un unknown call because
    // we use pulse-force-continue option
    $taint = \Level1\taintSource();
    \Level1\taintSink($taint);
  }

  public function FP_callBogusVecOK(): void {
    $never_ret = $this->returnBogusVec();
    // note: this call is treated as un unknown call because
    // we use pulse-force-continue option
    $taint = \Level1\taintSource();
    \Level1\taintSink($taint);
  }

  public function FP_callBogusDictOK(): void {
    $never_ret = $this->returnBogusDict();
    // note: this call is treated as un unknown call because
    // we use pulse-force-continue option
    $taint = \Level1\taintSource();
    \Level1\taintSink($taint);
  }

  public function callMaybeBogusStringBad(bool $b): void {
    $maybe_ret = $this->maybeBogusString($b);
    $taint = \Level1\taintSource();
    \Level1\taintSink($taint);
  }
}
