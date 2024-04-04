// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Booleans;

class A {}
class SensitiveClass {}

function testBoolBad(SensitiveClass $sc): void {
  $b = true;
  if ($b) {
    \Level1\taintSink($sc);
  }
}

function testBoolGood(SensitiveClass $sc): void {
  $b = false;
  if ($b) {
    \Level1\taintSink($sc);
  }
}

function testNullBad(SensitiveClass $sc): void {
  $a = null;
  if (!$a) {
    \Level1\taintSink($sc);
  }
}

function testNullGood(SensitiveClass $sc): void {
  $a = new A();
  if (!$a) {
    \Level1\taintSink($sc);
  }
}

function testNotNullGood(SensitiveClass $sc): void {
  $a = null;
  if ($a) {
    \Level1\taintSink($sc);
  }
}

function testNotNullBad(SensitiveClass $sc): void {
  $a = new A();
  if ($a) {
    \Level1\taintSink($sc);
  }
}
