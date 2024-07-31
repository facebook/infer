// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Unknown;

class SensitiveClass {}

function basicFlowBad(SensitiveClass $sc): void {
  // Although, we don't have the definition of this method we should still match it against taint
  // configs.
  UnknownClass::explicitSinkAllArgs($sc);
}

function basicFlowReturnBad(Unknown $sc): void {
  // Although, we don't have the definition of myUnknownFun, return value should be tainted
  // pessimistically.
  $tainted = \Level1\unknownTaintSource();
  $res = $tainted->myUnknownFun();
  UnknownClass::explicitSinkAllArgs($res);
}

async function genAndUnknownOk(): Awaitable<void> {
  $x = async {
    return 42;
  };
  UnknownClass::mayAwait($x);
  return;
}
