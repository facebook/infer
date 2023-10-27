// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Level1;

function taintSource(): int {
  return 42;
}

function taintSink(mixed $args): void {
}

function basicFlowBad(): void {
  $tainted = taintSource();
  taintSink($tainted);
}

function basicFlowOk(int $untainted): void {
  taintSink($untainted);
}
