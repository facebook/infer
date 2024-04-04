// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace TrivialVec;

function taintSource(): int {
  return 42;
}

function taintSink(mixed ...$args): void {
}

function seriouslytrivialgood(): void {
  if (3 != 3) {
    taintSink(taintSource());
  }
}

function seriouslytrivialbad(): void {
  if (3 == 3) {
    taintSink(taintSource());
  }
}

function trivial(): void {
  $v = vec[1];
  $x = $v[0];
  if ($x == 0) {
    taintSink(taintSource());
  }
}
