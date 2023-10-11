// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace varadic;

function variadic(string $format, mixed ...$args) : void {
}

function main1(string $format, string $x) : void {
  variadic($format, $x);
}

function main2(string $format, string $x, string $y) : void {
  variadic($format, $x, $y);
}
