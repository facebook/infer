// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Unknown;

class Unknown {}

class UnknownClass {
  public static function explicitSinkAllArgs(SensitiveClass $sc): void {}
  public static function mayAwait<T>(Awaitable<T> $arg): void {}
}
