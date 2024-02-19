// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class ArgIndexMatcher {

  public static function source(): int {
    return \Level1\taintSource();
  }

  public static function sink1(mixed $arg, mixed $tainted): void {}

  public function sink2(mixed $arg, mixed $tainted): void {}
}

class Main {

  public function staticSink(): void {
    $tainted = ArgIndexMatcher::source();
    ArgIndexMatcher::sink1(null, $tainted);
  }

  public function instanceSink(): void {
    $instance = new ArgIndexMatcher();
    $tainted = ArgIndexMatcher::source();
    $instance->sink2(null, $tainted);
  }
}
