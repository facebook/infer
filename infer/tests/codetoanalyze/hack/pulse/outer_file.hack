// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class OuterFileSuper {

  public static function superTaintSource(): int {
    return 42;
  }
}

class OuterFile extends OuterFileSuper {

  public static function taintSource(): int {
    return 42;
  }

  public static function taintSink(int $i): void {
  }

  public static function tainted(): int {
    return self::taintSource();
  }

  public static function untainted(): int {
    return 42;
  }
}
