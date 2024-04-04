// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class TypeInUnit2 {

  public static function foo(
    TypeInUnit1 $arg1,
    TypeInUnit3 $missing_source,
  ): void {}

}
