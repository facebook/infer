// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Uninit;

abstract class A {
  abstract const string FIELD;

  public static function get_field(): string {
    return static::FIELD;
  }
}

function call_get_field_bad(): string {
  return A::get_field();
}
