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

class B extends A {
  const string FIELD = "defined";
}

function call_get_field_ok(): string {
  return B::get_field();
}

abstract class RetField {
  abstract const string ret;

  public static function get_ret_field(): string {
    return static::ret;
  }
}

// Current frontend translates the field name of [ret] as [ret_] in the class declaration, due to
// the reserved word in Textual.
function call_get_ret_field_bad_FN(): string {
  return RetField::get_ret_field();
}

trait MyTrait {
  require extends A;

  public static function get_field_from_trait(): string {
    return static::FIELD;
  }
}

abstract class AbstractUseTrait extends A {
  use MyTrait;

  public static function self_get_field(): string {
    return self::get_field_from_trait();
  }
}

function call_self_get_field_Bad(): string {
  return AbstractUseTrait::self_get_field();
}
