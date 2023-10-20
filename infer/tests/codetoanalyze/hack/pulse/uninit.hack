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

function call_get_field_after_cond_bad(shape('cond' => bool) $x): string {
  if (!$x['cond']) {
    return "abc";
  }
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

  public static function get_field_from_trait2(): string {
    return self::get_field_from_trait();
  }
}

abstract class AbstractUseTrait extends A {
  use MyTrait;

  public static function self_get_field(): string {
    return self::get_field_from_trait();
  }

  public static function self_get_field2(): string {
    return self::get_field_from_trait2();
  }

  public static function call_my_trait_get_field_Bad(): string {
    return MyTrait::get_field_from_trait();
  }
}

function call_self_get_field_Bad(): string {
  return AbstractUseTrait::self_get_field();
}

function call_self_get_field2_Bad(): string {
  return AbstractUseTrait::self_get_field2();
}

function call_my_trait_get_field_Bad(): string {
  return MyTrait::get_field_from_trait();
}

trait MyTraitConst {
  const string TRAIT_CONST_INIT = "defined";

  public static function get_self_const_init(): string {
    return self::TRAIT_CONST_INIT;
  }
}

function call_get_self_const_init_Ok(): string {
  return MyTraitConst::get_self_const_init();
}
