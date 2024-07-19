// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Uninit;

<<__ConsistentConstruct>>
abstract class A {
  abstract const string FIELD;

  const string INITIALIZED_FIELD = "hello";

  public static function get_field(): string {
    return static::FIELD;
  }

  public static function get_initialized_field(): string {
    return static::INITIALIZED_FIELD;
  }

  protected function __construct() {
    $_ = self::get_field();
  }

  public static function nop(): void {}

  public static function call_new_static_ok(): void {
    call_nop(); // for __lazy_class_initialize(A)
    $_ = new static();
  }
}

function call_nop(): void {
  A::nop();
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

function call_get_initialized_field_ok(): string {
  return B::get_initialized_field();
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

trait InitFieldTrait1 {
  require extends A;
}

trait InitFieldTrait2 {
  require extends A;

  const string FIELD = "hi";
}

trait InitFieldTrait3 {
  require extends A;
}

class InitFieldByTrait extends A {
  use InitFieldTrait1;
  use InitFieldTrait2;
  use InitFieldTrait3;
}

function init_field_by_trait_get_field_ok(): string {
  return InitFieldByTrait::get_field();
}

interface InitFieldInterface {
  const string FIELD = "abc";
}

trait TInitFieldInterface implements InitFieldInterface {
  public static function get_field(): string {
    return self::FIELD;
  }
}

abstract class CInitFieldInterface {
  use TInitFieldInterface;
}

function init_field_in_interface_ok(): string {
  return CInitFieldInterface::get_field();
}

enum E: int {
  S1 = 1;
  S2 = 2;
}

abstract final class InitEnumFields {
  // The long field name somehow triggers a deduplication of the field name evaluations in hackc,
  // which was needed to reproduce a false positive.
  const E F1_VERY_VERY_VERY_VERY_VERY_VERY_LONG_NAME = E::S1;
  const E F2 = E::S2;

  public static function access_f1(): void {
    $_ = static::F1_VERY_VERY_VERY_VERY_VERY_VERY_LONG_NAME;
  }
}

function call_init_enum_fields_access_f1_ok(): void {
  InitEnumFields::access_f1();
}

trait SetConstInTraitTrait {
  const string FIELD = "defined";
}

class SetConstInTraitClass extends A {
  use SetConstInTraitTrait;
}

function call_get_field_set_in_trait_ok(): string {
  return SetConstInTraitClass::get_field();
}

class SetConstInTraitDeepClass extends SetConstInTraitClass {
}

function call_get_field_set_in_trait_deep_ok(): string {
  return SetConstInTraitDeepClass::get_field();
}

class NonAbstractField {
  const string FIELD = "field";

  public function get_field(): string {
    return self::FIELD;
  }
}

class NonAbstractField2 extends NonAbstractField {}

function call_get_non_abstract_field_ok(): string {
  return (new NonAbstractField2())->get_field();
}
