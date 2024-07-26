// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace DictMissingKey;

function simple_ok(): int {
  $d = dict['hi' => 42, 'hello' => 52];
  return $d['hi'];
}

function simple_bad_FN(): int {
  $d = dict['hi' => 42, 'hello' => 52];
  return $d['bye'];
}

function simple_empty_bad_FN(): int {
  $d = dict[];
  return $d['bye'];
}

function simple_assign_bad_FN(): int {
  $d = dict[];
  $d['hi'] = 42;
  $d['hello'] = 52;
  return $d['bye'];
}

function dict_argument(dict<string, int> $d): int {
  return $d['bye'];
}

function call_dict_argument_ok(): int {
  return dict_argument(dict['bye' => 42]);
}

function call_dict_argument_bad(): int {
  return dict_argument(dict['hi' => 42]);
}

function coalesce_ok(): int {
  $d = dict['hi' => 42];
  return $d['bye'] ?? 52;
}

function generates_add_elem_c_constant_ok(): int {
  $key0 = '0';
  $key1 = '1';
  $d = dict[$key0 => 0, $key1 => 1];
  return $d['0'];
}

function generates_add_elem_c_constant_bad_FN(): int {
  $key0 = '0';
  $key1 = '1';
  $d = dict[$key0 => 0, $key1 => 1];
  return $d['2'];
}

function generates_add_elem_c_callee(string $key0): int {
  $key1 = '1';
  $d = dict[$key0 => 0, $key1 => 1];
  return $d['0'];
}

function caller_generates_add_elem_c_callee_ok(): int {
  return generates_add_elem_c_callee('0');
}

function caller_generates_add_elem_c_callee_bad_FN(): int {
  return generates_add_elem_c_callee('2');
}

type MyShapeT = shape(
  ?'hi' => int,
);

function shape_param(MyShapeT $x): int {
  if (Shapes::keyExists($x, 'hi')) {
    return $x['hi'];
  }
  return 0;
}

function call_shape_param_ok(): int {
  return shape_param(shape());
}

function shape_param2(MyShapeT $x): int {
  if (Shapes::idx($x, 'hi') is nonnull) {
    return $x['hi'];
  }
  return 0;
}

function call_shape_param2_ok(): int {
  return shape_param2(shape());
}

function return_shape(bool $b): MyShapeT {
  return shape();
}

function call_return_shape_ok(bool $b): int {
  $x = return_shape($b);
  if (Shapes::keyExists($x, 'hi')) {
    return $x['hi'];
  }
  return 0;
}

function return_shape2(bool $b): shape(?'hi' => int) {
  return shape();
}

function call_return_shape2_ok(bool $b): int {
  $x = return_shape2($b);
  if (Shapes::keyExists($x, 'hi')) {
    return $x['hi'];
  }
  return 0;
}

class ShapeField {
  public shape(?'bye' => int) $f = shape();

  public function read_shape(): int {
    $this->f['bye'] ??= 42;
    return $this->f['bye'];
  }

  public function call_read_shape_ok(): int {
    $this->f = shape();
    return $this->read_shape();
  }
}

function container_param(dict<string, int> $p): int {
  if (C\contains_key($p, 'hi')) {
    return $p['hi'];
  }
  return 42;
}

function call_container_param_ok(): int {
  return container_param(dict[]);
}

class DictField {
  public dict<string, int> $f = dict[];

  public function read_dict_hi(): int {
    return $this->f['hi'];
  }

  public function call_read_dict_hi_ok(): int {
    $this->f = dict['hi' => 42];
    return $this->read_dict_hi();
  }

  public function call_read_dict_hi2_ok(): int {
    $this->f['hi'] = 42;
    return $this->read_dict_hi();
  }

  public function call_read_dict_hi_bad(): int {
    $this->f = dict['bye' => 42];
    return $this->read_dict_hi();
  }
}

class NestedDictField {
  public function __construct(public DictField $g) {}

  public function read_dict_hi(): int {
    return $this->g->f['hi'];
  }

  public function call_read_dict_hi_bad(): int {
    $this->g->f = dict['bye' => 42];
    return $this->read_dict_hi();
  }
}

function suppress_args_param(dict<string, int> $args): int {
  return $args['bye'];
}

function call_suppress_args_param_ok(): int {
  return suppress_args_param(dict['hi' => 42]);
}

class BlockListedField {
  public dict<string, int> $block_listed_field = dict[];

  public function read_dict_hi(): int {
    return $this->block_listed_field['hi'];
  }

  public function call_read_dict_hi_ok(): int {
    $this->block_listed_field = dict['bye' => 42];
    return $this->read_dict_hi();
  }
}

abstract final class ConstKey {
  const string HI = 'hi';

  const string BYE = 'bye';
}

class DictFieldUsingConstKey {
  public dict<string, int> $f = dict[ConstKey::HI => 42];

  public static function read_dict_hi_ok(): int {
    $o = new DictFieldUsingConstKey();
    return $o->f[ConstKey::HI];
  }

  public function read_dict_hi_bad(): int {
    $o = new DictFieldUsingConstKey();
    return $o->f[ConstKey::BYE];
  }
}

abstract final class StaticDictField {
  const dict<string, int> f = dict['hi' => 42];

  public static function read_self_dict_hi(): int {
    return self::f['hi'];
  }

  public static function read_self_dict_bye(): int {
    return self::f['bye'];
  }
}

function call_read_self_dict_hi_ok(): int {
  return StaticDictField::read_self_dict_hi();
}

function call_read_self_dict_bye_bad(): int {
  return StaticDictField::read_self_dict_bye();
}

class InitStaticField {
  const int DUMMY = -1;
  public static bool $b = false;
  public static function nop(): void {}
}

class CallInitStaticField {
  public function call_get_bad(): void {
    InitStaticField::$b = true;
    InitStaticField::nop();
    if (InitStaticField::$b) {
      dict_argument(dict['hi' => 42]);
    }
  }

  public function call_get_ok(): void {
    InitStaticField::$b = true;
    InitStaticField::nop();
    if (!InitStaticField::$b) {
      dict_argument(dict['hi' => 42]);
    }
  }

  public function call_nop(): void {
    InitStaticField::nop();
  }

  public function call_get_interproc_bad(): void {
    InitStaticField::$b = true;
    $this->call_nop();
    if (InitStaticField::$b) {
      dict_argument(dict['hi' => 42]);
    }
  }

  public function call_get_interproc_ok_FP(): void {
    InitStaticField::$b = true;
    $this->call_nop();
    if (!InitStaticField::$b) {
      dict_argument(dict['hi' => 42]);
    }
  }
}
